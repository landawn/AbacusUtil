package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.DoubleBinaryOperator;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleToFloatFunction;
import com.landawn.abacus.util.function.DoubleToIntFunction;
import com.landawn.abacus.util.function.DoubleToLongFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ArrayDoubleStream extends DoubleStream {
    private final double[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    ArrayDoubleStream(double[] values) {
        this(values, null);
    }

    ArrayDoubleStream(double[] values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    ArrayDoubleStream(double[] values, Collection<Runnable> closeHandlers, boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayDoubleStream(double[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayDoubleStream(double[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayDoubleStream(double[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted) {
        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public DoubleStream filter(DoublePredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate, final long max) {
        return new ArrayDoubleStream(N.filter(elements, fromIndex, toIndex, predicate, Stream.toInt(max)), closeHandlers, sorted);
    }

    @Override
    public DoubleStream takeWhile(DoublePredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream takeWhile(DoublePredicate predicate, long max) {
        final DoubleList list = DoubleList.of(new double[N.min(9, Stream.toInt(max), (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new ArrayDoubleStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public DoubleStream dropWhile(DoublePredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public DoubleStream dropWhile(DoublePredicate predicate, long max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final DoubleList list = DoubleList.of(new double[N.min(9, Stream.toInt(max), (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new ArrayDoubleStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public DoubleStream map(DoubleUnaryOperator mapper) {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsDouble(elements[i]);
        }

        return new ArrayDoubleStream(a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(DoubleToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new ArrayIntStream(a, closeHandlers);
    }

    @Override
    public LongStream mapToLong(DoubleToLongFunction mapper) {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsLong(elements[i]);
        }

        return new ArrayLongStream(a, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(DoubleToFloatFunction mapper) {
        final float[] a = new float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsFloat(elements[i]);
        }

        return new ArrayFloatStream(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final DoubleFunction<? extends U> mapper) {
        //        final Object[] a = new Object[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.apply(elements[i]);
        //        }
        //
        //        return new ArrayStream<U>((U[]) a, closeHandlers);

        return new IteratorStream<U>(new ImmutableIterator<U>() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public U next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.apply(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) mapper.apply(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public DoubleStream flatMap(DoubleFunction<? extends DoubleStream> mapper) {
        final List<double[]> listOfArray = new ArrayList<double[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final double[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final double[] arrayOfAll = new double[lengthOfAll];
        int from = 0;
        for (double[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ArrayDoubleStream(arrayOfAll, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(DoubleFunction<? extends IntStream> mapper) {
        final List<int[]> listOfArray = new ArrayList<int[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final int[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final int[] arrayOfAll = new int[lengthOfAll];
        int from = 0;
        for (int[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ArrayIntStream(arrayOfAll, closeHandlers);
    }

    @Override
    public LongStream flatMapToLong(DoubleFunction<? extends LongStream> mapper) {
        final List<long[]> listOfArray = new ArrayList<long[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final long[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final long[] arrayOfAll = new long[lengthOfAll];
        int from = 0;
        for (long[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ArrayLongStream(arrayOfAll, closeHandlers);
    }

    @Override
    public FloatStream flatMapToFloat(DoubleFunction<? extends FloatStream> mapper) {
        final List<float[]> listOfArray = new ArrayList<float[]>();

        int lengthOfAll = 0;
        for (int i = fromIndex; i < toIndex; i++) {
            final float[] tmp = mapper.apply(elements[i]).toArray();
            lengthOfAll += tmp.length;
            listOfArray.add(tmp);
        }

        final float[] arrayOfAll = new float[lengthOfAll];
        int from = 0;
        for (float[] tmp : listOfArray) {
            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
            from += tmp.length;
        }

        return new ArrayFloatStream(arrayOfAll, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final DoubleFunction<? extends Stream<T>> mapper) {
        //        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        //        int lengthOfAll = 0;
        //
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final Object[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final Object[] arrayOfAll = new Object[lengthOfAll];
        //        int from = 0;
        //
        //        for (Object[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayStream<T>((T[]) arrayOfAll, closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private int cursor = fromIndex;
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public T next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public Stream<DoubleStream> split(int size) {
        final List<double[]> tmp = N.split(elements, fromIndex, toIndex, size);
        final DoubleStream[] a = new DoubleStream[tmp.size()];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = new ArrayDoubleStream(tmp.get(i), null, sorted);
        }

        return new ArrayStream<DoubleStream>(a, closeHandlers);
    }

    @Override
    public DoubleStream distinct() {
        return new ArrayDoubleStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public DoubleStream sorted() {
        if (sorted) {
            return new ArrayDoubleStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        }

        final double[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayDoubleStream(a, closeHandlers, true);
    }

    @Override
    public DoubleStream peek(DoubleConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new DoubleStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public DoubleStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        if (maxSize >= toIndex - fromIndex) {
            return new ArrayDoubleStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayDoubleStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
        }
    }

    @Override
    public DoubleStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayDoubleStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayDoubleStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(DoubleConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public double[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public DoubleList toDoubleList() {
        return DoubleList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public double reduce(double identity, DoubleBinaryOperator op) {
        double result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsDouble(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalDouble reduce(DoubleBinaryOperator op) {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        double result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsDouble(result, elements[i]);
        }

        return OptionalDouble.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalDouble min() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalDouble max() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalDouble kthLargest(int k) {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public Double sum() {
        // return N.sum(elements, fromIndex, toIndex);

        //        double[] summation = collect(() -> new double[3], (ll, d) -> {
        //            Collectors.sumWithCompensation(ll, d);
        //            ll[2] += d;
        //        }, (ll, rr) -> {
        //            Collectors.sumWithCompensation(ll, rr[0]);
        //            Collectors.sumWithCompensation(ll, rr[1]);
        //            ll[2] += rr[2];
        //        });
        //
        //        return Collectors.computeFinalSum(summation);

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[3];
            }
        };

        final ObjDoubleConsumer<double[]> accumulator = new ObjDoubleConsumer<double[]>() {
            @Override
            public void accept(double[] ll, double d) {
                Collectors.sumWithCompensation(ll, d);
                ll[2] += d;
            }
        };

        final BiConsumer<double[], double[]> combiner = new BiConsumer<double[], double[]>() {
            @Override
            public void accept(double[] ll, double[] rr) {
                Collectors.sumWithCompensation(ll, rr[0]);
                Collectors.sumWithCompensation(ll, rr[1]);
                ll[2] += rr[2];
            }
        };

        final double[] summation = collect(supplier, accumulator, combiner);

        return Collectors.computeFinalSum(summation);
    }

    @Override
    public OptionalDouble average() {
        //        if (count() == 0) {
        //            return OptionalDouble.empty();
        //        }
        //
        //        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));

        //        double[] avg = collect(() -> new double[4], (ll, d) -> {
        //            ll[2]++;
        //            Collectors.sumWithCompensation(ll, d);
        //            ll[3] += d;
        //        }, (ll, rr) -> {
        //            Collectors.sumWithCompensation(ll, rr[0]);
        //            Collectors.sumWithCompensation(ll, rr[1]);
        //            ll[2] += rr[2];
        //            ll[3] += rr[3];
        //        });
        //        
        //        return avg[2] > 0 ? OptionalDouble.of(Collectors.computeFinalSum(avg) / avg[2]) : OptionalDouble.empty();

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[4];
            }
        };

        final ObjDoubleConsumer<double[]> accumulator = new ObjDoubleConsumer<double[]>() {
            @Override
            public void accept(double[] ll, double d) {
                ll[2]++;
                Collectors.sumWithCompensation(ll, d);
                ll[3] += d;
            }
        };

        final BiConsumer<double[], double[]> combiner = new BiConsumer<double[], double[]>() {
            @Override
            public void accept(double[] ll, double[] rr) {
                Collectors.sumWithCompensation(ll, rr[0]);
                Collectors.sumWithCompensation(ll, rr[1]);
                ll[2] += rr[2];
                ll[3] += rr[3];
            }
        };

        final double[] avg = collect(supplier, accumulator, combiner);

        return avg[2] > 0 ? OptionalDouble.of(Collectors.computeFinalSum(avg) / avg[2]) : OptionalDouble.empty();
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public boolean anyMatch(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalDouble findFirst() {
    //        return count() == 0 ? OptionalDouble.empty() : OptionalDouble.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalDouble findFirst(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalDouble.of(elements[i]);
            }
        }

        return OptionalDouble.empty();
    }

    //    @Override
    //    public OptionalDouble findLast() {
    //        return count() == 0 ? OptionalDouble.empty() : OptionalDouble.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalDouble findLast(DoublePredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalDouble.of(elements[i]);
            }
        }

        return OptionalDouble.empty();
    }

    //    @Override
    //    public OptionalDouble findAny() {
    //        return count() == 0 ? OptionalDouble.empty() : OptionalDouble.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalDouble findAny(DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalDouble.of(elements[i]);
            }
        }

        return OptionalDouble.empty();
    }

    @Override
    public Stream<Double> boxed() {
        return new ArrayStream<Double>(Array.box(elements, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Double> iterator() {
        return new ImmutableIterator<Double>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Double next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }
        };
    }

    @Override
    ImmutableDoubleIterator doubleIterator() {
        return new ImmutableDoubleIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public double[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new ArrayDoubleStream(elements, fromIndex, toIndex, closeHandlerList, sorted);
    }

    @Override
    public void close() {
        if (N.notNullOrEmpty(closeHandlers)) {
            RuntimeException ex = null;

            for (Runnable closeHandler : closeHandlers) {
                try {
                    closeHandler.run();
                } catch (RuntimeException e) {
                    if (ex == null) {
                        ex = e;
                    } else {
                        ex.addSuppressed(e);
                    }
                }
            }

            if (ex != null) {
                throw ex;
            }
        }
    }
}
