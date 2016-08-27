package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.LongBinaryOperator;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.LongToDoubleFunction;
import com.landawn.abacus.util.function.LongToFloatFunction;
import com.landawn.abacus.util.function.LongToIntFunction;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ArrayLongStream extends LongStream {
    private final long[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    ArrayLongStream(long[] values) {
        this(values, null);
    }

    ArrayLongStream(long[] values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    ArrayLongStream(long[] values, Collection<Runnable> closeHandlers, boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayLongStream(long[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayLongStream(long[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayLongStream(long[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted) {
        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public LongStream filter(LongPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream filter(final LongPredicate predicate, final long max) {
        return new ArrayLongStream(N.filter(elements, fromIndex, toIndex, predicate, Stream.toInt(max)), closeHandlers, sorted);
    }

    @Override
    public LongStream takeWhile(LongPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream takeWhile(LongPredicate predicate, long max) {
        final LongList list = LongList.of(new long[N.min(9, Stream.toInt(max), (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new ArrayLongStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public LongStream dropWhile(LongPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream dropWhile(LongPredicate predicate, long max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final LongList list = LongList.of(new long[N.min(9, Stream.toInt(max), (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new ArrayLongStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public LongStream map(LongUnaryOperator mapper) {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsLong(elements[i]);
        }

        return new ArrayLongStream(a, closeHandlers);
    }

    @Override
    public IntStream mapToInt(LongToIntFunction mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new ArrayIntStream(a, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(LongToFloatFunction mapper) {
        final float[] a = new float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsFloat(elements[i]);
        }

        return new ArrayFloatStream(a, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(LongToDoubleFunction mapper) {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsDouble(elements[i]);
        }

        return new ArrayDoubleStream(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
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
    public LongStream flatMap(LongFunction<? extends LongStream> mapper) {
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
    public IntStream flatMapToInt(LongFunction<? extends IntStream> mapper) {
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
    public FloatStream flatMapToFloat(LongFunction<? extends FloatStream> mapper) {
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
    public DoubleStream flatMapToDouble(LongFunction<? extends DoubleStream> mapper) {
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
    public <T> Stream<T> flatMapToObj(final LongFunction<? extends Stream<T>> mapper) {
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
    public Stream<LongStream> split(int size) {
        final List<long[]> tmp = N.split(elements, fromIndex, toIndex, size);
        final LongStream[] a = new LongStream[tmp.size()];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = new ArrayLongStream(tmp.get(i), null, sorted);
        }

        return new ArrayStream<LongStream>(a, closeHandlers);
    }

    @Override
    public LongStream distinct() {
        return new ArrayLongStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return new ArrayLongStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        }

        final long[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayLongStream(a, closeHandlers, true);
    }

    @Override
    public LongStream peek(LongConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new LongStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public LongStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        if (maxSize >= toIndex - fromIndex) {
            return new ArrayLongStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayLongStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
        }
    }

    @Override
    public LongStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayLongStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayLongStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(LongConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public long[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public LongList toLongList() {
        return LongList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public long reduce(long identity, LongBinaryOperator op) {
        long result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsLong(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalLong reduce(LongBinaryOperator op) {
        if (count() == 0) {
            return OptionalLong.empty();
        }

        long result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsLong(result, elements[i]);
        }

        return OptionalLong.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public Long sum() {
        return N.sum(elements, fromIndex, toIndex);
    }

    @Override
    public OptionalLong min() {
        if (count() == 0) {
            return OptionalLong.empty();
        }

        return OptionalLong.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalLong max() {
        if (count() == 0) {
            return OptionalLong.empty();
        }

        return OptionalLong.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalLong kthLargest(int k) {
        if (count() == 0) {
            return OptionalLong.empty();
        }

        return OptionalLong.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public OptionalDouble average() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));
    }

    @Override
    public boolean anyMatch(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalLong findFirst() {
    //        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalLong findFirst(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalLong.of(elements[i]);
            }
        }

        return OptionalLong.empty();
    }

    //    @Override
    //    public OptionalLong findLast() {
    //        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalLong findLast(LongPredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalLong.of(elements[i]);
            }
        }

        return OptionalLong.empty();
    }

    //    @Override
    //    public OptionalLong findAny() {
    //        return count() == 0 ? OptionalLong.empty() : OptionalLong.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalLong findAny(LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalLong.of(elements[i]);
            }
        }

        return OptionalLong.empty();
    }

    @Override
    public FloatStream asFloatStream() {
        final float[] a = new float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = elements[i];
        }

        return new ArrayFloatStream(a, closeHandlers, sorted);
    }

    @Override
    public DoubleStream asDoubleStream() {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = elements[i];
        }

        return new ArrayDoubleStream(a, closeHandlers, sorted);
    }

    @Override
    public Stream<Long> boxed() {
        return new ArrayStream<Long>(Array.box(elements, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Long> iterator() {
        return new ImmutableIterator<Long>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Long next() {
                return elements[cursor++];
            }
        };
    }

    @Override
    ImmutableLongIterator longIterator() {
        return new ImmutableLongIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
                return elements[cursor++];
            }

            @Override
            public long[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new ArrayLongStream(elements, fromIndex, toIndex, closeHandlerList, sorted);
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
