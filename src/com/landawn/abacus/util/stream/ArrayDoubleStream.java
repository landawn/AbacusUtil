package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
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
final class ArrayDoubleStream extends AbstractDoubleStream {
    private final double[] elements;
    private final int fromIndex;
    private final int toIndex;

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
        super(closeHandlers, sorted);

        checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate, final long max) {
        // return new ArrayDoubleStream(N.filter(elements, fromIndex, toIndex, predicate, toInt(max)), closeHandlers, sorted);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex && cnt < max) {
                    do {
                        if (predicate.test(elements[cursor])) {
                            hasNext = true;
                            break;
                        } else {
                            cursor++;
                        }
                    } while (cursor < toIndex);
                }

                return hasNext;
            }

            @Override
            public double next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream takeWhile(final DoublePredicate predicate, final long max) {
        //        final DoubleList list = DoubleList.of(new double[N.min(9, toInt(max), (toIndex - fromIndex))], 0);
        //
        //        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
        //            if (predicate.test(elements[i])) {
        //                list.add(elements[i]);
        //                cnt++;
        //            } else {
        //                break;
        //            }
        //        }
        //
        //        return new ArrayDoubleStream(list.trimToSize().array(), closeHandlers, sorted);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex && cnt < max) {
                    do {
                        if (predicate.test(elements[cursor])) {
                            hasNext = true;
                            break;
                        } else {
                            cursor = Integer.MAX_VALUE;
                        }
                    } while (cursor < toIndex);
                }

                return hasNext;
            }

            @Override
            public double next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate, final long max) {
        //        int cursor = fromIndex;
        //        while (cursor < toIndex && predicate.test(elements[cursor])) {
        //            cursor++;
        //        }
        //
        //        final DoubleList list = DoubleList.of(new double[N.min(9, toInt(max), (toIndex - cursor))], 0);
        //        int cnt = 0;
        //
        //        while (cursor < toIndex && cnt < max) {
        //            list.add(elements[cursor]);
        //            cursor++;
        //            cnt++;
        //        }
        //
        //        return new ArrayDoubleStream(list.trimToSize().array(), closeHandlers, sorted);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private long cnt = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex && cnt < max) {
                    if (dropped == false) {
                        do {
                            if (predicate.test(elements[cursor]) == false) {
                                hasNext = true;
                                break;
                            } else {
                                cursor++;
                            }
                        } while (cursor < toIndex);

                        dropped = true;
                    } else {
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public double next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream map(final DoubleUnaryOperator mapper) {
        //        final double[] a = new double[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsDouble(elements[i]);
        //        }
        //
        //        return new ArrayDoubleStream(a, closeHandlers);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsDouble(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
            }

            @Override
            public double[] toArray() {
                final double[] a = new double[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsDouble(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final DoubleToIntFunction mapper) {
        //        final int[] a = new int[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsInt(elements[i]);
        //        }
        //
        //        return new ArrayIntStream(a, closeHandlers);

        return new IteratorIntStream(new ImmutableIntIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsInt(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
            }

            @Override
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsInt(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final DoubleToLongFunction mapper) {
        //        final long[] a = new long[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsLong(elements[i]);
        //        }
        //
        //        return new ArrayLongStream(a, closeHandlers);

        return new IteratorLongStream(new ImmutableLongIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsLong(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
            }

            @Override
            public long[] toArray() {
                final long[] a = new long[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsLong(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final DoubleToFloatFunction mapper) {
        //        final float[] a = new float[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsFloat(elements[i]);
        //        }
        //
        //        return new ArrayFloatStream(a, closeHandlers);

        return new IteratorFloatStream(new ImmutableFloatIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsFloat(elements[cursor++]);
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
            }

            @Override
            public float[] toArray() {
                final float[] a = new float[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsFloat(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
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
                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
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
    public DoubleStream flatMap(final DoubleFunction<? extends DoubleStream> mapper) {
        //        final List<double[]> listOfArray = new ArrayList<double[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final double[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final double[] arrayOfAll = new double[lengthOfAll];
        //        int from = 0;
        //        for (double[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayDoubleStream(arrayOfAll, closeHandlers);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private int cursor = fromIndex;
            private ImmutableDoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).doubleIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(final DoubleFunction<? extends IntStream> mapper) {
        //        final List<int[]> listOfArray = new ArrayList<int[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final int[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final int[] arrayOfAll = new int[lengthOfAll];
        //        int from = 0;
        //        for (int[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayIntStream(arrayOfAll, closeHandlers);

        return new IteratorIntStream(new ImmutableIntIterator() {
            private int cursor = fromIndex;
            private ImmutableIntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).intIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public LongStream flatMapToLong(final DoubleFunction<? extends LongStream> mapper) {
        //        final List<long[]> listOfArray = new ArrayList<long[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final long[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final long[] arrayOfAll = new long[lengthOfAll];
        //        int from = 0;
        //        for (long[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayLongStream(arrayOfAll, closeHandlers);

        return new IteratorLongStream(new ImmutableLongIterator() {
            private int cursor = fromIndex;
            private ImmutableLongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).longIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public long next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public FloatStream flatMapToFloat(final DoubleFunction<? extends FloatStream> mapper) {
        //        final List<float[]> listOfArray = new ArrayList<float[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final float[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final float[] arrayOfAll = new float[lengthOfAll];
        //        int from = 0;
        //        for (float[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayFloatStream(arrayOfAll, closeHandlers);

        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private int cursor = fromIndex;
            private ImmutableFloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).floatIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public float next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
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
    public Stream<DoubleStream> split(final int size) {
        //        final List<double[]> tmp = N.split(elements, fromIndex, toIndex, size);
        //        final DoubleStream[] a = new DoubleStream[tmp.size()];
        //
        //        for (int i = 0, len = a.length; i < len; i++) {
        //            a[i] = new ArrayDoubleStream(tmp.get(i), null, sorted);
        //        }
        //
        //        return new ArrayStream<DoubleStream>(a, closeHandlers);

        return new IteratorStream<DoubleStream>(new ImmutableIterator<DoubleStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayDoubleStream(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex), null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<DoubleStream> split(final U boundary, final BiFunction<? super Double, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<DoubleStream>(new ImmutableIterator<DoubleStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final DoubleList result = DoubleList.of(N.EMPTY_DOUBLE_ARRAY);

                while (cursor < toIndex) {
                    if (predicate.apply(elements[cursor], boundary)) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }
                        break;
                    }
                }

                return DoubleStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public DoubleStream distinct() {
        return new ArrayDoubleStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public DoubleStream top(int n) {
        return top(n, DOUBLE_COMPARATOR);
    }

    @Override
    public DoubleStream top(int n, Comparator<? super Double> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, DOUBLE_COMPARATOR)) {
            return new ArrayDoubleStream(elements, toIndex - n, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayDoubleStream(N.top(elements, fromIndex, toIndex, n, comparator), closeHandlers, sorted);
        }
    }

    @Override
    public DoubleStream sorted() {
        if (sorted) {
            return this;
        }

        final double[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayDoubleStream(a, closeHandlers, true);
    }

    //    @Override
    //    public DoubleStream parallelSorted() {
    //        if (sorted) {
    //            return this;
    //        }
    //
    //        final double[] a = N.copyOfRange(elements, fromIndex, toIndex);
    //        N.parallelSort(a);
    //        return new ArrayDoubleStream(a, closeHandlers, true);
    //    }

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
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayDoubleStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
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

    //    @Override
    //    public boolean forEach2(DoubleFunction<Boolean> action) {
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            if (action.apply(elements[i]).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

    @Override
    public double[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public DoubleList toDoubleList() {
        return DoubleList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Double> toList() {
        final List<Double> result = new ArrayList<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<Double> toList(Supplier<? extends List<Double>> supplier) {
        final List<Double> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Double> toSet() {
        final Set<Double> result = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Double> toSet(Supplier<? extends Set<Double>> supplier) {
        final Set<Double> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset() {
        final Multiset<Double> result = new Multiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset(Supplier<? extends Multiset<Double>> supplier) {
        final Multiset<Double> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset() {
        final LongMultiset<Double> result = new LongMultiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset(Supplier<? extends LongMultiset<Double>> supplier) {
        final LongMultiset<Double> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Double> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = N.requireNonNull(classifier.apply(elements[i]), "element cannot be mapped to a null key");
            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, elements[i]);
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        Collectors.replaceAll(intermediate, function);

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyMapper.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(DoubleFunction<? extends K> keyMapper, DoubleFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyMapper.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
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
        } else if (sorted) {
            return OptionalDouble.of(elements[fromIndex]);
        }

        return OptionalDouble.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalDouble max() {
        if (count() == 0) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements[toIndex - 1]);
        }

        return OptionalDouble.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalDouble kthLargest(int k) {
        if (count() == 0 || k > toIndex - fromIndex) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements[toIndex - k]);
        }

        return OptionalDouble.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public DoubleStream reverse() {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public double next() {
                if (cursor <= fromIndex) {
                    throw new NoSuchElementException();
                }
                return elements[--cursor];
            }

            @Override
            public long count() {
                return cursor - fromIndex;
            }

            @Override
            public void skip(long n) {
                cursor = cursor - fromIndex > n ? cursor - (int) n : fromIndex;
            }
        }, closeHandlers);
    }

    @Override
    public DoubleSummaryStatistics summarize() {
        final DoubleSummaryStatistics result = new DoubleSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            result.accept(elements[i]);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final DoublePredicate predicate) {
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
    public OptionalDouble findFirst(final DoublePredicate predicate) {
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
    public OptionalDouble findLast(final DoublePredicate predicate) {
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
    public OptionalDouble findAny(final DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalDouble.of(elements[i]);
            }
        }

        return OptionalDouble.empty();
    }

    //    @Override
    //    public DoubleStream exclude(Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new DoublePredicate() {
    //            @Override
    //            public boolean test(double value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    @Override
    public Stream<Double> boxed() {
        return new IteratorStream<Double>(iterator(), closeHandlers, sorted, sorted ? DOUBLE_COMPARATOR : null);
    }

    @Override
    public ImmutableIterator<Double> iterator() {
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

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) Double.valueOf(elements[cursor++]);
                }

                return a;
            }
        };
    }

    @Override
    public ImmutableDoubleIterator doubleIterator() {
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
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = toIndex - cursor > n ? cursor + (int) n : toIndex;
            }

            @Override
            public double[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public DoubleStream parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelArrayDoubleStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayDoubleStream(elements, fromIndex, toIndex, newCloseHandlers, sorted);
    }
}
