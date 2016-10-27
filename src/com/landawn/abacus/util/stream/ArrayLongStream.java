package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.LongBiFunction;
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
final class ArrayLongStream extends AbstractLongStream {
    private final long[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;

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
        super(closeHandlers);

        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
    }

    @Override
    public LongStream filter(final LongPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream filter(final LongPredicate predicate, final long max) {
        // return new ArrayLongStream(N.filter(elements, fromIndex, toIndex, predicate, Stream.toInt(max)), closeHandlers, sorted);

        return new IteratorLongStream(new ImmutableLongIterator() {
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
            public long next() {
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
    public LongStream takeWhile(final LongPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate, final long max) {
        //        final LongList list = LongList.of(new long[N.min(9, Stream.toInt(max), (toIndex - fromIndex))], 0);
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
        //        return new ArrayLongStream(list.trimToSize().array(), closeHandlers, sorted);

        return new IteratorLongStream(new ImmutableLongIterator() {
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
            public long next() {
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
    public LongStream dropWhile(final LongPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate, final long max) {
        //        int cursor = fromIndex;
        //        while (cursor < toIndex && predicate.test(elements[cursor])) {
        //            cursor++;
        //        }
        //
        //        final LongList list = LongList.of(new long[N.min(9, Stream.toInt(max), (toIndex - cursor))], 0);
        //        int cnt = 0;
        //
        //        while (cursor < toIndex && cnt < max) {
        //            list.add(elements[cursor]);
        //            cursor++;
        //            cnt++;
        //        }
        //
        //        return new ArrayLongStream(list.trimToSize().array(), closeHandlers, sorted);

        return new IteratorLongStream(new ImmutableLongIterator() {
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
            public long next() {
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
    public LongStream map(final LongUnaryOperator mapper) {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
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
    public IntStream mapToInt(final LongToIntFunction mapper) {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
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
    public FloatStream mapToFloat(final LongToFloatFunction mapper) {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
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
    public DoubleStream mapToDouble(final LongToDoubleFunction mapper) {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
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
    public LongStream flatMap(final LongFunction<? extends LongStream> mapper) {
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
    public IntStream flatMapToInt(final LongFunction<? extends IntStream> mapper) {
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
    public FloatStream flatMapToFloat(final LongFunction<? extends FloatStream> mapper) {
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
    public DoubleStream flatMapToDouble(final LongFunction<? extends DoubleStream> mapper) {
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
    public Stream<LongStream> split(final int size) {
        //        final List<long[]> tmp = N.split(elements, fromIndex, toIndex, size);
        //        final LongStream[] a = new LongStream[tmp.size()];
        //
        //        for (int i = 0, len = a.length; i < len; i++) {
        //            a[i] = new ArrayLongStream(tmp.get(i), null, sorted);
        //        }
        //
        //        return new ArrayStream<LongStream>(a, closeHandlers);

        return new IteratorStream<LongStream>(new ImmutableIterator<LongStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public LongStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayLongStream(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex), null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<LongStream> split(final LongPredicate predicate) {
        return new IteratorStream<LongStream>(new ImmutableIterator<LongStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public LongStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final LongList result = LongList.of(N.EMPTY_LONG_ARRAY);

                while (cursor < toIndex) {
                    if (predicate.test(elements[cursor])) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {
                        break;
                    }
                }

                return LongStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public LongStream distinct() {
        return new ArrayLongStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public LongStream top(int n) {
        return top(n, Stream.LONG_COMPARATOR);
    }

    @Override
    public LongStream top(int n, Comparator<? super Long> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && Stream.isSameComparator(comparator, Stream.LONG_COMPARATOR)) {
            return new ArrayLongStream(elements, toIndex - n, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayLongStream(N.top(elements, fromIndex, toIndex, n, comparator), closeHandlers, sorted);
        }
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        final long[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayLongStream(a, closeHandlers, true);
    }

    //    @Override
    //    public LongStream parallelSorted() {
    //        if (sorted) {
    //            return this;
    //        }
    //
    //        final long[] a = N.copyOfRange(elements, fromIndex, toIndex);
    //        N.parallelSort(a);
    //        return new ArrayLongStream(a, closeHandlers, true);
    //    }

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
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayLongStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
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

    //    @Override
    //    public boolean forEach2(LongFunction<Boolean> action) {
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            if (action.apply(elements[i]).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

    @Override
    public long[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public LongList toLongList() {
        return LongList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Long> toList() {
        final List<Long> result = new ArrayList<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<Long> toList(Supplier<? extends List<Long>> supplier) {
        final List<Long> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Long> toSet() {
        final Set<Long> result = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Long> toSet(Supplier<? extends Set<Long>> supplier) {
        final Set<Long> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset() {
        final Multiset<Long> result = new Multiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset(Supplier<? extends Multiset<Long>> supplier) {
        final Multiset<Long> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset() {
        final LongMultiset<Long> result = new LongMultiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset(Supplier<? extends LongMultiset<Long>> supplier) {
        final LongMultiset<Long> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K> Map<K, List<Long>> toMap(LongFunction<? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<Long>>>() {
            @Override
            public Map<K, List<Long>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<Long>>> M toMap(LongFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Long, ?, List<Long>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(LongFunction<? extends K> classifier, Collector<Long, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final LongFunction<? extends K> classifier, final Collector<Long, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Long> downstreamAccumulator = downstream.accumulator();
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
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyMapper.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U> Multimap<K, U, List<U>> toMultimap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyMapper.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
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
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalLong min() {
        if (count() == 0) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements[fromIndex]);
        }

        return OptionalLong.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalLong max() {
        if (count() == 0) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements[toIndex - 1]);
        }

        return OptionalLong.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalLong kthLargest(int k) {
        if (count() == 0 || k > toIndex - fromIndex) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements[toIndex - k]);
        }

        return OptionalLong.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public Long sum() {
        return N.sum(elements, fromIndex, toIndex);
    }

    @Override
    public OptionalDouble average() {
        if (count() == 0) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public LongSummaryStatistics summarize() {
        final LongSummaryStatistics result = new LongSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            result.accept(elements[i]);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final LongPredicate predicate) {
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
    public OptionalLong findFirst(final LongPredicate predicate) {
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
    public OptionalLong findLast(final LongPredicate predicate) {
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
    public OptionalLong findAny(final LongPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalLong.of(elements[i]);
            }
        }

        return OptionalLong.empty();
    }

    @Override
    public LongStream except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public LongStream intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new LongPredicate() {
            @Override
            public boolean test(long value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    //    @Override
    //    public LongStream exclude(Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new LongPredicate() {
    //            @Override
    //            public boolean test(long value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }

    @Override
    public FloatStream asFloatStream() {
        //        final float[] a = new float[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = elements[i];
        //        }
        //
        //        return new ArrayFloatStream(a, closeHandlers, sorted);

        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float next() {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public float[] toArray() {
                final float[] a = new float[toIndex - cursor];

                for (int i = cursor, j = 0; i < toIndex; i++, j++) {
                    a[j] = elements[i];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream asDoubleStream() {
        //        final double[] a = new double[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = elements[i];
        //        }
        //
        //        return new ArrayDoubleStream(a, closeHandlers, sorted);

        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public double[] toArray() {
                final double[] a = new double[toIndex - cursor];

                for (int i = cursor, j = 0; i < toIndex; i++, j++) {
                    a[j] = elements[i];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public Stream<Long> boxed() {
        return new IteratorStream<Long>(iterator(), closeHandlers, sorted, sorted ? Stream.LONG_COMPARATOR : null);
    }

    @Override
    public LongStream append(LongStream stream) {
        return LongStream.concat(this, stream);
    }

    @Override
    public LongStream merge(LongStream b, LongBiFunction<Nth> nextSelector) {
        return LongStream.merge(this, b, nextSelector);
    }

    @Override
    public ImmutableIterator<Long> iterator() {
        return new ImmutableIterator<Long>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Long next() {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) Long.valueOf(elements[cursor++]);
                }

                return a;
            }
        };
    }

    @Override
    public ImmutableLongIterator longIterator() {
        return new ImmutableLongIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
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
                cursor = n >= toIndex - cursor ? toIndex : cursor + (int) n;
            }

            @Override
            public long[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public boolean isParallel() {
        return false;
    }

    @Override
    public LongStream sequential() {
        return this;
    }

    @Override
    public LongStream parallel(int maxThreadNum, Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelArrayLongStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitter);
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayLongStream(elements, fromIndex, toIndex, newCloseHandlers, sorted);
    }
}
