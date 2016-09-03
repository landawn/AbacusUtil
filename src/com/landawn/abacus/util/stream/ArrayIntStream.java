package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.IntBinaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.IntToByteFunction;
import com.landawn.abacus.util.function.IntToCharFunction;
import com.landawn.abacus.util.function.IntToDoubleFunction;
import com.landawn.abacus.util.function.IntToFloatFunction;
import com.landawn.abacus.util.function.IntToLongFunction;
import com.landawn.abacus.util.function.IntToShortFunction;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.ObjIntConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 */
final class ArrayIntStream extends IntStream {
    private final int[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Set<Runnable> closeHandlers;

    ArrayIntStream(int[] values) {
        this(values, null);
    }

    ArrayIntStream(int[] values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    ArrayIntStream(int[] values, Collection<Runnable> closeHandlers, boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayIntStream(int[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayIntStream(int[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayIntStream(int[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted) {
        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public IntStream filter(IntPredicate predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream filter(final IntPredicate predicate, final long max) {
        return new ArrayIntStream(N.filter(elements, fromIndex, toIndex, predicate, Stream.toInt(max)), closeHandlers, sorted);
    }

    @Override
    public IntStream takeWhile(IntPredicate predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream takeWhile(IntPredicate predicate, long max) {
        final IntList list = IntList.of(new int[N.min(9, Stream.toInt(max), (toIndex - fromIndex))], 0);

        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
            if (predicate.test(elements[i])) {
                list.add(elements[i]);
                cnt++;
            } else {
                break;
            }
        }

        return new ArrayIntStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public IntStream dropWhile(IntPredicate predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public IntStream dropWhile(IntPredicate predicate, long max) {
        int index = fromIndex;
        while (index < toIndex && predicate.test(elements[index])) {
            index++;
        }

        final IntList list = IntList.of(new int[N.min(9, Stream.toInt(max), (toIndex - index))], 0);
        int cnt = 0;

        while (index < toIndex && cnt < max) {
            list.add(elements[index]);
            index++;
            cnt++;
        }

        return new ArrayIntStream(list.trimToSize().array(), closeHandlers, sorted);
    }

    @Override
    public IntStream map(IntUnaryOperator mapper) {
        final int[] a = new int[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsInt(elements[i]);
        }

        return new ArrayIntStream(a, closeHandlers);
    }

    @Override
    public CharStream mapToChar(IntToCharFunction mapper) {
        final char[] a = new char[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsChar(elements[i]);
        }

        return new ArrayCharStream(a, closeHandlers);
    }

    @Override
    public ByteStream mapToByte(IntToByteFunction mapper) {
        final byte[] a = new byte[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsByte(elements[i]);
        }

        return new ArrayByteStream(a, closeHandlers);
    }

    @Override
    public ShortStream mapToShort(IntToShortFunction mapper) {
        final short[] a = new short[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsShort(elements[i]);
        }

        return new ArrayShortStream(a, closeHandlers);
    }

    @Override
    public LongStream mapToLong(IntToLongFunction mapper) {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsLong(elements[i]);
        }

        return new ArrayLongStream(a, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(IntToFloatFunction mapper) {
        final float[] a = new float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsFloat(elements[i]);
        }

        return new ArrayFloatStream(a, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(IntToDoubleFunction mapper) {
        final double[] a = new double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = mapper.applyAsDouble(elements[i]);
        }

        return new ArrayDoubleStream(a, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final IntFunction<? extends U> mapper) {
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
    public IntStream flatMap(final IntFunction<? extends IntStream> mapper) {
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
    public CharStream flatMapToChar(final IntFunction<? extends CharStream> mapper) {
        //        final List<char[]> listOfArray = new ArrayList<char[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final char[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final char[] arrayOfAll = new char[lengthOfAll];
        //        int from = 0;
        //        for (char[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayCharStream(arrayOfAll, closeHandlers);

        return new IteratorCharStream(new ImmutableCharIterator() {
            private int cursor = fromIndex;
            private ImmutableCharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).charIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public ByteStream flatMapToByte(final IntFunction<? extends ByteStream> mapper) {
        //        final List<byte[]> listOfArray = new ArrayList<byte[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final byte[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final byte[] arrayOfAll = new byte[lengthOfAll];
        //        int from = 0;
        //        for (byte[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayByteStream(arrayOfAll, closeHandlers);

        return new IteratorByteStream(new ImmutableByteIterator() {
            private int cursor = fromIndex;
            private ImmutableByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).byteIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public byte next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream flatMapToShort(final IntFunction<? extends ShortStream> mapper) {
        //        final List<short[]> listOfArray = new ArrayList<short[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final short[] tmp = mapper.apply(elements[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final short[] arrayOfAll = new short[lengthOfAll];
        //        int from = 0;
        //        for (short[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayShortStream(arrayOfAll, closeHandlers);

        return new IteratorShortStream(new ImmutableShortIterator() {
            private int cursor = fromIndex;
            private ImmutableShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).shortIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public short next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public LongStream flatMapToLong(final IntFunction<? extends LongStream> mapper) {
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
    public FloatStream flatMapToFloat(final IntFunction<? extends FloatStream> mapper) {
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
    public DoubleStream flatMapToDouble(final IntFunction<? extends DoubleStream> mapper) {
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
    public <T> Stream<T> flatMapToObj(final IntFunction<? extends Stream<T>> mapper) {
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
    public Stream<IntStream> split(int size) {
        final List<int[]> tmp = N.split(elements, fromIndex, toIndex, size);
        final IntStream[] a = new IntStream[tmp.size()];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = new ArrayIntStream(tmp.get(i), null, sorted);
        }

        return new ArrayStream<IntStream>(a, closeHandlers);
    }

    @Override
    public IntStream distinct() {
        return new ArrayIntStream(N.removeDuplicates(elements, fromIndex, toIndex, sorted), closeHandlers, sorted);
    }

    @Override
    public IntStream top(int n) {
        return top(n, INT_COMPARATOR);
    }

    @Override
    public IntStream top(int n, Comparator<? super Integer> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && (comparator == null || comparator == INT_COMPARATOR)) {
            return new ArrayIntStream(elements, N.max(fromIndex, toIndex - n), toIndex, closeHandlers, sorted);
        } else {
            return new ArrayIntStream(N.top(elements, fromIndex, toIndex, n, comparator), closeHandlers);
        }
    }

    @Override
    public IntStream sorted() {
        if (sorted) {
            return new ArrayIntStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        }

        final int[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayIntStream(a, closeHandlers, true);
    }

    @Override
    public IntStream parallelSorted() {
        if (sorted) {
            return new ArrayIntStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        }

        final int[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a);
        return new ArrayIntStream(a, closeHandlers, true);
    }

    @Override
    public IntStream peek(IntConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new IntStreamImpl(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public IntStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        if (maxSize >= toIndex - fromIndex) {
            return new ArrayIntStream(elements, fromIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayIntStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
        }
    }

    @Override
    public IntStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayIntStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayIntStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(IntConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public int[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public IntList toIntList() {
        return IntList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public int reduce(int identity, IntBinaryOperator op) {
        int result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsInt(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalInt reduce(IntBinaryOperator op) {
        if (count() == 0) {
            return OptionalInt.empty();
        }

        int result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsInt(result, elements[i]);
        }

        return OptionalInt.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator) {
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
    public OptionalInt min() {
        if (count() == 0) {
            return OptionalInt.empty();
        }

        return OptionalInt.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalInt max() {
        if (count() == 0) {
            return OptionalInt.empty();
        }

        return OptionalInt.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalInt kthLargest(int k) {
        if (count() == 0 || k > toIndex - fromIndex) {
            return OptionalInt.empty();
        }

        return OptionalInt.of(N.kthLargest(elements, fromIndex, toIndex, k));
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
    public boolean anyMatch(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalInt findFirst() {
    //        return count() == 0 ? OptionalInt.empty() : OptionalInt.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalInt findFirst(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalInt.of(elements[i]);
            }
        }

        return OptionalInt.empty();
    }

    //    @Override
    //    public OptionalInt findLast() {
    //        return count() == 0 ? OptionalInt.empty() : OptionalInt.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalInt findLast(IntPredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalInt.of(elements[i]);
            }
        }

        return OptionalInt.empty();
    }

    //    @Override
    //    public OptionalInt findAny() {
    //        return count() == 0 ? OptionalInt.empty() : OptionalInt.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalInt findAny(IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalInt.of(elements[i]);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public LongStream asLongStream() {
        final long[] a = new long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            a[j] = elements[i];
        }

        return new ArrayLongStream(a, closeHandlers, sorted);
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
    public Stream<Integer> boxed() {
        return new ArrayStream<Integer>(Array.box(elements, fromIndex, toIndex), closeHandlers);
    }

    @Override
    public Iterator<Integer> iterator() {
        return new ImmutableIterator<Integer>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Integer next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }
        };
    }

    @Override
    ImmutableIntIterator intIterator() {
        return new ImmutableIntIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return elements[cursor++];
            }

            @Override
            public int[] toArray() {
                return N.copyOfRange(elements, cursor, toIndex);
            }
        };
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new ArrayIntStream(elements, fromIndex, toIndex, closeHandlerList, sorted);
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
