package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.PermutationIterator;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 */
final class ArrayStream<T> extends AbstractStream<T> {
    private final T[] elements;
    private final int fromIndex;
    private final int toIndex;

    ArrayStream(T[] values) {
        this(values, null);
    }

    ArrayStream(T[] values, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ArrayStream(T[] values, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator) {
        this(values, 0, values.length, closeHandlers, sorted, comparator);
    }

    ArrayStream(T[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayStream(T[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false, null);
    }

    ArrayStream(T[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator) {
        super(closeHandlers, sorted, comparator);

        checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate, final long max) {
        //        final ObjectList<T> list = ObjectList.of((T[]) N.newArray(elements.getClass().getComponentType(), N.min(9, toInt(max), (toIndex - fromIndex))),
        //                0);
        //
        //        for (int i = fromIndex, cnt = 0; i < toIndex && cnt < max; i++) {
        //            if (predicate.test(elements[i])) {
        //                list.add(elements[i]);
        //                cnt++;
        //            }
        //        }
        //
        //        return new ArrayStream<T>(list.trimToSize().array(), sorted, cmp, closeHandlers);

        // return new IteratorStream<T>(this.iterator(), closeHandlers, sorted, cmp).filter(predicate, max);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
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
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate, final long max) {
        //        final ObjectList<T> list = ObjectList.of((T[]) N.newArray(elements.getClass().getComponentType(), N.min(9, toInt(max), (toIndex - fromIndex))),
        //                0);
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
        //        return new ArrayStream<T>(list.trimToSize().array(), sorted, cmp, closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
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
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate, final long max) {
        //        int index = fromIndex;
        //        while (index < toIndex && predicate.test(elements[index])) {
        //            index++;
        //        }
        //
        //        final ObjectList<T> list = ObjectList.of((T[]) N.newArray(elements.getClass().getComponentType(), N.min(9, toInt(max), (toIndex - index))), 0);
        //        int cnt = 0;
        //
        //        while (index < toIndex && cnt < max) {
        //            list.add(elements[index]);
        //            index++;
        //            cnt++;
        //        }
        //
        //        return new ArrayStream<T>(list.trimToSize().array(), sorted, cmp, closeHandlers);

        return new IteratorStream<T>(new ImmutableIterator<T>() {
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
            public T next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                cnt++;
                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public <R> Stream<R> map(final Function<? super T, ? extends R> mapper) {
        //        final Object[] a = new Object[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.apply(values[i]);
        //        }
        //
        //        return new ArrayStream<R>((R[]) a, closeHandlers);

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public R next() {
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
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        //        final char[] a = new char[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsChar(elements[i]);
        //        }
        //
        //        return new ArrayCharStream(a, closeHandlers);

        return new IteratorCharStream(new ImmutableCharIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsChar(elements[cursor++]);
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
            public char[] toArray() {
                final char[] a = new char[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsChar(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        //        final byte[] a = new byte[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsByte(elements[i]);
        //        }
        //
        //        return new ArrayByteStream(a, closeHandlers);

        return new IteratorByteStream(new ImmutableByteIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public byte next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsByte(elements[cursor++]);
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
            public byte[] toArray() {
                final byte[] a = new byte[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsByte(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        //        final short[] a = new short[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsShort(elements[i]);
        //        }
        //
        //        return new ArrayShortStream(a, closeHandlers);

        return new IteratorShortStream(new ImmutableShortIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsShort(elements[cursor++]);
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
            public short[] toArray() {
                final short[] a = new short[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsShort(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
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
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
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
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
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
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        //        final double[] a = new double[toIndex - fromIndex];
        //
        //        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
        //            a[j] = mapper.applyAsDouble(elements[i]);
        //        }
        //
        //        return new DoubleStreamImpl(a, closeHandlers);

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
    <R> Stream<R> flatMap4(final Function<? super T, ? extends Iterator<? extends R>> mapper) {
        //        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final Object[] tmp = mapper.apply(values[i]).toArray();
        //            lengthOfAll += tmp.length;
        //            listOfArray.add(tmp);
        //        }
        //
        //        final Object[] arrayOfAll = new Object[lengthOfAll];
        //        int from = 0;
        //        for (Object[] tmp : listOfArray) {
        //            N.copy(tmp, 0, arrayOfAll, from, tmp.length);
        //            from += tmp.length;
        //        }
        //
        //        return new ArrayStream<R>((R[]) arrayOfAll, closeHandlers);

        return new IteratorStream<R>(new ImmutableIterator<R>() {
            private int cursor = fromIndex;
            private Iterator<? extends R> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public R next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    CharStream flatMapToChar4(final Function<? super T, CharIterator> mapper) {
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
            private CharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
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
    ByteStream flatMapToByte4(final Function<? super T, ByteIterator> mapper) {
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
            private ByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
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
    ShortStream flatMapToShort4(final Function<? super T, ShortIterator> mapper) {
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
            private ShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
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
    IntStream flatMapToInt4(final Function<? super T, IntIterator> mapper) {
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
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
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
    LongStream flatMapToLong4(final Function<? super T, LongIterator> mapper) {
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
            private LongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
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
    FloatStream flatMapToFloat4(final Function<? super T, FloatIterator> mapper) {
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
            private FloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
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
    DoubleStream flatMapToDouble4(final Function<? super T, DoubleIterator> mapper) {
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
            private DoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
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
    public Stream<Stream<T>> split(final int size) {
        //        final List<T[]> tmp = N.split(elements, fromIndex, toIndex, size);
        //        final Stream<T>[] a = new Stream[tmp.size()];
        //
        //        for (int i = 0, len = a.length; i < len; i++) {
        //            a[i] = new ArrayStream<T>(tmp.get(i), null, sorted, cmp);
        //        }
        //
        //        return new ArrayStream<Stream<T>>(a, closeHandlers);

        return new IteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayStream<T>(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex), null, sorted, cmp);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<List<T>> splitIntoList(final int size) {
        //        final List<T[]> tmp = N.split(elements, fromIndex, toIndex, size);
        //        final List<T>[] lists = new List[tmp.size()];
        //
        //        for (int i = 0, len = lists.length; i < len; i++) {
        //            lists[i] = N.asList(tmp.get(i));
        //        }
        //
        //        return new ArrayStream<List<T>>(lists, closeHandlers);

        return new IteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return Arrays.asList(N.copyOfRange(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex)));
            }

        }, closeHandlers);
    }

    @Override
    public Stream<Set<T>> splitIntoSet(final int size) {
        //        final List<T[]> tmp = N.split(elements, fromIndex, toIndex, size);
        //        final Set<T>[] sets = new Set[tmp.size()];
        //
        //        for (int i = 0, len = sets.length; i < len; i++) {
        //            sets[i] = N.asLinkedHashSet(tmp.get(i));
        //        }
        //
        //        return new ArrayStream<Set<T>>(sets, closeHandlers);

        return new IteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Set<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final Set<T> set = new HashSet<>(toIndex - cursor > size ? size : toIndex - cursor);

                for (int i = cursor, to = (cursor = toIndex - cursor > size ? cursor + size : toIndex); i < to; i++) {
                    set.add(elements[i]);
                }

                return set;
            }

        }, closeHandlers);
    }

    //    @Override
    //    public Stream<Stream<T>> split(final Predicate<? super T> predicate) {
    //        return new IteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {
    //            private int cursor = fromIndex;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return cursor < toIndex;
    //            }
    //
    //            @Override
    //            public Stream<T> next() {
    //                if (cursor >= toIndex) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                final List<T> result = new ArrayList<>();
    //
    //                while (cursor < toIndex) {
    //                    if (predicate.test(elements[cursor])) {
    //                        result.add(elements[cursor]);
    //                        cursor++;
    //                    } else {
    //                        break;
    //                    }
    //                }
    //
    //                return Stream.of(result);
    //            }
    //
    //        }, closeHandlers);
    //    }
    //
    //    @Override
    //    public Stream<List<T>> splitIntoList(final Predicate<? super T> predicate) {
    //        return new IteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
    //            private int cursor = fromIndex;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return cursor < toIndex;
    //            }
    //
    //            @Override
    //            public List<T> next() {
    //                if (cursor >= toIndex) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                final List<T> result = new ArrayList<>();
    //
    //                while (cursor < toIndex) {
    //                    if (predicate.test(elements[cursor])) {
    //                        result.add(elements[cursor]);
    //                        cursor++;
    //                    } else {
    //                        break;
    //                    }
    //                }
    //
    //                return result;
    //            }
    //
    //        }, closeHandlers);
    //    }
    //
    //    @Override
    //    public Stream<Set<T>> splitIntoSet(final Predicate<? super T> predicate) {
    //        return new IteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
    //            private int cursor = fromIndex;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return cursor < toIndex;
    //            }
    //
    //            @Override
    //            public Set<T> next() {
    //                if (cursor >= toIndex) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                final Set<T> result = new HashSet<>();
    //
    //                while (cursor < toIndex) {
    //                    if (predicate.test(elements[cursor])) {
    //                        result.add(elements[cursor]);
    //                        cursor++;
    //                    } else {
    //                        break;
    //                    }
    //                }
    //
    //                return result;
    //            }
    //
    //        }, closeHandlers);
    //    }

    @Override
    public <U> Stream<Stream<T>> split(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>();

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

                return Stream.of(result);
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<List<T>> splitIntoList(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>();

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

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<Set<T>> splitIntoSet(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Set<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final Set<T> result = new HashSet<>();

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

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<T> distinct() {
        return new ArrayStream<T>(N.distinct(elements, fromIndex, toIndex), closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyMapper) {
        final T[] a = N.distinct(elements, fromIndex, toIndex, keyMapper);
        return new ArrayStream<T>(a, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> top(int n) {
        return top(n, OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> top(int n, Comparator<? super T> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, cmp)) {
            return new ArrayStream<T>(elements, toIndex - n, toIndex, closeHandlers, sorted, cmp);
        } else {
            return new ArrayStream<T>(N.top(elements, fromIndex, toIndex, n, comparator), closeHandlers, sorted, cmp);
        }
    }

    @Override
    public Stream<T> sorted() {
        return sorted(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> sorted(Comparator<? super T> comparator) {
        if (sorted && isSameComparator(comparator, cmp)) {
            return this;
        }

        final T[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a, comparator);
        return new ArrayStream<T>(a, closeHandlers, true, comparator);
    }

    //    @Override
    //    public Stream<T> parallelSorted() {
    //        return parallelSorted(OBJECT_COMPARATOR);
    //    }
    //
    //    @Override
    //    public Stream<T> parallelSorted(Comparator<? super T> comparator) {
    //        if (sorted && isSameComparator(comparator, cmp)) {
    //            return new ArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, comparator);
    //        }
    //
    //        final T[] a = N.copyOfRange(elements, fromIndex, toIndex);
    //        N.parallelSort(a, comparator);
    //        return new ArrayStream<T>(a, closeHandlers, true, comparator);
    //    }

    @Override
    public Stream<T> peek(Consumer<? super T> action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }

        // return new ArrayStream<T>(values, fromIndex, toIndex, sorted, closeHandlers);
        return this;
    }

    @Override
    public Stream<T> limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayStream<T>(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayStream<T>(elements, toIndex, toIndex, closeHandlers, sorted, cmp);
        } else {
            return new ArrayStream<T>(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted, cmp);
        }
    }

    @Override
    public void forEach(Consumer<? super T> action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public <U> U forEach(U identity, BiFunction<U, ? super T, U> accumulator, Predicate<? super U> till) {
        U result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);

            if (till.test(result)) {
                break;
            }
        }

        return result;
    }

    //    @Override
    //    public boolean forEach2(Function<? super T, Boolean> action) {
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            if (action.apply(elements[i]).booleanValue() == false) {
    //                return false;
    //            }
    //        }
    //
    //        return true;
    //    }

    @Override
    public Object[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    <A> A[] toArray(A[] a) {
        if (a.length < (toIndex - fromIndex)) {
            a = N.newArray(a.getClass().getComponentType(), toIndex - fromIndex);
        }

        N.copy(elements, fromIndex, a, 0, toIndex - fromIndex);

        return a;
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        return toArray(generator.apply(toIndex - fromIndex));
    }

    @Override
    public <A> ObjectList<A> toObjectList(Class<A> cls) {
        return ObjectList.of(toArray((A[]) N.newArray(cls, toIndex - fromIndex)));
    }

    @Override
    public List<T> toList() {
        final List<T> result = new ArrayList<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<T> toList(Supplier<? extends List<T>> supplier) {
        final List<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<T> toSet() {
        final Set<T> result = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<T> toSet(Supplier<? extends Set<T>> supplier) {
        final Set<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier) {
        final Multiset<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset() {
        final LongMultiset<T> result = new LongMultiset<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier) {
        final LongMultiset<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
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
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyMapper.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyMapper.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
    }

    @Override
    public T reduce(T identity, BinaryOperator<T> accumulator) {
        T result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalNullable<T> reduce(BinaryOperator<T> accumulator) {
        if (count() == 0) {
            OptionalNullable.empty();
        }

        T result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);
        }

        return OptionalNullable.of(result);
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner) {
        U result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public <R, A> R collect(Collector<? super T, A, R> collector) {
        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(container, elements[i]);
        }

        return collector.finisher().apply(container);
    }

    @Override
    public OptionalNullable<T> min(Comparator<? super T> comparator) {
        if (count() == 0) {
            return OptionalNullable.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return OptionalNullable.of(elements[fromIndex]);
        }

        return OptionalNullable.of(N.min(elements, fromIndex, toIndex, comparator));
    }

    @Override
    public OptionalNullable<T> max(Comparator<? super T> comparator) {
        if (count() == 0) {
            return OptionalNullable.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return OptionalNullable.of(elements[toIndex - 1]);
        }

        return OptionalNullable.of(N.max(elements, fromIndex, toIndex, comparator));
    }

    @Override
    public OptionalNullable<T> kthLargest(int k, Comparator<? super T> comparator) {
        if (count() == 0 || k > toIndex - fromIndex) {
            return OptionalNullable.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return OptionalNullable.of(elements[toIndex - k]);
        }

        return OptionalNullable.of(N.kthLargest(elements, fromIndex, toIndex, k, comparator));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public Stream<T> reverse() {
        return new IteratorStream<>(new ImmutableIterator<T>() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public T next() {
                if (cursor <= fromIndex) {
                    throw new NoSuchElementException();
                }

                return elements[--cursor];
            }
        }, closeHandlers);
    }

    @Override
    public Stream<List<T>> permutation() {
        return new IteratorStream<List<T>>(PermutationIterator.of(toList()), closeHandlers);
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Stream<List<T>> orderedPermutation() {
        final Iterator<List<T>> iter = PermutationIterator.ordered((List) toList());
        return new IteratorStream<>(iter, closeHandlers);
    }

    @SuppressWarnings("rawtypes")
    @Override
    public Stream<List<T>> orderedPermutation(Comparator<? super T> comparator) {
        return new IteratorStream<List<T>>(PermutationIterator.ordered((List) toList(), comparator == null ? OBJECT_COMPARATOR : comparator), closeHandlers);
    }

    @Override
    public boolean anyMatch(final Predicate<? super T> predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final Predicate<? super T> predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final Predicate<? super T> predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    //    @Override
    //    public OptionalNullable<T> findFirst() {
    //        return count() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalNullable<T> findFirst(final Predicate<? super T> predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalNullable.of(elements[i]);
            }
        }

        return (OptionalNullable<T>) OptionalNullable.empty();
    }

    //    @Override
    //    public OptionalNullable<T> findLast() {
    //        return count() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(elements[toIndex - 1]);
    //    }

    @Override
    public OptionalNullable<T> findLast(final Predicate<? super T> predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalNullable.of(elements[i]);
            }
        }

        return (OptionalNullable<T>) OptionalNullable.empty();
    }

    //    @Override
    //    public OptionalNullable<T> findAny() {
    //        return count() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(elements[fromIndex]);
    //    }

    @Override
    public OptionalNullable<T> findAny(final Predicate<? super T> predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalNullable.of(elements[i]);
            }
        }

        return (OptionalNullable<T>) OptionalNullable.empty();
    }

    //    @Override
    //    public Stream<T> exclude(Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return !set.contains(value);
    //            }
    //        });
    //    }
    //
    //    @Override
    //    public Stream<T> exclude(final Function<? super T, ?> mapper, final Collection<?> c) {
    //        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);
    //
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return !set.contains(mapper.apply(value));
    //            }
    //        });
    //    }

    //    @Override
    //    public Stream<T> skipNull() {
    //        return filter(new Predicate<T>() {
    //            @Override
    //            public boolean test(T value) {
    //                return value != null;
    //            }
    //        });
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileNull() {
    //        return new IteratorStream<>(NullBreakIterator.of(elements, fromIndex, toIndex), closeHandlers, sorted, cmp);
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileError() {
    //        // Never happen
    //        // return new IteratorStream<>(ErrorBreakIterator.of(elements, fromIndex, toIndex), closeHandlers, sorted, cmp);
    //
    //        return this;
    //    }
    //
    //    @Override
    //    public Stream<T> breakWhileError(int maxRetries, long retryInterval) {
    //        // Never happen
    //        // return new IteratorStream<>(ErrorBreakIterator.of(elements, fromIndex, toIndex, maxRetries, retryInterval), closeHandlers, sorted, cmp);
    //
    //        return this;
    //    }

    @Override
    public Stream<T> queued() {
        return queued(DEFAULT_QUEUE_SIZE);
    }

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterator asynchronously.
     * 
     * @param stream
     * @param queueSize Default value is 8
     * @return
     */
    @Override
    public Stream<T> queued(int queueSize) {
        // Do nothing. No need for queue.
        //        final Iterator<T> iter = iterator();
        //
        //        if (iter instanceof QueuedIterator && ((QueuedIterator<? extends T>) iter).max() >= queueSize) {
        //            return this;
        //        } else {
        //            return new IteratorStream<>(Stream.parallelConcat(Arrays.asList(iter), queueSize, asyncExecutor), closeHandlers, sorted, cmp);
        //        }

        return this;
    }

    @Override
    public Stream<T> cached(IntFunction<T[]> generator) {
        return new ArrayStream<T>(toArray(generator), closeHandlers, sorted, cmp);
    }

    @Override
    public ImmutableIterator<T> iterator() {
        return ImmutableIterator.of(elements, fromIndex, toIndex);
    }

    @Override
    public Stream<T> parallel(int maxThreadNum, BaseStream.Splitter splitter) {
        if (maxThreadNum < 1) {
            throw new IllegalArgumentException("'maxThreadNum' must be bigger than 0");
        }

        return new ParallelArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitter);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayStream<T>(elements, fromIndex, toIndex, newCloseHandlers, sorted, cmp);
    }
}
