package com.landawn.abacus.util.stream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
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
final class ArrayStream<T> extends Stream<T> implements BaseStream<T, Stream<T>> {
    private final T[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final boolean sorted;
    private final Comparator<? super T> cmp;
    private final Set<Runnable> closeHandlers;

    ArrayStream(T[] values) {
        this(values, null);
    }

    ArrayStream(T[] values, Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ArrayStream(T[] values, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> cmp) {
        this(values, 0, values.length, closeHandlers, sorted, cmp);
    }

    ArrayStream(T[] values, int fromIndex, int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayStream(T[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false, null);
    }

    ArrayStream(T[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> cmp) {
        Stream.checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
        this.sorted = sorted;
        this.cmp = cmp;
        this.closeHandlers = N.isNullOrEmpty(closeHandlers) ? null : new LinkedHashSet<>(closeHandlers);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate) {
        return filter(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate, final long max) {
        //        final ObjectList<T> list = ObjectList.of((T[]) N.newArray(elements.getClass().getComponentType(), N.min(9, Stream.toInt(max), (toIndex - fromIndex))),
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
    public Stream<T> takeWhile(final Predicate<? super T> predicate) {
        return takeWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate, final long max) {
        //        final ObjectList<T> list = ObjectList.of((T[]) N.newArray(elements.getClass().getComponentType(), N.min(9, Stream.toInt(max), (toIndex - fromIndex))),
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
    public Stream<T> dropWhile(final Predicate<? super T> predicate) {
        return dropWhile(predicate, Long.MAX_VALUE);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate, final long max) {
        //        int index = fromIndex;
        //        while (index < toIndex && predicate.test(elements[index])) {
        //            index++;
        //        }
        //
        //        final ObjectList<T> list = ObjectList.of((T[]) N.newArray(elements.getClass().getComponentType(), N.min(9, Stream.toInt(max), (toIndex - index))), 0);
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
    public <R> Stream<R> flatMap(final Function<? super T, ? extends Stream<? extends R>> mapper) {
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
                    cur = mapper.apply(elements[cursor++]).iterator();
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
    public <R> Stream<R> flatMap2(final Function<? super T, ? extends R[]> mapper) {
        //        final List<Object[]> listOfArray = new ArrayList<Object[]>();
        //
        //        int lengthOfAll = 0;
        //        for (int i = fromIndex; i < toIndex; i++) {
        //            final Object[] tmp = mapper.apply(values[i]);
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
            private R[] cur = null;
            private int curIndex = 0;

            @Override
            public boolean hasNext() {
                while ((cur == null || curIndex >= cur.length) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]);
                    curIndex = 0;
                }

                return cur != null && curIndex < cur.length;
            }

            @Override
            public R next() {
                if (cur == null || curIndex >= cur.length) {
                    throw new NoSuchElementException();
                }

                return cur[curIndex++];
            }
        }, closeHandlers);
    }

    @Override
    public <R> Stream<R> flatMap3(final Function<? super T, ? extends Collection<? extends R>> mapper) {
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
                    cur = mapper.apply(elements[cursor++]).iterator();
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
    public CharStream flatMapToChar(final Function<? super T, ? extends CharStream> mapper) {
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
    public CharStream flatMapToChar2(final Function<? super T, char[]> mapper) {
        return flatMapToChar(new Function<T, CharStream>() {
            @Override
            public CharStream apply(T t) {
                return CharStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public CharStream flatMapToChar3(final Function<? super T, ? extends Collection<Character>> mapper) {
        return flatMapToChar(new Function<T, CharStream>() {
            @Override
            public CharStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToChar(new ToCharFunction<Character>() {
                    @Override
                    public char applyAsChar(Character value) {
                        return value == null ? 0 : value.charValue();
                    }
                });
            }
        });
    }

    @Override
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
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
    public ByteStream flatMapToByte2(final Function<? super T, byte[]> mapper) {
        return flatMapToByte(new Function<T, ByteStream>() {
            @Override
            public ByteStream apply(T t) {
                return ByteStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ByteStream flatMapToByte3(final Function<? super T, ? extends Collection<Byte>> mapper) {
        return flatMapToByte(new Function<T, ByteStream>() {
            @Override
            public ByteStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToByte(new ToByteFunction<Byte>() {
                    @Override
                    public byte applyAsByte(Byte value) {
                        return value == null ? 0 : value.byteValue();
                    }
                });
            }
        });
    }

    @Override
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
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
    public ShortStream flatMapToShort2(final Function<? super T, short[]> mapper) {
        return flatMapToShort(new Function<T, ShortStream>() {
            @Override
            public ShortStream apply(T t) {
                return ShortStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public ShortStream flatMapToShort3(final Function<? super T, ? extends Collection<Short>> mapper) {
        return flatMapToShort(new Function<T, ShortStream>() {
            @Override
            public ShortStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToShort(new ToShortFunction<Short>() {
                    @Override
                    public short applyAsShort(Short value) {
                        return value == null ? 0 : value.shortValue();
                    }
                });
            }
        });
    }

    @Override
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
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
    public IntStream flatMapToInt2(final Function<? super T, int[]> mapper) {
        return flatMapToInt(new Function<T, IntStream>() {
            @Override
            public IntStream apply(T t) {
                return IntStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public IntStream flatMapToInt3(final Function<? super T, ? extends Collection<Integer>> mapper) {
        return flatMapToInt(new Function<T, IntStream>() {
            @Override
            public IntStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToInt(new ToIntFunction<Integer>() {
                    @Override
                    public int applyAsInt(Integer value) {
                        return value == null ? 0 : value.intValue();
                    }
                });
            }
        });
    }

    @Override
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
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
    public LongStream flatMapToLong2(final Function<? super T, long[]> mapper) {
        return flatMapToLong(new Function<T, LongStream>() {
            @Override
            public LongStream apply(T t) {
                return LongStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public LongStream flatMapToLong3(final Function<? super T, ? extends Collection<Long>> mapper) {
        return flatMapToLong(new Function<T, LongStream>() {
            @Override
            public LongStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToLong(new ToLongFunction<Long>() {
                    @Override
                    public long applyAsLong(Long value) {
                        return value == null ? 0 : value.longValue();
                    }
                });
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
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
    public FloatStream flatMapToFloat2(final Function<? super T, float[]> mapper) {
        return flatMapToFloat(new Function<T, FloatStream>() {
            @Override
            public FloatStream apply(T t) {
                return FloatStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public FloatStream flatMapToFloat3(final Function<? super T, ? extends Collection<Float>> mapper) {
        return flatMapToFloat(new Function<T, FloatStream>() {
            @Override
            public FloatStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToFloat(new ToFloatFunction<Float>() {
                    @Override
                    public float applyAsFloat(Float value) {
                        return value == null ? 0 : value.floatValue();
                    }
                });
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
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
        //        return new DoubleStreamImpl(arrayOfAll, closeHandlers);

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
    public DoubleStream flatMapToDouble2(final Function<? super T, double[]> mapper) {
        return flatMapToDouble(new Function<T, DoubleStream>() {
            @Override
            public DoubleStream apply(T t) {
                return DoubleStream.of(mapper.apply(t));
            }
        });
    }

    @Override
    public DoubleStream flatMapToDouble3(final Function<? super T, ? extends Collection<Double>> mapper) {
        return flatMapToDouble(new Function<T, DoubleStream>() {
            @Override
            public DoubleStream apply(T t) {
                return Stream.of(mapper.apply(t)).mapToDouble(new ToDoubleFunction<Double>() {
                    @Override
                    public double applyAsDouble(Double value) {
                        return value == null ? 0 : value.doubleValue();
                    }
                });
            }
        });
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K> Stream<Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier, Supplier<Map<K, List<T>>> mapFactory) {
        final Map<K, List<T>> map = collect(Collectors.groupingBy(classifier, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, A, D> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, D, A> Stream<Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream,
            Supplier<Map<K, D>> mapFactory) {
        final Map<K, D> map = collect(Collectors.groupingBy(classifier, downstream, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
    }

    @Override
    public <K, U> Stream<Entry<K, U>> groupBy(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<Map<K, U>> mapFactory) {
        final Map<K, U> map = collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapFactory));

        return new IteratorStream<>(map.entrySet().iterator(), closeHandlers);
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

        return split(size).map(new Function<Stream<T>, List<T>>() {
            @Override
            public List<T> apply(Stream<T> t) {
                return t.toList();
            }
        });
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

        return split(size).map(new Function<Stream<T>, Set<T>>() {
            @Override
            public Set<T> apply(Stream<T> t) {
                return t.toSet();
            }
        });
    }

    @Override
    public Stream<T> distinct() {
        return new ArrayStream<T>(N.removeDuplicates(elements, fromIndex, toIndex, sorted && isSameComparator(null, cmp)), closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> distinct(Comparator<? super T> comparator) {
        final List<T> list = N.distinct(elements, fromIndex, toIndex, comparator);
        final T[] a = list.toArray((T[]) N.newArray(elements.getClass().getComponentType(), list.size()));
        return new ArrayStream<T>(a, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyMapper) {
        final List<T> list = N.distinct(elements, fromIndex, toIndex, keyMapper);
        final T[] a = list.toArray((T[]) N.newArray(elements.getClass().getComponentType(), list.size()));
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
            return new ArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, comparator);
        }

        final T[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a, comparator);
        return new ArrayStream<T>(a, closeHandlers, true, comparator);
    }

    @Override
    public Stream<T> parallelSorted() {
        return parallelSorted(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> parallelSorted(Comparator<? super T> comparator) {
        if (sorted && isSameComparator(comparator, cmp)) {
            return new ArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, comparator);
        }

        final T[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a, comparator);
        return new ArrayStream<T>(a, closeHandlers, true, comparator);
    }

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
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        if (maxSize >= toIndex - fromIndex) {
            return new ArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp);
        } else {
            return new ArrayStream<T>(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted, cmp);
        }
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
    public boolean forEach2(Function<? super T, Boolean> action) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (action.apply(elements[i]).booleanValue() == false) {
                return false;
            }
        }

        return true;
    }

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
    public <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier) {
        return toMap(classifier, new Supplier<Map<K, List<T>>>() {
            @Override
            public Map<K, List<T>> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, M extends Map<K, List<T>>> M toMap(Function<? super T, ? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();
        return toMap(classifier, downstream, mapFactory);
    }

    @Override
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        return toMap(classifier, downstream, new Supplier<Map<K, D>>() {
            @Override
            public Map<K, D> get() {
                return new HashMap<>();
            }
        });
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
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return toMap(keyMapper, valueMapper, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapSupplier) {
        final BinaryOperator<U> mergeFunction = Collectors.throwingMerger();
        return toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return toMap(keyMapper, valueMapper, mergeFunction, new Supplier<Map<K, U>>() {
            @Override
            public Map<K, U> get() {
                return new HashMap<>();
            }
        });
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
    public <K, U> Multimap<K, U, List<U>> toMultimap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return toMultimap(keyMapper, valueMapper, new Supplier<Multimap<K, U, List<U>>>() {
            @Override
            public Multimap<K, U, List<U>> get() {
                return N.newListMultimap();
            }
        });
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
    public Optional<T> reduce(BinaryOperator<T> accumulator) {
        if (count() == 0) {
            Optional.empty();
        }

        T result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = accumulator.apply(result, elements[i]);
        }

        return Optional.of(result);
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
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator) {
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
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator) {
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
    public Optional<T> min(Comparator<? super T> comparator) {
        if (count() == 0) {
            return Optional.empty();
        }

        return Optional.of(N.min(elements, fromIndex, toIndex, comparator));
    }

    @Override
    public Optional<T> max(Comparator<? super T> comparator) {
        if (count() == 0) {
            return Optional.empty();
        }

        return Optional.of(N.max(elements, fromIndex, toIndex, comparator));
    }

    @Override
    public Optional<T> kthLargest(int k, Comparator<? super T> cmp) {
        if (count() == 0 || k > toIndex - fromIndex) {
            return Optional.empty();
        }

        return Optional.of(N.kthLargest(elements, fromIndex, toIndex, k, cmp));
    }

    @Override
    public Long sumInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.summingInt(mapper));
    }

    @Override
    public Long sumLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.summingLong(mapper));
    }

    @Override
    public Double sumDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.summingDouble(mapper));
    }

    @Override
    public OptionalDouble averageInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.averagingInt2(mapper));
    }

    @Override
    public OptionalDouble averageLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.averagingLong2(mapper));
    }

    @Override
    public OptionalDouble averageDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.averagingDouble2(mapper));
    }

    @Override
    public IntSummaryStatistics summarizeInt(ToIntFunction<? super T> mapper) {
        return collect(Collectors.summarizingInt(mapper));
    }

    @Override
    public LongSummaryStatistics summarizeLong(ToLongFunction<? super T> mapper) {
        return collect(Collectors.summarizingLong(mapper));
    }

    @Override
    public DoubleSummaryStatistics summarizeDouble(ToDoubleFunction<? super T> mapper) {
        return collect(Collectors.summarizingDouble(mapper));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
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
    //    public Optional<T> findFirst() {
    //        return count() == 0 ? (Optional<T>) Optional.empty() : Optional.of(elements[fromIndex]);
    //    }

    @Override
    public Optional<T> findFirst(final Predicate<? super T> predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return Optional.of(elements[i]);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    //    @Override
    //    public Optional<T> findLast() {
    //        return count() == 0 ? (Optional<T>) Optional.empty() : Optional.of(elements[toIndex - 1]);
    //    }

    @Override
    public Optional<T> findLast(final Predicate<? super T> predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return Optional.of(elements[i]);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    //    @Override
    //    public Optional<T> findAny() {
    //        return count() == 0 ? (Optional<T>) Optional.empty() : Optional.of(elements[fromIndex]);
    //    }

    @Override
    public Optional<T> findAny(final Predicate<? super T> predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return Optional.of(elements[i]);
            }
        }

        return (Optional<T>) Optional.empty();
    }

    @Override
    public Stream<T> removeAll(Collection<?> c) {
        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !set.contains(value);
            }
        });
    }

    @Override
    public Stream<T> removeAll(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !set.contains(mapper.apply(value));
            }
        });
    }

    @Override
    public Stream<T> except(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) < 1;
            }
        });
    }

    @Override
    public Stream<T> except(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(mapper.apply(value)) < 1;
            }
        });
    }

    @Override
    public Stream<T> intersect(Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(value) > 0;
            }
        });
    }

    @Override
    public Stream<T> intersect(final Function<? super T, ?> mapper, final Collection<?> c) {
        final Multiset<?> multiset = Multiset.of(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return multiset.getAndRemove(mapper.apply(value)) > 0;
            }
        });
    }

    @Override
    public Stream<T> append(Stream<? extends T> stream) {
        return Stream.concat(this, stream);
    }

    @Override
    public Stream<T> append(Iterator<? extends T> iterator) {
        return Stream.concat(this, Stream.of(iterator));
    }

    @Override
    public Iterator<T> iterator() {
        return ImmutableIterator.of(elements, fromIndex, toIndex);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final List<Runnable> closeHandlerList = new ArrayList<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            closeHandlerList.addAll(this.closeHandlers);
        }

        closeHandlerList.add(closeHandler);

        return new ArrayStream<T>(elements, fromIndex, toIndex, closeHandlerList, sorted, cmp);
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
