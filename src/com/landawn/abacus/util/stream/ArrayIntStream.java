/*
 * Copyright (C) 2016 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

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

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
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
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class ArrayIntStream extends AbstractIntStream {
    final int[] elements;
    final int fromIndex;
    final int toIndex;

    ArrayIntStream(final int[] values) {
        this(values, 0, values.length);
    }

    ArrayIntStream(final int[] values, final Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ArrayIntStream(final int[] values, final Collection<Runnable> closeHandlers, final boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayIntStream(final int[] values, final int fromIndex, final int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayIntStream(final int[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayIntStream(final int[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);

        checkFromToIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public IntStream filter(final IntPredicate predicate) {
        return new IteratorIntStream(new ExIntIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex) {
                    do {
                        if (predicate.test(elements[cursor])) {
                            hasNext = true;
                            break;
                        }
                    } while (++cursor < toIndex);
                }

                return hasNext;
            }

            @Override
            public int nextInt() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream takeWhile(final IntPredicate predicate) {
        return new IteratorIntStream(new ExIntIterator() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && cursor < toIndex) {
                    if (predicate.test(elements[cursor])) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public int nextInt() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream dropWhile(final IntPredicate predicate) {
        return new IteratorIntStream(new ExIntIterator() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex) {
                    if (dropped == false) {
                        do {
                            if (predicate.test(elements[cursor]) == false) {
                                hasNext = true;
                                break;
                            }
                        } while (++cursor < toIndex);

                        dropped = true;
                    } else {
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public int nextInt() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream map(final IntUnaryOperator mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int nextInt() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
    public CharStream mapToChar(final IntToCharFunction mapper) {
        return new IteratorCharStream(new ExCharIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char nextChar() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
    public ByteStream mapToByte(final IntToByteFunction mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public byte nextByte() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
    public ShortStream mapToShort(final IntToShortFunction mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short nextShort() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
    public LongStream mapToLong(final IntToLongFunction mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long nextLong() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
    public FloatStream mapToFloat(final IntToFloatFunction mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float nextFloat() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
    public DoubleStream mapToDouble(final IntToDoubleFunction mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double nextDouble() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
    public <U> Stream<U> mapToObj(final IntFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ExIterator<U>() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
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
        return new IteratorIntStream(new ExIntIterator() {
            private int cursor = fromIndex;
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int nextInt() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextInt();
            }
        }, closeHandlers);
    }

    @Override
    public CharStream flatMapToChar(final IntFunction<? extends CharStream> mapper) {
        return new IteratorCharStream(new ExCharIterator() {
            private int cursor = fromIndex;
            private CharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char nextChar() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextChar();
            }
        }, closeHandlers);
    }

    @Override
    public ByteStream flatMapToByte(final IntFunction<? extends ByteStream> mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            private int cursor = fromIndex;
            private ByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public byte nextByte() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextByte();
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream flatMapToShort(final IntFunction<? extends ShortStream> mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            private int cursor = fromIndex;
            private ShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public short nextShort() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextShort();
            }
        }, closeHandlers);
    }

    @Override
    public LongStream flatMapToLong(final IntFunction<? extends LongStream> mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            private int cursor = fromIndex;
            private LongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public long nextLong() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextLong();
            }
        }, closeHandlers);
    }

    @Override
    public FloatStream flatMapToFloat(final IntFunction<? extends FloatStream> mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            private int cursor = fromIndex;
            private FloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public float nextFloat() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextFloat();
            }
        }, closeHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(final IntFunction<? extends DoubleStream> mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            private int cursor = fromIndex;
            private DoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double nextDouble() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextDouble();
            }
        }, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final IntFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ExIterator<T>() {
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
    public Stream<IntStream> split(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<IntStream>(new ExIterator<IntStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public IntStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayIntStream(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex), null, sorted);
            }
        }, closeHandlers);
    }

    @Override
    public Stream<IntList> split0(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<IntList>(new ExIterator<IntList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public IntList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new IntList(N.copyOfRange(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex)));
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<IntStream> split(final U identity, final BiFunction<? super Integer, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<IntStream>(new ExIterator<IntStream>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public IntStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.apply(elements[from], identity);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], identity) == preCondition) {
                        cursor++;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return new ArrayIntStream(elements, from, cursor, null, sorted);
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<IntList> split0(final U identity, final BiFunction<? super Integer, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<IntList>(new ExIterator<IntList>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public IntList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.apply(elements[from], identity);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], identity) == preCondition) {
                        cursor++;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return new IntList(N.copyOfRange(elements, from, cursor));
            }
        }, closeHandlers);
    }

    @Override
    public Stream<IntStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final IntStream[] a = new IntStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? IntStream.empty() : new ArrayIntStream(elements, fromIndex, middleIndex, null, sorted);
        a[1] = middleIndex == toIndex ? IntStream.empty() : new ArrayIntStream(elements, middleIndex, toIndex, null, sorted);

        return new ArrayStream<>(a, closeHandlers);
    }

    @Override
    public Stream<IntStream> splitBy(IntPredicate where) {
        N.requireNonNull(where);

        int n = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (where.test(elements[i])) {
                n++;
            } else {
                break;
            }
        }

        return splitAt(n);
    }

    @Override
    public Stream<IntStream> sliding(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<IntStream>(new ExIterator<IntStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public IntStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ArrayIntStream result = new ArrayIntStream(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex, null, sorted);

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<IntList> sliding0(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<IntList>(new ExIterator<IntList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public IntList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final IntList result = IntList.of(N.copyOfRange(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex));

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public IntStream top(int n) {
        return top(n, INT_COMPARATOR);
    }

    @Override
    public IntStream top(int n, Comparator<? super Integer> comparator) {
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, INT_COMPARATOR)) {
            return new ArrayIntStream(elements, toIndex - n, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayIntStream(N.top(elements, fromIndex, toIndex, n, comparator), closeHandlers, sorted);
        }
    }

    @Override
    public IntStream sorted() {
        if (sorted) {
            return this;
        }

        final int[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayIntStream(a, closeHandlers, true);
    }

    @Override
    public IntStream peek(final IntConsumer action) {
        return new IteratorIntStream(new ExIntIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int nextInt() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                action.accept(elements[cursor]);

                return elements[cursor++];
            }

            @Override
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    action.accept(elements[cursor]);

                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayIntStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
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
    public List<Integer> toList() {
        final List<Integer> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<Integer> toList(Supplier<? extends List<Integer>> supplier) {
        final List<Integer> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Integer> toSet() {
        final Set<Integer> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Integer> toSet(Supplier<? extends Set<Integer>> supplier) {
        final Set<Integer> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Integer> toMultiset() {
        final Multiset<Integer> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Integer> toMultiset(Supplier<? extends Multiset<Integer>> supplier) {
        final Multiset<Integer> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Integer> toLongMultiset() {
        final LongMultiset<Integer> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Integer> toLongMultiset(Supplier<? extends LongMultiset<Integer>> supplier) {
        final LongMultiset<Integer> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final IntFunction<? extends K> classifier, final Collector<Integer, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Integer> downstreamAccumulator = downstream.accumulator();
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
    public <K, U, M extends Map<K, U>> M toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyMapper.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapFactory) {
        final Multimap<K, U, V> result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyMapper.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
    }

    @Override
    public OptionalInt first() {
        return fromIndex < toIndex ? OptionalInt.of(elements[fromIndex]) : OptionalInt.empty();
    }

    @Override
    public OptionalInt last() {
        return fromIndex < toIndex ? OptionalInt.of(elements[toIndex - 1]) : OptionalInt.empty();
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
        if (fromIndex == toIndex) {
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
    public int head() {
        if (fromIndex == toIndex) {
            throw new NoSuchElementException();
        }

        return elements[fromIndex];
    }

    @Override
    public IntStream tail() {
        if (fromIndex == toIndex) {
            throw new IllegalStateException();
        }

        return new ArrayIntStream(elements, fromIndex + 1, toIndex, closeHandlers, sorted);
    }

    @Override
    public IntStream head2() {
        if (fromIndex == toIndex) {
            throw new IllegalStateException();
        }

        return new ArrayIntStream(elements, fromIndex, toIndex - 1, closeHandlers, sorted);
    }

    @Override
    public int tail2() {
        if (fromIndex == toIndex) {
            throw new NoSuchElementException();
        }

        return elements[toIndex - 1];
    }

    @Override
    public OptionalInt min() {
        if (fromIndex == toIndex) {
            return OptionalInt.empty();
        } else if (sorted) {
            return OptionalInt.of(elements[fromIndex]);
        }

        return OptionalInt.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalInt max() {
        if (fromIndex == toIndex) {
            return OptionalInt.empty();
        } else if (sorted) {
            return OptionalInt.of(elements[toIndex - 1]);
        }

        return OptionalInt.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalInt kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (k > toIndex - fromIndex) {
            return OptionalInt.empty();
        } else if (sorted) {
            return OptionalInt.of(elements[toIndex - k]);
        }

        return OptionalInt.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public Long sum() {
        return N.sum(elements, fromIndex, toIndex);
    }

    @Override
    public OptionalDouble average() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public IntStream reverse() {
        return new IteratorIntStream(new ExIntIterator() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public int nextInt() {
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
                cursor = n < cursor - fromIndex ? cursor - (int) n : fromIndex;
            }

            @Override
            public int[] toArray() {
                final int[] a = new int[cursor - fromIndex];

                for (int i = 0, len = a.length; i < len; i++) {
                    a[i] = elements[cursor - i - 1];
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public IntSummaryStatistics summarize() {
        final IntSummaryStatistics result = new IntSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            result.accept(elements[i]);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalInt findFirst(final IntPredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalInt.of(elements[i]);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public OptionalInt findLast(final IntPredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalInt.of(elements[i]);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public LongStream asLongStream() {
        return new IteratorLongStream(new ExLongIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long nextLong() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public long[] toArray() {
                final long[] a = new long[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream asFloatStream() {
        return new IteratorFloatStream(new ExFloatIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float nextFloat() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public float[] toArray() {
                final float[] a = new float[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double nextDouble() {
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
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public double[] toArray() {
                final double[] a = new double[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public Stream<Integer> boxed() {
        return new IteratorStream<Integer>(iterator(), closeHandlers, sorted, sorted ? INT_COMPARATOR : null);
    }

    @Override
    public IntStream cached() {
        return this;
    }

    @Override
    ExIntIterator exIterator() {
        return ExIntIterator.of(elements, fromIndex, toIndex);
    }

    @Override
    public IntStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelArrayIntStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayIntStream(elements, fromIndex, toIndex, newCloseHandlers, sorted);
    }
}
