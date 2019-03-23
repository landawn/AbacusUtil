/*
 * Copyright (C) 2016, 2017, 2018, 2019 HaiYang Li
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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Queue;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteMatrix;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.ContinuableFuture;
import com.landawn.abacus.util.Fn.Fnn;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.IndexedByte;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Percentage;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.u.Holder;
import com.landawn.abacus.util.u.Optional;
import com.landawn.abacus.util.u.OptionalByte;
import com.landawn.abacus.util.u.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.BooleanSupplier;
import com.landawn.abacus.util.function.ByteBiFunction;
import com.landawn.abacus.util.function.ByteBiPredicate;
import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.ByteNFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteSupplier;
import com.landawn.abacus.util.function.ByteToIntFunction;
import com.landawn.abacus.util.function.ByteTriFunction;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;

/** 
 * The Stream will be automatically closed after execution(A terminal method is executed/triggered).
 * 
 * @see Stream 
 */
public abstract class ByteStream extends StreamBase<Byte, byte[], BytePredicate, ByteConsumer, ByteList, OptionalByte, IndexedByte, ByteIterator, ByteStream> {

    ByteStream(final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, null, closeHandlers);
    }

    public abstract ByteStream map(ByteUnaryOperator mapper);

    public abstract IntStream mapToInt(ByteToIntFunction mapper);

    public abstract <U> Stream<U> mapToObj(ByteFunction<? extends U> mapper);

    public abstract ByteStream flatMap(ByteFunction<? extends ByteStream> mapper);

    public abstract ByteStream flattMap(ByteFunction<byte[]> mapper);

    public abstract IntStream flatMapToInt(ByteFunction<? extends IntStream> mapper);

    public abstract <T> Stream<T> flatMapToObj(ByteFunction<? extends Stream<T>> mapper);

    public abstract <T> Stream<T> flattMapToObj(ByteFunction<? extends Collection<T>> mapper);

    public abstract <T> Stream<T> flatMappToObj(ByteFunction<T[]> mapper);

    /**
     * Merge series of adjacent elements which satisfy the given predicate using
     * the merger function and return a new stream.
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param collapsible
     * @param mergeFunction
     * @return
     */
    @SequentialOnly
    public abstract ByteStream collapse(final ByteBiPredicate collapsible, final ByteBiFunction<Byte> mergeFunction);

    /**
     * Returns a {@code Stream} produced by iterative application of a accumulation function
     * to an initial element {@code seed} and next element of the current stream.
     * Produces a {@code Stream} consisting of {@code seed}, {@code acc(seed, value1)},
     * {@code acc(acc(seed, value1), value2)}, etc.
     *
     * <p>This is an intermediate operation.
     *
     * <p>Example:
     * <pre>
     * accumulator: (a, b) -&gt; a + b
     * stream: [1, 2, 3, 4, 5]
     * result: [1, 3, 6, 10, 15]
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     *
     * @param accumulator  the accumulation function
     * @return the new stream which has the extract same size as this stream.
     */
    @SequentialOnly
    public abstract ByteStream scan(final ByteBiFunction<Byte> accumulator);

    /**
     * Returns a {@code Stream} produced by iterative application of a accumulation function
     * to an initial element {@code seed} and next element of the current stream.
     * Produces a {@code Stream} consisting of {@code seed}, {@code acc(seed, value1)},
     * {@code acc(acc(seed, value1), value2)}, etc.
     *
     * <p>This is an intermediate operation.
     *
     * <p>Example:
     * <pre>
     * seed:10
     * accumulator: (a, b) -&gt; a + b
     * stream: [1, 2, 3, 4, 5]
     * result: [11, 13, 16, 20, 25]
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     *
     * @param seed the initial value. it's only used once by <code>accumulator</code> to calculate the fist element in the returned stream. 
     * It will be ignored if this stream is empty and won't be the first element of the returned stream.
     * 
     * @param accumulator  the accumulation function
     * @return the new stream which has the extract same size as this stream.
     */
    @SequentialOnly
    public abstract ByteStream scan(final byte seed, final ByteBiFunction<Byte> accumulator);

    /**
     * 
     * @param seed
     * @param accumulator
     * @param seedIncluded
     * @return
     */
    @SequentialOnly
    public abstract ByteStream scan(final byte seed, final ByteBiFunction<Byte> accumulator, final boolean seedIncluded);

    /**
     * 
     * @return
     */
    public abstract ByteList toByteList();

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#toMap(Function, Function)
     */
    public abstract <K, V> Map<K, V> toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends V> valueMapper);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, Supplier)
     */
    public abstract <K, V, M extends Map<K, V>> M toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends V> valueMapper, Supplier<M> mapFactory);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator)
     */
    public abstract <K, V> Map<K, V> toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends V> valueMapper, BinaryOperator<V> mergeFunction);

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public abstract <K, V, M extends Map<K, V>> M toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends V> valueMapper,
            BinaryOperator<V> mergeFunction, Supplier<M> mapFactory);

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public abstract <K, A, D> Map<K, D> toMap(final ByteFunction<? extends K> classifier, final Collector<Byte, A, D> downstream);

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public abstract <K, A, D, M extends Map<K, D>> M toMap(final ByteFunction<? extends K> classifier, final Collector<Byte, A, D> downstream,
            final Supplier<M> mapFactory);

    public abstract ByteMatrix toMatrix();

    /**
     * 
     * @param identity
     * @param op
     * @return
     */
    public abstract byte reduce(byte identity, ByteBinaryOperator op);

    /**
     * 
     * @param op
     * @return
     */
    public abstract OptionalByte reduce(ByteBinaryOperator op);

    /**
     * 
     * @param supplier
     * @param accumulator
     * @param combiner
     * @return
     */
    public abstract <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator, BiConsumer<R, R> combiner);

    /**
     * 
     * @param supplier
     * @param accumulator
     * @return
     */
    public abstract <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator);

    public abstract <E extends Exception> void forEach(Try.ByteConsumer<E> action) throws E;

    public abstract <E extends Exception> boolean anyMatch(final Try.BytePredicate<E> predicate) throws E;

    public abstract <E extends Exception> boolean allMatch(final Try.BytePredicate<E> predicate) throws E;

    public abstract <E extends Exception> boolean noneMatch(final Try.BytePredicate<E> predicate) throws E;

    public abstract <E extends Exception> OptionalByte findFirst(final Try.BytePredicate<E> predicate) throws E;

    public abstract <E extends Exception> OptionalByte findLast(final Try.BytePredicate<E> predicate) throws E;

    public abstract <E extends Exception, E2 extends Exception> OptionalByte findFirstOrLast(Try.BytePredicate<E> predicateForFirst,
            Try.BytePredicate<E> predicateForLast) throws E, E2;

    public abstract <E extends Exception> OptionalByte findAny(final Try.BytePredicate<E> predicate) throws E;

    /**
     * 
     * @return
     */
    public abstract OptionalByte min();

    /**
     * 
     * @return
     */
    public abstract OptionalByte max();

    /**
     * 
     * @param k
     * @return OptionalByte.empty() if there is no element or count less than k, otherwise the kth largest element.
     */
    public abstract OptionalByte kthLargest(int k);

    /**
     * 
     * @return int
     */
    public abstract int sum();

    public abstract OptionalDouble average();

    public abstract ByteSummaryStatistics summarize();

    public abstract Pair<ByteSummaryStatistics, Optional<Map<Percentage, Byte>>> summarizeAndPercentiles();

    /**
     * 
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public abstract ByteStream merge(final ByteStream b, final ByteBiFunction<Nth> nextSelector);

    public abstract ByteStream zipWith(ByteStream b, ByteBiFunction<Byte> zipFunction);

    public abstract ByteStream zipWith(ByteStream b, ByteStream c, ByteTriFunction<Byte> zipFunction);

    public abstract ByteStream zipWith(ByteStream b, byte valueForNoneA, byte valueForNoneB, ByteBiFunction<Byte> zipFunction);

    public abstract ByteStream zipWith(ByteStream b, ByteStream c, byte valueForNoneA, byte valueForNoneB, byte valueForNoneC,
            ByteTriFunction<Byte> zipFunction);

    public abstract IntStream asIntStream();

    public abstract Stream<Byte> boxed();

    /**
     * Remember to close this Stream after the iteration is done, if required.
     * 
     * @return
     */
    @SequentialOnly
    @Override
    public ByteIterator iterator() {
        return iteratorEx();
    }

    abstract ByteIteratorEx iteratorEx();

    @Override
    public <R> R __(Function<? super ByteStream, R> transfer) {
        return transfer.apply(this);
    }

    public static ByteStream empty() {
        return new ArrayByteStream(N.EMPTY_BYTE_ARRAY, true, null);
    }

    @SafeVarargs
    public static ByteStream of(final byte... a) {
        return N.isNullOrEmpty(a) ? empty() : new ArrayByteStream(a);
    }

    public static ByteStream of(final byte[] a, final int startIndex, final int endIndex) {
        return N.isNullOrEmpty(a) && (startIndex == 0 && endIndex == 0) ? empty() : new ArrayByteStream(a, startIndex, endIndex);
    }

    public static ByteStream of(final Byte[] a) {
        return Stream.of(a).mapToByte(Fnn.unboxB());
    }

    public static ByteStream of(final Byte[] a, final int startIndex, final int endIndex) {
        return Stream.of(a, startIndex, endIndex).mapToByte(Fnn.unboxB());
    }

    public static ByteStream of(final Collection<Byte> c) {
        return Stream.of(c).mapToByte(Fnn.unboxB());
    }

    public static ByteStream of(final ByteIterator iterator) {
        return iterator == null ? empty() : new IteratorByteStream(iterator);
    }

    /**
     * Lazy evaluation.
     * @param supplier
     * @return
     */
    public static ByteStream of(final Supplier<ByteList> supplier) {
        final ByteIterator iter = new ByteIteratorEx() {
            private ByteIterator iterator = null;

            @Override
            public boolean hasNext() {
                if (iterator == null) {
                    init();
                }

                return iterator.hasNext();
            }

            @Override
            public byte nextByte() {
                if (iterator == null) {
                    init();
                }

                return iterator.nextByte();
            }

            private void init() {
                final ByteList c = supplier.get();

                if (N.isNullOrEmpty(c)) {
                    iterator = ByteIterator.empty();
                } else {
                    iterator = c.iterator();
                }
            }
        };

        return of(iter);
    }

    private static final Function<byte[], ByteStream> flatMapper = new Function<byte[], ByteStream>() {
        @Override
        public ByteStream apply(byte[] t) {
            return ByteStream.of(t);
        }
    };

    private static final Function<byte[][], ByteStream> flatMappper = new Function<byte[][], ByteStream>() {
        @Override
        public ByteStream apply(byte[][] t) {
            return ByteStream.flat(t);
        }
    };

    public static ByteStream flat(final byte[][] a) {
        return N.isNullOrEmpty(a) ? empty() : Stream.of(a).flatMapToByte(flatMapper);
    }

    public static ByteStream flat(final byte[][] a, final boolean vertically) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return of(a[0]);
        } else if (vertically == false) {
            return Stream.of(a).flatMapToByte(flatMapper);
        }

        long n = 0;

        for (byte[] e : a) {
            n += N.len(e);
        }

        if (n == 0) {
            return empty();
        }

        final int rows = N.len(a);
        final long count = n;

        final ByteIterator iter = new ByteIteratorEx() {
            private int rowNum = 0;
            private int colNum = 0;
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < count;
            }

            @Override
            public byte nextByte() {
                if (cnt++ >= count) {
                    throw new NoSuchElementException();
                }

                if (rowNum == rows) {
                    rowNum = 0;
                    colNum++;
                }

                while (a[rowNum] == null || colNum >= a[rowNum].length) {
                    if (rowNum < rows - 1) {
                        rowNum++;
                    } else {
                        rowNum = 0;
                        colNum++;
                    }
                }

                return a[rowNum++][colNum];
            }
        };

        return of(iter);
    }

    public static ByteStream flat(final byte[][] a, final byte valueForNone, final boolean vertically) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        } else if (a.length == 1) {
            return of(a[0]);
        }

        long n = 0;
        int maxLen = 0;

        for (byte[] e : a) {
            n += N.len(e);
            maxLen = N.max(maxLen, N.len(e));
        }

        if (n == 0) {
            return empty();
        }

        final int rows = N.len(a);
        final int cols = maxLen;
        final long count = rows * cols;
        ByteIterator iter = null;

        if (vertically) {
            iter = new ByteIteratorEx() {
                private int rowNum = 0;
                private int colNum = 0;
                private long cnt = 0;

                @Override
                public boolean hasNext() {
                    return cnt < count;
                }

                @Override
                public byte nextByte() {
                    if (cnt++ >= count) {
                        throw new NoSuchElementException();
                    }

                    if (rowNum == rows) {
                        rowNum = 0;
                        colNum++;
                    }

                    if (a[rowNum] == null || colNum >= a[rowNum].length) {
                        rowNum++;
                        return valueForNone;
                    } else {
                        return a[rowNum++][colNum];
                    }
                }
            };

        } else {
            iter = new ByteIteratorEx() {
                private int rowNum = 0;
                private int colNum = 0;
                private long cnt = 0;

                @Override
                public boolean hasNext() {
                    return cnt < count;
                }

                @Override
                public byte nextByte() {
                    if (cnt++ >= count) {
                        throw new NoSuchElementException();
                    }

                    if (colNum >= cols) {
                        colNum = 0;
                        rowNum++;
                    }

                    if (a[rowNum] == null || colNum >= a[rowNum].length) {
                        colNum++;
                        return valueForNone;
                    } else {
                        return a[rowNum][colNum++];
                    }
                }
            };
        }

        return of(iter);
    }

    public static ByteStream flat(final byte[][][] a) {
        return N.isNullOrEmpty(a) ? empty() : Stream.of(a).flatMapToByte(flatMappper);
    }

    public static ByteStream range(final byte startInclusive, final byte endExclusive) {
        if (startInclusive >= endExclusive) {
            return empty();
        }

        return new IteratorByteStream(new ByteIteratorEx() {
            private byte next = startInclusive;
            private int cnt = endExclusive * 1 - startInclusive;

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public byte nextByte() {
                if (cnt-- <= 0) {
                    throw new NoSuchElementException();
                }

                return next++;
            }

            @Override
            public void skip(long n) {
                N.checkArgNotNegative(n, "n");

                cnt = n >= cnt ? 0 : cnt - (int) n;
                next += n;
            }

            @Override
            public long count() {
                return cnt;
            }

            @Override
            public byte[] toArray() {
                final byte[] result = new byte[cnt];

                for (int i = 0; i < cnt; i++) {
                    result[i] = next++;
                }

                cnt = 0;

                return result;
            }
        });
    }

    public static ByteStream range(final byte startInclusive, final byte endExclusive, final byte by) {
        if (by == 0) {
            throw new IllegalArgumentException("'by' can't be zero");
        }

        if (endExclusive == startInclusive || endExclusive > startInclusive != by > 0) {
            return empty();
        }

        return new IteratorByteStream(new ByteIteratorEx() {
            private byte next = startInclusive;
            private int cnt = (endExclusive * 1 - startInclusive) / by + ((endExclusive * 1 - startInclusive) % by == 0 ? 0 : 1);

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public byte nextByte() {
                if (cnt-- <= 0) {
                    throw new NoSuchElementException();
                }

                byte result = next;
                next += by;
                return result;
            }

            @Override
            public void skip(long n) {
                N.checkArgNotNegative(n, "n");

                cnt = n >= cnt ? 0 : cnt - (int) n;
                next += n * by;
            }

            @Override
            public long count() {
                return cnt;
            }

            @Override
            public byte[] toArray() {
                final byte[] result = new byte[cnt];

                for (int i = 0; i < cnt; i++, next += by) {
                    result[i] = next;
                }

                cnt = 0;

                return result;
            }
        });
    }

    public static ByteStream rangeClosed(final byte startInclusive, final byte endInclusive) {
        if (startInclusive > endInclusive) {
            empty();
        } else if (startInclusive == endInclusive) {
            return of(startInclusive);
        }

        return new IteratorByteStream(new ByteIteratorEx() {
            private byte next = startInclusive;
            private int cnt = endInclusive * 1 - startInclusive + 1;

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public byte nextByte() {
                if (cnt-- <= 0) {
                    throw new NoSuchElementException();
                }

                return next++;
            }

            @Override
            public void skip(long n) {
                N.checkArgNotNegative(n, "n");

                cnt = n >= cnt ? 0 : cnt - (int) n;
                next += n;
            }

            @Override
            public long count() {
                return cnt;
            }

            @Override
            public byte[] toArray() {
                final byte[] result = new byte[cnt];

                for (int i = 0; i < cnt; i++) {
                    result[i] = next++;
                }

                cnt = 0;

                return result;
            }
        });
    }

    public static ByteStream rangeClosed(final byte startInclusive, final byte endInclusive, final byte by) {
        if (by == 0) {
            throw new IllegalArgumentException("'by' can't be zero");
        }

        if (endInclusive == startInclusive) {
            return of(startInclusive);
        } else if (endInclusive > startInclusive != by > 0) {
            return empty();
        }

        return new IteratorByteStream(new ByteIteratorEx() {
            private byte next = startInclusive;
            private int cnt = (endInclusive * 1 - startInclusive) / by + 1;

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public byte nextByte() {
                if (cnt-- <= 0) {
                    throw new NoSuchElementException();
                }

                byte result = next;
                next += by;
                return result;
            }

            @Override
            public void skip(long n) {
                N.checkArgNotNegative(n, "n");

                cnt = n >= cnt ? 0 : cnt - (int) n;
                next += n * by;
            }

            @Override
            public long count() {
                return cnt;
            }

            @Override
            public byte[] toArray() {
                final byte[] result = new byte[cnt];

                for (int i = 0; i < cnt; i++, next += by) {
                    result[i] = next;
                }

                cnt = 0;

                return result;
            }
        });
    }

    public static ByteStream repeat(final byte element, final long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return empty();
        }

        return new IteratorByteStream(new ByteIteratorEx() {
            private long cnt = n;

            @Override
            public boolean hasNext() {
                return cnt > 0;
            }

            @Override
            public byte nextByte() {
                if (cnt-- <= 0) {
                    throw new NoSuchElementException();
                }

                return element;
            }

            @Override
            public void skip(long n) {
                N.checkArgNotNegative(n, "n");

                cnt = n >= cnt ? 0 : cnt - (int) n;
            }

            @Override
            public long count() {
                return cnt;
            }

            @Override
            public byte[] toArray() {
                final byte[] result = new byte[(int) cnt];

                for (int i = 0; i < cnt; i++) {
                    result[i] = element;
                }

                cnt = 0;

                return result;
            }
        });
    }

    public static ByteStream random() {
        return generate(new ByteSupplier() {
            @Override
            public byte getAsByte() {
                return (byte) RAND.nextInt();
            }
        });
    }

    public static ByteStream iterate(final BooleanSupplier hasNext, final ByteSupplier next) {
        N.checkArgNotNull(hasNext);
        N.checkArgNotNull(next);

        return new IteratorByteStream(new ByteIteratorEx() {
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.getAsBoolean();
                }

                return hasNextVal;
            }

            @Override
            public byte nextByte() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;
                return next.getAsByte();
            }
        });
    }

    public static ByteStream iterate(final byte seed, final BooleanSupplier hasNext, final ByteUnaryOperator f) {
        N.checkArgNotNull(hasNext);
        N.checkArgNotNull(f);

        return new IteratorByteStream(new ByteIteratorEx() {
            private byte t = 0;
            private boolean isFirst = true;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false) {
                    hasNextVal = hasNext.getAsBoolean();
                }

                return hasNextVal;
            }

            @Override
            public byte nextByte() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNextVal = false;

                if (isFirst) {
                    isFirst = false;
                    t = seed;
                } else {
                    t = f.applyAsByte(t);
                }

                return t;
            }
        });
    }

    /**
     * 
     * @param seed
     * @param hasNext test if has next by hasNext.test(seed) for first time and hasNext.test(f.apply(previous)) for remaining.
     * @param f
     * @return
     */
    public static ByteStream iterate(final byte seed, final BytePredicate hasNext, final ByteUnaryOperator f) {
        N.checkArgNotNull(hasNext);
        N.checkArgNotNull(f);

        return new IteratorByteStream(new ByteIteratorEx() {
            private byte t = 0;
            private byte cur = 0;
            private boolean isFirst = true;
            private boolean hasMore = true;
            private boolean hasNextVal = false;

            @Override
            public boolean hasNext() {
                if (hasNextVal == false && hasMore) {
                    if (isFirst) {
                        isFirst = false;
                        hasNextVal = hasNext.test(cur = seed);
                    } else {
                        hasNextVal = hasNext.test(cur = f.applyAsByte(t));
                    }

                    if (hasNextVal == false) {
                        hasMore = false;
                    }
                }

                return hasNextVal;
            }

            @Override
            public byte nextByte() {
                if (hasNextVal == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                t = cur;
                hasNextVal = false;
                return t;
            }
        });
    }

    public static ByteStream iterate(final byte seed, final ByteUnaryOperator f) {
        N.checkArgNotNull(f);

        return new IteratorByteStream(new ByteIteratorEx() {
            private byte t = 0;
            private boolean isFirst = true;

            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public byte nextByte() {
                if (isFirst) {
                    isFirst = false;
                    t = seed;
                } else {
                    t = f.applyAsByte(t);
                }

                return t;
            }
        });
    }

    public static ByteStream generate(final ByteSupplier s) {
        N.checkArgNotNull(s);

        return new IteratorByteStream(new ByteIteratorEx() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public byte nextByte() {
                return s.getAsByte();
            }
        });
    }

    @SafeVarargs
    public static ByteStream concat(final byte[]... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorByteStream(new ByteIteratorEx() {
            private final Iterator<byte[]> iter = N.asList(a).iterator();
            private ByteIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = ByteIteratorEx.of(iter.next());
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
        });
    }

    @SafeVarargs
    public static ByteStream concat(final ByteIterator... a) {
        return N.isNullOrEmpty(a) ? empty() : new IteratorByteStream(new ByteIteratorEx() {
            private final Iterator<? extends ByteIterator> iter = N.asList(a).iterator();
            private ByteIterator cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
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
        });
    }

    @SafeVarargs
    public static ByteStream concat(final ByteStream... a) {
        return N.isNullOrEmpty(a) ? empty() : concat(N.asList(a));
    }

    public static ByteStream concat(final Collection<? extends ByteStream> c) {
        return N.isNullOrEmpty(c) ? empty() : new IteratorByteStream(new ByteIteratorEx() {
            private final Iterator<? extends ByteStream> iterators = c.iterator();
            private ByteStream cur;
            private ByteIterator iter;

            @Override
            public boolean hasNext() {
                while ((iter == null || iter.hasNext() == false) && iterators.hasNext()) {
                    if (cur != null) {
                        cur.close();
                    }

                    cur = iterators.next();
                    iter = cur.iterator();
                }

                return iter != null && iter.hasNext();
            }

            @Override
            public byte nextByte() {
                if ((iter == null || iter.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return iter.nextByte();
            }
        }).onClose(newCloseHandler(c));
    }

    /**
     * Zip together the "a" and "b" arrays until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ByteStream zip(final byte[] a, final byte[] b, final ByteBiFunction<Byte> zipFunction) {
        return Stream.zip(a, b, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" arrays until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ByteStream zip(final byte[] a, final byte[] b, final byte[] c, final ByteTriFunction<Byte> zipFunction) {
        return Stream.zip(a, b, c, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ByteStream zip(final ByteIterator a, final ByteIterator b, final ByteBiFunction<Byte> zipFunction) {
        return Stream.zip(a, b, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ByteStream zip(final ByteIterator a, final ByteIterator b, final ByteIterator c, final ByteTriFunction<Byte> zipFunction) {
        return Stream.zip(a, b, c, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" streams until one of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ByteStream zip(final ByteStream a, final ByteStream b, final ByteBiFunction<Byte> zipFunction) {
        return Stream.zip(a, b, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" streams until one of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @return
     */
    public static ByteStream zip(final ByteStream a, final ByteStream b, final ByteStream c, final ByteTriFunction<Byte> zipFunction) {
        return Stream.zip(a, b, c, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the iterators until one of them runs out of values.
     * Each array of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param c
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final Collection<? extends ByteStream> c, final ByteNFunction<Byte> zipFunction) {
        return Stream.zip(c, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB, final ByteBiFunction<Byte> zipFunction) {
        return Stream.zip(a, b, valueForNoneA, valueForNoneB, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB, final byte valueForNoneC,
            final ByteTriFunction<Byte> zipFunction) {
        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final ByteIterator a, final ByteIterator b, final byte valueForNoneA, final byte valueForNoneB,
            final ByteBiFunction<Byte> zipFunction) {
        return Stream.zip(a, b, valueForNoneA, valueForNoneB, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final ByteIterator a, final ByteIterator b, final ByteIterator c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final ByteTriFunction<Byte> zipFunction) {
        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a" and "b" iterators until all of them runs out of values.
     * Each pair of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param valueForNoneA value to fill if "a" runs out of values first.
     * @param valueForNoneB value to fill if "b" runs out of values first.
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final ByteStream a, final ByteStream b, final byte valueForNoneA, final byte valueForNoneB,
            final ByteBiFunction<Byte> zipFunction) {
        return Stream.zip(a, b, valueForNoneA, valueForNoneB, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the "a", "b" and "c" iterators until all of them runs out of values.
     * Each triple of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param a
     * @param b
     * @param c
     * @param valueForNoneA value to fill if "a" runs out of values.
     * @param valueForNoneB value to fill if "b" runs out of values.
     * @param valueForNoneC value to fill if "c" runs out of values.
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final ByteStream a, final ByteStream b, final ByteStream c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final ByteTriFunction<Byte> zipFunction) {
        return Stream.zip(a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * Zip together the iterators until all of them runs out of values.
     * Each array of values is combined into a single value using the supplied zipFunction function.
     * 
     * @param c
     * @param valuesForNone value to fill for any iterator runs out of values.
     * @param zipFunction
     * @return
     */
    public static ByteStream zip(final Collection<? extends ByteStream> c, final byte[] valuesForNone, final ByteNFunction<Byte> zipFunction) {
        return Stream.zip(c, valuesForNone, zipFunction).mapToByte(ToByteFunction.UNBOX);
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream merge(final byte[] a, final byte[] b, final ByteBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return of(b);
        } else if (N.isNullOrEmpty(b)) {
            return of(a);
        }

        return new IteratorByteStream(new ByteIteratorEx() {
            private final int lenA = a.length;
            private final int lenB = b.length;
            private int cursorA = 0;
            private int cursorB = 0;

            @Override
            public boolean hasNext() {
                return cursorA < lenA || cursorB < lenB;
            }

            @Override
            public byte nextByte() {
                if (cursorA < lenA) {
                    if (cursorB < lenB) {
                        if (nextSelector.apply(a[cursorA], b[cursorB]) == Nth.FIRST) {
                            return a[cursorA++];
                        } else {
                            return b[cursorB++];
                        }
                    } else {
                        return a[cursorA++];
                    }
                } else if (cursorB < lenB) {
                    return b[cursorB++];
                } else {
                    throw new NoSuchElementException();
                }
            }
        });
    }

    /**
     * 
     * @param a
     * @param b
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream merge(final byte[] a, final byte[] b, final byte[] c, final ByteBiFunction<Nth> nextSelector) {
        return merge(merge(a, b, nextSelector).iteratorEx(), ByteStream.of(c).iteratorEx(), nextSelector);
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream merge(final ByteIterator a, final ByteIterator b, final ByteBiFunction<Nth> nextSelector) {
        return new IteratorByteStream(new ByteIteratorEx() {
            private byte nextA = 0;
            private byte nextB = 0;
            private boolean hasNextA = false;
            private boolean hasNextB = false;

            @Override
            public boolean hasNext() {
                return a.hasNext() || b.hasNext() || hasNextA || hasNextB;
            }

            @Override
            public byte nextByte() {
                if (hasNextA) {
                    if (b.hasNext()) {
                        if (nextSelector.apply(nextA, (nextB = b.nextByte())) == Nth.FIRST) {
                            hasNextA = false;
                            hasNextB = true;
                            return nextA;
                        } else {
                            return nextB;
                        }
                    } else {
                        hasNextA = false;
                        return nextA;
                    }
                } else if (hasNextB) {
                    if (a.hasNext()) {
                        if (nextSelector.apply((nextA = a.nextByte()), nextB) == Nth.FIRST) {
                            return nextA;
                        } else {
                            hasNextA = true;
                            hasNextB = false;
                            return nextB;
                        }
                    } else {
                        hasNextB = false;
                        return nextB;
                    }
                } else if (a.hasNext()) {
                    if (b.hasNext()) {
                        if (nextSelector.apply((nextA = a.nextByte()), (nextB = b.nextByte())) == Nth.FIRST) {
                            hasNextB = true;
                            return nextA;
                        } else {
                            hasNextA = true;
                            return nextB;
                        }
                    } else {
                        return a.nextByte();
                    }
                } else if (b.hasNext()) {
                    return b.nextByte();
                } else {
                    throw new NoSuchElementException();
                }
            }
        });
    }

    /**
     * 
     * @param a
     * @param b
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream merge(final ByteIterator a, final ByteIterator b, final ByteIterator c, final ByteBiFunction<Nth> nextSelector) {
        return merge(merge(a, b, nextSelector).iteratorEx(), c, nextSelector);
    }

    /**
     * 
     * @param a
     * @param b
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream merge(final ByteStream a, final ByteStream b, final ByteBiFunction<Nth> nextSelector) {
        return merge(a.iteratorEx(), b.iteratorEx(), nextSelector).onClose(newCloseHandler(N.asList(a, b)));
    }

    /**
     * 
     * @param a
     * @param b
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream merge(final ByteStream a, final ByteStream b, final ByteStream c, final ByteBiFunction<Nth> nextSelector) {
        return merge(merge(a, b, nextSelector), c, nextSelector);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream merge(final Collection<? extends ByteStream> c, final ByteBiFunction<Nth> nextSelector) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return c.iterator().next();
        } else if (c.size() == 2) {
            final Iterator<? extends ByteStream> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        }

        final Iterator<? extends ByteStream> iter = c.iterator();
        ByteStream result = merge(iter.next(), iter.next(), nextSelector);

        while (iter.hasNext()) {
            result = merge(result, iter.next(), nextSelector);
        }

        return result;
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @return
     */
    public static ByteStream parallelMerge(final Collection<? extends ByteStream> c, final ByteBiFunction<Nth> nextSelector) {
        return parallelMerge(c, nextSelector, DEFAULT_MAX_THREAD_NUM);
    }

    /**
     * 
     * @param c
     * @param nextSelector first parameter is selected if <code>Nth.FIRST</code> is returned, otherwise the second parameter is selected.
     * @param maxThreadNum
     * @return
     */
    public static ByteStream parallelMerge(final Collection<? extends ByteStream> c, final ByteBiFunction<Nth> nextSelector, final int maxThreadNum) {
        N.checkArgument(maxThreadNum > 0, "'maxThreadNum' must not less than 1");

        if (maxThreadNum <= 1) {
            return merge(c, nextSelector);
        } else if (N.isNullOrEmpty(c)) {
            return empty();
        } else if (c.size() == 1) {
            return c.iterator().next();
        } else if (c.size() == 2) {
            final Iterator<? extends ByteStream> iter = c.iterator();
            return merge(iter.next(), iter.next(), nextSelector);
        } else if (c.size() == 3) {
            final Iterator<? extends ByteStream> iter = c.iterator();
            return merge(iter.next(), iter.next(), iter.next(), nextSelector);
        }

        final Queue<ByteStream> queue = N.newLinkedList();

        for (ByteStream e : c) {
            queue.add(e);
        }

        final Holder<Throwable> eHolder = new Holder<>();
        final MutableInt cnt = MutableInt.of(c.size());
        final List<ContinuableFuture<Void>> futureList = new ArrayList<>(c.size() - 1);

        for (int i = 0, n = N.min(maxThreadNum, c.size() / 2 + 1); i < n; i++) {
            futureList.add(DEFAULT_ASYNC_EXECUTOR.execute(new Try.Runnable<RuntimeException>() {
                @Override
                public void run() {
                    ByteStream a = null;
                    ByteStream b = null;
                    ByteStream c = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (queue) {
                                if (cnt.intValue() > 2 && queue.size() > 1) {
                                    a = queue.poll();
                                    b = queue.poll();

                                    cnt.decrement();
                                } else {
                                    break;
                                }
                            }

                            c = ByteStream.of(merge(a, b, nextSelector).toArray());

                            synchronized (queue) {
                                queue.offer(c);
                            }
                        }
                    } catch (Exception e) {
                        setError(eHolder, e);
                    }
                }
            }));
        }

        try {
            complete(futureList, eHolder);
        } finally {
            if (eHolder.value() != null) {
                IOUtil.closeAllQuietly(c);
            }
        }

        return merge(queue.poll(), queue.poll(), nextSelector);
    }
}
