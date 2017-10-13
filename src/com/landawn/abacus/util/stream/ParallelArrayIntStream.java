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
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Callable;

import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IndexedInt;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntBiFunction;
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
import com.landawn.abacus.util.function.IntTriFunction;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.ObjIntConsumer;
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
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class ParallelArrayIntStream extends ArrayIntStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile ArrayIntStream sequential;
    private volatile Stream<Integer> boxed;

    ParallelArrayIntStream(final int[] values, final int fromIndex, final int toIndex, final boolean sorted, int maxThreadNum, Splitor splitor,
            final Collection<Runnable> closeHandlers) {
        super(values, fromIndex, toIndex, sorted, closeHandlers);

        this.maxThreadNum = fromIndex >= toIndex ? 1 : N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION, toIndex - fromIndex);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    @Override
    public IntStream filter(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().filter(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Integer> stream = boxed().filter(new Predicate<Integer>() {
            @Override
            public boolean test(Integer value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream takeWhile(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().takeWhile(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Integer> stream = boxed().takeWhile(new Predicate<Integer>() {
            @Override
            public boolean test(Integer value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream dropWhile(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().dropWhile(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Integer> stream = boxed().dropWhile(new Predicate<Integer>() {
            @Override
            public boolean test(Integer value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream map(final IntUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().map(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Integer>() {
            @Override
            public int applyAsInt(Integer value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public CharStream mapToChar(final IntToCharFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().mapToChar(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final CharStream stream = boxed().mapToChar(new ToCharFunction<Integer>() {
            @Override
            public char applyAsChar(Integer value) {
                return mapper.applyAsChar(value);
            }
        });

        return new ParallelIteratorCharStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public ByteStream mapToByte(final IntToByteFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().mapToByte(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final ByteStream stream = boxed().mapToByte(new ToByteFunction<Integer>() {
            @Override
            public byte applyAsByte(Integer value) {
                return mapper.applyAsByte(value);
            }
        });

        return new ParallelIteratorByteStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public ShortStream mapToShort(final IntToShortFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().mapToShort(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final ShortStream stream = boxed().mapToShort(new ToShortFunction<Integer>() {
            @Override
            public short applyAsShort(Integer value) {
                return mapper.applyAsShort(value);
            }
        });

        return new ParallelIteratorShortStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final IntToLongFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Integer>() {
            @Override
            public long applyAsLong(Integer value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final IntToFloatFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Integer>() {
            @Override
            public float applyAsFloat(Integer value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(final IntToDoubleFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Integer>() {
            @Override
            public double applyAsDouble(Integer value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final IntFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        return boxed().map(new Function<Integer, U>() {
            @Override
            public U apply(Integer value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public IntStream flatMap(final IntFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMap(mapper), false, maxThreadNum, splitor, null);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Integer, IntStream>() {
            @Override
            public IntStream apply(Integer value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public CharStream flatMapToChar(final IntFunction<? extends CharStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().flatMapToChar(mapper), false, maxThreadNum, splitor, null);
        }

        final CharStream stream = boxed().flatMapToChar(new Function<Integer, CharStream>() {
            @Override
            public CharStream apply(Integer value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorCharStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public ByteStream flatMapToByte(final IntFunction<? extends ByteStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().flatMapToByte(mapper), false, maxThreadNum, splitor, null);
        }

        final ByteStream stream = boxed().flatMapToByte(new Function<Integer, ByteStream>() {
            @Override
            public ByteStream apply(Integer value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorByteStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public ShortStream flatMapToShort(final IntFunction<? extends ShortStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().flatMapToShort(mapper), false, maxThreadNum, splitor, null);
        }

        final ShortStream stream = boxed().flatMapToShort(new Function<Integer, ShortStream>() {
            @Override
            public ShortStream apply(Integer value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorShortStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public LongStream flatMapToLong(final IntFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper), false, maxThreadNum, splitor, null);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Integer, LongStream>() {
            @Override
            public LongStream apply(Integer value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public FloatStream flatMapToFloat(final IntFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper), false, maxThreadNum, splitor, null);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Integer, FloatStream>() {
            @Override
            public FloatStream apply(Integer value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public DoubleStream flatMapToDouble(final IntFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper), false, maxThreadNum, splitor, null);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Integer, DoubleStream>() {
            @Override
            public DoubleStream apply(Integer value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final IntFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper), false, null, maxThreadNum, splitor, null);
        }

        return boxed().flatMap(new Function<Integer, Stream<T>>() {
            @Override
            public Stream<T> apply(Integer value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<IntStream> split(final int size) {
        return new ParallelIteratorStream<IntStream>(sequential().split(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<IntList> splitToList(final int size) {
        return new ParallelIteratorStream<IntList>(sequential().splitToList(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<IntStream> split(final U seed, final BiFunction<? super Integer, ? super U, Boolean> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<IntStream>(sequential().split(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public <U> Stream<IntList> splitToList(final U seed, final BiFunction<? super Integer, ? super U, Boolean> predicate,
            final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<IntList>(sequential().splitToList(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public Stream<IntStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final IntStream[] a = new IntStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? IntStream.empty() : new ArrayIntStream(elements, fromIndex, middleIndex, sorted, null);
        a[1] = middleIndex == toIndex ? IntStream.empty() : new ArrayIntStream(elements, middleIndex, toIndex, sorted, null);

        return new ParallelArrayStream<>(a, 0, a.length, false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<IntStream> splitBy(final IntPredicate where) {
        N.requireNonNull(where);

        final NullabLe<IndexedInt> first = indexed().findFirst(new Predicate<IndexedInt>() {
            @Override
            public boolean test(IndexedInt indexed) {
                return !where.test(indexed.value());
            }
        });

        return splitAt(first.isPresent() ? (int) first.get().index() : toIndex - fromIndex);
    }

    @Override
    public Stream<IntStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<IntStream>(sequential().sliding(windowSize, increment).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<IntList> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<IntList>(sequential().slidingToList(windowSize, increment).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
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
            return new ParallelArrayIntStream(elements, toIndex - n, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            final int[] a = N.top(elements, fromIndex, toIndex, n, comparator);
            return new ParallelArrayIntStream(a, 0, a.length, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public IntStream sorted() {
        if (sorted) {
            return this;
        }

        final int[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a);
        return new ParallelArrayIntStream(a, 0, a.length, true, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream peek(final IntConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().peek(action).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final IntStream stream = boxed().peek(new Consumer<Integer>() {
            @Override
            public void accept(Integer t) {
                action.accept(t);
            }
        }).sequential().mapToInt(ToIntFunction.UNBOX);

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayIntStream(elements, fromIndex, (int) (fromIndex + maxSize), sorted, maxThreadNum, splitor, closeHandlers);

    }

    @Override
    public IntStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayIntStream(elements, toIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            return new ParallelArrayIntStream(elements, (int) (fromIndex + n), toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public void forEach(final IntConsumer action) {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                action.accept(elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {

                    @Override
                    public void run() {
                        int next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                action.accept(next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complete(futureList, eHolder);
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
    public <R extends List<Integer>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

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
    public <R extends Set<Integer>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

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
    public <K, U, M extends Map<K, U>> M toMap(final IntFunction<? extends K> keyExtractor, final IntFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Integer, ? extends K> keyExtractor2 = new Function<Integer, K>() {
            @Override
            public K apply(Integer value) {
                return keyExtractor.apply(value);
            }
        };

        final Function<? super Integer, ? extends U> valueMapper2 = new Function<Integer, U>() {
            @Override
            public U apply(Integer value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyExtractor2, valueMapper2, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final IntFunction<? extends K> classifier, final Collector<Integer, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Integer, ? extends K> classifier2 = new Function<Integer, K>() {
            @Override
            public K apply(Integer value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
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
    public int reduce(final int identity, final IntBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Integer>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Integer>() {
                    @Override
                    public Integer call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        int result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = op.applyAsInt(result, elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Integer>() {

                    @Override
                    public Integer call() {
                        int result = identity;
                        int next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = op.applyAsInt(result, next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        Integer result = null;

        try {
            for (CompletableFuture<Integer> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsInt(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalInt reduce(final IntBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Integer>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Integer>() {
                    @Override
                    public Integer call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        if (cursor >= to) {
                            return null;
                        }

                        int result = elements[cursor++];

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.applyAsInt(result, elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Integer>() {

                    @Override
                    public Integer call() {
                        int result = 0;

                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                result = elements[cursor.getAndIncrement()];
                            } else {
                                return null;
                            }
                        }

                        int next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = accumulator.applyAsInt(result, next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return result;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        Integer result = null;

        try {
            for (CompletableFuture<Integer> future : futureList) {
                final Integer tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsInt(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalInt.empty() : OptionalInt.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjIntConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().collect(supplier, accumulator, combiner);
        }

        final List<CompletableFuture<R>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<R>() {
                    @Override
                    public R call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        final R container = supplier.get();

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                accumulator.accept(container, elements[cursor++]);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return container;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<R>() {

                    @Override
                    public R call() {
                        final R container = supplier.get();
                        int next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                accumulator.accept(container, next);
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return container;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        R container = (R) NONE;

        try {
            for (CompletableFuture<R> future : futureList) {
                if (container == NONE) {
                    container = future.get();
                } else {
                    combiner.accept(container, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return container == NONE ? supplier.get() : container;
    }

    @Override
    public IntStream tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayIntStream(elements, fromIndex + 1, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream head2() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayIntStream(elements, fromIndex, toIndex - 1, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public OptionalInt min() {
        if (fromIndex == toIndex) {
            return OptionalInt.empty();
        } else if (sorted) {
            return OptionalInt.of(elements[fromIndex]);
        } else if (maxThreadNum <= 1) {
            return OptionalInt.of(N.min(elements, fromIndex, toIndex));
        }

        final List<CompletableFuture<Integer>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Integer>() {
                @Override
                public Integer call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    return cursor >= to ? null : N.min(elements, cursor, to);
                }
            }));
        }

        Integer candidate = null;

        try {
            for (CompletableFuture<Integer> future : futureList) {
                final Integer tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || tmp.intValue() < candidate.intValue()) {
                    candidate = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalInt.empty() : OptionalInt.of(candidate);
    }

    @Override
    public OptionalInt max() {
        if (fromIndex == toIndex) {
            return OptionalInt.empty();
        } else if (sorted) {
            return OptionalInt.of(elements[toIndex - 1]);
        } else if (maxThreadNum <= 1) {
            return OptionalInt.of(N.max(elements, fromIndex, toIndex));
        }

        final List<CompletableFuture<Integer>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Integer>() {
                @Override
                public Integer call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    return cursor >= to ? null : N.max(elements, cursor, to);
                }
            }));
        }

        Integer candidate = null;

        try {
            for (CompletableFuture<Integer> future : futureList) {
                final Integer tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || tmp.intValue() > candidate.intValue()) {
                    candidate = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalInt.empty() : OptionalInt.of(candidate);
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
    public long sum() {
        if (fromIndex == toIndex) {
            return 0L;
        } else if (maxThreadNum <= 1) {
            return sum(elements, fromIndex, toIndex);
        }

        final List<CompletableFuture<Long>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Long>() {
                @Override
                public Long call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    return cursor >= to ? null : sum(elements, cursor, to);
                }
            }));
        }

        long result = 0;

        try {
            for (CompletableFuture<Long> future : futureList) {
                final Long tmp = future.get();

                if (tmp == null) {
                    continue;
                } else {
                    result += tmp.longValue();
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result;
    }

    @Override
    public OptionalDouble average() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(sum() / toIndex - fromIndex);
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public IntStream reversed() {
        return new ParallelIteratorIntStream(sequential().reversed().iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntSummaryStatistics summarize() {
        if (fromIndex == toIndex) {
            return new IntSummaryStatistics();
        } else if (maxThreadNum <= 1) {
            return sequential().summarize();
        }

        final List<CompletableFuture<IntSummaryStatistics>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<IntSummaryStatistics>() {
                @Override
                public IntSummaryStatistics call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    final IntSummaryStatistics result = new IntSummaryStatistics();

                    for (int i = cursor; i < to; i++) {
                        result.accept(elements[i]);
                    }

                    return result;
                }
            }));
        }

        IntSummaryStatistics result = null;

        try {
            for (CompletableFuture<IntSummaryStatistics> future : futureList) {
                final IntSummaryStatistics tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result.combine(tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && result.isFalse() && eHolder.value() == null) {
                                if (predicate.test(elements[cursor++])) {
                                    result.setTrue();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {

                    @Override
                    public void run() {
                        int next = 0;

                        try {
                            while (result.isFalse() && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(next)) {
                                    result.setTrue();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complete(futureList, eHolder);

        return result.value();
    }

    @Override
    public boolean allMatch(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && result.isTrue() && eHolder.value() == null) {
                                if (predicate.test(elements[cursor++]) == false) {
                                    result.setFalse();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {

                    @Override
                    public void run() {
                        int next = 0;

                        try {
                            while (result.isTrue() && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(next) == false) {
                                    result.setFalse();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complete(futureList, eHolder);

        return result.value();
    }

    @Override
    public boolean noneMatch(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        try {
                            while (cursor < to && result.isTrue() && eHolder.value() == null) {
                                if (predicate.test(elements[cursor++])) {
                                    result.setFalse();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {

                    @Override
                    public void run() {
                        int next = 0;

                        try {
                            while (result.isTrue() && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(next)) {
                                    result.setFalse();
                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complete(futureList, eHolder);

        return result.value();
    }

    @Override
    public OptionalInt findFirst(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Integer>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, Integer> pair = new Pair<>();

                        try {
                            while (cursor < to && (resultHolder.value() == null || cursor < resultHolder.value().left) && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[cursor++];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final Pair<Integer, Integer> pair = new Pair<>();

                        try {
                            while (resultHolder.value() == null && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        pair.left = cursor.intValue();
                                        pair.right = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complete(futureList, eHolder);

        return resultHolder.value() == null ? OptionalInt.empty() : OptionalInt.of(resultHolder.value().right);
    }

    @Override
    public OptionalInt findLast(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Integer>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, Integer> pair = new Pair<>();

                        try {
                            while (cursor > from && (resultHolder.value() == null || cursor > resultHolder.value().left) && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[--cursor];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(toIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final Pair<Integer, Integer> pair = new Pair<>();

                        try {
                            while (resultHolder.value() == null && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() > fromIndex) {
                                        pair.left = cursor.intValue();
                                        pair.right = elements[cursor.decrementAndGet()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                            resultHolder.setValue(pair.copy());
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complete(futureList, eHolder);

        return resultHolder.value() == null ? OptionalInt.empty() : OptionalInt.of(resultHolder.value().right);
    }

    @Override
    public OptionalInt findAny(final IntPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Object> resultHolder = Holder.of(NONE);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        int next = 0;

                        try {
                            while (cursor < to && resultHolder.value() == NONE && eHolder.value() == null) {
                                next = elements[cursor++];

                                if (predicate.test(next)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == NONE) {
                                            resultHolder.setValue(next);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int next = 0;

                        try {
                            while (resultHolder.value() == NONE && eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(next)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == NONE) {
                                            resultHolder.setValue(next);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }
                    }
                }));
            }
        }

        complete(futureList, eHolder);

        return resultHolder.value() == NONE ? OptionalInt.empty() : OptionalInt.of((Integer) resultHolder.value());
    }

    @Override
    public LongStream asLongStream() {
        return new ParallelIteratorLongStream(new LongIteratorEx() {
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
        }, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream asFloatStream() {
        return new ParallelIteratorFloatStream(new FloatIteratorEx() {
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
        }, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new ParallelIteratorDoubleStream(new DoubleIteratorEx() {
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
        }, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<Integer> boxed() {
        Stream<Integer> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Integer>(iterator(), sorted, sorted ? INT_COMPARATOR : null, maxThreadNum, splitor, closeHandlers);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public IntStream cached() {
        return this;
    }

    @Override
    public IntStream append(IntStream stream) {
        return new ParallelIteratorIntStream(IntStream.concat(this, stream), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream prepend(IntStream stream) {
        return new ParallelIteratorIntStream(IntStream.concat(stream, this), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream merge(final IntStream b, final IntBiFunction<Nth> nextSelector) {
        return new ParallelIteratorIntStream(IntStream.merge(this, b, nextSelector), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream zipWith(IntStream b, IntBiFunction<Integer> zipFunction) {
        return new ParallelIteratorIntStream(IntStream.zip(this, b, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream zipWith(IntStream b, IntStream c, IntTriFunction<Integer> zipFunction) {
        return new ParallelIteratorIntStream(IntStream.zip(this, b, c, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream zipWith(IntStream b, int valueForNoneA, int valueForNoneB, IntBiFunction<Integer> zipFunction) {
        return new ParallelIteratorIntStream(IntStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream zipWith(IntStream b, IntStream c, int valueForNoneA, int valueForNoneB, int valueForNoneC, IntTriFunction<Integer> zipFunction) {
        return new ParallelIteratorIntStream(IntStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), false, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public IntStream sequential() {
        ArrayIntStream tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayIntStream(elements, fromIndex, toIndex, sorted, closeHandlers);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public IntStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum && this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayIntStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public IntStream maxThreadNum(int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelArrayIntStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public IntStream splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayIntStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayIntStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, newCloseHandlers);
    }
}
