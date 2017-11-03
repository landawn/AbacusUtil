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
import com.landawn.abacus.util.IndexedLong;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Nullable;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.LongBiFunction;
import com.landawn.abacus.util.function.LongBinaryOperator;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.LongToDoubleFunction;
import com.landawn.abacus.util.function.LongToFloatFunction;
import com.landawn.abacus.util.function.LongToIntFunction;
import com.landawn.abacus.util.function.LongTriFunction;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class ParallelArrayLongStream extends ArrayLongStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile ArrayLongStream sequential;
    private volatile Stream<Long> boxed;

    ParallelArrayLongStream(final long[] values, final int fromIndex, final int toIndex, final boolean sorted, int maxThreadNum, Splitor splitor,
            final Collection<Runnable> closeHandlers) {
        super(values, fromIndex, toIndex, sorted, closeHandlers);

        this.maxThreadNum = fromIndex >= toIndex ? 1 : N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION, toIndex - fromIndex);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    @Override
    public LongStream filter(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().filter(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Long> stream = boxed().filter(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value.longValue());
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().takeWhile(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Long> stream = boxed().takeWhile(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value.longValue());
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().dropWhile(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Long> stream = boxed().dropWhile(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value.longValue());
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream map(final LongUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().map(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Long>() {
            @Override
            public long applyAsLong(Long value) {
                return mapper.applyAsLong(value.longValue());
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final LongToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Long>() {
            @Override
            public int applyAsInt(Long value) {
                return mapper.applyAsInt(value.longValue());
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final LongToFloatFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Long>() {
            @Override
            public float applyAsFloat(Long value) {
                return mapper.applyAsFloat(value.longValue());
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(final LongToDoubleFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Long>() {
            @Override
            public double applyAsDouble(Long value) {
                return mapper.applyAsDouble(value.longValue());
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        return boxed().map(new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return mapper.apply(value.longValue());
            }
        });
    }

    @Override
    public LongStream flatMap(final LongFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMap(mapper), false, maxThreadNum, splitor, null);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Long, LongStream>() {
            @Override
            public LongStream apply(Long value) {
                return mapper.apply(value.longValue());
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public IntStream flatMapToInt(final LongFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper), false, maxThreadNum, splitor, null);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Long, IntStream>() {
            @Override
            public IntStream apply(Long value) {
                return mapper.apply(value.longValue());
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public FloatStream flatMapToFloat(final LongFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper), false, maxThreadNum, splitor, null);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Long, FloatStream>() {
            @Override
            public FloatStream apply(Long value) {
                return mapper.apply(value.longValue());
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public DoubleStream flatMapToDouble(final LongFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper), false, maxThreadNum, splitor, null);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Long, DoubleStream>() {
            @Override
            public DoubleStream apply(Long value) {
                return mapper.apply(value.longValue());
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final LongFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper), false, null, maxThreadNum, splitor, null);
        }

        return boxed().flatMap(new Function<Long, Stream<T>>() {
            @Override
            public Stream<T> apply(Long value) {
                return mapper.apply(value.longValue());
            }
        });
    }

    @Override
    public Stream<LongStream> split(final int size) {
        return new ParallelIteratorStream<LongStream>(sequential().split(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<LongList> splitToList(final int size) {
        return new ParallelIteratorStream<LongList>(sequential().splitToList(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<LongStream> split(final U seed, final BiFunction<? super Long, ? super U, Boolean> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<LongStream>(sequential().split(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public <U> Stream<LongList> splitToList(final U seed, final BiFunction<? super Long, ? super U, Boolean> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<LongList>(sequential().splitToList(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public Stream<LongStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final LongStream[] a = new LongStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? LongStream.empty() : new ArrayLongStream(elements, fromIndex, middleIndex, sorted, null);
        a[1] = middleIndex == toIndex ? LongStream.empty() : new ArrayLongStream(elements, middleIndex, toIndex, sorted, null);

        return new ParallelArrayStream<>(a, 0, a.length, false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<LongStream> splitBy(final LongPredicate where) {
        N.requireNonNull(where);

        final Nullable<IndexedLong> first = indexed().findFirst(new Predicate<IndexedLong>() {
            @Override
            public boolean test(IndexedLong indexed) {
                return !where.test(indexed.value());
            }
        });

        return splitAt(first.isPresent() ? (int) first.get().index() : toIndex - fromIndex);
    }

    @Override
    public Stream<LongStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<LongStream>(sequential().sliding(windowSize, increment).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public Stream<LongList> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<LongList>(sequential().slidingToList(windowSize, increment).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public LongStream top(int n) {
        return top(n, LONG_COMPARATOR);
    }

    @Override
    public LongStream top(int n, Comparator<? super Long> comparator) {
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, LONG_COMPARATOR)) {
            return new ParallelArrayLongStream(elements, toIndex - n, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            final long[] a = N.top(elements, fromIndex, toIndex, n, comparator);
            return new ParallelArrayLongStream(a, 0, a.length, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        final long[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a);
        return new ParallelArrayLongStream(a, 0, a.length, true, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream peek(final LongConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().peek(action).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final LongStream stream = boxed().peek(new Consumer<Long>() {
            @Override
            public void accept(Long t) {
                action.accept(t);
            }
        }).sequential().mapToLong(ToLongFunction.UNBOX);

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayLongStream(elements, fromIndex, (int) (fromIndex + maxSize), sorted, maxThreadNum, splitor, closeHandlers);

    }

    @Override
    public LongStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayLongStream(elements, toIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            return new ParallelArrayLongStream(elements, (int) (fromIndex + n), toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public void forEach(final LongConsumer action) {
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
                        long next = 0;

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
    public long[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public LongList toLongList() {
        return LongList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Long> toList() {
        final List<Long> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends List<Long>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Long> toSet() {
        final Set<Long> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends Set<Long>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset() {
        final Multiset<Long> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

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
        final LongMultiset<Long> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

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
    public <K, U, M extends Map<K, U>> M toMap(final LongFunction<? extends K> keyExtractor, final LongFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Long, ? extends K> keyExtractor2 = new Function<Long, K>() {
            @Override
            public K apply(Long value) {
                return keyExtractor.apply(value);
            }
        };

        final Function<? super Long, ? extends U> valueMapper2 = new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyExtractor2, valueMapper2, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final LongFunction<? extends K> classifier, final Collector<Long, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Long, ? extends K> classifier2 = new Function<Long, K>() {
            @Override
            public K apply(Long value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public OptionalLong first() {
        return fromIndex < toIndex ? OptionalLong.of(elements[fromIndex]) : OptionalLong.empty();
    }

    @Override
    public OptionalLong last() {
        return fromIndex < toIndex ? OptionalLong.of(elements[toIndex - 1]) : OptionalLong.empty();
    }

    @Override
    public long reduce(final long identity, final LongBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Long>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Long>() {
                    @Override
                    public Long call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        long result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = op.applyAsLong(result, elements[cursor++]);
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
                futureList.add(asyncExecutor.execute(new Callable<Long>() {
                    @Override
                    public Long call() {
                        long result = identity;
                        long next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = op.applyAsLong(result, next);
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

        Long result = null;

        try {
            for (CompletableFuture<Long> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsLong(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalLong reduce(final LongBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Long>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Long>() {
                    @Override
                    public Long call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        if (cursor >= to) {
                            return null;
                        }

                        long result = elements[cursor++];

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.applyAsLong(result, elements[cursor++]);
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
                futureList.add(asyncExecutor.execute(new Callable<Long>() {
                    @Override
                    public Long call() {
                        long result = 0;

                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                result = elements[cursor.getAndIncrement()];
                            } else {
                                return null;
                            }
                        }

                        long next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = accumulator.applyAsLong(result, next);
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

        Long result = null;

        try {
            for (CompletableFuture<Long> future : futureList) {
                final Long tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsLong(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalLong.empty() : OptionalLong.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjLongConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
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
                        long next = 0;

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
    public LongStream tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayLongStream(elements, fromIndex + 1, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream head2() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayLongStream(elements, fromIndex, toIndex - 1, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public OptionalLong min() {
        if (fromIndex == toIndex) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements[fromIndex]);
        } else if (maxThreadNum <= 1) {
            return OptionalLong.of(N.min(elements, fromIndex, toIndex));
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

                    return cursor >= to ? null : N.min(elements, cursor, to);
                }
            }));
        }

        Long candidate = null;

        try {
            for (CompletableFuture<Long> future : futureList) {
                final Long tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || tmp.longValue() < candidate.longValue()) {
                    candidate = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalLong.empty() : OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong max() {
        if (fromIndex == toIndex) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements[toIndex - 1]);
        } else if (maxThreadNum <= 1) {
            return OptionalLong.of(N.max(elements, fromIndex, toIndex));
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
                    return cursor >= to ? null : N.max(elements, cursor, to);
                }
            }));
        }

        Long candidate = null;

        try {
            for (CompletableFuture<Long> future : futureList) {
                final Long tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || tmp.longValue() > candidate.longValue()) {
                    candidate = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalLong.empty() : OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (k > toIndex - fromIndex) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements[toIndex - k]);
        }

        return OptionalLong.of(N.kthLargest(elements, fromIndex, toIndex, k));
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
    public LongStream reversed() {
        return new ParallelIteratorLongStream(sequential().reversed().iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongSummaryStatistics summarize() {
        if (fromIndex == toIndex) {
            return new LongSummaryStatistics();
        } else if (maxThreadNum <= 1) {
            return sequential().summarize();
        }

        final List<CompletableFuture<LongSummaryStatistics>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<LongSummaryStatistics>() {
                @Override
                public LongSummaryStatistics call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    final LongSummaryStatistics result = new LongSummaryStatistics();

                    for (int i = cursor; i < to; i++) {
                        result.accept(elements[i]);
                    }

                    return result;
                }
            }));
        }

        LongSummaryStatistics result = null;

        try {
            for (CompletableFuture<LongSummaryStatistics> future : futureList) {
                final LongSummaryStatistics tmp = future.get();

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
    public boolean anyMatch(final LongPredicate predicate) {
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
                        long next = 0;

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
    public boolean allMatch(final LongPredicate predicate) {
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
                        long next = 0;

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
    public boolean noneMatch(final LongPredicate predicate) {
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
                        long next = 0;

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
    public OptionalLong findFirst(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Long>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, Long> pair = new Pair<>();

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
                        final Pair<Integer, Long> pair = new Pair<>();

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

        return resultHolder.value() == null ? OptionalLong.empty() : OptionalLong.of(resultHolder.value().right);
    }

    @Override
    public OptionalLong findLast(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Long>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, Long> pair = new Pair<>();

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
                        final Pair<Integer, Long> pair = new Pair<>();

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

        return resultHolder.value() == null ? OptionalLong.empty() : OptionalLong.of(resultHolder.value().right);
    }

    @Override
    public OptionalLong findAny(final LongPredicate predicate) {
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
                        long next = 0;

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
                        long next = 0;

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

        return resultHolder.value() == NONE ? OptionalLong.empty() : OptionalLong.of((Long) resultHolder.value());
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
    public Stream<Long> boxed() {
        Stream<Long> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Long>(iterator(), sorted, sorted ? LONG_COMPARATOR : null, maxThreadNum, splitor, closeHandlers);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public LongStream cached() {
        return this;
    }

    @Override
    public LongStream append(LongStream stream) {
        return new ParallelIteratorLongStream(LongStream.concat(this, stream), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream prepend(LongStream stream) {
        return new ParallelIteratorLongStream(LongStream.concat(stream, this), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream merge(final LongStream b, final LongBiFunction<Nth> nextSelector) {
        return new ParallelIteratorLongStream(LongStream.merge(this, b, nextSelector), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream zipWith(LongStream b, LongBiFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, LongTriFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, c, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream zipWith(LongStream b, long valueForNoneA, long valueForNoneB, LongBiFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, long valueForNoneA, long valueForNoneB, long valueForNoneC, LongTriFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), false, maxThreadNum,
                splitor, closeHandlers);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public LongStream sequential() {
        ArrayLongStream tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayLongStream(elements, fromIndex, toIndex, sorted, closeHandlers);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public LongStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum && this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayLongStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public LongStream maxThreadNum(int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelArrayLongStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public LongStream splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayLongStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayLongStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, newCloseHandlers);
    }
}
