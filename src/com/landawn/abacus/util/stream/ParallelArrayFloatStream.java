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
import java.util.concurrent.ExecutionException;

import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IndexedFloat;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Nullable;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.FloatBiFunction;
import com.landawn.abacus.util.function.FloatBinaryOperator;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.function.FloatToDoubleFunction;
import com.landawn.abacus.util.function.FloatToIntFunction;
import com.landawn.abacus.util.function.FloatToLongFunction;
import com.landawn.abacus.util.function.FloatTriFunction;
import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjFloatConsumer;
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
final class ParallelArrayFloatStream extends ArrayFloatStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile ArrayFloatStream sequential;
    private volatile Stream<Float> boxed;

    ParallelArrayFloatStream(final float[] values, final int fromIndex, final int toIndex, final boolean sorted, int maxThreadNum, Splitor splitor,
            final Collection<Runnable> closeHandlers) {
        super(values, fromIndex, toIndex, sorted, closeHandlers);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    @Override
    public FloatStream filter(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().filter(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Float> stream = boxed().filter(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream takeWhile(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().takeWhile(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Float> stream = boxed().takeWhile(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream dropWhile(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().dropWhile(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Float> stream = boxed().dropWhile(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream map(final FloatUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().map(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Float>() {
            @Override
            public float applyAsFloat(Float value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final FloatToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Float>() {
            @Override
            public int applyAsInt(Float value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final FloatToLongFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Float>() {
            @Override
            public long applyAsLong(Float value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(final FloatToDoubleFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Float>() {
            @Override
            public double applyAsDouble(Float value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final FloatFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        return boxed().map(new Function<Float, U>() {
            @Override
            public U apply(Float value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public FloatStream flatMap(final FloatFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMap(mapper), false, maxThreadNum, splitor, null);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Float, FloatStream>() {
            @Override
            public FloatStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public IntStream flatMapToInt(final FloatFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper), false, maxThreadNum, splitor, null);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Float, IntStream>() {
            @Override
            public IntStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public LongStream flatMapToLong(final FloatFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper), false, maxThreadNum, splitor, null);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Float, LongStream>() {
            @Override
            public LongStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public DoubleStream flatMapToDouble(final FloatFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper), false, maxThreadNum, splitor, null);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Float, DoubleStream>() {
            @Override
            public DoubleStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, null);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final FloatFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper), false, null, maxThreadNum, splitor, null);
        }

        return boxed().flatMap(new Function<Float, Stream<T>>() {
            @Override
            public Stream<T> apply(Float value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<FloatStream> split(final int size) {
        return new ParallelIteratorStream<FloatStream>(sequential().split(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<FloatList> splitToList(final int size) {
        return new ParallelIteratorStream<FloatList>(sequential().splitToList(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<FloatStream> split(final U seed, final BiPredicate<? super Float, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<FloatStream>(sequential().split(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public <U> Stream<FloatList> splitToList(final U seed, final BiPredicate<? super Float, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<FloatList>(sequential().splitToList(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public Stream<FloatStream> splitAt(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        final FloatStream[] a = new FloatStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? FloatStream.empty() : new ArrayFloatStream(elements, fromIndex, middleIndex, sorted, null);
        a[1] = middleIndex == toIndex ? FloatStream.empty() : new ArrayFloatStream(elements, middleIndex, toIndex, sorted, null);

        return new ParallelArrayStream<>(a, 0, a.length, false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<FloatStream> splitBy(final FloatPredicate where) {
        N.requireNonNull(where);

        final Nullable<IndexedFloat> first = indexed().findFirst(new Predicate<IndexedFloat>() {
            @Override
            public boolean test(IndexedFloat indexed) {
                return !where.test(indexed.value());
            }
        });

        return splitAt(first.isPresent() ? (int) first.get().index() : toIndex - fromIndex);
    }

    @Override
    public Stream<FloatStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<FloatStream>(sequential().sliding(windowSize, increment).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public Stream<FloatList> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<FloatList>(sequential().slidingToList(windowSize, increment).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public FloatStream top(int n) {
        return top(n, FLOAT_COMPARATOR);
    }

    @Override
    public FloatStream top(int n, Comparator<? super Float> comparator) {
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, FLOAT_COMPARATOR)) {
            return new ParallelArrayFloatStream(elements, toIndex - n, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            final float[] a = N.top(elements, fromIndex, toIndex, n, comparator);
            return new ParallelArrayFloatStream(a, 0, a.length, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public FloatStream sorted() {
        if (sorted) {
            return this;
        }

        final float[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a);
        return new ParallelArrayFloatStream(a, 0, a.length, true, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream peek(final FloatConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().peek(action).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final FloatStream stream = boxed().peek(new Consumer<Float>() {
            @Override
            public void accept(Float t) {
                action.accept(t);
            }
        }).sequential().mapToFloat(ToFloatFunction.UNBOX);

        return new ParallelIteratorFloatStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream limit(long maxSize) {
        N.checkArgument(maxSize >= 0, "'maxSizse' can't be negative: %s", maxSize);

        if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayFloatStream(elements, fromIndex, (int) (fromIndex + maxSize), sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream skip(long n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayFloatStream(elements, toIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        } else {
            return new ParallelArrayFloatStream(elements, (int) (fromIndex + n), toIndex, sorted, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public <E extends Exception> void forEach(final Try.FloatConsumer<E> action) throws E {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        float next = 0;

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

        completee(futureList, eHolder, (E) null);
    }

    @Override
    public float[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public FloatList toFloatList() {
        return FloatList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Float> toList() {
        final List<Float> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends List<Float>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Float> toSet() {
        final Set<Float> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends Set<Float>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset() {
        final Multiset<Float> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset(Supplier<? extends Multiset<Float>> supplier) {
        final Multiset<Float> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset() {
        final LongMultiset<Float> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset(Supplier<? extends LongMultiset<Float>> supplier) {
        final LongMultiset<Float> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final FloatFunction<? extends K> keyExtractor, final FloatFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Float, ? extends K> keyExtractor2 = new Function<Float, K>() {
            @Override
            public K apply(Float value) {
                return keyExtractor.apply(value);
            }
        };

        final Function<? super Float, ? extends U> valueMapper2 = new Function<Float, U>() {
            @Override
            public U apply(Float value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyExtractor2, valueMapper2, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final FloatFunction<? extends K> classifier, final Collector<Float, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Float, ? extends K> classifier2 = new Function<Float, K>() {
            @Override
            public K apply(Float value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public OptionalFloat first() {
        return fromIndex < toIndex ? OptionalFloat.of(elements[fromIndex]) : OptionalFloat.empty();
    }

    @Override
    public OptionalFloat last() {
        return fromIndex < toIndex ? OptionalFloat.of(elements[toIndex - 1]) : OptionalFloat.empty();
    }

    @Override
    public float reduce(final float identity, final FloatBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Float>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Float>() {
                    @Override
                    public Float call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        float result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = op.applyAsFloat(result, elements[cursor++]);
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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Float>() {
                    @Override
                    public Float call() {
                        float result = identity;
                        float next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = op.applyAsFloat(result, next);
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

        Float result = null;

        try {
            for (CompletableFuture<Float> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsFloat(result, future.get());
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalFloat reduce(final FloatBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Float>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Float>() {
                    @Override
                    public Float call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        if (cursor >= to) {
                            return null;
                        }

                        float result = elements[cursor++];

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.applyAsFloat(result, elements[cursor++]);
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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Float>() {
                    @Override
                    public Float call() {
                        float result = 0;

                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                result = elements[cursor.getAndIncrement()];
                            } else {
                                return null;
                            }
                        }

                        float next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = accumulator.applyAsFloat(result, next);
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

        Float result = null;

        try {
            for (CompletableFuture<Float> future : futureList) {
                final Float tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsFloat(result, tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalFloat.empty() : OptionalFloat.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjFloatConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().collect(supplier, accumulator, combiner);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<R>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<R>() {
                    @Override
                    public R call() {
                        final R container = supplier.get();
                        float next = 0;

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
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return container == NONE ? supplier.get() : container;
    }

    @Override
    public FloatStream tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayFloatStream(elements, fromIndex + 1, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream headd() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayFloatStream(elements, fromIndex, toIndex - 1, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public OptionalFloat min() {
        if (fromIndex == toIndex) {
            return OptionalFloat.empty();
        } else if (sorted) {
            return OptionalFloat.of(elements[fromIndex]);
        } else if (maxThreadNum <= 1) {
            return OptionalFloat.of(N.min(elements, fromIndex, toIndex));
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Float>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Float>() {
                @Override
                public Float call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    return cursor >= to ? null : N.min(elements, cursor, to);
                }
            }));
        }

        Float candidate = null;

        try {
            for (CompletableFuture<Float> future : futureList) {
                final Float tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || N.compare(tmp.floatValue(), candidate.floatValue()) < 0) {
                    candidate = tmp;
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalFloat.empty() : OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat max() {
        if (fromIndex == toIndex) {
            return OptionalFloat.empty();
        } else if (sorted) {
            return OptionalFloat.of(elements[toIndex - 1]);
        } else if (maxThreadNum <= 1) {
            return OptionalFloat.of(N.max(elements, fromIndex, toIndex));
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Float>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Float>() {
                @Override
                public Float call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    return cursor >= to ? null : N.max(elements, cursor, to);
                }
            }));
        }

        Float candidate = null;

        try {
            for (CompletableFuture<Float> future : futureList) {
                final Float tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || N.compare(tmp.floatValue(), candidate.floatValue()) > 0) {
                    candidate = tmp;
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalFloat.empty() : OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (k > toIndex - fromIndex) {
            return OptionalFloat.empty();
        } else if (sorted) {
            return OptionalFloat.of(elements[toIndex - k]);
        }

        return OptionalFloat.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public double sum() {
        if (fromIndex == toIndex) {
            return 0d;
        } else if (maxThreadNum <= 1) {
            return sequential().sum();
        }

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[3];
            }
        };

        final ObjFloatConsumer<double[]> accumulator = new ObjFloatConsumer<double[]>() {
            @Override
            public void accept(double[] ll, float f) {
                Collectors.sumWithCompensation(ll, f);
                ll[2] += f;
            }
        };

        final BiConsumer<double[], double[]> combiner = new BiConsumer<double[], double[]>() {
            @Override
            public void accept(double[] ll, double[] rr) {
                Collectors.sumWithCompensation(ll, rr[0]);
                Collectors.sumWithCompensation(ll, rr[1]);
                ll[2] += rr[2];
            }
        };

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<double[]>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<double[]>() {
                @Override
                public double[] call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    double[] container = supplier.get();

                    while (cursor < to) {
                        accumulator.accept(container, elements[cursor++]);
                    }

                    return container;
                }
            }));
        }

        double[] summation = null;

        try {
            for (CompletableFuture<double[]> future : futureList) {
                final double[] tmp = future.get();

                if (summation == null) {
                    summation = tmp;
                } else {
                    combiner.accept(summation, tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return Collectors.computeFinalSum(summation);
    }

    @Override
    public OptionalDouble average() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        } else if (maxThreadNum <= 1) {
            return sequential().average();
        }

        final Supplier<double[]> supplier = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return new double[4];
            }
        };

        final ObjFloatConsumer<double[]> accumulator = new ObjFloatConsumer<double[]>() {
            @Override
            public void accept(double[] ll, float f) {
                ll[2]++;
                Collectors.sumWithCompensation(ll, f);
                ll[3] += f;
            }
        };

        final BiConsumer<double[], double[]> combiner = new BiConsumer<double[], double[]>() {
            @Override
            public void accept(double[] ll, double[] rr) {
                Collectors.sumWithCompensation(ll, rr[0]);
                Collectors.sumWithCompensation(ll, rr[1]);
                ll[2] += rr[2];
                ll[3] += rr[3];
            }
        };

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<double[]>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<double[]>() {
                @Override
                public double[] call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    double[] container = supplier.get();

                    while (cursor < to) {
                        accumulator.accept(container, elements[cursor++]);
                    }

                    return container;
                }
            }));
        }

        double[] avg = null;

        try {
            for (CompletableFuture<double[]> future : futureList) {
                final double[] tmp = future.get();

                if (avg == null) {
                    avg = tmp;
                } else {
                    combiner.accept(avg, tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return avg[2] > 0 ? OptionalDouble.of(Collectors.computeFinalSum(avg) / avg[2]) : OptionalDouble.empty();
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public FloatStream reversed() {
        return new ParallelIteratorFloatStream(sequential().reversed().iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatSummaryStatistics summarize() {
        if (fromIndex == toIndex) {
            return new FloatSummaryStatistics();
        } else if (maxThreadNum <= 1) {
            return sequential().summarize();
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<FloatSummaryStatistics>> futureList = new ArrayList<>(threadNum);
        final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

        for (int i = 0; i < threadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<FloatSummaryStatistics>() {
                @Override
                public FloatSummaryStatistics call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    final FloatSummaryStatistics result = new FloatSummaryStatistics();

                    for (int i = cursor; i < to; i++) {
                        result.accept(elements[i]);
                    }

                    return result;
                }
            }));
        }

        FloatSummaryStatistics result = null;

        try {
            for (CompletableFuture<FloatSummaryStatistics> future : futureList) {
                final FloatSummaryStatistics tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result.combine(tmp);
                }
            }
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }

        return result;
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.FloatPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        float next = 0;

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

        completee(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.FloatPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        float next = 0;

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

        completee(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.FloatPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        float next = 0;

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

        completee(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> OptionalFloat findFirst(final Try.FloatPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Float>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, Float> pair = new Pair<>();

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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final Pair<Integer, Float> pair = new Pair<>();

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

        completee(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? OptionalFloat.empty() : OptionalFloat.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> OptionalFloat findLast(final Try.FloatPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Float>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, Float> pair = new Pair<>();

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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final Pair<Integer, Float> pair = new Pair<>();

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

        completee(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? OptionalFloat.empty() : OptionalFloat.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> OptionalFloat findAny(final Try.FloatPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Object> resultHolder = Holder.of(NONE);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        float next = 0;

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

            for (int i = 0; i < threadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        float next = 0;

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

        completee(futureList, eHolder, (E) null);

        return resultHolder.value() == NONE ? OptionalFloat.empty() : OptionalFloat.of((Float) resultHolder.value());
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
    public Stream<Float> boxed() {
        Stream<Float> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Float>(iterator(), sorted, sorted ? FLOAT_COMPARATOR : null, maxThreadNum, splitor, closeHandlers);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public FloatStream cached() {
        return this;
    }

    @Override
    public FloatStream append(FloatStream stream) {
        return new ParallelIteratorFloatStream(FloatStream.concat(this, stream), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream prepend(FloatStream stream) {
        return new ParallelIteratorFloatStream(FloatStream.concat(stream, this), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream merge(final FloatStream b, final FloatBiFunction<Nth> nextSelector) {
        return new ParallelIteratorFloatStream(FloatStream.merge(this, b, nextSelector), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatBiFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatStream c, FloatTriFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, c, zipFunction), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream zipWith(FloatStream b, float valueForNoneA, float valueForNoneB, FloatBiFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), false, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatStream c, float valueForNoneA, float valueForNoneB, float valueForNoneC,
            FloatTriFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), false, maxThreadNum,
                splitor, closeHandlers);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public FloatStream sequential() {
        ArrayFloatStream tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayFloatStream(elements, fromIndex, toIndex, sorted, closeHandlers);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public FloatStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum && this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayFloatStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public FloatStream maxThreadNum(int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelArrayFloatStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public FloatStream splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayFloatStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayFloatStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, newCloseHandlers);
    }
}
