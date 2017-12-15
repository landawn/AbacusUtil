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
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IndexedLong;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Nullable;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiPredicate;
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
final class ParallelIteratorLongStream extends IteratorLongStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile IteratorLongStream sequential;
    private volatile Stream<Long> boxed;

    ParallelIteratorLongStream(final LongIterator values, final boolean sorted, final int maxThreadNum, final Splitor splitor,
            final Collection<Runnable> closeHandlers) {
        super(values, sorted, closeHandlers);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    ParallelIteratorLongStream(final LongStream stream, final boolean sorted, final int maxThreadNum, final Splitor splitor,
            final Set<Runnable> closeHandlers) {
        this(stream.iteratorEx(), sorted, maxThreadNum, splitor, mergeCloseHandlers(stream, closeHandlers));
    }

    ParallelIteratorLongStream(final Stream<Long> stream, final boolean sorted, final int maxThreadNum, final Splitor splitor,
            final Set<Runnable> closeHandlers) {
        this(longIterator(stream.iteratorEx()), sorted, maxThreadNum, splitor, mergeCloseHandlers(stream, closeHandlers));
    }

    @Override
    public LongStream filter(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().filter(predicate).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
        }

        final Stream<Long> stream = boxed().filter(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value);
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
                return predicate.test(value);
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
                return predicate.test(value);
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
                return mapper.applyAsLong(value);
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
                return mapper.applyAsInt(value);
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
                return mapper.applyAsFloat(value);
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
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().mapToObj(mapper).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        return boxed().map(new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return mapper.apply(value);
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
        return new ParallelIteratorStream<>(sequential().split(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<LongList> splitToList(final int size) {
        return new ParallelIteratorStream<>(sequential().splitToList(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<LongStream> split(final U seed, final BiPredicate<? super Long, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<>(sequential().split(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<LongList> splitToList(final U seed, final BiPredicate<? super Long, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<>(sequential().splitToList(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public Stream<LongStream> splitBy(final LongPredicate where) {
        N.requireNonNull(where);

        final List<IndexedLong> testedElements = new ArrayList<>();

        final Nullable<IndexedLong> first = indexed().findFirst(new Predicate<IndexedLong>() {
            @Override
            public boolean test(IndexedLong indexed) {
                synchronized (testedElements) {
                    testedElements.add(indexed);
                }

                return !where.test(indexed.value());
            }
        });

        N.sort(testedElements, INDEXED_LONG_COMPARATOR);

        final int n = first.isPresent() ? (int) first.get().index() : testedElements.size();

        final LongList list1 = new LongList(n);
        final LongList list2 = new LongList(testedElements.size() - n);

        for (int i = 0; i < n; i++) {
            list1.add(testedElements.get(i).value());
        }

        for (int i = n, size = testedElements.size(); i < size; i++) {
            list2.add(testedElements.get(i).value());
        }

        final LongStream[] a = new LongStream[2];
        a[0] = new ArrayLongStream(list1.array(), sorted, null);
        a[1] = new IteratorLongStream(elements, sorted, null);

        if (N.notNullOrEmpty(list2)) {
            if (sorted) {
                a[1] = new IteratorLongStream(a[1].prepend(list2.stream()).iteratorEx(), sorted, null);
            } else {
                a[1] = a[1].prepend(list2.stream());
            }
        }

        return new ParallelArrayStream<>(a, 0, a.length, false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<LongStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding(windowSize, increment).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<LongList> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().slidingToList(windowSize, increment).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream top(int n) {
        return top(n, LONG_COMPARATOR);
    }

    @Override
    public LongStream top(int n, Comparator<? super Long> comparator) {
        return new ParallelIteratorLongStream(this.sequential().top(n, comparator).iteratorEx(), sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorLongStream(new LongIteratorEx() {
            long[] a = null;
            int toIndex = 0;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < toIndex;
            }

            @Override
            public long nextLong() {
                if (a == null) {
                    sort();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    sort();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    sort();
                }

                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public long[] toArray() {
                if (a == null) {
                    sort();
                }

                if (cursor == 0) {
                    return a;
                } else {
                    return N.copyOfRange(a, cursor, toIndex);
                }
            }

            private void sort() {
                a = elements.toArray();
                toIndex = a.length;

                N.parallelSort(a);
            }
        }, true, maxThreadNum, splitor, closeHandlers);
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
    public LongStream limit(final long maxSize) {
        N.checkArgument(maxSize >= 0, "'maxSizse' can't be negative: %s", maxSize);

        return new ParallelIteratorLongStream(new LongIteratorEx() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public long nextLong() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextLong();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream skip(final long n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (n == 0) {
            return this;
        }

        return new ParallelIteratorLongStream(new LongIteratorEx() {
            private boolean skipped = false;

            @Override
            public boolean hasNext() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextLong();
            }

            @Override
            public long count() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.count();
            }

            @Override
            public void skip(long n2) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n2);
            }

            @Override
            public long[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <E extends Exception> void forEach(final Try.LongConsumer<E> action) throws E {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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

        complete2(futureList, eHolder, (E) null);
    }

    @Override
    public long[] toArray() {
        return elements.toArray();
    }

    @Override
    public LongList toLongList() {
        return LongList.of(toArray());
    }

    @Override
    public List<Long> toList() {
        final List<Long> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public <R extends List<Long>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public Set<Long> toSet() {
        final Set<Long> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public <R extends Set<Long>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset() {
        final Multiset<Long> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset(Supplier<? extends Multiset<Long>> supplier) {
        final Multiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset() {
        final LongMultiset<Long> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset(Supplier<? extends LongMultiset<Long>> supplier) {
        final LongMultiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
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
    public long reduce(final long identity, final LongBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Long>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Long>() {
                @Override
                public Long call() {
                    long result = identity;
                    long next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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
        } catch (InterruptedException | ExecutionException e) {
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Long>() {
                @Override
                public Long call() {
                    long result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.nextLong();
                        } else {
                            return null;
                        }
                    }

                    long next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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
        } catch (InterruptedException | ExecutionException e) {
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<R>() {
                @Override
                public R call() {
                    final R container = supplier.get();
                    long next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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
    public OptionalLong head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalLong.of(elements.nextLong()) : OptionalLong.empty();
            tail = new ParallelIteratorLongStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
        }

        return head;
    }

    @Override
    public LongStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalLong.of(elements.nextLong()) : OptionalLong.empty();
            tail = new ParallelIteratorLongStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
        }

        return tail;
    }

    @Override
    public LongStream head2() {
        if (head2 == null) {
            final long[] a = elements.toArray();
            head2 = new ParallelArrayLongStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, maxThreadNum, splitor, closeHandlers);
            tail2 = a.length == 0 ? OptionalLong.empty() : OptionalLong.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalLong tail2() {
        if (tail2 == null) {
            final long[] a = elements.toArray();
            head2 = new ParallelArrayLongStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, maxThreadNum, splitor, closeHandlers);
            tail2 = a.length == 0 ? OptionalLong.empty() : OptionalLong.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalLong min() {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements.nextLong());
        }

        long candidate = elements.nextLong();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.nextLong();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong max() {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        } else if (sorted) {
            long next = 0;

            while (elements.hasNext()) {
                next = elements.nextLong();
            }

            return OptionalLong.of(next);
        }

        long candidate = elements.nextLong();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.nextLong();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        final Nullable<Long> optional = boxed().kthLargest(k, LONG_COMPARATOR);

        return optional.isPresent() ? OptionalLong.of(optional.get()) : OptionalLong.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextLong();
        }

        return result;
    }

    @Override
    public OptionalDouble average() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        return sequential().average();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public LongSummaryStatistics summarize() {
        final LongSummaryStatistics result = new LongSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextLong());
        }

        return result;
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.LongPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

                    try {
                        while (result.isFalse() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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

        complete2(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.LongPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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

        complete2(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.LongPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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

        complete2(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> OptionalLong findFirst(final Try.LongPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Long>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Long> pair = new Pair<>();

                    try {
                        while (resultHolder.value() == null && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextLong();
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

        complete2(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? OptionalLong.empty() : OptionalLong.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> OptionalLong findLast(final Try.LongPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Long>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Long> pair = new Pair<>();

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextLong();
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
                            }
                        }
                    } catch (Throwable e) {
                        setError(eHolder, e);
                    }
                }
            }));
        }

        complete2(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? OptionalLong.empty() : OptionalLong.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> OptionalLong findAny(final Try.LongPredicate<E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Object> resultHolder = Holder.of(NONE);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

                    try {
                        while (resultHolder.value() == NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextLong();
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

        complete2(futureList, eHolder, (E) null);

        return resultHolder.value() == NONE ? OptionalLong.empty() : OptionalLong.of((Long) resultHolder.value());
    }

    @Override
    public FloatStream asFloatStream() {
        return new ParallelIteratorFloatStream(new FloatIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return elements.nextLong();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new ParallelIteratorDoubleStream(new DoubleIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return elements.nextLong();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<Long> boxed() {
        Stream<Long> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<>(iterator(), sorted, sorted ? LONG_COMPARATOR : null, maxThreadNum, splitor, closeHandlers);
            boxed = tmp;
        }

        return tmp;
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
        IteratorLongStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorLongStream(elements, sorted, closeHandlers);
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

        return new ParallelIteratorLongStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
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

        return new ParallelIteratorLongStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
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

        return new ParallelIteratorLongStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorLongStream(elements, sorted, maxThreadNum, splitor, newCloseHandlers);
    }
}
