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
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.IndexedDouble;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Output;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleBinaryOperator;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleToFloatFunction;
import com.landawn.abacus.util.function.DoubleToIntFunction;
import com.landawn.abacus.util.function.DoubleToLongFunction;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
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
final class ParallelIteratorDoubleStream extends IteratorDoubleStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile IteratorDoubleStream sequential;
    private volatile Stream<Double> boxed;

    ParallelIteratorDoubleStream(final DoubleIterator values, final Collection<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        super(values, closeHandlers, sorted);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    ParallelIteratorDoubleStream(final DoubleStream stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(stream.exIterator(), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    ParallelIteratorDoubleStream(final Stream<Double> stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(doubleIterator(stream.exIterator()), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().filter(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Double> stream = boxed().filter(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream takeWhile(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().takeWhile(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Double> stream = boxed().takeWhile(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().dropWhile(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Double> stream = boxed().dropWhile(new Predicate<Double>() {
            @Override
            public boolean test(Double value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream map(final DoubleUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().map(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Double>() {
            @Override
            public double applyAsDouble(Double value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream mapToInt(final DoubleToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Double>() {
            @Override
            public int applyAsInt(Double value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream mapToLong(final DoubleToLongFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Double>() {
            @Override
            public long applyAsLong(Double value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream mapToFloat(final DoubleToFloatFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Double>() {
            @Override
            public float applyAsFloat(Double value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<U> mapToObj(final DoubleFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        return boxed().map(new Function<Double, U>() {
            @Override
            public U apply(Double value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public DoubleStream flatMap(final DoubleFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMap(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Double, DoubleStream>() {
            @Override
            public DoubleStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream flatMapToInt(final DoubleFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Double, IntStream>() {
            @Override
            public IntStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream flatMapToLong(final DoubleFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Double, LongStream>() {
            @Override
            public LongStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream flatMapToFloat(final DoubleFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Double, FloatStream>() {
            @Override
            public FloatStream apply(Double value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final DoubleFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        return boxed().flatMap(new Function<Double, Stream<T>>() {
            @Override
            public Stream<T> apply(Double value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<DoubleStream> split(final int size) {
        return new ParallelIteratorStream<>(sequential().split(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<DoubleList> split0(final int size) {
        return new ParallelIteratorStream<>(sequential().split0(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<DoubleStream> split(final U identity, final BiFunction<? super Double, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public <U> Stream<DoubleList> split0(final U identity, final BiFunction<? super Double, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split0(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public Stream<DoubleStream> splitBy(final DoublePredicate where) {
        N.requireNonNull(where);

        final List<IndexedDouble> testedElements = new ArrayList<>();

        final NullabLe<IndexedDouble> first = indexed().findFirst(new Predicate<IndexedDouble>() {
            @Override
            public boolean test(IndexedDouble indexed) {
                synchronized (testedElements) {
                    testedElements.add(indexed);
                }

                return !where.test(indexed.value());
            }
        });

        N.sort(testedElements, INDEXED_DOUBLE_COMPARATOR);

        final int n = first.isPresent() ? (int) first.get().index() : testedElements.size();

        final DoubleList list1 = new DoubleList(n);
        final DoubleList list2 = new DoubleList(testedElements.size() - n);

        for (int i = 0; i < n; i++) {
            list1.add(testedElements.get(i).value());
        }

        for (int i = n, size = testedElements.size(); i < size; i++) {
            list2.add(testedElements.get(i).value());
        }

        final DoubleStream[] a = new DoubleStream[2];
        a[0] = new ArrayDoubleStream(list1.array(), null, sorted);
        a[1] = new IteratorDoubleStream(elements, null, sorted);

        if (N.notNullOrEmpty(list2)) {
            if (sorted) {
                a[1] = new IteratorDoubleStream(a[1].prepend(list2.stream0()).exIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(list2.stream0());
            }
        }

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<DoubleStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<DoubleList> sliding0(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding0(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream top(int n) {
        return top(n, DOUBLE_COMPARATOR);
    }

    @Override
    public DoubleStream top(int n, Comparator<? super Double> comparator) {
        return new ParallelIteratorDoubleStream(this.sequential().top(n, comparator).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorDoubleStream(new ExDoubleIterator() {
            double[] a = null;
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
            public double nextDouble() {
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
            public double[] toArray() {
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
        }, closeHandlers, true, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream peek(final DoubleConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().peek(action).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final DoubleStream stream = boxed().peek(new Consumer<Double>() {
            @Override
            public void accept(Double t) {
                action.accept(t);
            }
        }).sequential().mapToDouble(ToDoubleFunction.UNBOX);

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new ParallelIteratorDoubleStream(new ExDoubleIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public double nextDouble() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextDouble();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorDoubleStream(new ExDoubleIterator() {
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
            public double nextDouble() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextDouble();
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
            public double[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public void forEach(final DoubleConsumer action) {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    double next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
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

        complete(futureList, eHolder);
    }

    @Override
    public double[] toArray() {
        return elements.toArray();
    }

    @Override
    public DoubleList toDoubleList() {
        return DoubleList.of(toArray());
    }

    @Override
    public List<Double> toList() {
        final List<Double> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public List<Double> toList(Supplier<? extends List<Double>> supplier) {
        final List<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public Set<Double> toSet() {
        final Set<Double> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public Set<Double> toSet(Supplier<? extends Set<Double>> supplier) {
        final Set<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset() {
        final Multiset<Double> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset(Supplier<? extends Multiset<Double>> supplier) {
        final Multiset<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset() {
        final LongMultiset<Double> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset(Supplier<? extends LongMultiset<Double>> supplier) {
        final LongMultiset<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Double, ? extends K> classifier2 = new Function<Double, K>() {
            @Override
            public K apply(Double value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final DoubleFunction<? extends K> keyMapper, final DoubleFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Double, ? extends K> keyMapper2 = new Function<Double, K>() {
            @Override
            public K apply(Double value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Double, ? extends U> valueMapper2 = new Function<Double, U>() {
            @Override
            public U apply(Double value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapFactory);
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final DoubleFunction<? extends K> keyMapper,
            final DoubleFunction<? extends U> valueMapper, final Supplier<Multimap<K, U, V>> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapFactory);
        }

        final Function<? super Double, ? extends K> keyMapper2 = new Function<Double, K>() {
            @Override
            public K apply(Double value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Double, ? extends U> valueMapper2 = new Function<Double, U>() {
            @Override
            public U apply(Double value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapFactory);
    }

    @Override
    public double reduce(final double identity, final DoubleBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Double>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Double>() {
                @Override
                public Double call() {
                    double result = identity;
                    double next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
                                } else {
                                    break;
                                }
                            }

                            result = op.applyAsDouble(result, next);
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

        Double result = null;

        try {
            for (CompletableFuture<Double> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsDouble(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalDouble reduce(final DoubleBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Double>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Double>() {
                @Override
                public Double call() {
                    double result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.nextDouble();
                        } else {
                            return null;
                        }
                    }

                    double next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.applyAsDouble(result, next);
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

        Double result = null;

        try {
            for (CompletableFuture<Double> future : futureList) {
                final Double tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsDouble(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalDouble.empty() : OptionalDouble.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjDoubleConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().collect(supplier, accumulator, combiner);
        }

        final List<CompletableFuture<R>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<R>() {
                @Override
                public R call() {
                    final R container = supplier.get();
                    double next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
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
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return container == NONE ? supplier.get() : container;
    };

    @Override
    public double head() {
        if (tail == null) {
            if (elements.hasNext() == false) {
                throw new NoSuchElementException();
            }

            head = elements.nextDouble();
            tail = new ParallelIteratorDoubleStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return head;
    }

    @Override
    public DoubleStream tail() {
        if (tail == null) {
            if (elements.hasNext() == false) {
                throw new IllegalStateException();
            }

            head = elements.nextDouble();
            tail = new ParallelIteratorDoubleStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return tail;
    }

    @Override
    public DoubleStream head2() {
        if (head2 == null) {
            if (elements.hasNext() == false) {
                throw new IllegalStateException();
            }

            final double[] a = elements.toArray();
            head2 = new ParallelArrayDoubleStream(a, 0, a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a[a.length - 1];
        }

        return head2;
    }

    @Override
    public double tail2() {
        if (head2 == null) {
            if (elements.hasNext() == false) {
                throw new NoSuchElementException();
            }

            final double[] a = elements.toArray();
            head2 = new ArrayDoubleStream(a, 0, a.length - 1, closeHandlers, sorted);
            tail2 = a[a.length - 1];
        }

        return tail2;
    }

    @Override
    public OptionalDouble min() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements.nextDouble());
        }

        double candidate = elements.nextDouble();
        double next = 0;

        while (elements.hasNext()) {
            next = elements.nextDouble();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalDouble.of(candidate);
    }

    @Override
    public OptionalDouble max() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        } else if (sorted) {
            double next = 0;

            while (elements.hasNext()) {
                next = elements.nextDouble();
            }

            return OptionalDouble.of(next);
        }

        double candidate = elements.nextDouble();
        double next = 0;

        while (elements.hasNext()) {
            next = elements.nextDouble();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalDouble.of(candidate);
    }

    @Override
    public OptionalDouble kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        final NullabLe<Double> optional = boxed().kthLargest(k, DOUBLE_COMPARATOR);

        return optional.isPresent() ? OptionalDouble.of(optional.get()) : OptionalDouble.empty();
    }

    @Override
    public Double sum() {
        return sequential().sum();
    }

    @Override
    public OptionalDouble average() {
        return sequential().average();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public DoubleSummaryStatistics summarize() {
        return sequential().summarize();
    }

    @Override
    public boolean anyMatch(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final MutableBoolean result = MutableBoolean.of(false);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    double next = 0;

                    try {
                        while (result.isFalse() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
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

        complete(futureList, eHolder);

        return result.value();
    }

    @Override
    public boolean allMatch(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    double next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
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

        complete(futureList, eHolder);

        return result.value();
    }

    @Override
    public boolean noneMatch(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final MutableBoolean result = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    double next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
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

        complete(futureList, eHolder);

        return result.value();
    }

    @Override
    public OptionalDouble findFirst(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Long, Double>> resultHolder = new Output<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Double> pair = new Pair<>();

                    try {
                        while (resultHolder.value() == null && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextDouble();
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

        complete(futureList, eHolder);

        return resultHolder.value() == null ? OptionalDouble.empty() : OptionalDouble.of(resultHolder.value().right);
    }

    @Override
    public OptionalDouble findLast(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Long, Double>> resultHolder = new Output<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Double> pair = new Pair<>();

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextDouble();
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

        complete(futureList, eHolder);

        return resultHolder.value() == null ? OptionalDouble.empty() : OptionalDouble.of(resultHolder.value().right);
    }

    @Override
    public OptionalDouble findAny(final DoublePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Object> resultHolder = Output.of(NONE);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    double next = 0;

                    try {
                        while (resultHolder.value() == NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextDouble();
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

        complete(futureList, eHolder);

        return resultHolder.value() == NONE ? OptionalDouble.empty() : OptionalDouble.of((Double) resultHolder.value());
    }

    @Override
    public Stream<Double> boxed() {
        Stream<Double> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<>(iterator(), closeHandlers, sorted, sorted ? DOUBLE_COMPARATOR : null, maxThreadNum, splitor);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public DoubleStream append(DoubleStream stream) {
        return new ParallelIteratorDoubleStream(DoubleStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream prepend(DoubleStream stream) {
        return new ParallelIteratorDoubleStream(DoubleStream.concat(stream, this), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream merge(final DoubleStream b, final DoubleBiFunction<Nth> nextSelector) {
        return new ParallelIteratorDoubleStream(DoubleStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleBiFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, DoubleTriFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, c, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, double valueForNoneA, double valueForNoneB, DoubleBiFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, maxThreadNum,
                splitor);
    }

    @Override
    public DoubleStream zipWith(DoubleStream b, DoubleStream c, double valueForNoneA, double valueForNoneB, double valueForNoneC,
            DoubleTriFunction<Double> zipFunction) {
        return new ParallelIteratorDoubleStream(DoubleStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false,
                maxThreadNum, splitor);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public DoubleStream sequential() {
        IteratorDoubleStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorDoubleStream(elements, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public DoubleStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum && this.splitor == splitor) {
            return this;
        }

        return new ParallelIteratorDoubleStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public DoubleStream maxThreadNum(int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorDoubleStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public DoubleStream splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelIteratorDoubleStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorDoubleStream(elements, newCloseHandlers, sorted, maxThreadNum, splitor);
    }
}
