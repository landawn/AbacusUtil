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
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.IndexedFloat;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.Output;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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
final class ParallelIteratorFloatStream extends IteratorFloatStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile IteratorFloatStream sequential;
    private volatile Stream<Float> boxed;

    ParallelIteratorFloatStream(final FloatIterator values, final Collection<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        super(values, closeHandlers, sorted);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    ParallelIteratorFloatStream(final FloatStream stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(stream.exIterator(), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    ParallelIteratorFloatStream(final Stream<Float> stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(floatIterator(stream.exIterator()), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    @Override
    public FloatStream filter(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().filter(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Float> stream = boxed().filter(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream takeWhile(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().takeWhile(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Float> stream = boxed().takeWhile(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream dropWhile(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().dropWhile(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Float> stream = boxed().dropWhile(new Predicate<Float>() {
            @Override
            public boolean test(Float value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream map(final FloatUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().map(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Float>() {
            @Override
            public float applyAsFloat(Float value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream mapToInt(final FloatToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Float>() {
            @Override
            public int applyAsInt(Float value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream mapToLong(final FloatToLongFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Float>() {
            @Override
            public long applyAsLong(Float value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream mapToDouble(final FloatToDoubleFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Float>() {
            @Override
            public double applyAsDouble(Float value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<U> mapToObj(final FloatFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
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
            return new ParallelIteratorFloatStream(sequential().flatMap(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Float, FloatStream>() {
            @Override
            public FloatStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream flatMapToInt(final FloatFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Float, IntStream>() {
            @Override
            public IntStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream flatMapToLong(final FloatFunction<? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Float, LongStream>() {
            @Override
            public LongStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream flatMapToDouble(final FloatFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Float, DoubleStream>() {
            @Override
            public DoubleStream apply(Float value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final FloatFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
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
        return new ParallelIteratorStream<>(sequential().split(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<FloatList> split0(final int size) {
        return new ParallelIteratorStream<>(sequential().split0(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<FloatStream> split(final U identity, final BiFunction<? super Float, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public <U> Stream<FloatList> split0(final U identity, final BiFunction<? super Float, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split0(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public Stream<FloatStream> splitBy(final FloatPredicate where) {
        N.requireNonNull(where);

        final List<IndexedFloat> testedElements = new ArrayList<>();

        final NullabLe<IndexedFloat> first = indexed().findFirst(new Predicate<IndexedFloat>() {
            @Override
            public boolean test(IndexedFloat indexed) {
                synchronized (testedElements) {
                    testedElements.add(indexed);
                }

                return !where.test(indexed.value());
            }
        });

        N.sort(testedElements, INDEXED_FLOAT_COMPARATOR);

        final int n = first.isPresent() ? (int) first.get().index() : testedElements.size();

        final FloatList list1 = new FloatList(n);
        final FloatList list2 = new FloatList(testedElements.size() - n);

        for (int i = 0; i < n; i++) {
            list1.add(testedElements.get(i).value());
        }

        for (int i = n, size = testedElements.size(); i < size; i++) {
            list2.add(testedElements.get(i).value());
        }

        final FloatStream[] a = new FloatStream[2];
        a[0] = new ArrayFloatStream(list1.array(), null, sorted);
        a[1] = new IteratorFloatStream(elements, null, sorted);

        if (N.notNullOrEmpty(list2)) {
            if (sorted) {
                a[1] = new IteratorFloatStream(a[1].prepend(list2.stream0()).exIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(list2.stream0());
            }
        }

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<FloatStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<FloatList> sliding0(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding0(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public FloatStream top(int n) {
        return top(n, FLOAT_COMPARATOR);
    }

    @Override
    public FloatStream top(int n, Comparator<? super Float> comparator) {
        return new ParallelIteratorFloatStream(this.sequential().top(n, comparator).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public FloatStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorFloatStream(new ExFloatIterator() {
            float[] a = null;
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
            public float nextFloat() {
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
            public float[] toArray() {
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
    public FloatStream peek(final FloatConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().peek(action).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final FloatStream stream = boxed().peek(new Consumer<Float>() {
            @Override
            public void accept(Float t) {
                action.accept(t);
            }
        }).sequential().mapToFloat(ToFloatFunction.UNBOX);

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new ParallelIteratorFloatStream(new ExFloatIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public float nextFloat() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextFloat();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public FloatStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorFloatStream(new ExFloatIterator() {
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
            public float nextFloat() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextFloat();
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
            public float[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public void forEach(final FloatConsumer action) {
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
                    float next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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
    public float[] toArray() {
        return elements.toArray();
    }

    @Override
    public FloatList toFloatList() {
        return FloatList.of(toArray());
    }

    @Override
    public List<Float> toList() {
        final List<Float> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public <R extends List<Float>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public Set<Float> toSet() {
        final Set<Float> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public <R extends Set<Float>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset() {
        final Multiset<Float> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset(Supplier<? extends Multiset<Float>> supplier) {
        final Multiset<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset() {
        final LongMultiset<Float> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset(Supplier<? extends LongMultiset<Float>> supplier) {
        final LongMultiset<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextFloat());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final FloatFunction<? extends K> keyMapper, final FloatFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Float, ? extends K> keyMapper2 = new Function<Float, K>() {
            @Override
            public K apply(Float value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Float, ? extends U> valueMapper2 = new Function<Float, U>() {
            @Override
            public U apply(Float value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapFactory);
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
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final FloatFunction<? extends K> keyMapper,
            final FloatFunction<? extends U> valueMapper, final Supplier<Multimap<K, U, V>> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapFactory);
        }

        final Function<? super Float, ? extends K> keyMapper2 = new Function<Float, K>() {
            @Override
            public K apply(Float value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Float, ? extends U> valueMapper2 = new Function<Float, U>() {
            @Override
            public U apply(Float value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapFactory);
    }

    @Override
    public float reduce(final float identity, final FloatBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Float>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Float>() {
                @Override
                public Float call() {
                    float result = identity;
                    float next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalFloat reduce(final FloatBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Float>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Float>() {
                @Override
                public Float call() {
                    float result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.nextFloat();
                        } else {
                            return null;
                        }
                    }

                    float next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalFloat.empty() : OptionalFloat.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjFloatConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
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
                    float next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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
    }

    @Override
    public OptionalFloat head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalFloat.of(elements.nextFloat()) : OptionalFloat.empty();
            tail = new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return head;
    }

    @Override
    public FloatStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalFloat.of(elements.nextFloat()) : OptionalFloat.empty();
            tail = new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return tail;
    }

    @Override
    public FloatStream head2() {
        if (head2 == null) {
            final float[] a = elements.toArray();
            head2 = new ParallelArrayFloatStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a.length == 0 ? OptionalFloat.empty() : OptionalFloat.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalFloat tail2() {
        if (tail2 == null) {
            final float[] a = elements.toArray();
            head2 = new ParallelArrayFloatStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a.length == 0 ? OptionalFloat.empty() : OptionalFloat.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalFloat min() {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        } else if (sorted) {
            return OptionalFloat.of(elements.nextFloat());
        }

        float candidate = elements.nextFloat();
        float next = 0;

        while (elements.hasNext()) {
            next = elements.nextFloat();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat max() {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        } else if (sorted) {
            float next = 0;

            while (elements.hasNext()) {
                next = elements.nextFloat();
            }

            return OptionalFloat.of(next);
        }

        float candidate = elements.nextFloat();
        float next = 0;

        while (elements.hasNext()) {
            next = elements.nextFloat();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        final NullabLe<Float> optional = boxed().kthLargest(k, FLOAT_COMPARATOR);

        return optional.isPresent() ? OptionalFloat.of(optional.get()) : OptionalFloat.empty();
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
    public FloatSummaryStatistics summarize() {
        final FloatSummaryStatistics result = new FloatSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextFloat());
        }

        return result;
    }

    @Override
    public boolean anyMatch(final FloatPredicate predicate) {
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
                    float next = 0;

                    try {
                        while (result.isFalse() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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
    public boolean allMatch(final FloatPredicate predicate) {
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
                    float next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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
    public boolean noneMatch(final FloatPredicate predicate) {
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
                    float next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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
    public OptionalFloat findFirst(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Long, Float>> resultHolder = new Output<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Float> pair = new Pair<>();

                    try {
                        while (resultHolder.value() == null && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextFloat();
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

        return resultHolder.value() == null ? OptionalFloat.empty() : OptionalFloat.of(resultHolder.value().right);
    }

    @Override
    public OptionalFloat findLast(final FloatPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Long, Float>> resultHolder = new Output<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Float> pair = new Pair<>();

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextFloat();
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

        return resultHolder.value() == null ? OptionalFloat.empty() : OptionalFloat.of(resultHolder.value().right);
    }

    @Override
    public OptionalFloat findAny(final FloatPredicate predicate) {
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
                    float next = 0;

                    try {
                        while (resultHolder.value() == NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextFloat();
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

        return resultHolder.value() == NONE ? OptionalFloat.empty() : OptionalFloat.of((Float) resultHolder.value());
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new ParallelIteratorDoubleStream(new ExDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return elements.nextFloat();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public Stream<Float> boxed() {
        Stream<Float> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<>(iterator(), closeHandlers, sorted, sorted ? FLOAT_COMPARATOR : null, maxThreadNum, splitor);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public FloatStream append(FloatStream stream) {
        return new ParallelIteratorFloatStream(FloatStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream prepend(FloatStream stream) {
        return new ParallelIteratorFloatStream(FloatStream.concat(stream, this), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream merge(final FloatStream b, final FloatBiFunction<Nth> nextSelector) {
        return new ParallelIteratorFloatStream(FloatStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatBiFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatStream c, FloatTriFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, c, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream zipWith(FloatStream b, float valueForNoneA, float valueForNoneB, FloatBiFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, maxThreadNum,
                splitor);
    }

    @Override
    public FloatStream zipWith(FloatStream b, FloatStream c, float valueForNoneA, float valueForNoneB, float valueForNoneC,
            FloatTriFunction<Float> zipFunction) {
        return new ParallelIteratorFloatStream(FloatStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false,
                maxThreadNum, splitor);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public FloatStream sequential() {
        IteratorFloatStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorFloatStream(elements, closeHandlers, sorted);
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

        return new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
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

        return new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
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

        return new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public FloatStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorFloatStream(elements, newCloseHandlers, sorted, maxThreadNum, splitor);
    }
}
