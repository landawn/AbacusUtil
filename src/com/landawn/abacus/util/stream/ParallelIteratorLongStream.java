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
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.OptionalNullable;
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
final class ParallelIteratorLongStream extends AbstractLongStream {
    private final ImmutableLongIterator elements;
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile IteratorLongStream sequential;
    private volatile Stream<Long> boxed;

    private long head;
    private LongStream tail;

    private LongStream head2;
    private long tail2;

    ParallelIteratorLongStream(final LongIterator values, final Collection<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        super(closeHandlers, sorted);

        this.elements = values instanceof ImmutableLongIterator ? (ImmutableLongIterator) values : new ImmutableLongIterator() {
            @Override
            public boolean hasNext() {
                return values.hasNext();
            }

            @Override
            public long next() {
                return values.next();
            }
        };

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    ParallelIteratorLongStream(final LongStream stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(stream.longIterator(), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    ParallelIteratorLongStream(final Stream<Long> stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(longIterator(stream.iterator()), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    @Override
    public LongStream filter(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().filter(predicate).longIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Long> stream = boxed().filter(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().takeWhile(predicate).longIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Long> stream = boxed().takeWhile(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().dropWhile(predicate).longIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Long> stream = boxed().dropWhile(new Predicate<Long>() {
            @Override
            public boolean test(Long value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream map(final LongUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().map(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final LongStream stream = boxed().mapToLong(new ToLongFunction<Long>() {
            @Override
            public long applyAsLong(Long value) {
                return mapper.applyAsLong(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream mapToInt(final LongToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Long>() {
            @Override
            public int applyAsInt(Long value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream mapToFloat(final LongToFloatFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final FloatStream stream = boxed().mapToFloat(new ToFloatFunction<Long>() {
            @Override
            public float applyAsFloat(Long value) {
                return mapper.applyAsFloat(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream mapToDouble(final LongToDoubleFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final DoubleStream stream = boxed().mapToDouble(new ToDoubleFunction<Long>() {
            @Override
            public double applyAsDouble(Long value) {
                return mapper.applyAsDouble(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
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
            return new ParallelIteratorLongStream(sequential().flatMap(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final LongStream stream = boxed().flatMapToLong(new Function<Long, LongStream>() {
            @Override
            public LongStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream flatMapToInt(final LongFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Long, IntStream>() {
            @Override
            public IntStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream flatMapToFloat(final LongFunction<? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final FloatStream stream = boxed().flatMapToFloat(new Function<Long, FloatStream>() {
            @Override
            public FloatStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorFloatStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream flatMapToDouble(final LongFunction<? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final DoubleStream stream = boxed().flatMapToDouble(new Function<Long, DoubleStream>() {
            @Override
            public DoubleStream apply(Long value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorDoubleStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final LongFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        return boxed().flatMap(new Function<Long, Stream<T>>() {
            @Override
            public Stream<T> apply(Long value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<LongStream> split(final int size) {
        return new ParallelIteratorStream<LongStream>(sequential().split(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<LongList> split0(final int size) {
        return new ParallelIteratorStream<LongList>(sequential().split0(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<LongStream> split(final U identity, final BiFunction<? super Long, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<LongStream>(sequential().split(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null,
                maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<LongList> split0(final U identity, final BiFunction<? super Long, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<LongList>(sequential().split0(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null,
                maxThreadNum, splitor);
    }

    @Override
    public Stream<LongStream> splitBy(final LongPredicate where) {
        N.requireNonNull(where);

        final List<IndexedLong> testedElements = new ArrayList<>();

        final OptionalNullable<IndexedLong> first = indexed().findFirst(new Predicate<IndexedLong>() {
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
        a[0] = new ArrayLongStream(list1.array(), null, sorted);
        a[1] = new IteratorLongStream(elements, null, sorted);

        if (N.notNullOrEmpty(list2)) {
            if (sorted) {
                a[1] = new IteratorLongStream(a[1].prepend(list2.stream()).longIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(list2.stream());
            }
        }

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<LongStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<LongStream>(sequential().sliding(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public Stream<LongList> sliding0(final int windowSize, final int increment) {
        return new ParallelIteratorStream<LongList>(sequential().sliding0(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public LongStream top(int n) {
        return top(n, LONG_COMPARATOR);
    }

    @Override
    public LongStream top(int n, Comparator<? super Long> comparator) {
        return new ParallelIteratorLongStream(this.sequential().top(n, comparator).longIterator(), closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
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
            public long next() {
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
        }, closeHandlers, true, maxThreadNum, splitor);
    }

    @Override
    public LongStream peek(final LongConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().peek(action).longIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final LongStream stream = boxed().peek(new Consumer<Long>() {
            @Override
            public void accept(Long t) {
                action.accept(t);
            }
        }).sequential().mapToLong(ToLongFunction.UNBOX);

        return new ParallelIteratorLongStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public long next() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.next();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public LongStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorLongStream(new ImmutableLongIterator() {
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
            public long next() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.next();
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
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public void forEach(final LongConsumer action) {
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
                                    next = elements.next();
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
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Long> toList(Supplier<? extends List<Long>> supplier) {
        final List<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Long> toSet() {
        final Set<Long> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Long> toSet(Supplier<? extends Set<Long>> supplier) {
        final Set<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset() {
        final Multiset<Long> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset(Supplier<? extends Multiset<Long>> supplier) {
        final Multiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset() {
        final LongMultiset<Long> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset(Supplier<? extends LongMultiset<Long>> supplier) {
        final LongMultiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
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
    public <K, U, M extends Map<K, U>> M toMap(final LongFunction<? extends K> keyMapper, final LongFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapSupplier) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyMapper, valueMapper, mergeFunction, mapSupplier);
        }

        final Function<? super Long, ? extends K> keyMapper2 = new Function<Long, K>() {
            @Override
            public K apply(Long value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Long, ? extends U> valueMapper2 = new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyMapper2, valueMapper2, mergeFunction, mapSupplier);
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final LongFunction<? extends K> keyMapper, final LongFunction<? extends U> valueMapper,
            final Supplier<Multimap<K, U, V>> mapSupplier) {
        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyMapper, valueMapper, mapSupplier);
        }

        final Function<? super Long, ? extends K> keyMapper2 = new Function<Long, K>() {
            @Override
            public K apply(Long value) {
                return keyMapper.apply(value);
            }
        };

        final Function<? super Long, ? extends U> valueMapper2 = new Function<Long, U>() {
            @Override
            public U apply(Long value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyMapper2, valueMapper2, mapSupplier);
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
                                    next = elements.next();
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Long>() {
                @Override
                public Long call() {
                    long result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.next();
                        } else {
                            return null;
                        }
                    }

                    long next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
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
                                    next = elements.next();
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
    public long head() {
        if (tail == null) {
            if (elements.hasNext() == false) {
                throw new NoSuchElementException();
            }

            head = elements.next();
            tail = new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return head;
    }

    @Override
    public LongStream tail() {
        if (tail == null) {
            if (elements.hasNext() == false) {
                throw new IllegalStateException();
            }

            head = elements.next();
            tail = new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return tail;
    }

    @Override
    public LongStream head2() {
        if (head2 == null) {
            if (elements.hasNext() == false) {
                throw new IllegalStateException();
            }

            final long[] a = elements.toArray();
            head2 = new ParallelArrayLongStream(a, 0, a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a[a.length - 1];
        }

        return head2;
    }

    @Override
    public long tail2() {
        if (head2 == null) {
            if (elements.hasNext() == false) {
                throw new NoSuchElementException();
            }

            final long[] a = elements.toArray();
            head2 = new ArrayLongStream(a, 0, a.length - 1, closeHandlers, sorted);
            tail2 = a[a.length - 1];
        }

        return tail2;
    }

    @Override
    public OptionalLong min() {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements.next());
        }

        long candidate = elements.next();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.next();

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
                next = elements.next();
            }

            return OptionalLong.of(next);
        }

        long candidate = elements.next();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.next();

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

        final OptionalNullable<Long> optional = boxed().kthLargest(k, LONG_COMPARATOR);

        return optional.isPresent() ? OptionalLong.of(optional.get()) : OptionalLong.empty();
    }

    @Override
    public Long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.next();
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
            result.accept(elements.next());
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

                    try {
                        while (result.isFalse() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
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
    public boolean allMatch(final LongPredicate predicate) {
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
                                    next = elements.next();
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
    public boolean noneMatch(final LongPredicate predicate) {
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
                                    next = elements.next();
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
    public OptionalLong findFirst(final LongPredicate predicate) {
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
                                    pair.right = elements.next();
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

        return resultHolder.value() == null ? OptionalLong.empty() : OptionalLong.of(resultHolder.value().right);
    }

    @Override
    public OptionalLong findLast(final LongPredicate predicate) {
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
                                    pair.right = elements.next();
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long next = 0;

                    try {
                        while (resultHolder.value() == NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
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

        return resultHolder.value() == NONE ? OptionalLong.empty() : OptionalLong.of((Long) resultHolder.value());
    }

    @Override
    public FloatStream asFloatStream() {
        return new ParallelIteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
                return elements.next();
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
    public DoubleStream asDoubleStream() {
        return new ParallelIteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
                return elements.next();
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
    public Stream<Long> boxed() {
        Stream<Long> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Long>(iterator(), closeHandlers, sorted, sorted ? LONG_COMPARATOR : null, maxThreadNum, splitor);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public LongStream append(LongStream stream) {
        return new ParallelIteratorLongStream(LongStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream prepend(LongStream stream) {
        return new ParallelIteratorLongStream(LongStream.concat(stream, this), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream merge(final LongStream b, final LongBiFunction<Nth> nextSelector) {
        return new ParallelIteratorLongStream(LongStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream zipWith(LongStream b, LongBiFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, LongTriFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, c, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream zipWith(LongStream b, long valueForNoneA, long valueForNoneB, LongBiFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream zipWith(LongStream b, LongStream c, long valueForNoneA, long valueForNoneB, long valueForNoneC, LongTriFunction<Long> zipFunction) {
        return new ParallelIteratorLongStream(LongStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false,
                maxThreadNum, splitor);
    }

    @Override
    public ImmutableIterator<Long> iterator() {
        return this.sequential().iterator();
    }

    @Override
    public ImmutableLongIterator longIterator() {
        return elements;
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public LongStream sequential() {
        IteratorLongStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorLongStream(elements, closeHandlers, sorted);
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

        return new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
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

        return new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
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

        return new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorLongStream(elements, newCloseHandlers, sorted, maxThreadNum, splitor);
    }
}
