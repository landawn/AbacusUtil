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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Callable;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IndexedByte;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalByte;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ByteBiFunction;
import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteToIntFunction;
import com.landawn.abacus.util.function.ByteTriFunction;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class ParallelIteratorByteStream extends IteratorByteStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile IteratorByteStream sequential;
    private volatile Stream<Byte> boxed;

    ParallelIteratorByteStream(final ByteIterator values, final Collection<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        super(values, closeHandlers, sorted);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    ParallelIteratorByteStream(final ByteStream stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(stream.skippableIterator(), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    ParallelIteratorByteStream(final Stream<Byte> stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(byteIterator(stream.skippableIterator()), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    @Override
    public ByteStream filter(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().filter(predicate).skippableIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Byte> stream = boxed().filter(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream takeWhile(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().takeWhile(predicate).skippableIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Byte> stream = boxed().takeWhile(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream dropWhile(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().dropWhile(predicate).skippableIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Byte> stream = boxed().dropWhile(new Predicate<Byte>() {
            @Override
            public boolean test(Byte value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream map(final ByteUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().map(mapper).skippableIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final ByteStream stream = boxed().mapToByte(new ToByteFunction<Byte>() {
            @Override
            public byte applyAsByte(Byte value) {
                return mapper.applyAsByte(value);
            }
        });

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream mapToInt(final ByteToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).skippableIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Byte>() {
            @Override
            public int applyAsInt(Byte value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<U> mapToObj(final ByteFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        return boxed().map(new Function<Byte, U>() {
            @Override
            public U apply(Byte value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public ByteStream flatMap(final ByteFunction<? extends ByteStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().flatMap(mapper).skippableIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final ByteStream stream = boxed().flatMapToByte(new Function<Byte, ByteStream>() {
            @Override
            public ByteStream apply(Byte value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream flatMapToInt(final ByteFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).skippableIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Byte, IntStream>() {
            @Override
            public IntStream apply(Byte value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final ByteFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        return boxed().flatMap(new Function<Byte, Stream<T>>() {
            @Override
            public Stream<T> apply(Byte value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<ByteStream> split(final int size) {
        return new ParallelIteratorStream<>(sequential().split(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<ByteList> splitToList(final int size) {
        return new ParallelIteratorStream<>(sequential().splitToList(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<ByteStream> split(final U identity, final BiFunction<? super Byte, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public <U> Stream<ByteList> splitToList(final U identity, final BiFunction<? super Byte, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().splitToList(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public Stream<ByteStream> splitBy(final BytePredicate where) {
        N.requireNonNull(where);

        final List<IndexedByte> testedElements = new ArrayList<>();

        final NullabLe<IndexedByte> first = indexed().findFirst(new Predicate<IndexedByte>() {
            @Override
            public boolean test(IndexedByte indexed) {
                synchronized (testedElements) {
                    testedElements.add(indexed);
                }

                return !where.test(indexed.value());
            }
        });

        N.sort(testedElements, INDEXED_BYTE_COMPARATOR);

        final int n = first.isPresent() ? (int) first.get().index() : testedElements.size();

        final ByteList list1 = new ByteList(n);
        final ByteList list2 = new ByteList(testedElements.size() - n);

        for (int i = 0; i < n; i++) {
            list1.add(testedElements.get(i).value());
        }

        for (int i = n, size = testedElements.size(); i < size; i++) {
            list2.add(testedElements.get(i).value());
        }

        final ByteStream[] a = new ByteStream[2];
        a[0] = new ArrayByteStream(list1.array(), null, sorted);
        a[1] = new IteratorByteStream(elements, null, sorted);

        if (N.notNullOrEmpty(list2)) {
            if (sorted) {
                a[1] = new IteratorByteStream(a[1].prepend(list2.stream()).skippableIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(list2.stream());
            }
        }

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<ByteStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<ByteList> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().slidingToList(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public ByteStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorByteStream(new SkippableByteIterator() {
            byte[] a = null;
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
            public byte nextByte() {
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
            public byte[] toArray() {
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
    public ByteStream peek(final ByteConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().peek(action).skippableIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final ByteStream stream = boxed().peek(new Consumer<Byte>() {
            @Override
            public void accept(Byte t) {
                action.accept(t);
            }
        }).sequential().mapToByte(ToByteFunction.UNBOX);

        return new ParallelIteratorByteStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new ParallelIteratorByteStream(new SkippableByteIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public byte nextByte() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextByte();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public ByteStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorByteStream(new SkippableByteIterator() {
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
            public byte nextByte() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextByte();
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
            public byte[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public void forEach(final ByteConsumer action) {
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
                    byte next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
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
    public byte[] toArray() {
        return elements.toArray();
    }

    @Override
    public ByteList toByteList() {
        return ByteList.of(toArray());
    }

    @Override
    public List<Byte> toList() {
        final List<Byte> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public <R extends List<Byte>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public Set<Byte> toSet() {
        final Set<Byte> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public <R extends Set<Byte>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset() {
        final Multiset<Byte> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset(Supplier<? extends Multiset<Byte>> supplier) {
        final Multiset<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset() {
        final LongMultiset<Byte> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset(Supplier<? extends LongMultiset<Byte>> supplier) {
        final LongMultiset<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final ByteFunction<? extends K> keyExtractor, final ByteFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Byte, ? extends K> keyExtractor2 = new Function<Byte, K>() {
            @Override
            public K apply(Byte value) {
                return keyExtractor.apply(value);
            }
        };

        final Function<? super Byte, ? extends U> valueMapper2 = new Function<Byte, U>() {
            @Override
            public U apply(Byte value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyExtractor2, valueMapper2, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final ByteFunction<? extends K> classifier, final Collector<Byte, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Byte, ? extends K> classifier2 = new Function<Byte, K>() {
            @Override
            public K apply(Byte value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public byte reduce(final byte identity, final ByteBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Byte>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                @Override
                public Byte call() {
                    byte result = identity;
                    byte next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
                                } else {
                                    break;
                                }
                            }

                            result = op.applyAsByte(result, next);
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

        Byte result = null;

        try {
            for (CompletableFuture<Byte> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsByte(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalByte reduce(final ByteBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Byte>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Byte>() {
                @Override
                public Byte call() {
                    byte result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.nextByte();
                        } else {
                            return null;
                        }
                    }

                    byte next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.applyAsByte(result, next);
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

        Byte result = null;

        try {
            for (CompletableFuture<Byte> future : futureList) {
                final Byte tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsByte(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalByte.empty() : OptionalByte.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjByteConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
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
                    byte next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
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
    public OptionalByte head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalByte.of(elements.nextByte()) : OptionalByte.empty();
            tail = new ParallelIteratorByteStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return head;
    }

    @Override
    public ByteStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalByte.of(elements.nextByte()) : OptionalByte.empty();
            tail = new ParallelIteratorByteStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return tail;
    }

    @Override
    public ByteStream head2() {
        if (head2 == null) {
            final byte[] a = elements.toArray();
            head2 = new ParallelArrayByteStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a.length == 0 ? OptionalByte.empty() : OptionalByte.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalByte tail2() {
        if (tail2 == null) {
            final byte[] a = elements.toArray();
            head2 = new ParallelArrayByteStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a.length == 0 ? OptionalByte.empty() : OptionalByte.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalByte min() {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        } else if (sorted) {
            return OptionalByte.of(elements.nextByte());
        }

        byte candidate = elements.nextByte();
        byte next = 0;

        while (elements.hasNext()) {
            next = elements.nextByte();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalByte.of(candidate);
    }

    @Override
    public OptionalByte max() {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        } else if (sorted) {
            byte next = 0;

            while (elements.hasNext()) {
                next = elements.nextByte();
            }

            return OptionalByte.of(next);
        }

        byte candidate = elements.nextByte();
        byte next = 0;

        while (elements.hasNext()) {
            next = elements.nextByte();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalByte.of(candidate);
    }

    @Override
    public OptionalByte kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        final NullabLe<Byte> optional = boxed().kthLargest(k, BYTE_COMPARATOR);

        return optional.isPresent() ? OptionalByte.of(optional.get()) : OptionalByte.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextByte();
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
    public ByteSummaryStatistics summarize() {
        final ByteSummaryStatistics result = new ByteSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextByte());
        }

        return result;
    }

    @Override
    public boolean anyMatch(final BytePredicate predicate) {
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
                    byte next = 0;

                    try {
                        while (result.isFalse() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
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
    public boolean allMatch(final BytePredicate predicate) {
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
                    byte next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
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
    public boolean noneMatch(final BytePredicate predicate) {
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
                    byte next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
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
    public OptionalByte findFirst(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Byte>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Byte> pair = new Pair<>();

                    try {
                        while (resultHolder.value() == null && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextByte();
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

        return resultHolder.value() == null ? OptionalByte.empty() : OptionalByte.of(resultHolder.value().right);
    }

    @Override
    public OptionalByte findLast(final BytePredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Byte>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Byte> pair = new Pair<>();

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextByte();
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

        return resultHolder.value() == null ? OptionalByte.empty() : OptionalByte.of(resultHolder.value().right);
    }

    @Override
    public OptionalByte findAny(final BytePredicate predicate) {
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
                    byte next = 0;

                    try {
                        while (resultHolder.value() == NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextByte();
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

        return resultHolder.value() == NONE ? OptionalByte.empty() : OptionalByte.of((Byte) resultHolder.value());
    }

    @Override
    public IntStream asIntStream() {
        return new ParallelIteratorIntStream(new SkippableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return elements.nextByte();
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
    public Stream<Byte> boxed() {
        Stream<Byte> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<>(iterator(), closeHandlers, sorted, sorted ? BYTE_COMPARATOR : null, maxThreadNum, splitor);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public ByteStream append(ByteStream stream) {
        return new ParallelIteratorByteStream(ByteStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream prepend(ByteStream stream) {
        return new ParallelIteratorByteStream(ByteStream.concat(stream, this), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream merge(final ByteStream b, final ByteBiFunction<Nth> nextSelector) {
        return new ParallelIteratorByteStream(ByteStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteBiFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteStream c, ByteTriFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, c, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream zipWith(ByteStream b, byte valueForNoneA, byte valueForNoneB, ByteBiFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream zipWith(ByteStream b, ByteStream c, byte valueForNoneA, byte valueForNoneB, byte valueForNoneC, ByteTriFunction<Byte> zipFunction) {
        return new ParallelIteratorByteStream(ByteStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false,
                maxThreadNum, splitor);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public ByteStream sequential() {
        IteratorByteStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorByteStream(elements, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public ByteStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum && this.splitor == splitor) {
            return this;
        }

        return new ParallelIteratorByteStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public ByteStream maxThreadNum(int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorByteStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public ByteStream splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelIteratorByteStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public ByteStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorByteStream(elements, newCloseHandlers, sorted, maxThreadNum, splitor);
    }
}
