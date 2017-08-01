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

import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IndexedChar;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.CharBiFunction;
import com.landawn.abacus.util.function.CharBinaryOperator;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharToIntFunction;
import com.landawn.abacus.util.function.CharTriFunction;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class ParallelArrayCharStream extends ArrayCharStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile ArrayCharStream sequential;
    private volatile Stream<Character> boxed;

    ParallelArrayCharStream(final char[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers, final boolean sorted,
            int maxThreadNum, Splitor splitor) {
        super(values, closeHandlers, sorted);

        this.maxThreadNum = fromIndex >= toIndex ? 1 : N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION, toIndex - fromIndex);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    @Override
    public CharStream filter(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().filter(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Character> stream = boxed().filter(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().takeWhile(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Character> stream = boxed().takeWhile(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().dropWhile(predicate).exIterator(), closeHandlers, sorted, maxThreadNum, splitor);
        }

        final Stream<Character> stream = boxed().dropWhile(new Predicate<Character>() {
            @Override
            public boolean test(Character value) {
                return predicate.test(value);
            }
        });

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream map(final CharUnaryOperator mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().map(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final CharStream stream = boxed().mapToChar(new ToCharFunction<Character>() {
            @Override
            public char applyAsChar(Character value) {
                return mapper.applyAsChar(value);
            }
        });

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream mapToInt(final CharToIntFunction mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().mapToInt(new ToIntFunction<Character>() {
            @Override
            public int applyAsInt(Character value) {
                return mapper.applyAsInt(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<U> mapToObj(final CharFunction<? extends U> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<U>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        return boxed().map(new Function<Character, U>() {
            @Override
            public U apply(Character value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public CharStream flatMap(final CharFunction<? extends CharStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().flatMap(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final CharStream stream = boxed().flatMapToChar(new Function<Character, CharStream>() {
            @Override
            public CharStream apply(Character value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream flatMapToInt(final CharFunction<? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final IntStream stream = boxed().flatMapToInt(new Function<Character, IntStream>() {
            @Override
            public IntStream apply(Character value) {
                return mapper.apply(value);
            }
        });

        return new ParallelIteratorIntStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final CharFunction<? extends Stream<T>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        return boxed().flatMap(new Function<Character, Stream<T>>() {
            @Override
            public Stream<T> apply(Character value) {
                return mapper.apply(value);
            }
        });
    }

    @Override
    public Stream<CharStream> split(final int size) {
        return new ParallelIteratorStream<CharStream>(sequential().split(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<CharList> splitToList(final int size) {
        return new ParallelIteratorStream<CharList>(sequential().splitToList(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<CharStream> split(final U identity, final BiFunction<? super Character, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<CharStream>(sequential().split(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null,
                maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<CharList> splitToList(final U identity, final BiFunction<? super Character, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<CharList>(sequential().splitToList(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null,
                maxThreadNum, splitor);
    }

    @Override
    public Stream<CharStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final CharStream[] a = new CharStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? CharStream.empty() : new ArrayCharStream(elements, fromIndex, middleIndex, null, sorted);
        a[1] = middleIndex == toIndex ? CharStream.empty() : new ArrayCharStream(elements, middleIndex, toIndex, null, sorted);

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<CharStream> splitBy(final CharPredicate where) {
        N.requireNonNull(where);

        final NullabLe<IndexedChar> first = indexed().findFirst(new Predicate<IndexedChar>() {
            @Override
            public boolean test(IndexedChar indexed) {
                return !where.test(indexed.value());
            }
        });

        return splitAt(first.isPresent() ? (int) first.get().index() : toIndex - fromIndex);
    }

    @Override
    public Stream<CharStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<CharStream>(sequential().sliding(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public Stream<CharList> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<CharList>(sequential().slidingToList(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return this;
        }

        final char[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a);
        return new ParallelArrayCharStream(a, 0, a.length, closeHandlers, true, maxThreadNum, splitor);
    }

    @Override
    public CharStream peek(final CharConsumer action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().peek(action).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final CharStream stream = boxed().peek(new Consumer<Character>() {
            @Override
            public void accept(Character t) {
                action.accept(t);
            }
        }).sequential().mapToChar(ToCharFunction.UNBOX);

        return new ParallelIteratorCharStream(stream, closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayCharStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public CharStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayCharStream(elements, toIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
        } else {
            return new ParallelArrayCharStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted, maxThreadNum, splitor);
        }
    }

    @Override
    public void forEach(final CharConsumer action) {
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
                        char next = 0;

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
    public char[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public CharList toCharList() {
        return CharList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Character> toList() {
        final List<Character> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends List<Character>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Character> toSet() {
        final Set<Character> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends Set<Character>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset() {
        final Multiset<Character> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset(Supplier<? extends Multiset<Character>> supplier) {
        final Multiset<Character> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset() {
        final LongMultiset<Character> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset(Supplier<? extends LongMultiset<Character>> supplier) {
        final LongMultiset<Character> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(final CharFunction<? extends K> keyExtractor, final CharFunction<? extends U> valueMapper,
            final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
        }

        final Function<? super Character, ? extends K> keyExtractor2 = new Function<Character, K>() {
            @Override
            public K apply(Character value) {
                return keyExtractor.apply(value);
            }
        };

        final Function<? super Character, ? extends U> valueMapper2 = new Function<Character, U>() {
            @Override
            public U apply(Character value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMap(keyExtractor2, valueMapper2, mergeFunction, mapFactory);
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final CharFunction<? extends K> classifier, final Collector<Character, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        final Function<? super Character, ? extends K> classifier2 = new Function<Character, K>() {
            @Override
            public K apply(Character value) {
                return classifier.apply(value);
            }
        };

        return boxed().toMap(classifier2, downstream, mapFactory);
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final CharFunction<? extends K> keyExtractor,
            final CharFunction<? extends U> valueMapper, final Supplier<Multimap<K, U, V>> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyExtractor, valueMapper, mapFactory);
        }

        final Function<? super Character, ? extends K> keyExtractor2 = new Function<Character, K>() {
            @Override
            public K apply(Character value) {
                return keyExtractor.apply(value);
            }
        };

        final Function<? super Character, ? extends U> valueMapper2 = new Function<Character, U>() {
            @Override
            public U apply(Character value) {
                return valueMapper.apply(value);
            }
        };

        return boxed().toMultimap(keyExtractor2, valueMapper2, mapFactory);
    }

    @Override
    public OptionalChar first() {
        return fromIndex < toIndex ? OptionalChar.of(elements[fromIndex]) : OptionalChar.empty();
    }

    @Override
    public OptionalChar last() {
        return fromIndex < toIndex ? OptionalChar.of(elements[toIndex - 1]) : OptionalChar.empty();
    }

    @Override
    public char reduce(final char identity, final CharBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Character>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Character>() {
                    @Override
                    public Character call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        char result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = op.applyAsChar(result, elements[cursor++]);
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
                futureList.add(asyncExecutor.execute(new Callable<Character>() {
                    @Override
                    public Character call() {
                        char result = identity;
                        char next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = op.applyAsChar(result, next);
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

        Character result = null;

        try {
            for (CompletableFuture<Character> future : futureList) {
                if (result == null) {
                    result = future.get();
                } else {
                    result = op.applyAsChar(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? identity : result;
    }

    @Override
    public OptionalChar reduce(final CharBinaryOperator accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<Character>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Character>() {
                    @Override
                    public Character call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        if (cursor >= to) {
                            return null;
                        }

                        char result = elements[cursor++];

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.applyAsChar(result, elements[cursor++]);
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
                futureList.add(asyncExecutor.execute(new Callable<Character>() {
                    @Override
                    public Character call() {
                        char result = 0;

                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                result = elements[cursor.getAndIncrement()];
                            } else {
                                return null;
                            }
                        }

                        char next = 0;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                result = accumulator.applyAsChar(result, next);
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

        Character result = null;

        try {
            for (CompletableFuture<Character> future : futureList) {
                final Character tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (result == null) {
                    result = tmp;
                } else {
                    result = accumulator.applyAsChar(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == null ? OptionalChar.empty() : OptionalChar.of(result);
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final ObjCharConsumer<R> accumulator, final BiConsumer<R, R> combiner) {
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
                        char next = 0;

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
    public CharStream tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayCharStream(elements, fromIndex + 1, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public CharStream head2() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayCharStream(elements, fromIndex, toIndex - 1, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public OptionalChar min() {
        if (fromIndex == toIndex) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements[fromIndex]);
        } else if (maxThreadNum <= 1) {
            return OptionalChar.of(N.min(elements, fromIndex, toIndex));
        }

        final List<CompletableFuture<Character>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Character>() {
                @Override
                public Character call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    return cursor >= to ? null : N.min(elements, cursor, to);
                }
            }));
        }

        Character candidate = null;

        try {
            for (CompletableFuture<Character> future : futureList) {
                final Character tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || tmp.charValue() < candidate.charValue()) {
                    candidate = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalChar.empty() : OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar max() {
        if (fromIndex == toIndex) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements[toIndex - 1]);
        } else if (maxThreadNum <= 1) {
            return OptionalChar.of(N.max(elements, fromIndex, toIndex));
        }

        final List<CompletableFuture<Character>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<Character>() {
                @Override
                public Character call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    return cursor >= to ? null : N.max(elements, cursor, to);
                }
            }));
        }

        Character candidate = null;

        try {
            for (CompletableFuture<Character> future : futureList) {
                final Character tmp = future.get();

                if (tmp == null) {
                    continue;
                } else if (candidate == null || tmp.charValue() > candidate.charValue()) {
                    candidate = tmp;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return candidate == null ? OptionalChar.empty() : OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (k > toIndex - fromIndex) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements[toIndex - k]);
        }

        return OptionalChar.of(N.kthLargest(elements, fromIndex, toIndex, k));
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
    public CharStream reversed() {
        return new ParallelIteratorCharStream(sequential().reversed().exIterator(), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharSummaryStatistics summarize() {
        if (fromIndex == toIndex) {
            return new CharSummaryStatistics();
        } else if (maxThreadNum <= 1) {
            return sequential().summarize();
        }

        final List<CompletableFuture<CharSummaryStatistics>> futureList = new ArrayList<>(maxThreadNum);
        final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

        for (int i = 0; i < maxThreadNum; i++) {
            final int sliceIndex = i;

            futureList.add(asyncExecutor.execute(new Callable<CharSummaryStatistics>() {
                @Override
                public CharSummaryStatistics call() {
                    int cursor = fromIndex + sliceIndex * sliceSize;
                    final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    final CharSummaryStatistics result = new CharSummaryStatistics();

                    for (int i = cursor; i < to; i++) {
                        result.accept(elements[i]);
                    }

                    return result;
                }
            }));
        }

        CharSummaryStatistics result = null;

        try {
            for (CompletableFuture<CharSummaryStatistics> future : futureList) {
                final CharSummaryStatistics tmp = future.get();

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
    public boolean anyMatch(final CharPredicate predicate) {
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
                        char next = 0;

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
    public boolean allMatch(final CharPredicate predicate) {
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
                        char next = 0;

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
    public boolean noneMatch(final CharPredicate predicate) {
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
                        char next = 0;

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
    public OptionalChar findFirst(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Character>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, Character> pair = new Pair<>();

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
                        final Pair<Integer, Character> pair = new Pair<>();

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

        return resultHolder.value() == null ? OptionalChar.empty() : OptionalChar.of(resultHolder.value().right);
    }

    @Override
    public OptionalChar findLast(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, Character>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, Character> pair = new Pair<>();

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
                        final Pair<Integer, Character> pair = new Pair<>();

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

        return resultHolder.value() == null ? OptionalChar.empty() : OptionalChar.of(resultHolder.value().right);
    }

    @Override
    public OptionalChar findAny(final CharPredicate predicate) {
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
                        char next = 0;

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
                        char next = 0;

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

        return resultHolder.value() == NONE ? OptionalChar.empty() : OptionalChar.of((Character) resultHolder.value());
    }

    @Override
    public IntStream asIntStream() {
        return new ParallelIteratorIntStream(new ExIntIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int nextInt() {
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
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public Stream<Character> boxed() {
        Stream<Character> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<Character>(iterator(), closeHandlers, sorted, sorted ? CHAR_COMPARATOR : null, maxThreadNum, splitor);
            boxed = tmp;
        }

        return tmp;
    }

    @Override
    public CharStream cached() {
        return this;
    }

    @Override
    public CharStream append(CharStream stream) {
        return new ParallelIteratorCharStream(CharStream.concat(this, stream), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream prepend(CharStream stream) {
        return new ParallelIteratorCharStream(CharStream.concat(stream, this), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream merge(final CharStream b, final CharBiFunction<Nth> nextSelector) {
        return new ParallelIteratorCharStream(CharStream.merge(this, b, nextSelector), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream zipWith(CharStream b, CharBiFunction<Character> zipFunction) {
        return new ParallelIteratorCharStream(CharStream.zip(this, b, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream zipWith(CharStream b, CharStream c, CharTriFunction<Character> zipFunction) {
        return new ParallelIteratorCharStream(CharStream.zip(this, b, c, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream zipWith(CharStream b, char valueForNoneA, char valueForNoneB, CharBiFunction<Character> zipFunction) {
        return new ParallelIteratorCharStream(CharStream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public CharStream zipWith(CharStream b, CharStream c, char valueForNoneA, char valueForNoneB, char valueForNoneC, CharTriFunction<Character> zipFunction) {
        return new ParallelIteratorCharStream(CharStream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false,
                maxThreadNum, splitor);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public CharStream sequential() {
        ArrayCharStream tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayCharStream(elements, fromIndex, toIndex, closeHandlers, sorted);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public CharStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum && this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayCharStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public CharStream maxThreadNum(int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelArrayCharStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public CharStream splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayCharStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayCharStream(elements, fromIndex, toIndex, newCloseHandlers, sorted, maxThreadNum, splitor);
    }
}
