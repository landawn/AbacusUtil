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

import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IndexedChar;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
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
final class ParallelIteratorCharStream extends IteratorCharStream {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile IteratorCharStream sequential;
    private volatile Stream<Character> boxed;

    ParallelIteratorCharStream(final CharIterator values, final Collection<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        super(values, closeHandlers, sorted);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    ParallelIteratorCharStream(final CharStream stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(stream.exIterator(), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
    }

    ParallelIteratorCharStream(final Stream<Character> stream, final Set<Runnable> closeHandlers, final boolean sorted, final int maxThreadNum,
            final Splitor splitor) {
        this(charIterator(stream.exIterator()), mergeCloseHandlers(stream, closeHandlers), sorted, maxThreadNum, splitor);
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
            return new ParallelIteratorStream<>(sequential().mapToObj(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
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
        return new ParallelIteratorStream<>(sequential().split(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<CharList> splitToList(final int size) {
        return new ParallelIteratorStream<>(sequential().splitToList(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<CharStream> split(final U identity, final BiFunction<? super Character, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public <U> Stream<CharList> splitToList(final U identity, final BiFunction<? super Character, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().splitToList(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public Stream<CharStream> splitBy(final CharPredicate where) {
        N.requireNonNull(where);

        final List<IndexedChar> testedElements = new ArrayList<>();

        final NullabLe<IndexedChar> first = indexed().findFirst(new Predicate<IndexedChar>() {
            @Override
            public boolean test(IndexedChar indexed) {
                synchronized (testedElements) {
                    testedElements.add(indexed);
                }

                return !where.test(indexed.value());
            }
        });

        N.sort(testedElements, INDEXED_CHAR_COMPARATOR);

        final int n = first.isPresent() ? (int) first.get().index() : testedElements.size();

        final CharList list1 = new CharList(n);
        final CharList list2 = new CharList(testedElements.size() - n);

        for (int i = 0; i < n; i++) {
            list1.add(testedElements.get(i).value());
        }

        for (int i = n, size = testedElements.size(); i < size; i++) {
            list2.add(testedElements.get(i).value());
        }

        final CharStream[] a = new CharStream[2];
        a[0] = new ArrayCharStream(list1.array(), null, sorted);
        a[1] = new IteratorCharStream(elements, null, sorted);

        if (N.notNullOrEmpty(list2)) {
            if (sorted) {
                a[1] = new IteratorCharStream(a[1].prepend(list2.stream()).exIterator(), null, sorted);
            } else {
                a[1] = a[1].prepend(list2.stream());
            }
        }

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<CharStream> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<CharList> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().slidingToList(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return this;
        }

        return new ParallelIteratorCharStream(new ExCharIterator() {
            char[] a = null;
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
            public char nextChar() {
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
            public char[] toArray() {
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
    public CharStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new ParallelIteratorCharStream(new ExCharIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public char nextChar() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextChar();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public CharStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorCharStream(new ExCharIterator() {
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
            public char nextChar() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextChar();
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
            public char[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public void forEach(final CharConsumer action) {
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
                    char next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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
    public char[] toArray() {
        return elements.toArray();
    }

    @Override
    public CharList toCharList() {
        return CharList.of(toArray());
    }

    @Override
    public List<Character> toList() {
        final List<Character> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public <R extends List<Character>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public Set<Character> toSet() {
        final Set<Character> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public <R extends Set<Character>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset() {
        final Multiset<Character> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset(Supplier<? extends Multiset<Character>> supplier) {
        final Multiset<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset() {
        final LongMultiset<Character> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset(Supplier<? extends LongMultiset<Character>> supplier) {
        final LongMultiset<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
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
    public char reduce(final char identity, final CharBinaryOperator op) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, op);
        }

        final List<CompletableFuture<Character>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Character>() {
                @Override
                public Character call() {
                    char result = identity;
                    char next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<Character>() {
                @Override
                public Character call() {
                    char result = 0;

                    synchronized (elements) {
                        if (elements.hasNext()) {
                            result = elements.nextChar();
                        } else {
                            return null;
                        }
                    }

                    char next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<R>() {
                @Override
                public R call() {
                    final R container = supplier.get();
                    char next = 0;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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
    public OptionalChar head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalChar.of(elements.nextChar()) : OptionalChar.empty();
            tail = new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return head;
    }

    @Override
    public CharStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalChar.of(elements.nextChar()) : OptionalChar.empty();
            tail = new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
        }

        return tail;
    }

    @Override
    public CharStream head2() {
        if (head2 == null) {
            final char[] a = elements.toArray();
            head2 = new ParallelArrayCharStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a.length == 0 ? OptionalChar.empty() : OptionalChar.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalChar tail2() {
        if (tail2 == null) {
            final char[] a = elements.toArray();
            head2 = new ParallelArrayCharStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, maxThreadNum, splitor);
            tail2 = a.length == 0 ? OptionalChar.empty() : OptionalChar.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalChar min() {
        if (elements.hasNext() == false) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements.nextChar());
        }

        char candidate = elements.nextChar();
        char next = 0;

        while (elements.hasNext()) {
            next = elements.nextChar();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar max() {
        if (elements.hasNext() == false) {
            return OptionalChar.empty();
        } else if (sorted) {
            char next = 0;

            while (elements.hasNext()) {
                next = elements.nextChar();
            }

            return OptionalChar.of(next);
        }

        char candidate = elements.nextChar();
        char next = 0;

        while (elements.hasNext()) {
            next = elements.nextChar();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalChar.empty();
        }

        final NullabLe<Character> optional = boxed().kthLargest(k, CHAR_COMPARATOR);

        return optional.isPresent() ? OptionalChar.of(optional.get()) : OptionalChar.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextChar();
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
    public CharSummaryStatistics summarize() {
        final CharSummaryStatistics result = new CharSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextChar());
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    char next = 0;

                    try {
                        while (result.isFalse() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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
    public boolean allMatch(final CharPredicate predicate) {
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
                    char next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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
    public boolean noneMatch(final CharPredicate predicate) {
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
                    char next = 0;

                    try {
                        while (result.isTrue() && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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
    public OptionalChar findFirst(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Character>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Character> pair = new Pair<>();

                    try {
                        while (resultHolder.value() == null && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextChar();
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

        return resultHolder.value() == null ? OptionalChar.empty() : OptionalChar.of(resultHolder.value().right);
    }

    @Override
    public OptionalChar findLast(final CharPredicate predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Long, Character>> resultHolder = new Holder<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, Character> pair = new Pair<>();

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    pair.left = index.getAndIncrement();
                                    pair.right = elements.nextChar();
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

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    char next = 0;

                    try {
                        while (resultHolder.value() == NONE && eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.nextChar();
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

        return resultHolder.value() == NONE ? OptionalChar.empty() : OptionalChar.of((Character) resultHolder.value());
    }

    @Override
    public IntStream asIntStream() {
        return new ParallelIteratorIntStream(new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return elements.nextChar();
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
    public Stream<Character> boxed() {
        Stream<Character> tmp = boxed;

        if (tmp == null) {
            tmp = new ParallelIteratorStream<>(iterator(), closeHandlers, sorted, sorted ? CHAR_COMPARATOR : null, maxThreadNum, splitor);
            boxed = tmp;
        }

        return tmp;
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
        IteratorCharStream tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorCharStream(elements, closeHandlers, sorted);
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

        return new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
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

        return new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
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

        return new ParallelIteratorCharStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorCharStream(elements, newCloseHandlers, sorted, maxThreadNum, splitor);
    }
}
