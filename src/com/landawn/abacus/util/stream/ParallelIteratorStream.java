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

import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CompletableFuture;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.ExList;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.Indexed;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableLong;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.Output;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.stream.ExIterator.QueuedIterator;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class ParallelIteratorStream<T> extends IteratorStream<T> {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile IteratorStream<T> sequential;

    ParallelIteratorStream(final Iterator<? extends T> values, final Collection<Runnable> closeHandlers, final boolean sorted,
            final Comparator<? super T> comparator, final int maxThreadNum, final Splitor splitor) {
        super(values, closeHandlers, sorted, comparator);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    ParallelIteratorStream(final Stream<T> stream, final Set<Runnable> closeHandlers, final boolean sorted, final Comparator<? super T> comparator,
            final int maxThreadNum, final Splitor splitor) {
        this(stream.iterator(), mergeCloseHandlers(stream, closeHandlers), sorted, comparator, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().filter(predicate).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<T>() {
                private T next = null;
                private boolean hasNext = false;

                @Override
                public boolean hasNext() {
                    if (hasNext == false) {
                        while (true) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            if (predicate.test(next)) {
                                hasNext = true;
                                break;
                            }
                        }
                    }

                    return hasNext;
                }

                @Override
                public T next() {
                    if (hasNext == false && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    hasNext = false;
                    return next;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().takeWhile(predicate).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);
        final MutableBoolean hasMore = MutableBoolean.of(true);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<T>() {
                private T next = null;
                private boolean hasNext = false;

                @Override
                public boolean hasNext() {
                    if (hasNext == false && hasMore.isTrue()) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                                hasNext = true;
                            } else {
                                hasMore.setFalse();
                            }
                        }

                        if (hasNext && predicate.test(next) == false) {
                            hasNext = false;
                            hasMore.setFalse();
                        }
                    }

                    return hasNext;
                }

                @Override
                public T next() {
                    if (hasNext == false && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    hasNext = false;
                    return next;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().dropWhile(predicate).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);
        final MutableBoolean dropped = MutableBoolean.of(false);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<T>() {
                private T next = null;
                private boolean hasNext = false;

                @Override
                public boolean hasNext() {
                    if (hasNext == false) {
                        // Only one thread is kept for running after it's dropped.
                        if (dropped.isTrue()) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                    hasNext = true;
                                }
                            }
                        } else {
                            while (dropped.isFalse()) {
                                synchronized (elements) {
                                    if (elements.hasNext()) {
                                        next = elements.next();
                                    } else {
                                        break;
                                    }
                                }

                                if (predicate.test(next) == false) {
                                    hasNext = true;
                                    dropped.setTrue();
                                    break;
                                }
                            }

                            if (hasNext == false && dropped.isTrue()) {
                                synchronized (elements) {
                                    if (elements.hasNext()) {
                                        next = elements.next();
                                        hasNext = true;
                                    }
                                }
                            }
                        }
                    }

                    return hasNext;
                }

                @Override
                public T next() {
                    if (hasNext == false && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    hasNext = false;
                    return next;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <R> Stream<R> map(final Function<? super T, ? extends R> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().map(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<R>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public R next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    R result = mapper.apply((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <R> Stream<R> map2(final BiFunction<? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().map2(mapper, ignoreNotPaired).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<R>() {
                private Object pre = NONE;
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (pre == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                pre = elements.next();

                                if (elements.hasNext()) {
                                    next = elements.next();
                                }
                            }
                        }
                    }

                    return ignoreNotPaired ? next != NONE : pre != NONE;
                }

                @Override
                public R next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    final R result = mapper.apply((T) pre, next == NONE ? null : (T) next);
                    pre = NONE;
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <R> Stream<R> map3(final TriFunction<? super T, ? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().map3(mapper, ignoreNotPaired).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<R>() {
                private Object prepre = NONE;
                private Object pre = NONE;
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (prepre == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                prepre = elements.next();

                                if (elements.hasNext()) {
                                    pre = elements.next();

                                    if (elements.hasNext()) {
                                        next = elements.next();
                                    }
                                }
                            }
                        }
                    }

                    return ignoreNotPaired ? next != NONE : prepre != NONE;
                }

                @Override
                public R next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    final R result = mapper.apply((T) prepre, pre == NONE ? null : (T) pre, next == NONE ? null : (T) next);
                    prepre = NONE;
                    pre = NONE;
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().mapToChar(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Character>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public Character next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Character result = mapper.applyAsChar((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().mapToByte(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Byte>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public Byte next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Byte result = mapper.applyAsByte((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().mapToShort(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Short>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public Short next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Short result = mapper.applyAsShort((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Integer>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public Integer next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Integer result = mapper.applyAsInt((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Long>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public Long next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Long result = mapper.applyAsLong((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Float>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public Float next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Float result = mapper.applyAsFloat((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Double>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public Double next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    Double result = mapper.applyAsDouble((T) next);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    <R> Stream<R> flatMap0(final Function<? super T, ? extends Iterator<? extends R>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(((IteratorStream<T>) sequential()).flatMap0(mapper).iterator(), closeHandlers, false, null, maxThreadNum,
                    splitor);
        }

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<R>() {
                private T next = null;
                private Iterator<? extends R> cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public R next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.next();
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    CharStream flatMapToChar0(final Function<? super T, CharIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(((IteratorStream<T>) sequential()).flatMapToChar0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Character>() {
                private T next = null;
                private CharIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Character next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.nextChar();
                }
            });
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    ByteStream flatMapToByte0(final Function<? super T, ByteIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(((IteratorStream<T>) sequential()).flatMapToByte0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Byte>() {
                private T next = null;
                private ByteIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Byte next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.nextByte();
                }
            });
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    ShortStream flatMapToShort0(final Function<? super T, ShortIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(((IteratorStream<T>) sequential()).flatMapToShort0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Short>() {
                private T next = null;
                private ShortIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Short next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.nextShort();
                }
            });
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    IntStream flatMapToInt0(final Function<? super T, IntIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(((IteratorStream<T>) sequential()).flatMapToInt0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Integer>() {
                private T next = null;
                private IntIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Integer next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.nextInt();
                }
            });
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    LongStream flatMapToLong0(final Function<? super T, LongIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(((IteratorStream<T>) sequential()).flatMapToLong0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Long>() {
                private T next = null;
                private LongIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Long next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.nextLong();
                }
            });
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    FloatStream flatMapToFloat0(final Function<? super T, FloatIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(((IteratorStream<T>) sequential()).flatMapToFloat0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Float>() {
                private T next = null;
                private FloatIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Float next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.nextFloat();
                }
            });
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    DoubleStream flatMapToDouble0(final Function<? super T, DoubleIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(((IteratorStream<T>) sequential()).flatMapToDouble0(mapper).exIterator(), closeHandlers, false,
                    maxThreadNum, splitor);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<Double>() {
                private T next = null;
                private DoubleIterator cur = null;

                @Override
                public boolean hasNext() {
                    while ((cur == null || cur.hasNext() == false) && next != NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            } else {
                                next = (T) NONE;
                                break;
                            }
                        }

                        cur = mapper.apply(next);
                    }

                    return cur != null && cur.hasNext();
                }

                @Override
                public Double next() {
                    if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    return cur.nextDouble();
                }
            });
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        return new ParallelIteratorStream<>(sequential().split(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<ExList<T>> split0(final int size) {
        return new ParallelIteratorStream<>(sequential().split0(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<List<T>> split2(final int size) {
        return new ParallelIteratorStream<>(sequential().split2(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<Set<T>> split3(final int size) {
        return new ParallelIteratorStream<>(sequential().split3(size).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<Stream<T>> split(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public <U> Stream<ExList<T>> split0(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split0(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public <U> Stream<List<T>> split2(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split2(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public <U> Stream<Set<T>> split3(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate) {
        return new ParallelIteratorStream<>(sequential().split3(identity, predicate, identityUpdate).iterator(), closeHandlers, false, null, maxThreadNum,
                splitor);
    }

    @Override
    public Stream<Stream<T>> splitBy(final Predicate<? super T> where) {
        N.requireNonNull(where);

        final List<Indexed<T>> testedElements = new ArrayList<>();

        final NullabLe<Indexed<T>> first = indexed().findFirst(new Predicate<Indexed<T>>() {
            @Override
            public boolean test(Indexed<T> indexed) {
                synchronized (testedElements) {
                    testedElements.add(indexed);
                }

                return !where.test(indexed.value());
            }
        });

        N.sort(testedElements, INDEXED_COMPARATOR);

        final int n = first.isPresent() ? (int) first.get().index() : testedElements.size();
        final Stream<T>[] a = new Stream[2];

        if (n == testedElements.size()) {
            a[0] = new ArrayStream<>((T[]) testedElements.toArray(), null, sorted, cmp);
            a[1] = Stream.empty();
        } else {
            final List<T> list1 = new ArrayList<>(n);
            final List<T> list2 = new ArrayList<>(testedElements.size() - n);

            for (int i = 0; i < n; i++) {
                list1.add(testedElements.get(i).value());
            }

            for (int i = n, size = testedElements.size(); i < size; i++) {
                list2.add(testedElements.get(i).value());
            }

            a[0] = new ArrayStream<>((T[]) list1.toArray(), null, sorted, cmp);
            a[1] = new IteratorStream<>(elements, null, sorted, cmp);

            if (N.notNullOrEmpty(list2)) {
                if (sorted) {
                    a[1] = new IteratorStream<>(a[1].prepend(Stream.of(list2)).iterator(), null, sorted, cmp);
                } else {
                    a[1] = a[1].prepend(Stream.of(list2));
                }
            }
        }

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<Stream<T>> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<ExList<T>> sliding0(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding0(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<List<T>> sliding2(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding2(windowSize, increment).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyExtractor) {
        final Set<Object> set = new HashSet<>();

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                final Object key = hashKey(keyExtractor.apply(value));

                synchronized (set) {
                    return set.add(key);
                }
            }
        });
    }

    @Override
    public Stream<T> top(int n) {
        return top(n, OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> top(final int n, final Comparator<? super T> comparator) {
        return new ParallelIteratorStream<>(sequential().top(n, comparator).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> sorted() {
        return sorted(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> sorted(final Comparator<? super T> comparator) {
        if (sorted && isSameComparator(comparator, cmp)) {
            return this;
        }

        return new ParallelIteratorStream<>(new ExIterator<T>() {
            T[] a = null;
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
            public T next() {
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
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    sort();
                }

                if (b.getClass().equals(a.getClass()) && b.length < toIndex - cursor) {
                    if (cursor == 0) {
                        return (A[]) a;
                    } else {
                        return (A[]) N.copyOfRange(a, cursor, toIndex);
                    }
                } else {
                    if (b.length < toIndex - cursor) {
                        b = N.newArray(b.getClass().getComponentType(), toIndex - cursor);
                    }

                    N.copy(a, cursor, b, 0, toIndex - cursor);

                    return b;
                }
            }

            private void sort() {
                a = (T[]) elements.toArray(N.EMPTY_OBJECT_ARRAY);
                toIndex = a.length;

                if (comparator == null) {
                    N.parallelSort(a);
                } else {
                    N.parallelSort(a, comparator);
                }
            }
        }, closeHandlers, true, comparator, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().peek(action).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<T>() {
                private Object next = NONE;

                @Override
                public boolean hasNext() {
                    if (next == NONE) {
                        synchronized (elements) {
                            if (elements.hasNext()) {
                                next = elements.next();
                            }
                        }
                    }

                    return next != NONE;
                }

                @Override
                public T next() {
                    if (next == NONE && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    final T result = (T) next;
                    action.accept(result);
                    next = NONE;
                    return result;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new ParallelIteratorStream<>(new ExIterator<T>() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public T next() {
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
        }, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new ParallelIteratorStream<>(new ExIterator<T>() {
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
            public T next() {
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
            public <A> A[] toArray(A[] a) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray(a);
            }
        }, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public void forEach(final Consumer<? super T> action) {
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
                    T next = null;

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
    public <U> U forEach(U seed, BiFunction<U, ? super T, U> accumulator, BiPredicate<? super T, ? super U> conditionToBreak) {
        if (logger.isWarnEnabled()) {
            logger.warn("The 'forEach' with break condition is sequentially executed in parallel stream");
        }

        return sequential().forEach(seed, accumulator, conditionToBreak);
    }

    @Override
    public Object[] toArray() {
        return toArray(N.EMPTY_OBJECT_ARRAY);
    }

    @Override
    <A> A[] toArray(A[] a) {
        return elements.toArray(a);
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        return toArray(generator.apply(0));
    }

    @Override
    public ExList<T> toExList() {
        return ExList.of((T[]) toArray());
    }

    @Override
    public List<T> toList() {
        final List<T> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <R extends List<T>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<T> toSet() {
        final Set<T> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <R extends Set<T>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier) {
        final Multiset<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset() {
        final LongMultiset<T> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier) {
        final LongMultiset<T> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        return collect(Collectors.toMap(keyExtractor, valueMapper, mergeFunction, mapFactory));
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        return collect(Collectors.groupingBy(classifier, downstream, mapFactory));
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapFactory) {
        return collect(Collectors.toMultimap(keyExtractor, valueMapper, mapFactory));
    }

    @Override
    public T reduce(final T identity, final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<T>() {
                @Override
                public T call() {
                    T result = identity;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.apply(result, next);
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

        T result = (T) NONE;

        try {
            for (CompletableFuture<T> future : futureList) {
                if (result == NONE) {
                    result = future.get();
                } else {
                    result = accumulator.apply(result, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == NONE ? identity : result;
    }

    @Override
    public NullabLe<T> reduce(final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<T>() {
                @Override
                public T call() {
                    T result = (T) NONE;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = result == NONE ? next : accumulator.apply(result, next);
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

        T result = (T) NONE;

        try {
            for (CompletableFuture<T> future : futureList) {
                final T tmp = future.get();

                if (tmp == NONE) {
                    continue;
                } else if (result == NONE) {
                    result = tmp;
                } else {
                    result = accumulator.apply(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(result);
    }

    @Override
    public <U> U reduce(final U identity, final BiFunction<U, ? super T, U> accumulator, final BinaryOperator<U> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator, combiner);
        }

        final List<CompletableFuture<U>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<U>() {
                @Override
                public U call() {
                    U result = identity;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            result = accumulator.apply(result, next);
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

        U result = (U) NONE;

        try {
            for (CompletableFuture<U> future : futureList) {
                final U tmp = future.get();

                if (result == NONE) {
                    result = tmp;
                } else {
                    result = combiner.apply(result, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result == NONE ? identity : result;
    }

    @Override
    public <R> R collect(final Supplier<R> supplier, final BiConsumer<R, ? super T> accumulator, final BiConsumer<R, R> combiner) {
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
                    T next = null;

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
    public <R, A> R collect(final Collector<? super T, A, R> collector) {
        if (maxThreadNum <= 1) {
            return sequential().collect(collector);
        }

        final Supplier<A> supplier = collector.supplier();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final BinaryOperator<A> combiner = collector.combiner();
        final Function<A, R> finisher = collector.finisher();

        final List<CompletableFuture<A>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Callable<A>() {
                @Override
                public A call() {
                    A container = supplier.get();
                    T next = null;

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

        A container = (A) NONE;

        try {
            for (CompletableFuture<A> future : futureList) {
                if (container == NONE) {
                    container = future.get();
                } else {
                    combiner.apply(container, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return finisher.apply(container == NONE ? supplier.get() : container);
    }

    @Override
    public NullabLe<T> head() {
        if (tail == null) {
            head = elements.hasNext() ? NullabLe.of(elements.next()) : NullabLe.<T> empty();
            tail = new ParallelIteratorStream<>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        return head;
    }

    @Override
    public Stream<T> tail() {
        if (tail == null) {
            head = elements.hasNext() ? NullabLe.of(elements.next()) : NullabLe.<T> empty();
            tail = new ParallelIteratorStream<>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        return tail;
    }

    @Override
    public Stream<T> head2() {
        if (head2 == null) {
            final Object[] a = this.toArray();
            head2 = new ParallelArrayStream<>((T[]) a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, cmp, maxThreadNum, splitor);
            tail2 = a.length == 0 ? NullabLe.<T> empty() : NullabLe.of((T) a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public NullabLe<T> tail2() {
        if (head2 == null) {
            final Object[] a = this.toArray();
            head2 = new ParallelArrayStream<>((T[]) a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, cmp, maxThreadNum, splitor);
            tail2 = a.length == 0 ? NullabLe.<T> empty() : NullabLe.of((T) a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public Stream<T> last(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative");

        if (n == 0) {
            return new ParallelIteratorStream<>(ExIterator.EMPTY, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        final Deque<T> dqueue = new ArrayDeque<>(n);

        while (elements.hasNext()) {
            if (dqueue.size() >= n) {
                dqueue.pollFirst();
            }

            dqueue.offerLast(elements.next());
        }

        return new ParallelIteratorStream<>(dqueue.iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public NullabLe<T> min(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return NullabLe.empty();
        } else if (sorted && isSameComparator(comparator, cmp)) {
            return NullabLe.of(elements.next());
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return collect(Collectors.minBy(comparator));
    }

    @Override
    public NullabLe<T> max(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return NullabLe.empty();
        } else if (sorted && isSameComparator(comparator, cmp)) {
            T next = null;

            while (elements.hasNext()) {
                next = elements.next();
            }

            return NullabLe.of(next);
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return collect(Collectors.maxBy(comparator));
    }

    @Override
    public NullabLe<T> kthLargest(int k, Comparator<? super T> comparator) {
        return sequential().kthLargest(k, comparator);
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public boolean anyMatch(final Predicate<? super T> predicate) {
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
                    T next = null;

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
    public boolean allMatch(final Predicate<? super T> predicate) {
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
                    T next = null;

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
    public boolean noneMatch(final Predicate<? super T> predicate) {
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
                    T next = null;

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
    public NullabLe<T> findFirst(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Long, T>> resultHolder = new Output<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, T> pair = new Pair<>();

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

        return resultHolder.value() == null ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(resultHolder.value().right);
    }

    @Override
    public NullabLe<T> findLast(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Long, T>> resultHolder = new Output<>();
        final MutableLong index = MutableLong.of(0);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    final Pair<Long, T> pair = new Pair<>();

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

        return resultHolder.value() == null ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(resultHolder.value().right);
    }

    @Override
    public NullabLe<T> findAny(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<T> resultHolder = Output.of((T) NONE);

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    T next = null;

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

        return resultHolder.value() == NONE ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(resultHolder.value());
    }

    @Override
    public Stream<T> intersection(final Function<? super T, ?> mapper, final Collection<?> c) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().intersection(mapper, c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        final Multiset<?> multiset = Multiset.from(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                final Object key = mapper.apply(value);

                synchronized (multiset) {
                    return multiset.getAndRemove(key) > 0;
                }
            }
        });
    }

    @Override
    public Stream<T> difference(final Function<? super T, ?> mapper, final Collection<?> c) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().intersection(mapper, c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        final Multiset<?> multiset = Multiset.from(c);

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                final Object key = mapper.apply(value);

                synchronized (multiset) {
                    return multiset.getAndRemove(key) < 1;
                }
            }
        });
    }

    @Override
    public Stream<T> queued(int queueSize) {
        final Iterator<T> iter = iterator();

        if (iter instanceof QueuedIterator && ((QueuedIterator<? extends T>) iter).max() >= queueSize) {
            return this;
        } else {
            return new ParallelIteratorStream<>(Stream.parallelConcat(Arrays.asList(iter), queueSize, asyncExecutor), closeHandlers, sorted, cmp, maxThreadNum,
                    splitor);
        }
    }

    @Override
    public Stream<T> append(Stream<T> stream) {
        return new ParallelIteratorStream<>(Stream.concat(this, stream), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> prepend(Stream<T> stream) {
        return new ParallelIteratorStream<>(Stream.concat(stream, this), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> merge(final Stream<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return new ParallelIteratorStream<>(Stream.merge(this, b, nextSelector), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <T2, R> Stream<R> zipWith(Stream<T2> b, BiFunction<? super T, ? super T2, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, zipFunction), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <T2, T3, R> Stream<R> zipWith(Stream<T2> b, Stream<T3> c, TriFunction<? super T, ? super T2, ? super T3, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, c, zipFunction), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <T2, R> Stream<R> zipWith(Stream<T2> b, T valueForNoneA, T2 valueForNoneB, BiFunction<? super T, ? super T2, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <T2, T3, R> Stream<R> zipWith(Stream<T2> b, Stream<T3> c, T valueForNoneA, T2 valueForNoneB, T3 valueForNoneC,
            TriFunction<? super T, ? super T2, ? super T3, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), closeHandlers, false, null,
                maxThreadNum, splitor);
    }

    @Override
    public long persist(final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) {
        if (maxThreadNum <= 1) {
            return sequential().persist(stmt, batchSize, batchInterval, stmtSetter);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final AtomicLong result = new AtomicLong();

        for (int i = 0; i < maxThreadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    long cnt = 0;
                    T next = null;

                    try {
                        while (eHolder.value() == null) {
                            synchronized (elements) {
                                if (elements.hasNext()) {
                                    next = elements.next();
                                } else {
                                    break;
                                }
                            }

                            stmtSetter.accept(stmt, next);
                            stmt.addBatch();

                            if ((++cnt % batchSize) == 0) {
                                stmt.executeBatch();
                                stmt.clearBatch();

                                if (batchInterval > 0) {
                                    N.sleep(batchInterval);
                                }
                            }
                        }

                        if ((cnt % batchSize) > 0) {
                            stmt.executeBatch();
                            stmt.clearBatch();
                        }

                        result.addAndGet(cnt);
                    } catch (Throwable e) {
                        setError(eHolder, e);
                    }
                }
            }));
        }

        complete(futureList, eHolder);

        return result.longValue();
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public Stream<T> sequential() {
        IteratorStream<T> tmp = sequential;

        if (tmp == null) {
            tmp = new IteratorStream<>(elements, closeHandlers, sorted, cmp);
            sequential = tmp;
        }

        return tmp;
    }

    @Override
    public Stream<T> parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum && this.splitor == splitor) {
            return this;
        }

        return new ParallelIteratorStream<>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public int maxThreadNum() {
        return maxThreadNum;
    }

    @Override
    public Stream<T> maxThreadNum(int maxThreadNum) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        if (this.maxThreadNum == maxThreadNum) {
            return this;
        }

        return new ParallelIteratorStream<>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Splitor splitor() {
        return splitor;
    }

    @Override
    public Stream<T> splitor(Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelIteratorStream<>(elements, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelIteratorStream<>(elements, newCloseHandlers, sorted, cmp, maxThreadNum, splitor);
    }
}
