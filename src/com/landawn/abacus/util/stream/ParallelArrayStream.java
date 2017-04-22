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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
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
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.Output;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortIterator;
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

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @param <T>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class ParallelArrayStream<T> extends ArrayStream<T> {
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile ArrayStream<T> sequential;

    ParallelArrayStream(final T[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers, final boolean sorted,
            Comparator<? super T> comparator, int maxThreadNum, Splitor splitor) {
        super(values, fromIndex, toIndex, closeHandlers, sorted, comparator);

        this.maxThreadNum = fromIndex >= toIndex ? 1 : N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION, toIndex - fromIndex);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().filter(predicate).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<T>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private T next = null;
                    private boolean hasNext = false;

                    @Override
                    public boolean hasNext() {
                        if (hasNext == false) {
                            while (cursor < to) {
                                next = elements[cursor++];

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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<T>() {
                    private T next = null;
                    private boolean hasNext = false;

                    @Override
                    public boolean hasNext() {
                        if (hasNext == false) {
                            while (true) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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
        final MutableInt cursor = MutableInt.of(fromIndex);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ExIterator<T>() {
                private T next = null;
                private boolean hasNext = false;

                @Override
                public boolean hasNext() {
                    if (hasNext == false && hasMore.isTrue()) {
                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                next = elements[cursor.getAndIncrement()];
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
        final MutableInt cursor = MutableInt.of(fromIndex);

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
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
                                    hasNext = true;
                                }
                            }
                        } else {
                            while (dropped.isFalse()) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<R>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public R next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.apply(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<R>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <R> Stream<R> map2(final BiFunction<? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().map2(mapper, ignoreNotPaired).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final int atLeast = ignoreNotPaired ? 2 : 1;
        final int count = (toIndex - fromIndex) / 2 + (ignoreNotPaired || (toIndex - fromIndex) % 2 == 0 ? 0 : 1);
        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = count / maxThreadNum + (count % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<R>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize * 2;
                    private final int to = toIndex - cursor > sliceSize * 2 ? cursor + sliceSize * 2 : toIndex;

                    @Override
                    public boolean hasNext() {
                        return to - cursor >= atLeast;
                    }

                    @Override
                    public R next() {
                        if (to - cursor < atLeast) {
                            throw new NoSuchElementException();
                        }

                        return mapper.apply(elements[cursor++], cursor == toIndex ? null : elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<R>() {
                    private Object pre = NONE;
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (pre == NONE) {
                            synchronized (elements) {
                                if (toIndex - cursor.intValue() >= atLeast) {
                                    pre = elements[cursor.getAndIncrement()];
                                    next = cursor.intValue() == toIndex ? null : elements[cursor.getAndIncrement()];
                                }
                            }
                        }

                        return pre != NONE;
                    }

                    @Override
                    public R next() {
                        if (pre == NONE && hasNext() == false) {
                            throw new NoSuchElementException();
                        }

                        final R result = mapper.apply((T) pre, (T) next);
                        pre = NONE;
                        next = NONE;
                        return result;
                    }
                });
            }
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <R> Stream<R> map3(final TriFunction<? super T, ? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().map3(mapper, ignoreNotPaired).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final int atLeast = ignoreNotPaired ? 3 : 1;
        final int count = (toIndex - fromIndex) / 3 + (ignoreNotPaired || (toIndex - fromIndex) % 3 == 0 ? 0 : 1);
        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = count / maxThreadNum + (count % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<R>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize * 3;
                    private final int to = toIndex - cursor > sliceSize * 3 ? cursor + sliceSize * 3 : toIndex;

                    @Override
                    public boolean hasNext() {
                        return to - cursor >= atLeast;
                    }

                    @Override
                    public R next() {
                        if (to - cursor < atLeast) {
                            throw new NoSuchElementException();
                        }

                        return mapper.apply(elements[cursor++], cursor == toIndex ? null : elements[cursor++], cursor == toIndex ? null : elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<R>() {
                    private Object prepre = NONE;
                    private Object pre = NONE;
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (prepre == NONE) {
                            synchronized (elements) {
                                if (toIndex - cursor.intValue() >= atLeast) {
                                    prepre = elements[cursor.getAndIncrement()];
                                    pre = cursor.intValue() == toIndex ? null : elements[cursor.getAndIncrement()];
                                    next = cursor.intValue() == toIndex ? null : elements[cursor.getAndIncrement()];
                                }
                            }
                        }

                        return prepre != NONE;
                    }

                    @Override
                    public R next() {
                        if (prepre == NONE && hasNext() == false) {
                            throw new NoSuchElementException();
                        }

                        final R result = mapper.apply((T) prepre, (T) pre, (T) next);
                        prepre = NONE;
                        pre = NONE;
                        next = NONE;
                        return result;
                    }
                });
            }
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().mapToChar(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ExIterator<Character>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Character>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public Character next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.applyAsChar(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Character>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().mapToByte(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ExIterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Byte>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public Byte next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.applyAsByte(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Byte>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().mapToShort(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ExIterator<Short>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Short>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public Short next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.applyAsShort(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Short>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ExIterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Integer>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public Integer next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.applyAsInt(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Integer>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ExIterator<Long>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Long>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public Long next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.applyAsLong(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Long>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ExIterator<Float>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Float>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public Float next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.applyAsFloat(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Float>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).exIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ExIterator<Double>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Double>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public Double next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        return mapper.applyAsDouble(elements[cursor++]);
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Double>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    <R> Stream<R> flatMap0(final Function<? super T, ? extends Iterator<? extends R>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(((ArrayStream<T>) sequential()).flatMap0(mapper).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<R>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private Iterator<? extends R> cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<R>() {
                    private T next = null;
                    private Iterator<? extends R> cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    CharStream flatMapToChar0(final Function<? super T, CharIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(((ArrayStream<T>) sequential()).flatMapToChar0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Character>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private CharIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Character>() {
                    private T next = null;
                    private CharIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    ByteStream flatMapToByte0(final Function<? super T, ByteIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(((ArrayStream<T>) sequential()).flatMapToByte0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Byte>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private ByteIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Byte>() {
                    private T next = null;
                    private ByteIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    ShortStream flatMapToShort0(final Function<? super T, ShortIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(((ArrayStream<T>) sequential()).flatMapToShort0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Short>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private ShortIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Short>() {
                    private T next = null;
                    private ShortIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    IntStream flatMapToInt0(final Function<? super T, IntIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(((ArrayStream<T>) sequential()).flatMapToInt0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Integer>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private IntIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Integer>() {
                    private T next = null;
                    private IntIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    LongStream flatMapToLong0(final Function<? super T, LongIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(((ArrayStream<T>) sequential()).flatMapToLong0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Long>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private LongIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Long>() {
                    private T next = null;
                    private LongIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    FloatStream flatMapToFloat0(final Function<? super T, FloatIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(((ArrayStream<T>) sequential()).flatMapToFloat0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Float>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private FloatIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Float>() {
                    private T next = null;
                    private FloatIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    DoubleStream flatMapToDouble0(final Function<? super T, DoubleIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(((ArrayStream<T>) sequential()).flatMapToDouble0(mapper).exIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<Double>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private DoubleIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            cur = mapper.apply(elements[cursor++]);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<Double>() {
                    private T next = null;
                    private DoubleIterator cur = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && next != NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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
    public Stream<Stream<T>> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final Stream<T>[] a = new Stream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? (Stream<T>) Stream.empty() : new ArrayStream<>(elements, fromIndex, middleIndex, null, sorted, cmp);
        a[1] = middleIndex == toIndex ? (Stream<T>) Stream.empty() : new ArrayStream<>(elements, middleIndex, toIndex, null, sorted, cmp);

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<Stream<T>> splitBy(final Predicate<? super T> where) {
        N.requireNonNull(where);

        final NullabLe<Indexed<T>> first = indexed().findFirst(new Predicate<Indexed<T>>() {
            @Override
            public boolean test(Indexed<T> indexed) {
                return !where.test(indexed.value());
            }
        });

        return splitAt(first.isPresent() ? (int) first.get().index() : toIndex - fromIndex);
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
    public Stream<T> distinct(final Function<? super T, ?> keyMapper) {
        final Set<Object> set = new HashSet<>();

        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                final Object key = hashKey(keyMapper.apply(value));

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
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, cmp)) {
            return new ParallelArrayStream<>(elements, toIndex - n, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        } else {
            final T[] a = N.top(elements, fromIndex, toIndex, n, comparator);
            return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }
    }

    @Override
    public Stream<T> sorted() {
        return sorted(OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> sorted(Comparator<? super T> comparator) {
        if (sorted && isSameComparator(comparator, cmp)) {
            return this;
        }

        final T[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.parallelSort(a, comparator);
        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, true, comparator, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().peek(action).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ExIterator<T>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                    @Override
                    public boolean hasNext() {
                        return cursor < to;
                    }

                    @Override
                    public T next() {
                        if (cursor >= to) {
                            throw new NoSuchElementException();
                        }

                        action.accept(elements[cursor]);

                        return elements[cursor++];
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ExIterator<T>() {
                    private Object next = NONE;

                    @Override
                    public boolean hasNext() {
                        if (next == NONE) {
                            synchronized (elements) {
                                if (cursor.intValue() < toIndex) {
                                    next = elements[cursor.getAndIncrement()];
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

                        T result = (T) next;
                        action.accept(result);
                        next = NONE;
                        return result;
                    }
                });
            }
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayStream<>(elements, toIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        } else {
            return new ParallelArrayStream<>(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }
    }

    @Override
    public void forEach(final Consumer<? super T> action) {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

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
                        T next = null;

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
    public <U> U forEach(U seed, BiFunction<U, ? super T, U> accumulator, BiPredicate<? super T, ? super U> conditionToBreak) {
        if (logger.isWarnEnabled()) {
            logger.warn("The 'forEach' with break condition is sequentially executed in parallel stream");
        }

        return sequential().forEach(seed, accumulator, conditionToBreak);
    }

    @Override
    public Object[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    <A> A[] toArray(A[] a) {
        if (a.length < (toIndex - fromIndex)) {
            a = N.newArray(a.getClass().getComponentType(), toIndex - fromIndex);
        }

        N.copy(elements, fromIndex, a, 0, toIndex - fromIndex);

        return a;
    }

    @Override
    public <A> A[] toArray(IntFunction<A[]> generator) {
        return toArray(generator.apply(toIndex - fromIndex));
    }

    @Override
    public ExList<T> toExList() {
        return ExList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<T> toList() {
        final List<T> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends List<T>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<T> toSet() {
        final Set<T> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends Set<T>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset(Supplier<? extends Multiset<T>> supplier) {
        final Multiset<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset() {
        final LongMultiset<T> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<T> toLongMultiset(Supplier<? extends LongMultiset<T>> supplier) {
        final LongMultiset<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        return collect(Collectors.groupingBy(classifier, downstream, mapFactory));
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        return collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapFactory));
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapFactory) {
        return collect(Collectors.toMultimap(keyMapper, valueMapper, mapFactory));
    }

    @Override
    public NullabLe<T> first() {
        if (fromIndex == toIndex) {
            return NullabLe.empty();
        }

        return NullabLe.of(elements[fromIndex]);
    }

    @Override
    public NullabLe<T> last() {
        if (fromIndex == toIndex) {
            return NullabLe.empty();
        }

        return NullabLe.of(elements[toIndex - 1]);
    }

    @Override
    public T reduce(final T identity, final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<T>() {
                    @Override
                    public T call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        T result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.apply(result, elements[cursor++]);
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
                futureList.add(asyncExecutor.execute(new Callable<T>() {
                    @Override
                    public T call() {
                        T result = identity;
                        T next = null;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<T>() {
                    @Override
                    public T call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        if (cursor >= to) {
                            return (T) NONE;
                        }

                        T result = elements[cursor++];

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.apply(result, elements[cursor++]);
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
                futureList.add(asyncExecutor.execute(new Callable<T>() {
                    @Override
                    public T call() {
                        T result = null;

                        synchronized (elements) {
                            if (cursor.intValue() < toIndex) {
                                result = elements[cursor.getAndIncrement()];
                            } else {
                                return (T) NONE;
                            }
                        }

                        T next = null;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<U>() {
                    @Override
                    public U call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        U result = identity;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                result = accumulator.apply(result, elements[cursor++]);
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
                futureList.add(asyncExecutor.execute(new Callable<U>() {
                    @Override
                    public U call() {
                        U result = identity;
                        T next = null;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
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
                        T next = null;

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
    public <R, A> R collect(Collector<? super T, A, R> collector) {
        if (maxThreadNum <= 1) {
            return sequential().collect(collector);
        }

        final Supplier<A> supplier = collector.supplier();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final BinaryOperator<A> combiner = collector.combiner();
        final Function<A, R> finisher = collector.finisher();

        final List<CompletableFuture<A>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<A>() {
                    @Override
                    public A call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        A container = supplier.get();

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
                futureList.add(asyncExecutor.execute(new Callable<A>() {
                    @Override
                    public A call() {
                        A container = supplier.get();
                        T next = null;

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
        return fromIndex == toIndex ? NullabLe.<T> empty() : NullabLe.of(elements[fromIndex]);
    }

    @Override
    public Stream<T> tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex + 1, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> head2() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex, toIndex - 1, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public NullabLe<T> tail2() {
        return fromIndex == toIndex ? NullabLe.<T> empty() : NullabLe.of(elements[toIndex - 1]);
    }

    @Override
    public NullabLe<T> min(Comparator<? super T> comparator) {
        if (fromIndex == toIndex) {
            return NullabLe.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return NullabLe.of(elements[fromIndex]);
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return collect(Collectors.minBy(comparator));
    }

    @Override
    public NullabLe<T> max(Comparator<? super T> comparator) {
        if (fromIndex == toIndex) {
            return NullabLe.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return NullabLe.of(elements[toIndex - 1]);
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
        return toIndex - fromIndex;
    }

    @Override
    public Stream<T> reverse() {
        return new ParallelIteratorStream<>(sequential().reverse().iterator(), closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public boolean anyMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
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
                        T next = null;

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
    public boolean allMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
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
                        T next = null;

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
    public boolean noneMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
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
                        T next = null;

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
    public NullabLe<T> findFirst(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Integer, T>> resultHolder = new Output<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, T> pair = new Pair<>();

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
                        final Pair<Integer, T> pair = new Pair<>();

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

        return resultHolder.value() == null ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(resultHolder.value().right);
    }

    @Override
    public NullabLe<T> findLast(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final Output<Pair<Integer, T>> resultHolder = new Output<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, T> pair = new Pair<>();

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
                        final Pair<Integer, T> pair = new Pair<>();

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

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        T next = null;

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
                        T next = null;

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
    public Stream<T> cached() {
        return this;
    }

    @Override
    public Stream<T> queued(int queueSize) {
        return this;
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
            final BiConsumer<? super T, ? super PreparedStatement> stmtSetter) {
        if (maxThreadNum <= 1) {
            return sequential().persist(stmt, batchSize, batchInterval, stmtSetter);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Output<Throwable> eHolder = new Output<>();
        final AtomicLong result = new AtomicLong();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / maxThreadNum + ((toIndex - fromIndex) % maxThreadNum == 0 ? 0 : 1);

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        long cnt = 0;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                stmtSetter.accept(elements[cursor++], stmt);
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
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        long cnt = 0;
                        T next = null;

                        try {
                            while (eHolder.value() == null) {
                                synchronized (elements) {
                                    if (cursor.intValue() < toIndex) {
                                        next = elements[cursor.getAndIncrement()];
                                    } else {
                                        break;
                                    }
                                }

                                stmtSetter.accept(next, stmt);
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
        ArrayStream<T> tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayStream<>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp);
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

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
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

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public BaseStream.Splitor splitor() {
        return splitor;
    }

    @Override
    public Stream<T> splitor(BaseStream.Splitor splitor) {
        if (this.splitor == splitor) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, newCloseHandlers, sorted, cmp, maxThreadNum, splitor);
    }
}
