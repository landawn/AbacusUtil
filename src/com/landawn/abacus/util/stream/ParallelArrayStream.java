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
import java.util.Arrays;
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
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.Holder;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.MutableInt;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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
final class ParallelArrayStream<T> extends AbstractStream<T> {
    private final T[] elements;
    private final int fromIndex;
    private final int toIndex;
    private final int maxThreadNum;
    private final Splitor splitor;
    private volatile ArrayStream<T> sequential;

    ParallelArrayStream(T[] values, int fromIndex, int toIndex, Collection<Runnable> closeHandlers, boolean sorted, Comparator<? super T> comparator,
            int maxThreadNum, Splitor splitor) {
        super(closeHandlers, sorted, comparator);

        checkIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
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
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<T>() {
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
                iters.add(new ImmutableIterator<T>() {
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
            iters.add(new ImmutableIterator<T>() {
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
            iters.add(new ImmutableIterator<T>() {
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
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<R>() {
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
                iters.add(new ImmutableIterator<R>() {
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
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().mapToChar(mapper).charIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ImmutableIterator<Character>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Character>() {
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
                iters.add(new ImmutableIterator<Character>() {
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
            return new ParallelIteratorByteStream(sequential().mapToByte(mapper).byteIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ImmutableIterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Byte>() {
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
                iters.add(new ImmutableIterator<Byte>() {
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
            return new ParallelIteratorShortStream(sequential().mapToShort(mapper).shortIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ImmutableIterator<Short>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Short>() {
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
                iters.add(new ImmutableIterator<Short>() {
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
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).intIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ImmutableIterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Integer>() {
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
                iters.add(new ImmutableIterator<Integer>() {
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
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).longIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ImmutableIterator<Long>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Long>() {
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
                iters.add(new ImmutableIterator<Long>() {
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
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).floatIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ImmutableIterator<Float>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Float>() {
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
                iters.add(new ImmutableIterator<Float>() {
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
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).doubleIterator(), closeHandlers, false, maxThreadNum, splitor);
        }

        final List<ImmutableIterator<Double>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Double>() {
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
                iters.add(new ImmutableIterator<Double>() {
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
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<R>() {
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
                iters.add(new ImmutableIterator<R>() {
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
            return new ParallelIteratorCharStream(((ArrayStream<T>) sequential()).flatMapToChar0(mapper).charIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Character>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Character>() {
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

                        return cur.next();
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ImmutableIterator<Character>() {
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

                        return cur.next();
                    }
                });
            }
        }

        return new ParallelIteratorCharStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    ByteStream flatMapToByte0(final Function<? super T, ByteIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(((ArrayStream<T>) sequential()).flatMapToByte0(mapper).byteIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Byte>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Byte>() {
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

                        return cur.next();
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ImmutableIterator<Byte>() {
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

                        return cur.next();
                    }
                });
            }
        }

        return new ParallelIteratorByteStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    ShortStream flatMapToShort0(final Function<? super T, ShortIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(((ArrayStream<T>) sequential()).flatMapToShort0(mapper).shortIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Short>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Short>() {
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

                        return cur.next();
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ImmutableIterator<Short>() {
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

                        return cur.next();
                    }
                });
            }
        }

        return new ParallelIteratorShortStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    IntStream flatMapToInt0(final Function<? super T, IntIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(((ArrayStream<T>) sequential()).flatMapToInt0(mapper).intIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Integer>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Integer>() {
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

                        return cur.next();
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ImmutableIterator<Integer>() {
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

                        return cur.next();
                    }
                });
            }
        }

        return new ParallelIteratorIntStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    LongStream flatMapToLong0(final Function<? super T, LongIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(((ArrayStream<T>) sequential()).flatMapToLong0(mapper).longIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Long>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Long>() {
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

                        return cur.next();
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ImmutableIterator<Long>() {
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

                        return cur.next();
                    }
                });
            }
        }

        return new ParallelIteratorLongStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    FloatStream flatMapToFloat0(final Function<? super T, FloatIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(((ArrayStream<T>) sequential()).flatMapToFloat0(mapper).floatIterator(), closeHandlers, false, maxThreadNum,
                    splitor);
        }

        final List<Iterator<Float>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Float>() {
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

                        return cur.next();
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ImmutableIterator<Float>() {
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

                        return cur.next();
                    }
                });
            }
        }

        return new ParallelIteratorFloatStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    DoubleStream flatMapToDouble0(final Function<? super T, DoubleIterator> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(((ArrayStream<T>) sequential()).flatMapToDouble0(mapper).doubleIterator(), closeHandlers, false,
                    maxThreadNum, splitor);
        }

        final List<Iterator<Double>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<Double>() {
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

                        return cur.next();
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                iters.add(new ImmutableIterator<Double>() {
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

                        return cur.next();
                    }
                });
            }
        }

        return new ParallelIteratorDoubleStream(Stream.parallelConcat(iters, asyncExecutor), closeHandlers, false, maxThreadNum, splitor);
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        return new ParallelIteratorStream<Stream<T>>(new ImmutableIterator<Stream<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayStream<T>(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex), null, sorted, cmp);
            }

        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<List<T>> split2(final int size) {
        return new ParallelIteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return Arrays.asList(N.copyOfRange(elements, cursor, (cursor = toIndex - cursor > size ? cursor + size : toIndex)));
            }

        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<Set<T>> split3(final int size) {
        return new ParallelIteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Set<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final Set<T> set = new HashSet<>(N.initHashCapacity(toIndex - cursor > size ? size : toIndex - cursor));

                for (int i = cursor, to = (cursor = toIndex - cursor > size ? cursor + size : toIndex); i < to; i++) {
                    set.add(elements[i]);
                }

                return set;
            }

        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<List<T>> split2(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> boundaryUpdate) {
        return new ParallelIteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>();

                while (cursor < toIndex) {
                    if (result.size() == 0) {
                        preCondition = predicate.apply(elements[cursor], boundary);
                        result.add(elements[cursor]);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], boundary) == preCondition) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }

                        break;
                    }
                }

                return result;
            }

        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public <U> Stream<Set<T>> split3(final U boundary, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> boundaryUpdate) {
        return new ParallelIteratorStream<Set<T>>(new ImmutableIterator<Set<T>>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Set<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final Set<T> result = new HashSet<>();

                while (cursor < toIndex) {
                    if (result.size() == 0) {
                        preCondition = predicate.apply(elements[cursor], boundary);
                        result.add(elements[cursor]);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], boundary) == preCondition) {
                        result.add(elements[cursor]);
                        cursor++;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }

                        break;
                    }
                }

                return result;
            }

        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<Stream<T>> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final Stream<T>[] a = new Stream[2];
        final int middleIndex = n >= toIndex - fromIndex ? toIndex : fromIndex + n;
        a[0] = middleIndex == fromIndex ? (Stream<T>) Stream.empty() : new ArrayStream<T>(elements, fromIndex, middleIndex, null, sorted, cmp);
        a[1] = middleIndex == toIndex ? (Stream<T>) Stream.empty() : new ArrayStream<T>(elements, middleIndex, toIndex, null, sorted, cmp);

        return new ParallelArrayStream<>(a, 0, a.length, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<ObjectList<T>> sliding(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new ParallelIteratorStream<ObjectList<T>>(new ImmutableIterator<ObjectList<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ObjectList<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ObjectList<T> result = ObjectList.of(N.copyOfRange(elements, cursor, toIndex - cursor > windowSize ? cursor + windowSize : toIndex));

                cursor = cursor >= toIndex - increment || cursor >= toIndex - windowSize ? toIndex : cursor + increment;

                return result;
            }

        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<List<T>> sliding2(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new ParallelIteratorStream<List<T>>(new ImmutableIterator<List<T>>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public List<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final List<T> result = N.asList(N.copyOfRange(elements, cursor, toIndex - cursor > windowSize ? cursor + windowSize : toIndex));

                cursor = cursor >= toIndex - increment || cursor >= toIndex - windowSize ? toIndex : cursor + increment;

                return result;
            }

        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> distinct(final Function<? super T, ?> keyMapper) {
        return filter(new Predicate<T>() {
            private final Set<Object> set = new HashSet<>();

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
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, cmp)) {
            return new ParallelArrayStream<T>(elements, toIndex - n, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        } else {
            final T[] a = N.top(elements, fromIndex, toIndex, n, comparator);
            return new ParallelArrayStream<T>(a, 0, a.length, closeHandlers, sorted, cmp, maxThreadNum, splitor);
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
        return new ParallelArrayStream<T>(a, 0, a.length, closeHandlers, true, comparator, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().peek(action).iterator(), closeHandlers, false, null, maxThreadNum, splitor);
        }

        final List<Iterator<T>> iters = new ArrayList<>(maxThreadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ImmutableIterator<T>() {
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
                iters.add(new ImmutableIterator<T>() {
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

        return new ParallelArrayStream<T>(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayStream<T>(elements, toIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        } else {
            return new ParallelArrayStream<T>(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }
    }

    @Override
    public void forEach(final Consumer<? super T> action) {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    @Override
    public <U> U forEach(U identity, BiFunction<U, ? super T, U> accumulator, Predicate<? super U> predicate) {
        if (logger.isWarnEnabled()) {
            logger.warn("'forEach' is sequentially executed in parallel stream");
        }

        return sequential().forEach(identity, accumulator, predicate);
    }

    @Override
    public Object[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

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
    public ObjectList<T> toObjectList() {
        return ObjectList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<T> toList() {
        final List<T> result = new ArrayList<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public List<T> toList(Supplier<? extends List<T>> supplier) {
        final List<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<T> toSet() {
        final Set<T> result = new HashSet<>();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<T> toSet(Supplier<? extends Set<T>> supplier) {
        final Set<T> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>();

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
        final LongMultiset<T> result = new LongMultiset<>();

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
    public <K, D, A, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        return collect(Collectors.groupingBy(classifier, downstream, mapFactory));
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapSupplier) {
        return collect(Collectors.toMap(keyMapper, valueMapper, mergeFunction, mapSupplier));
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(Function<? super T, ? extends K> keyMapper,
            Function<? super T, ? extends U> valueMapper, Supplier<Multimap<K, U, V>> mapSupplier) {
        return collect(Collectors.toMultimap(keyMapper, valueMapper, mapSupplier));
    }

    @Override
    public OptionalNullable<T> first() {
        if (fromIndex == toIndex) {
            return OptionalNullable.empty();
        }

        return OptionalNullable.of(elements[fromIndex]);
    }

    @Override
    public OptionalNullable<T> last() {
        if (fromIndex == toIndex) {
            return OptionalNullable.empty();
        }

        return OptionalNullable.of(elements[toIndex - 1]);
    }

    @Override
    public T reduce(final T identity, final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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
    public OptionalNullable<T> reduce(final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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

        return result == NONE ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(result);
    }

    @Override
    public <U> U reduce(final U identity, final BiFunction<U, ? super T, U> accumulator, final BinaryOperator<U> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator, combiner);
        }

        final List<CompletableFuture<U>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<R>() {
                    @Override
                    public R call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;

                        R container = supplier.get();

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
                        R container = supplier.get();
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
                final R tmp = future.get();

                if (container == NONE) {
                    container = tmp;
                } else {
                    combiner.accept(container, tmp);
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
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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
                final A tmp = future.get();

                if (container == NONE) {
                    container = tmp;
                } else {
                    combiner.apply(container, tmp);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return finisher.apply(container == NONE ? supplier.get() : container);
    }

    @Override
    public OptionalNullable<T> min(Comparator<? super T> comparator) {
        if (count() == 0) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return collect(Collectors.minBy(comparator));
    }

    @Override
    public OptionalNullable<T> max(Comparator<? super T> comparator) {
        if (count() == 0) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return collect(Collectors.maxBy(comparator));
    }

    @Override
    public OptionalNullable<T> kthLargest(int k, Comparator<? super T> comparator) {
        if (count() == 0) {
            return OptionalNullable.empty();
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return sequential().kthLargest(k, comparator);
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public Stream<T> reverse() {
        return new ParallelIteratorStream<>(new ImmutableIterator<T>() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public T next() {
                if (cursor <= fromIndex) {
                    throw new NoSuchElementException();
                }

                return elements[--cursor];
            }

            @Override
            public long count() {
                return cursor - fromIndex;
            }

            @Override
            public void skip(long n) {
                cursor = cursor - fromIndex > n ? cursor - (int) n : fromIndex;
            }
        }, closeHandlers, false, null, maxThreadNum, splitor);
    }

    @Override
    public boolean anyMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.value();
    }

    @Override
    public boolean allMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.value();
    }

    @Override
    public boolean noneMatch(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final List<CompletableFuture<Void>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.value();
    }

    @Override
    public OptionalNullable<T> findFirst(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final List<CompletableFuture<Pair<Integer, T>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, T>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, T>>() {
                    @Override
                    public Pair<Integer, T> call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        final Pair<Integer, T> pair = new Pair<>();

                        try {
                            while (cursor < to && resultHolder.value() == null && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[cursor++];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, T>>() {
                    @Override
                    public Pair<Integer, T> call() {
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
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Integer, T>> future : futureList) {
                final Pair<Integer, T> pair = future.get();

                if (resultHolder.value() == null || pair.left < resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(resultHolder.value().right);
    }

    @Override
    public OptionalNullable<T> findLast(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final List<CompletableFuture<Pair<Integer, T>>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, T>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, T>>() {
                    @Override
                    public Pair<Integer, T> call() {
                        final int from = fromIndex + sliceIndex * sliceSize;
                        int cursor = toIndex - from > sliceSize ? from + sliceSize : toIndex;
                        final Pair<Integer, T> pair = new Pair<>();

                        try {
                            while (cursor > from && resultHolder.value() == null && eHolder.value() == null) {
                                pair.left = cursor;
                                pair.right = elements[--cursor];

                                if (predicate.test(pair.right)) {
                                    synchronized (resultHolder) {
                                        if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(toIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<Pair<Integer, T>>() {
                    @Override
                    public Pair<Integer, T> call() {
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
                                            resultHolder.setValue(pair);
                                        }
                                    }

                                    break;
                                }
                            }
                        } catch (Throwable e) {
                            setError(eHolder, e);
                        }

                        return pair;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Pair<Integer, T>> future : futureList) {
                final Pair<Integer, T> pair = future.get();

                if (resultHolder.value() == null || pair.left > resultHolder.value().left) {
                    resultHolder.setValue(pair);
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == null ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(resultHolder.value().right);
    }

    @Override
    public OptionalNullable<T> findAny(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final List<CompletableFuture<T>> futureList = new ArrayList<>(maxThreadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<T> resultHolder = Holder.of((T) NONE);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

            for (int i = 0; i < maxThreadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Callable<T>() {
                    @Override
                    public T call() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        T next = null;

                        try {
                            while (cursor < to && resultHolder.value() == null && eHolder.value() == null) {
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

                        return next;
                    }
                }));
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < maxThreadNum; i++) {
                futureList.add(asyncExecutor.execute(new Callable<T>() {
                    @Override
                    public T call() {
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

                        return next;
                    }
                }));
            }
        }

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<T> future : futureList) {
                if (resultHolder.value() == NONE) {
                    future.get();
                } else {
                    break;
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return resultHolder.value() == NONE ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(resultHolder.value());
    }

    @Override
    public Stream<T> except(final Function<? super T, ?> mapper, final Collection<?> c) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().intersect(mapper, c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        return filter(new Predicate<T>() {
            final Multiset<?> multiset = Multiset.of(c);

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
    public Stream<T> intersect(final Function<? super T, ?> mapper, final Collection<?> c) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().intersect(mapper, c).iterator(), closeHandlers, sorted, cmp, maxThreadNum, splitor);
        }

        return filter(new Predicate<T>() {
            final Multiset<?> multiset = Multiset.of(c);

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
    public Stream<T> cached() {
        return this;
    }

    @Override
    public Stream<T> queued() {
        return queued(DEFAULT_QUEUE_SIZE_PER_ITERATOR);
    }

    @Override
    public Stream<T> queued(int queueSize) {
        return this;
    }

    @Override
    public Stream<T> append(final Stream<T> stream) {
        return new ParallelIteratorStream<>(Stream.concat(this, stream), closeHandlers, false, null, maxThreadNum, splitor);
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
        final Holder<Throwable> eHolder = new Holder<>();
        final AtomicLong result = new AtomicLong();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) % maxThreadNum == 0 ? (toIndex - fromIndex) / maxThreadNum : (toIndex - fromIndex) / maxThreadNum + 1;

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

        if (eHolder.value() != null) {
            throw N.toRuntimeException(eHolder.value());
        }

        try {
            for (CompletableFuture<Void> future : futureList) {
                future.get();
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return result.longValue();
    }

    @Override
    public ImmutableIterator<T> iterator() {
        return ImmutableIterator.of(elements, fromIndex, toIndex);
    }

    @Override
    public boolean isParallel() {
        return true;
    }

    @Override
    public Stream<T> sequential() {
        ArrayStream<T> tmp = sequential;

        if (tmp == null) {
            tmp = new ArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp);
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

        return new ParallelArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
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

        return new ParallelArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
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

        return new ParallelArrayStream<T>(elements, fromIndex, toIndex, closeHandlers, sorted, cmp, maxThreadNum, splitor);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayStream<T>(elements, fromIndex, toIndex, newCloseHandlers, sorted, cmp, maxThreadNum, splitor);
    }
}
