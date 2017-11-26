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
import com.landawn.abacus.util.Nullable;
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

    ParallelArrayStream(final T[] values, final int fromIndex, final int toIndex, final boolean sorted, Comparator<? super T> comparator, int maxThreadNum,
            Splitor splitor, final Collection<Runnable> closeHandlers) {
        super(values, fromIndex, toIndex, sorted, comparator, closeHandlers);

        this.maxThreadNum = N.min(maxThreadNum, MAX_THREAD_NUM_PER_OPERATION);
        this.splitor = splitor == null ? DEFAULT_SPLITOR : splitor;
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().filter(predicate).iterator(), sorted, cmp, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<Iterator<T>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<T>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<T>() {

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

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().takeWhile(predicate).iterator(), sorted, cmp, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<Iterator<T>> iters = new ArrayList<>(threadNum);
        final MutableBoolean hasMore = MutableBoolean.of(true);
        final MutableInt cursor = MutableInt.of(fromIndex);

        for (int i = 0; i < threadNum; i++) {
            iters.add(new ObjIteratorEx<T>() {
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

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().dropWhile(predicate).iterator(), sorted, cmp, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<Iterator<T>> iters = new ArrayList<>(threadNum);
        final MutableBoolean dropped = MutableBoolean.of(false);
        final MutableInt cursor = MutableInt.of(fromIndex);

        for (int i = 0; i < threadNum; i++) {
            iters.add(new ObjIteratorEx<T>() {
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

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <R> Stream<R> map(final Function<? super T, ? extends R> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().map(mapper).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<Iterator<R>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<R>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<R>() {

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

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    }

    //    @Override
    //    public <R> Stream<R> biMap(final BiFunction<? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorStream<>(sequential().biMap(mapper, ignoreNotPaired).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    //        }
    //
    //        final int atLeast = ignoreNotPaired ? 2 : 1;
    //        final int count = (toIndex - fromIndex) / 2 + (ignoreNotPaired || (toIndex - fromIndex) % 2 == 0 ? 0 : 1);
    //        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
    //        final List<Iterator<R>> iters = new ArrayList<>(threadNum);
    //
    //        if (splitor == Splitor.ARRAY) {
    //            final int sliceSize = count / maxThreadNum + (count % maxThreadNum == 0 ? 0 : 1);
    //
    //            for (int i = 0; i < threadNum; i++) {
    //                final int sliceIndex = i;
    //                iters.add(new ObjIteratorEx<R>() {
    //                    private int cursor = fromIndex + sliceIndex * sliceSize * 2;
    //                    private final int to = toIndex - cursor > sliceSize * 2 ? cursor + sliceSize * 2 : toIndex;
    //
    //                    @Override
    //                    public boolean hasNext() {
    //                        return to - cursor >= atLeast;
    //                    }
    //
    //                    @Override
    //                    public R next() {
    //                        if (to - cursor < atLeast) {
    //                            throw new NoSuchElementException();
    //                        }
    //
    //                        return mapper.apply(elements[cursor++], cursor == toIndex ? null : elements[cursor++]);
    //                    }
    //                });
    //            }
    //        } else {
    //            final MutableInt cursor = MutableInt.of(fromIndex);
    //
    //            for (int i = 0; i < threadNum; i++) {
    //                iters.add(new ObjIteratorEx<R>() {
    //
    //                    private Object pre = NONE;
    //                    private Object next = NONE;
    //
    //                    @Override
    //                    public boolean hasNext() {
    //                        if (pre == NONE) {
    //                            synchronized (elements) {
    //                                if (toIndex - cursor.intValue() >= atLeast) {
    //                                    pre = elements[cursor.getAndIncrement()];
    //                                    next = cursor.intValue() == toIndex ? null : elements[cursor.getAndIncrement()];
    //                                }
    //                            }
    //                        }
    //
    //                        return pre != NONE;
    //                    }
    //
    //                    @Override
    //                    public R next() {
    //                        if (pre == NONE && hasNext() == false) {
    //                            throw new NoSuchElementException();
    //                        }
    //
    //                        final R result = mapper.apply((T) pre, (T) next);
    //                        pre = NONE;
    //                        next = NONE;
    //                        return result;
    //                    }
    //
    //                });
    //            }
    //        }
    //
    //        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    //    }
    //
    //    @Override
    //    public <R> Stream<R> triMap(final TriFunction<? super T, ? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
    //        if (maxThreadNum <= 1) {
    //            return new ParallelIteratorStream<>(sequential().triMap(mapper, ignoreNotPaired).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    //        }
    //
    //        final int atLeast = ignoreNotPaired ? 3 : 1;
    //        final int count = (toIndex - fromIndex) / 3 + (ignoreNotPaired || (toIndex - fromIndex) % 3 == 0 ? 0 : 1);
    //        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
    //        final List<Iterator<R>> iters = new ArrayList<>(threadNum);
    //
    //        if (splitor == Splitor.ARRAY) {
    //            final int sliceSize = count / maxThreadNum + (count % maxThreadNum == 0 ? 0 : 1);
    //
    //            for (int i = 0; i < threadNum; i++) {
    //                final int sliceIndex = i;
    //                iters.add(new ObjIteratorEx<R>() {
    //                    private int cursor = fromIndex + sliceIndex * sliceSize * 3;
    //                    private final int to = toIndex - cursor > sliceSize * 3 ? cursor + sliceSize * 3 : toIndex;
    //
    //                    @Override
    //                    public boolean hasNext() {
    //                        return to - cursor >= atLeast;
    //                    }
    //
    //                    @Override
    //                    public R next() {
    //                        if (to - cursor < atLeast) {
    //                            throw new NoSuchElementException();
    //                        }
    //
    //                        return mapper.apply(elements[cursor++], cursor == toIndex ? null : elements[cursor++], cursor == toIndex ? null : elements[cursor++]);
    //                    }
    //                });
    //            }
    //        } else {
    //            final MutableInt cursor = MutableInt.of(fromIndex);
    //
    //            for (int i = 0; i < threadNum; i++) {
    //                iters.add(new ObjIteratorEx<R>() {
    //
    //                    private Object prepre = NONE;
    //                    private Object pre = NONE;
    //                    private Object next = NONE;
    //
    //                    @Override
    //                    public boolean hasNext() {
    //                        if (prepre == NONE) {
    //                            synchronized (elements) {
    //                                if (toIndex - cursor.intValue() >= atLeast) {
    //                                    prepre = elements[cursor.getAndIncrement()];
    //                                    pre = cursor.intValue() == toIndex ? null : elements[cursor.getAndIncrement()];
    //                                    next = cursor.intValue() == toIndex ? null : elements[cursor.getAndIncrement()];
    //                                }
    //                            }
    //                        }
    //
    //                        return prepre != NONE;
    //                    }
    //
    //                    @Override
    //                    public R next() {
    //                        if (prepre == NONE && hasNext() == false) {
    //                            throw new NoSuchElementException();
    //                        }
    //
    //                        final R result = mapper.apply((T) prepre, (T) pre, (T) next);
    //                        prepre = NONE;
    //                        pre = NONE;
    //                        next = NONE;
    //                        return result;
    //                    }
    //
    //                });
    //            }
    //        }
    //
    //        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    //    }

    @Override
    public <R> Stream<R> slidingMap(final BiFunction<? super T, ? super T, R> mapper, final int increment, final boolean ignoreNotPaired) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().slidingMap(mapper, increment).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        final int windowSize = 2;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);
        final MutableInt curIndex = MutableInt.of(fromIndex);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ObjIteratorEx<R>() {
                private int cursor = -1;

                @Override
                public boolean hasNext() {
                    if (cursor == -1) {
                        synchronized (elements) {
                            if (ignoreNotPaired ? toIndex - curIndex.intValue() >= windowSize : curIndex.intValue() < toIndex) {
                                cursor = curIndex.value();
                                curIndex.setValue(increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex);
                            }
                        }
                    }

                    return cursor != -1;
                }

                @Override
                public R next() {
                    if (cursor == -1 && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    final R result = mapper.apply(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null);
                    cursor = -1;
                    return result;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <R> Stream<R> slidingMap(final TriFunction<? super T, ? super T, ? super T, R> mapper, final int increment, final boolean ignoreNotPaired) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().slidingMap(mapper, increment).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        final int windowSize = 3;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        final List<Iterator<R>> iters = new ArrayList<>(maxThreadNum);
        final MutableInt curIndex = MutableInt.of(fromIndex);

        for (int i = 0; i < maxThreadNum; i++) {
            iters.add(new ObjIteratorEx<R>() {
                private int cursor = -1;

                @Override
                public boolean hasNext() {
                    if (cursor == -1) {
                        synchronized (elements) {
                            if (ignoreNotPaired ? toIndex - curIndex.intValue() >= windowSize : curIndex.intValue() < toIndex) {
                                cursor = curIndex.value();
                                curIndex.setValue(increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex);
                            }
                        }
                    }

                    return cursor != -1;
                }

                @Override
                public R next() {
                    if (cursor == -1 && hasNext() == false) {
                        throw new NoSuchElementException();
                    }

                    final R result = mapper.apply(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null,
                            cursor < toIndex - 2 ? elements[cursor + 2] : null);
                    cursor = -1;
                    return result;
                }
            });
        }

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> mapFirst(final Function<? super T, ? extends T> mapperForFirst) {
        N.requireNonNull(mapperForFirst);

        if (fromIndex == toIndex) {
            return this;
        } else if (toIndex - fromIndex == 1) {
            return map(mapperForFirst);
        } else {
            final Iterator<T> iter = new ObjIteratorEx<T>() {
                private int cursor = fromIndex;

                @Override
                public boolean hasNext() {
                    return cursor < toIndex;
                }

                @Override
                public T next() {
                    if (cursor >= toIndex) {
                        throw new NoSuchElementException();
                    }

                    if (cursor == fromIndex) {
                        return mapperForFirst.apply(elements[cursor++]);
                    } else {
                        return elements[cursor++];
                    }
                }

                //                @Override
                //                public long count() {
                //                    return toIndex - cursor;
                //                }
                //
                //                @Override
                //                public void skip(long n) {
                //                    cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
                //                }

                @Override
                public long count() {
                    if (hasNext()) {
                        next();
                        return toIndex - cursor + 1;
                    }

                    return 0;
                }

                @Override
                public void skip(long n) {
                    if (hasNext()) {
                        next();
                        n -= 1;
                        cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
                    }
                }

                @Override
                public <A> A[] toArray(A[] a) {
                    a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                    for (int i = 0, len = toIndex - cursor; i < len; i++) {
                        if (cursor == fromIndex) {
                            a[i] = (A) mapperForFirst.apply(elements[cursor++]);
                        } else {
                            a[i] = (A) elements[cursor++];
                        }
                    }

                    return a;
                }
            };

            return new ParallelIteratorStream<>(iter, false, null, maxThreadNum, splitor, closeHandlers);
        }
    }

    @SuppressWarnings("resource")
    @Override
    public <R> Stream<R> mapFirstOrElse(final Function<? super T, ? extends R> mapperForFirst, final Function<? super T, ? extends R> mapperForElse) {
        N.requireNonNull(mapperForFirst);
        N.requireNonNull(mapperForElse);

        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().mapFirstOrElse(mapperForFirst, mapperForElse).iterator(), false, null, maxThreadNum, splitor,
                    closeHandlers);
        }

        if (fromIndex == toIndex) {
            return (Stream<R>) this;
        } else if (toIndex - fromIndex == 1) {
            return map(mapperForFirst);
        } else {
            final Function<T, R> mapperForFirst2 = (Function<T, R>) mapperForFirst;
            final Function<T, R> mapperForElse2 = (Function<T, R>) mapperForElse;

            return new ParallelArrayStream<>(elements, fromIndex + 1, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers).map(mapperForElse2)
                    .prepend(new ArrayStream<>(elements, fromIndex, fromIndex + 1).map(mapperForFirst2));
        }
    }

    @Override
    public Stream<T> mapLast(final Function<? super T, ? extends T> mapperForLast) {
        N.requireNonNull(mapperForLast);

        if (fromIndex == toIndex) {
            return this;
        } else if (toIndex - fromIndex == 1) {
            return map(mapperForLast);
        } else {
            final Iterator<T> iter = new ObjIteratorEx<T>() {
                private int last = toIndex - 1;
                private int cursor = fromIndex;

                @Override
                public boolean hasNext() {
                    return cursor < toIndex;
                }

                @Override
                public T next() {
                    if (cursor >= toIndex) {
                        throw new NoSuchElementException();
                    }

                    if (cursor == last) {
                        return mapperForLast.apply(elements[cursor++]);
                    } else {
                        return elements[cursor++];
                    }
                }

                //                @Override
                //                public long count() {
                //                    return toIndex - cursor;
                //                }
                //
                //                @Override
                //                public void skip(long n) {
                //                    cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
                //                }

                @Override
                public <A> A[] toArray(A[] a) {
                    a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                    for (int i = 0, len = toIndex - cursor; i < len; i++) {
                        if (cursor == last) {
                            a[i] = (A) mapperForLast.apply(elements[cursor++]);
                        } else {
                            a[i] = (A) elements[cursor++];
                        }
                    }

                    return a;
                }
            };

            return new ParallelIteratorStream<>(iter, false, null, maxThreadNum, splitor, closeHandlers);
        }
    }

    @SuppressWarnings("resource")
    @Override
    public <R> Stream<R> mapLastOrElse(final Function<? super T, ? extends R> mapperForLast, final Function<? super T, ? extends R> mapperForElse) {
        N.requireNonNull(mapperForLast);
        N.requireNonNull(mapperForElse);

        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().mapLastOrElse(mapperForLast, mapperForElse).iterator(), false, null, maxThreadNum, splitor,
                    closeHandlers);
        }

        if (fromIndex == toIndex) {
            return (Stream<R>) this;
        } else if (toIndex - fromIndex == 1) {
            return map(mapperForLast);
        } else {
            final Function<T, R> mapperForLast2 = (Function<T, R>) mapperForLast;
            final Function<T, R> mapperForElse2 = (Function<T, R>) mapperForElse;

            return new ParallelArrayStream<>(elements, fromIndex, toIndex - 1, sorted, cmp, maxThreadNum, splitor, closeHandlers).map(mapperForElse2)
                    .append(new ArrayStream<>(elements, toIndex - 1, toIndex).map(mapperForLast2));
        }
    }

    @Override
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().mapToChar(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Character>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Character>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Character>() {

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

        return new ParallelIteratorCharStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().mapToByte(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Byte>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Byte>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Byte>() {

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

        return new ParallelIteratorByteStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().mapToShort(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Short>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Short>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Short>() {

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

        return new ParallelIteratorShortStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().mapToInt(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Integer>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Integer>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Integer>() {

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

        return new ParallelIteratorIntStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().mapToLong(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Long>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Long>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Long>() {

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

        return new ParallelIteratorLongStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().mapToFloat(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Float>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Float>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Float>() {

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

        return new ParallelIteratorFloatStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().mapToDouble(mapper).iteratorEx(), false, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Double>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Double>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Double>() {

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

        return new ParallelIteratorDoubleStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <R> Stream<R> flatMap(final Function<? super T, ? extends Stream<? extends R>> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().flatMap(mapper), false, null, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<R>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<R>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private Iterator<? extends R> cur = null;
                    private Stream<? extends R> s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<R>() {

                    private T next = null;
                    private Iterator<? extends R> cur = null;
                    private Stream<? extends R> s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };
                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public CharStream flatMapToChar(final Function<? super T, ? extends CharStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorCharStream(sequential().flatMapToChar(mapper), false, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Character>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Character>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private CharIterator cur = null;
                    private CharStream s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Character>() {

                    private T next = null;
                    private CharIterator cur = null;
                    private CharStream s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorCharStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public ByteStream flatMapToByte(final Function<? super T, ? extends ByteStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorByteStream(sequential().flatMapToByte(mapper), false, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Byte>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Byte>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private ByteIterator cur = null;
                    private ByteStream s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Byte>() {

                    private T next = null;
                    private ByteIterator cur = null;
                    private ByteStream s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorByteStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public ShortStream flatMapToShort(final Function<? super T, ? extends ShortStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorShortStream(sequential().flatMapToShort(mapper), false, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Short>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Short>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private ShortIterator cur = null;
                    private ShortStream s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Short>() {

                    private T next = null;
                    private ShortIterator cur = null;
                    private ShortStream s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorShortStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorIntStream(sequential().flatMapToInt(mapper), false, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Integer>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Integer>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private IntIterator cur = null;
                    private IntStream s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Integer>() {

                    private T next = null;
                    private IntIterator cur = null;
                    private IntStream s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorIntStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorLongStream(sequential().flatMapToLong(mapper), false, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Long>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Long>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private LongIterator cur = null;
                    private LongStream s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Long>() {

                    private T next = null;
                    private LongIterator cur = null;
                    private LongStream s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorLongStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public FloatStream flatMapToFloat(final Function<? super T, ? extends FloatStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorFloatStream(sequential().flatMapToFloat(mapper), false, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Float>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Float>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private FloatIterator cur = null;
                    private FloatStream s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Float>() {

                    private T next = null;
                    private FloatIterator cur = null;
                    private FloatStream s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorFloatStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorDoubleStream(sequential().flatMapToDouble(mapper), false, maxThreadNum, splitor, null);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<ObjIteratorEx<Double>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<Double>() {
                    private int cursor = fromIndex + sliceIndex * sliceSize;
                    private final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                    private DoubleIterator cur = null;
                    private DoubleStream s = null;
                    private Runnable closeHandle = null;

                    @Override
                    public boolean hasNext() {
                        while ((cur == null || cur.hasNext() == false) && cursor < to) {
                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(elements[cursor++]);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        } else {
            final MutableInt cursor = MutableInt.of(fromIndex);

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<Double>() {

                    private T next = null;
                    private DoubleIterator cur = null;
                    private DoubleStream s = null;
                    private Runnable closeHandle = null;

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

                            if (closeHandle != null) {
                                final Runnable tmp = closeHandle;
                                closeHandle = null;
                                tmp.run();
                            }

                            s = mapper.apply(next);

                            if (N.notNullOrEmpty(s.closeHandlers)) {
                                final Set<Runnable> tmp = s.closeHandlers;

                                closeHandle = new Runnable() {
                                    @Override
                                    public void run() {
                                        Stream.close(tmp);
                                    }
                                };

                            }

                            cur = s.iterator();
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

                    @Override
                    public void close() {
                        if (closeHandle != null) {
                            final Runnable tmp = closeHandle;
                            closeHandle = null;
                            tmp.run();
                        }
                    }
                });
            }
        }

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                Stream.close(iters);
            }
        });

        return new ParallelIteratorDoubleStream(Stream.parallelConcat2(iters, iters.size()), false, maxThreadNum, splitor, newCloseHandlers);
    }

    @Override
    public Stream<Stream<T>> split(final int size) {
        return new ParallelIteratorStream<>(sequential().split(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<List<T>> splitToList(final int size) {
        return new ParallelIteratorStream<>(sequential().splitToList(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<Set<T>> splitToSet(final int size) {
        return new ParallelIteratorStream<>(sequential().splitToSet(size).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<Stream<T>> split(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<>(sequential().split(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <U> Stream<List<T>> splitToList(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<>(sequential().splitToList(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor,
                closeHandlers);
    }

    @Override
    public <U> Stream<Set<T>> splitToSet(final U seed, final BiPredicate<? super T, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new ParallelIteratorStream<>(sequential().splitToSet(seed, predicate, seedUpdate).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<Stream<T>> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final Stream<T>[] a = new Stream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? (Stream<T>) Stream.empty() : new ArrayStream<>(elements, fromIndex, middleIndex, sorted, cmp, null);
        a[1] = middleIndex == toIndex ? (Stream<T>) Stream.empty() : new ArrayStream<>(elements, middleIndex, toIndex, sorted, cmp, null);

        return new ParallelArrayStream<>(a, 0, a.length, false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<Stream<T>> splitBy(final Predicate<? super T> where) {
        N.requireNonNull(where);

        final Nullable<Indexed<T>> first = indexed().findFirst(new Predicate<Indexed<T>>() {
            @Override
            public boolean test(Indexed<T> indexed) {
                return !where.test(indexed.value());
            }
        });

        return splitAt(first.isPresent() ? (int) first.get().index() : toIndex - fromIndex);
    }

    @Override
    public Stream<Stream<T>> sliding(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().sliding(windowSize, increment).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<List<T>> slidingToList(final int windowSize, final int increment) {
        return new ParallelIteratorStream<>(sequential().slidingToList(windowSize, increment).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> distinctBy(final Function<? super T, ?> keyExtractor) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().distinctBy(keyExtractor).iterator(), sorted, cmp, maxThreadNum, splitor, closeHandlers);
        }

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
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, cmp)) {
            return new ParallelArrayStream<>(elements, toIndex - n, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
        } else {
            final T[] a = N.top(elements, fromIndex, toIndex, n, comparator);
            return new ParallelArrayStream<>(a, 0, a.length, sorted, cmp, maxThreadNum, splitor, closeHandlers);
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
        return new ParallelArrayStream<>(a, 0, a.length, true, comparator, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().peek(action).iterator(), false, null, maxThreadNum, splitor, closeHandlers);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<Iterator<T>> iters = new ArrayList<>(threadNum);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;
                iters.add(new ObjIteratorEx<T>() {
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

            for (int i = 0; i < threadNum; i++) {
                iters.add(new ObjIteratorEx<T>() {

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

        return new ParallelIteratorStream<>(Stream.parallelConcat2(iters, iters.size()), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex, (int) (fromIndex + maxSize), sorted, cmp, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ParallelArrayStream<>(elements, toIndex, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
        } else {
            return new ParallelArrayStream<>(elements, (int) (fromIndex + n), toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
        }
    }

    @Override
    public <E extends Exception> void forEach(final Try.Consumer<? super T, E> action) throws E {
        if (maxThreadNum <= 1) {
            sequential().forEach(action);
            return;
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        complete2(futureList, eHolder, (E) null);
    }

    @Override
    public <R, E extends Exception, E2 extends Exception> R forEach(R seed, Try.BiFunction<R, ? super T, R, E> accumulator,
            Try.BiPredicate<? super R, ? super T, E2> conditionToBreak) throws E, E2 {
        if (logger.isWarnEnabled()) {
            logger.warn("The 'forEach' with break condition is sequentially executed in parallel stream");
        }

        return sequential().forEach(seed, accumulator, conditionToBreak);
    }

    @Override
    public <E extends Exception> void forEachPair(final Try.BiConsumer<? super T, ? super T, E> action, final int increment) throws E {
        if (maxThreadNum <= 1) {
            sequential().forEachPair(action, increment);
            return;
        }

        final int windowSize = 2;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableInt curIndex = MutableInt.of(fromIndex);

        for (int i = 0; i < threadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                private int cursor = -1;

                @Override
                public void run() {
                    try {
                        while (curIndex.intValue() < toIndex && eHolder.value() == null) {
                            synchronized (elements) {
                                if (curIndex.intValue() < toIndex) {
                                    cursor = curIndex.value();
                                    curIndex.setValue(increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex);
                                } else {
                                    break;
                                }
                            }

                            action.accept(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null);
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
    public <E extends Exception> void forEachTriple(final Try.TriConsumer<? super T, ? super T, ? super T, E> action, final int increment) throws E {
        if (maxThreadNum <= 1) {
            sequential().forEachTriple(action, increment);
            return;
        }

        final int windowSize = 3;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableInt curIndex = MutableInt.of(fromIndex);

        for (int i = 0; i < threadNum; i++) {
            futureList.add(asyncExecutor.execute(new Runnable() {
                private int cursor = -1;

                @Override
                public void run() {
                    try {
                        while (curIndex.intValue() < toIndex && eHolder.value() == null) {
                            synchronized (elements) {
                                if (curIndex.intValue() < toIndex) {
                                    cursor = curIndex.value();
                                    curIndex.setValue(increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex);
                                } else {
                                    break;
                                }
                            }

                            action.accept(elements[cursor], cursor < toIndex - 1 ? elements[cursor + 1] : null,
                                    cursor < toIndex - 2 ? elements[cursor + 2] : null);
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
    public List<T> toList() {
        if (fromIndex == 0 && toIndex == elements.length && elements.length > 9) {
            return new ArrayList<>(Arrays.asList(elements));
        }

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
    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(keyExtractor, valueMapper, mapFactory);
        }

        //    final M res = mapFactory.get();
        //    res.putAll(collect(Collectors.toConcurrentMap(keyExtractor, valueMapper, mergeFunction)));
        //    return res;

        return collect(Collectors.toMap(keyExtractor, valueMapper, mapFactory));
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMap(classifier, downstream, mapFactory);
        }

        //    final M res = mapFactory.get();
        //    res.putAll(collect(Collectors.groupingByConcurrent(classifier, downstream)));
        //    return res;

        return collect(Collectors.groupingBy(classifier, downstream, mapFactory));
    }

    @Override
    public <K, U, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Function<? super T, ? extends U> valueMapper,
            final Collector<? super U, A, D> downstream, final Supplier<M> mapFactory) {
        return toMap(classifier, Collectors.mapping(valueMapper, downstream), mapFactory);
    }

    @Override
    public <K, U, V extends Collection<U>, M extends Multimap<K, U, V>> M toMultimap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, Supplier<M> mapFactory) {
        if (maxThreadNum <= 1) {
            return sequential().toMultimap(keyExtractor, valueMapper, mapFactory);
        }

        //    final M res = mapFactory.get();
        //    final ConcurrentMap<K, List<U>> tmp = collect(Collectors.groupingByConcurrent(keyExtractor, Collectors.mapping(valueMapper, Collectors.<U> toList())));
        //
        //    for (Map.Entry<K, List<U>> entry : tmp.entrySet()) {
        //        res.putAll(entry.getKey(), entry.getValue());
        //    }
        //
        //    return res;

        return collect(Collectors.toMultimap(keyExtractor, valueMapper, mapFactory));
    }

    @Override
    public Nullable<T> first() {
        if (fromIndex == toIndex) {
            return Nullable.empty();
        }

        return Nullable.of(elements[fromIndex]);
    }

    @Override
    public Nullable<T> last() {
        if (fromIndex == toIndex) {
            return Nullable.empty();
        }

        return Nullable.of(elements[toIndex - 1]);
    }

    @Override
    public T reduce(final T identity, final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<T>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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
    public Nullable<T> reduce(final BinaryOperator<T> accumulator) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(accumulator);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<T>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        return result == NONE ? (Nullable<T>) Nullable.empty() : Nullable.of(result);
    }

    @Override
    public <U> U reduce(final U identity, final BiFunction<U, ? super T, U> accumulator, final BinaryOperator<U> combiner) {
        if (maxThreadNum <= 1) {
            return sequential().reduce(identity, accumulator, combiner);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<U>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<R>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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
        if (maxThreadNum <= 1 || collector.characteristics().contains(Collector.Characteristics.CONCURRENT) == false
                || collector.characteristics().contains(Collector.Characteristics.UNORDERED) == false) {
            return sequential().collect(collector);
        }

        final Supplier<A> supplier = collector.supplier();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final BinaryOperator<A> combiner = collector.combiner();
        final Function<A, R> finisher = collector.finisher();

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<A>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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
                    container = combiner.apply(container, future.get());
                }
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }

        return finisher.apply(container == NONE ? supplier.get() : container);
    }

    @Override
    public Stream<T> tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex + 1, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> head2() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex, toIndex - 1, sorted, cmp, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> last(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative");

        if (toIndex - fromIndex <= n) {
            return this;
        }

        return new ParallelArrayStream<>(elements, toIndex - n, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> skipLast(int n) {
        N.checkArgument(n >= 0, "'n' can't be negative");

        if (n == 0) {
            return this;
        }

        return new ParallelArrayStream<>(elements, fromIndex, N.max(fromIndex, toIndex - n), sorted, cmp, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Nullable<T> min(Comparator<? super T> comparator) {
        if (fromIndex == toIndex) {
            return Nullable.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return Nullable.of(elements[fromIndex]);
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return collect(Collectors.minBy(comparator));
    }

    @Override
    public Nullable<T> max(Comparator<? super T> comparator) {
        if (fromIndex == toIndex) {
            return Nullable.empty();
        } else if (sorted && isSameComparator(cmp, comparator)) {
            return Nullable.of(elements[toIndex - 1]);
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;

        return collect(Collectors.maxBy(comparator));
    }

    @Override
    public Nullable<T> kthLargest(int k, Comparator<? super T> comparator) {
        return sequential().kthLargest(k, comparator);
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public Stream<T> reversed() {
        return new ParallelIteratorStream<>(sequential().reversed().iterator(), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.Predicate<? super T, E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().anyMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(false);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        complete2(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.Predicate<? super T, E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().allMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        complete2(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.Predicate<? super T, E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().noneMatch(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final MutableBoolean result = MutableBoolean.of(true);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        complete2(futureList, eHolder, (E) null);

        return result.value();
    }

    @Override
    public <E extends Exception> Nullable<T> findFirst(final Try.Predicate<? super T, E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findFirst(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, T>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        complete2(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? (Nullable<T>) Nullable.empty() : Nullable.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> Nullable<T> findLast(final Try.Predicate<? super T, E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findLast(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<Pair<Integer, T>> resultHolder = new Holder<>();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        complete2(futureList, eHolder, (E) null);

        return resultHolder.value() == null ? (Nullable<T>) Nullable.empty() : Nullable.of(resultHolder.value().right);
    }

    @Override
    public <E extends Exception> Nullable<T> findAny(final Try.Predicate<? super T, E> predicate) throws E {
        if (maxThreadNum <= 1) {
            return sequential().findAny(predicate);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final Holder<T> resultHolder = Holder.of((T) NONE);

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
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

            for (int i = 0; i < threadNum; i++) {
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

        complete2(futureList, eHolder, (E) null);

        return resultHolder.value() == NONE ? (Nullable<T>) Nullable.empty() : Nullable.of(resultHolder.value());
    }

    @Override
    public Stream<T> intersection(final Function<? super T, ?> mapper, final Collection<?> c) {
        if (maxThreadNum <= 1) {
            return new ParallelIteratorStream<>(sequential().intersection(mapper, c).iterator(), sorted, cmp, maxThreadNum, splitor, closeHandlers);
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
            return new ParallelIteratorStream<>(sequential().intersection(mapper, c).iterator(), sorted, cmp, maxThreadNum, splitor, closeHandlers);
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
        return new ParallelIteratorStream<>(Stream.concat(this, stream), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> prepend(Stream<T> stream) {
        return new ParallelIteratorStream<>(Stream.concat(stream, this), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> merge(final Stream<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return new ParallelIteratorStream<>(Stream.merge(this, b, nextSelector), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <T2, R> Stream<R> zipWith(Stream<T2> b, BiFunction<? super T, ? super T2, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, zipFunction), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <T2, T3, R> Stream<R> zipWith(Stream<T2> b, Stream<T3> c, TriFunction<? super T, ? super T2, ? super T3, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, c, zipFunction), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <T2, R> Stream<R> zipWith(Stream<T2> b, T valueForNoneA, T2 valueForNoneB, BiFunction<? super T, ? super T2, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, valueForNoneA, valueForNoneB, zipFunction), false, null, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public <T2, T3, R> Stream<R> zipWith(Stream<T2> b, Stream<T3> c, T valueForNoneA, T2 valueForNoneB, T3 valueForNoneC,
            TriFunction<? super T, ? super T2, ? super T3, R> zipFunction) {
        return new ParallelIteratorStream<>(Stream.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction), false, null, maxThreadNum,
                splitor, closeHandlers);
    }

    @Override
    public long persist(final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) {
        N.checkArgument(batchSize > 0 && batchInterval >= 0, "'batchSize'=%s must be greater than 0 and 'batchInterval'=%s can't be negative", batchSize,
                batchInterval);

        if (maxThreadNum <= 1) {
            return sequential().persist(stmt, batchSize, batchInterval, stmtSetter);
        }

        final int threadNum = N.min(maxThreadNum, (toIndex - fromIndex));
        final List<CompletableFuture<Void>> futureList = new ArrayList<>(threadNum);
        final Holder<Throwable> eHolder = new Holder<>();
        final AtomicLong result = new AtomicLong();

        if (splitor == Splitor.ARRAY) {
            final int sliceSize = (toIndex - fromIndex) / threadNum + ((toIndex - fromIndex) % threadNum == 0 ? 0 : 1);

            for (int i = 0; i < threadNum; i++) {
                final int sliceIndex = i;

                futureList.add(asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        int cursor = fromIndex + sliceIndex * sliceSize;
                        final int to = toIndex - cursor > sliceSize ? cursor + sliceSize : toIndex;
                        long cnt = 0;

                        try {
                            while (cursor < to && eHolder.value() == null) {
                                stmtSetter.accept(stmt, elements[cursor++]);
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

            for (int i = 0; i < threadNum; i++) {
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
            tmp = new ArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, closeHandlers);
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

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
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

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
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

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public Stream<T> onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ParallelArrayStream<>(elements, fromIndex, toIndex, sorted, cmp, maxThreadNum, splitor, newCloseHandlers);
    }
}
