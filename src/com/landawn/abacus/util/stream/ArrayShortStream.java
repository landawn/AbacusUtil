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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.ObjShortConsumer;
import com.landawn.abacus.util.function.ShortBinaryOperator;
import com.landawn.abacus.util.function.ShortConsumer;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ShortToIntFunction;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class ArrayShortStream extends AbstractShortStream {
    final short[] elements;
    final int fromIndex;
    final int toIndex;

    ArrayShortStream(short[] values) {
        this(values, 0, values.length);
    }

    ArrayShortStream(short[] values, final Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ArrayShortStream(short[] values, final boolean sorted, final Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, sorted, closeHandlers);
    }

    ArrayShortStream(short[] values, final int fromIndex, final int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayShortStream(short[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, false, closeHandlers);
    }

    ArrayShortStream(short[] values, final int fromIndex, final int toIndex, final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);

        checkFromToIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate) {
        return new IteratorShortStream(new ShortIteratorEx() {
            private boolean hasNext = false;
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex) {
                    do {
                        if (predicate.test(elements[cursor])) {
                            hasNext = true;
                            break;
                        }
                    } while (++cursor < toIndex);
                }

                return hasNext;
            }

            @Override
            public short nextShort() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, sorted, closeHandlers);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate) {
        return new IteratorShortStream(new ShortIteratorEx() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && cursor < toIndex) {
                    if (predicate.test(elements[cursor])) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public short nextShort() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, sorted, closeHandlers);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate) {
        return new IteratorShortStream(new ShortIteratorEx() {
            private boolean hasNext = false;
            private int cursor = fromIndex;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false && cursor < toIndex) {
                    if (dropped == false) {
                        do {
                            if (predicate.test(elements[cursor]) == false) {
                                hasNext = true;
                                break;
                            }
                        } while (++cursor < toIndex);

                        dropped = true;
                    } else {
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public short nextShort() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, sorted, closeHandlers);
    }

    @Override
    public ShortStream map(final ShortUnaryOperator mapper) {
        return new IteratorShortStream(new ShortIteratorEx() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short nextShort() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsShort(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public short[] toArray() {
                final short[] a = new short[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsShort(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final ShortToIntFunction mapper) {
        return new IteratorIntStream(new IntIteratorEx() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int nextInt() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsInt(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public int[] toArray() {
                final int[] a = new int[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsInt(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final ShortFunction<? extends U> mapper) {
        return new IteratorStream<>(new ObjIteratorEx<U>() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public U next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.apply(elements[cursor++]);
            }

            //            @Override
            //            public long count() {
            //                return toIndex - cursor;
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            //            }

            @Override
            public <A> A[] toArray(A[] a) {
                a = a.length >= toIndex - cursor ? a : (A[]) N.newArray(a.getClass().getComponentType(), toIndex - cursor);

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = (A) mapper.apply(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream flatMap(final ShortFunction<? extends ShortStream> mapper) {
        final ShortIteratorEx iter = new ShortIteratorEx() {
            private int cursor = fromIndex;
            private ShortIterator cur = null;
            private ShortStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
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
            public short nextShort() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorShortStream(iter, newCloseHandlers);
    }

    @Override
    public IntStream flatMapToInt(final ShortFunction<? extends IntStream> mapper) {
        final IntIteratorEx iter = new IntIteratorEx() {
            private int cursor = fromIndex;
            private IntIterator cur = null;
            private IntStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
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
            public int nextInt() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorIntStream(iter, newCloseHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final ShortFunction<? extends Stream<T>> mapper) {
        final ObjIteratorEx<T> iter = new ObjIteratorEx<T>() {
            private int cursor = fromIndex;
            private Iterator<T> cur = null;
            private Stream<T> s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
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
            public T next() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorStream<>(iter, newCloseHandlers);
    }

    @Override
    public Stream<ShortStream> split(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0. Can't be: %s", size);

        return new IteratorStream<>(new ObjIteratorEx<ShortStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayShortStream(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex), sorted, null);
            }

            @Override
            public long count() {
                final long len = toIndex - cursor;
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) {
                final long len = toIndex - cursor;
                cursor = n <= len / size ? cursor + (int) n * size : toIndex;
            }
        }, closeHandlers);
    }

    @Override
    public Stream<ShortList> splitToList(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0. Can't be: %s", size);

        return new IteratorStream<>(new ObjIteratorEx<ShortList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ShortList(N.copyOfRange(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex)));
            }

            @Override
            public long count() {
                final long len = toIndex - cursor;
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) {
                final long len = toIndex - cursor;
                cursor = n <= len / size ? cursor + (int) n * size : toIndex;
            }
        }, closeHandlers);
    }

    @Override
    public Stream<ShortStream> split(final ShortPredicate predicate) {
        return new IteratorStream<>(new ObjIteratorEx<ShortStream>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from]);
                        cursor++;
                    } else if (predicate.test(elements[cursor]) == preCondition) {
                        cursor++;
                    } else {
                        break;
                    }
                }

                return new ArrayShortStream(elements, from, cursor, sorted, null);
            }
        }, closeHandlers);
    }

    @Override
    public Stream<ShortList> splitToList(final ShortPredicate predicate) {
        return new IteratorStream<>(new ObjIteratorEx<ShortList>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from]);
                        cursor++;
                    } else if (predicate.test(elements[cursor]) == preCondition) {
                        cursor++;
                    } else {

                        break;
                    }
                }

                return new ShortList(N.copyOfRange(elements, from, cursor));
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<ShortStream> split(final U seed, final BiPredicate<? super Short, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new IteratorStream<>(new ObjIteratorEx<ShortStream>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from], seed);
                        cursor++;
                    } else if (predicate.test(elements[cursor], seed) == preCondition) {
                        cursor++;
                    } else {
                        if (seedUpdate != null) {
                            seedUpdate.accept(seed);
                        }

                        break;
                    }
                }

                return new ArrayShortStream(elements, from, cursor, sorted, null);
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<ShortList> splitToList(final U seed, final BiPredicate<? super Short, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return new IteratorStream<>(new ObjIteratorEx<ShortList>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.test(elements[from], seed);
                        cursor++;
                    } else if (predicate.test(elements[cursor], seed) == preCondition) {
                        cursor++;
                    } else {
                        if (seedUpdate != null) {
                            seedUpdate.accept(seed);
                        }

                        break;
                    }
                }

                return new ShortList(N.copyOfRange(elements, from, cursor));
            }
        }, closeHandlers);
    }

    @Override
    public Stream<ShortStream> splitAt(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        final ShortStream[] a = new ShortStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? ShortStream.empty() : new ArrayShortStream(elements, fromIndex, middleIndex, sorted, null);
        a[1] = middleIndex == toIndex ? ShortStream.empty() : new ArrayShortStream(elements, middleIndex, toIndex, sorted, null);

        return new ArrayStream<>(a, closeHandlers);
    }

    @Override
    public Stream<ShortStream> splitBy(ShortPredicate where) {
        N.requireNonNull(where);

        int n = 0;

        for (int i = fromIndex; i < toIndex; i++) {
            if (where.test(elements[i])) {
                n++;
            } else {
                break;
            }
        }

        return splitAt(n);
    }

    @Override
    public Stream<ShortStream> sliding(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<>(new ObjIteratorEx<ShortStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ArrayShortStream result = new ArrayShortStream(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex, sorted,
                        null);

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

            @Override
            public long count() {
                if (toIndex - cursor == 0) {
                    return 0;
                } else if (toIndex - cursor <= windowSize) {
                    return 1;
                } else {
                    final long len = (toIndex - cursor) - windowSize;
                    return 1 + (len % increment == 0 ? len / increment : len / increment + 1);
                }
            }

            @Override
            public void skip(long n) {
                if (n > 0) {
                    if (n >= count()) {
                        cursor = toIndex;
                    } else {
                        cursor += n * increment;
                    }
                }
            }
        }, closeHandlers);
    }

    @Override
    public Stream<ShortList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<>(new ObjIteratorEx<ShortList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ShortList result = ShortList.of(N.copyOfRange(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex));

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

            @Override
            public long count() {
                if (toIndex - cursor == 0) {
                    return 0;
                } else if (toIndex - cursor <= windowSize) {
                    return 1;
                } else {
                    final long len = (toIndex - cursor) - windowSize;
                    return 1 + (len % increment == 0 ? len / increment : len / increment + 1);
                }
            }

            @Override
            public void skip(long n) {
                if (n > 0) {
                    if (n >= count()) {
                        cursor = toIndex;
                    } else {
                        cursor += n * increment;
                    }
                }
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream top(int n) {
        return top(n, SHORT_COMPARATOR);
    }

    @Override
    public ShortStream top(int n, Comparator<? super Short> comparator) {
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, SHORT_COMPARATOR)) {
            return new ArrayShortStream(elements, toIndex - n, toIndex, sorted, closeHandlers);
        } else {
            return new ArrayShortStream(N.top(elements, fromIndex, toIndex, n, comparator), sorted, closeHandlers);
        }
    }

    @Override
    public ShortStream sorted() {
        if (sorted) {
            return this;
        }

        final short[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayShortStream(a, true, closeHandlers);
    }

    @Override
    public ShortStream peek(final ShortConsumer action) {
        return new IteratorShortStream(new ShortIteratorEx() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short nextShort() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                action.accept(elements[cursor]);

                return elements[cursor++];
            }

            @Override
            public short[] toArray() {
                final short[] a = new short[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    action.accept(elements[cursor]);

                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, sorted, closeHandlers);
    }

    @Override
    public ShortStream limit(long maxSize) {
        N.checkArgument(maxSize >= 0, "'maxSizse' can't be negative: %s", maxSize);

        if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayShortStream(elements, fromIndex, (int) (fromIndex + maxSize), sorted, closeHandlers);
    }

    @Override
    public ShortStream skip(long n) {
        N.checkArgument(n >= 0, "'n' can't be negative: %s", n);

        if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayShortStream(elements, toIndex, toIndex, sorted, closeHandlers);
        } else {
            return new ArrayShortStream(elements, (int) (fromIndex + n), toIndex, sorted, closeHandlers);
        }
    }

    @Override
    public <E extends Exception> void forEach(final Try.ShortConsumer<E> action) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public short[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public ShortList toShortList() {
        return ShortList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Short> toList() {
        final List<Short> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends List<Short>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Short> toSet() {
        final Set<Short> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends Set<Short>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset() {
        final Multiset<Short> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset(Supplier<? extends Multiset<Short>> supplier) {
        final Multiset<Short> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset() {
        final LongMultiset<Short> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset(Supplier<? extends LongMultiset<Short>> supplier) {
        final LongMultiset<Short> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ShortFunction<? extends K> keyExtractor, ShortFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyExtractor.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final ShortFunction<? extends K> classifier, final Collector<Short, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Short> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = N.requireNonNull(classifier.apply(elements[i]), "element cannot be mapped to a null key");

            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, elements[i]);
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        Collectors.replaceAll(intermediate, function);

        return result;
    }

    @Override
    public OptionalShort first() {
        return fromIndex < toIndex ? OptionalShort.of(elements[fromIndex]) : OptionalShort.empty();
    }

    @Override
    public OptionalShort last() {
        return fromIndex < toIndex ? OptionalShort.of(elements[toIndex - 1]) : OptionalShort.empty();
    }

    @Override
    public short reduce(short identity, ShortBinaryOperator op) {
        short result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsShort(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalShort reduce(ShortBinaryOperator op) {
        if (fromIndex == toIndex) {
            return OptionalShort.empty();
        }

        short result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsShort(result, elements[i]);
        }

        return OptionalShort.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalShort head() {
        return fromIndex == toIndex ? OptionalShort.empty() : OptionalShort.of(elements[fromIndex]);
    }

    @Override
    public ShortStream tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ArrayShortStream(elements, fromIndex + 1, toIndex, sorted, closeHandlers);
    }

    @Override
    public ShortStream headd() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ArrayShortStream(elements, fromIndex, toIndex - 1, sorted, closeHandlers);
    }

    @Override
    public OptionalShort taill() {
        return fromIndex == toIndex ? OptionalShort.empty() : OptionalShort.of(elements[toIndex - 1]);
    }

    @Override
    public OptionalShort min() {
        if (fromIndex == toIndex) {
            return OptionalShort.empty();
        } else if (sorted) {
            return OptionalShort.of(elements[fromIndex]);
        }

        return OptionalShort.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalShort max() {
        if (fromIndex == toIndex) {
            return OptionalShort.empty();
        } else if (sorted) {
            return OptionalShort.of(elements[toIndex - 1]);
        }

        return OptionalShort.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalShort kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (k > toIndex - fromIndex) {
            return OptionalShort.empty();
        } else if (sorted) {
            return OptionalShort.of(elements[toIndex - k]);
        }

        return OptionalShort.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public long sum() {
        return sum(elements, fromIndex, toIndex);
    }

    @Override
    public OptionalDouble average() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        return OptionalDouble.of(N.average(elements, fromIndex, toIndex));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public ShortStream reversed() {
        return new IteratorShortStream(new ShortIteratorEx() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public short nextShort() {
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
                cursor = n < cursor - fromIndex ? cursor - (int) n : fromIndex;
            }

            @Override
            public short[] toArray() {
                final short[] a = new short[cursor - fromIndex];

                for (int i = 0, len = a.length; i < len; i++) {
                    a[i] = elements[cursor - i - 1];
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public ShortSummaryStatistics summarize() {
        final ShortSummaryStatistics result = new ShortSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            result.accept(elements[i]);
        }

        return result;
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.ShortPredicate<E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.ShortPredicate<E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.ShortPredicate<E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> OptionalShort findFirst(final Try.ShortPredicate<E> predicate) throws E {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalShort.of(elements[i]);
            }
        }

        return OptionalShort.empty();
    }

    @Override
    public <E extends Exception> OptionalShort findLast(final Try.ShortPredicate<E> predicate) throws E {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalShort.of(elements[i]);
            }
        }

        return OptionalShort.empty();
    }

    @Override
    public IntStream asIntStream() {
        return new IteratorIntStream(new IntIteratorEx() {
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
        }, sorted, closeHandlers);
    }

    @Override
    public Stream<Short> boxed() {
        return new IteratorStream<>(iterator(), sorted, sorted ? SHORT_COMPARATOR : null, closeHandlers);
    }

    @Override
    public ShortStream cached() {
        return this;
    }

    @Override
    ShortIteratorEx iteratorEx() {
        return ShortIteratorEx.of(elements, fromIndex, toIndex);
    }

    @Override
    public ShortStream parallel(int maxThreadNum, Splitor splitor) {
        return new ParallelArrayShortStream(elements, fromIndex, toIndex, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public ShortStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayShortStream(elements, fromIndex, toIndex, sorted, newCloseHandlers);
    }
}
