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

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.Indexed;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriConsumer;
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
class IteratorStream<T> extends AbstractStream<T> {
    final ExIterator<T> elements;

    NullabLe<T> head;
    Stream<T> tail;

    Stream<T> head2;
    NullabLe<T> tail2;

    IteratorStream(final Iterator<? extends T> values) {
        this(values, null);
    }

    IteratorStream(final Iterator<? extends T> values, final Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false, null);
    }

    IteratorStream(final Iterator<? extends T> values, final Collection<Runnable> closeHandlers, final boolean sorted, final Comparator<? super T> comparator) {
        super(closeHandlers, sorted, comparator);

        N.requireNonNull(values);

        ExIterator<T> tmp = null;

        if (values instanceof ExIterator) {
            tmp = (ExIterator<T>) values;
        } else if (values instanceof SkippableIterator) {
            tmp = new ExIterator<T>() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public T next() {
                    return values.next();
                }

                @Override
                public void skip(long n) {
                    ((SkippableIterator) values).skip(n);
                }

                @Override
                public long count() {
                    return ((SkippableIterator) values).count();
                }
            };
        } else {
            tmp = new ExIterator<T>() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public T next() {
                    return values.next();
                }
            };
        }

        this.elements = tmp;
    }

    IteratorStream(final Stream<T> stream, final Set<Runnable> closeHandlers, final boolean sorted, final Comparator<? super T> comparator) {
        this(stream.iterator(), mergeCloseHandlers(stream, closeHandlers), sorted, comparator);
    }

    @Override
    public Stream<T> filter(final Predicate<? super T> predicate) {
        return new IteratorStream<>(new ExIterator<T>() {
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    while (elements.hasNext()) {
                        next = elements.next();

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
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> takeWhile(final Predicate<? super T> predicate) {
        return new IteratorStream<>(new ExIterator<T>() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private T next = null;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.next();

                    if (predicate.test(next)) {
                        hasNext = true;
                    } else {
                        hasMore = false;
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

        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> dropWhile(final Predicate<? super T> predicate) {
        return new IteratorStream<>(new ExIterator<T>() {
            private boolean hasNext = false;
            private T next = null;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.next();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else if (elements.hasNext()) {
                        next = elements.next();
                        hasNext = true;
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

        }, closeHandlers, sorted, cmp);
    }

    @Override
    public <R> Stream<R> map(final Function<? super T, ? extends R> mapper) {
        return new IteratorStream<>(new ExIterator<R>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public R next() {
                return mapper.apply(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public <R> Stream<R> biMap(final BiFunction<? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
        return new IteratorStream<>(new ExIterator<R>() {
            private T pre = (T) NONE;

            @Override
            public boolean hasNext() {
                if (ignoreNotPaired && pre == NONE) {
                    if (elements.hasNext()) {
                        pre = elements.next();
                    } else {
                        return false;
                    }
                }

                return elements.hasNext();
            }

            @Override
            public R next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }

                if (ignoreNotPaired) {
                    final R res = mapper.apply(pre, elements.next());
                    pre = (T) NONE;
                    return res;
                } else {
                    return mapper.apply(elements.next(), elements.hasNext() ? elements.next() : null);
                }
            }
        }, closeHandlers);
    }

    @Override
    public <R> Stream<R> triMap(final TriFunction<? super T, ? super T, ? super T, ? extends R> mapper, final boolean ignoreNotPaired) {
        return new IteratorStream<>(new ExIterator<R>() {
            private T prepre = (T) NONE;
            private T pre = (T) NONE;

            @Override
            public boolean hasNext() {
                if (ignoreNotPaired && pre == NONE) {
                    if (elements.hasNext()) {
                        prepre = elements.next();

                        if (elements.hasNext()) {
                            pre = elements.next();
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }

                return elements.hasNext();
            }

            @Override
            public R next() {
                if (!hasNext()) {
                    throw new NoSuchElementException();
                }

                if (ignoreNotPaired) {
                    final R res = mapper.apply(prepre, pre, elements.next());
                    prepre = (T) NONE;
                    pre = (T) NONE;
                    return res;
                } else {
                    return mapper.apply(elements.next(), elements.hasNext() ? elements.next() : null, elements.hasNext() ? elements.next() : null);
                }
            }
        }, closeHandlers);
    }

    @Override
    public Stream<T> mapFirst(final Function<? super T, ? extends T> mapperForFirst) {
        N.requireNonNull(mapperForFirst);

        if (elements.hasNext()) {
            T first = elements.next();
            return prepend(Stream.of(first).map(mapperForFirst));
        } else {
            return this;
        }
    }

    @Override
    public <R> Stream<R> mapFirstOrElse(final Function<? super T, ? extends R> mapperForFirst, final Function<? super T, ? extends R> mapperForElse) {
        N.requireNonNull(mapperForFirst);
        N.requireNonNull(mapperForElse);

        if (elements.hasNext()) {
            final Function<T, R> mapperForFirst2 = (Function<T, R>) mapperForFirst;
            final Function<T, R> mapperForElse2 = (Function<T, R>) mapperForElse;
            final T first = elements.next();

            return map(mapperForElse2).prepend(Stream.of(first).map(mapperForFirst2));
        } else {
            return (Stream<R>) this;
        }
    }

    @Override
    public Stream<T> mapLast(final Function<? super T, ? extends T> mapperForLast) {
        N.requireNonNull(mapperForLast);

        return new IteratorStream<>(new ExIterator<T>() {
            private T last = (T) Stream.NONE;

            @Override
            public boolean hasNext() {
                return last != Stream.NONE || elements.hasNext();
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (last == Stream.NONE) {
                    last = elements.next();
                }

                final T next = last;

                if (elements.hasNext()) {
                    last = elements.next();
                    return next;
                } else {
                    last = (T) Stream.NONE;
                    return mapperForLast.apply(next);
                }
            }
        }, closeHandlers);
    }

    @Override
    public <R> Stream<R> mapLastOrElse(final Function<? super T, ? extends R> mapperForLast, final Function<? super T, ? extends R> mapperForElse) {
        N.requireNonNull(mapperForLast);
        N.requireNonNull(mapperForElse);

        return new IteratorStream<>(new ExIterator<R>() {
            private T last = (T) Stream.NONE;

            @Override
            public boolean hasNext() {
                return last != Stream.NONE || elements.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (last == Stream.NONE) {
                    last = elements.next();
                }

                final T next = last;

                if (elements.hasNext()) {
                    last = elements.next();
                    return mapperForElse.apply(next);
                } else {
                    last = (T) Stream.NONE;
                    return mapperForLast.apply(next);
                }
            }
        }, closeHandlers);
    }

    @Override
    public CharStream mapToChar(final ToCharFunction<? super T> mapper) {
        return new IteratorCharStream(new ExCharIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char nextChar() {
                return mapper.applyAsChar(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public ByteStream mapToByte(final ToByteFunction<? super T> mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public byte nextByte() {
                return mapper.applyAsByte(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public ShortStream mapToShort(final ToShortFunction<? super T> mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short nextShort() {
                return mapper.applyAsShort(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return mapper.applyAsLong(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final ToFloatFunction<? super T> mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return mapper.applyAsFloat(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return mapper.applyAsDouble(elements.next());
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers);
    }

    @Override
    <R> Stream<R> flatMap0(final Function<? super T, ? extends Iterator<? extends R>> mapper) {
        return new IteratorStream<>(new ExIterator<R>() {
            private Iterator<? extends R> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
        }, closeHandlers);
    }

    @Override
    CharStream flatMapToChar0(final Function<? super T, CharIterator> mapper) {
        return new IteratorCharStream(new ExCharIterator() {
            private CharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char nextChar() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextChar();
            }
        }, closeHandlers);
    }

    @Override
    ByteStream flatMapToByte0(final Function<? super T, ByteIterator> mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            private ByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public byte nextByte() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextByte();
            }
        }, closeHandlers);
    }

    @Override
    ShortStream flatMapToShort0(final Function<? super T, ShortIterator> mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            private ShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
        }, closeHandlers);
    }

    @Override
    IntStream flatMapToInt0(final Function<? super T, IntIterator> mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
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
        }, closeHandlers);
    }

    @Override
    LongStream flatMapToLong0(final Function<? super T, LongIterator> mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            private LongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public long nextLong() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextLong();
            }
        }, closeHandlers);
    }

    @Override
    FloatStream flatMapToFloat0(final Function<? super T, FloatIterator> mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            private FloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public float nextFloat() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextFloat();
            }
        }, closeHandlers);
    }

    @Override
    DoubleStream flatMapToDouble0(final Function<? super T, DoubleIterator> mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            private DoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next());
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double nextDouble() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextDouble();
            }
        }, closeHandlers);
    }

    @Override
    public <R> Stream<R> slidingMap(final BiFunction<? super T, ? super T, R> mapper, final int increment) {
        final int windowSize = 2;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<>(new ExIterator<R>() {
            private T prev = (T) NONE;

            @Override
            public boolean hasNext() {
                if (increment > windowSize && prev != NONE) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.next();
                    }

                    prev = (T) NONE;
                }

                return elements.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (increment == 1) {
                    return mapper.apply(prev == NONE ? elements.next() : prev, (prev = (elements.hasNext() ? elements.next() : null)));
                } else {
                    return mapper.apply(elements.next(), (prev = (elements.hasNext() ? elements.next() : null)));
                }
            }
        }, closeHandlers);
    }

    @Override
    public <R> Stream<R> slidingMap(final TriFunction<? super T, ? super T, ? super T, R> mapper, final int increment) {
        final int windowSize = 3;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<>(new ExIterator<R>() {
            private T prev = (T) NONE;
            private T prev2 = (T) NONE;

            @Override
            public boolean hasNext() {
                if (increment > windowSize && prev != NONE) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.next();
                    }

                    prev = (T) NONE;
                }

                return elements.hasNext();
            }

            @Override
            public R next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (increment == 1) {
                    return mapper.apply(prev2 == NONE ? elements.next() : prev2,
                            (prev2 = (prev == NONE ? (elements.hasNext() ? elements.next() : null) : prev)),
                            (prev = (elements.hasNext() ? elements.next() : null)));

                } else if (increment == 2) {
                    return mapper.apply(prev == NONE ? elements.next() : prev, (prev2 = (elements.hasNext() ? elements.next() : null)),
                            (prev = (elements.hasNext() ? elements.next() : null)));
                } else {
                    return mapper.apply(elements.next(), (prev2 = (elements.hasNext() ? elements.next() : null)),
                            (prev = (elements.hasNext() ? elements.next() : null)));
                }
            }
        }, closeHandlers);
    }

    @Override
    public Stream<List<T>> splitToList(final int size) {
        return new IteratorStream<>(new ExIterator<List<T>>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public List<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>(size);
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    result.add(elements.next());
                    cnt++;
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<Set<T>> splitToSet(final int size) {
        return new IteratorStream<>(new ExIterator<Set<T>>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Set<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final Set<T> result = new HashSet<>(N.min(9, size));
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    result.add(elements.next());
                    cnt++;
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<List<T>> splitToList(final Predicate<? super T> predicate) {
        return new IteratorStream<>(new ExIterator<List<T>>() {
            private T next = (T) NONE;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return next != NONE || elements.hasNext();
            }

            @Override
            public List<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>();

                if (next == NONE) {
                    next = elements.next();
                }

                while (next != NONE) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else if (predicate.test(next) == preCondition) {
                        result.add(next);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else {

                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<List<T>> splitToList(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<>(new ExIterator<List<T>>() {
            private T next = (T) NONE;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return next != NONE || elements.hasNext();
            }

            @Override
            public List<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>();

                if (next == NONE) {
                    next = elements.next();
                }

                while (next != NONE) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.apply(next, identity);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else if (predicate.apply(next, identity) == preCondition) {
                        result.add(next);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<Set<T>> splitToSet(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<>(new ExIterator<Set<T>>() {
            private T next = (T) NONE;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return next != NONE || elements.hasNext();
            }

            @Override
            public Set<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final Set<T> result = new HashSet<>();

                if (next == NONE) {
                    next = elements.next();
                }

                while (next != NONE) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.apply(next, identity);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else if (predicate.apply(next, identity) == preCondition) {
                        result.add(next);
                        next = elements.hasNext() ? elements.next() : (T) NONE;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<List<T>> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<>(new ExIterator<List<T>>() {
            private List<T> prev = null;

            @Override
            public boolean hasNext() {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.next();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public List<T> next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final List<T> result = new ArrayList<>(windowSize);
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;

                    if (cnt <= 8) {
                        for (int i = windowSize - cnt; i < windowSize; i++) {
                            result.add(prev.get(i));
                        }
                    } else {
                        result.addAll(prev.subList(windowSize - cnt, windowSize));
                    }
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.next());
                }

                return prev = result;
            }
        }, closeHandlers);
    }

    @Override
    public Stream<T> top(int n) {
        return top(n, OBJECT_COMPARATOR);
    }

    @Override
    public Stream<T> top(final int n, final Comparator<? super T> comparator) {
        N.checkArgument(n > 0, "'n' must be bigger than 0");

        return new IteratorStream<>(new ExIterator<T>() {
            T[] a = null;
            int cursor = 0;
            int toIndex;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    top();
                }

                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (a == null) {
                    top();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    top();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    top();
                }

                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                if (a == null) {
                    top();
                }

                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }

            private void top() {
                if (sorted && isSameComparator(comparator, cmp)) {
                    final LinkedList<T> queue = new LinkedList<>();

                    while (elements.hasNext()) {
                        if (queue.size() >= n) {
                            queue.poll();
                        }

                        queue.offer(elements.next());
                    }

                    a = (T[]) queue.toArray();
                } else {
                    final Comparator<Indexed<T>> pairCmp = new Comparator<Indexed<T>>() {
                        @Override
                        public int compare(final Indexed<T> o1, final Indexed<T> o2) {
                            return comparator.compare(o1.value(), o2.value());
                        }
                    };

                    final Queue<Indexed<T>> heap = new PriorityQueue<>(n, pairCmp);

                    Indexed<T> pair = null;
                    for (long i = 0; elements.hasNext(); i++) {
                        pair = Indexed.of(elements.next(), i);

                        if (heap.size() >= n) {
                            if (pairCmp.compare(pair, heap.peek()) > 0) {
                                heap.poll();
                                heap.offer(pair);
                            }
                        } else {
                            heap.offer(pair);
                        }
                    }

                    final Indexed<T>[] arrayOfPair = heap.toArray(new Indexed[heap.size()]);

                    N.sort(arrayOfPair, new Comparator<Indexed<T>>() {
                        @Override
                        public int compare(final Indexed<T> o1, final Indexed<T> o2) {
                            return N.compare(o1.longIndex(), o2.longIndex());
                        }
                    });

                    a = (T[]) new Object[arrayOfPair.length];

                    for (int i = 0, len = arrayOfPair.length; i < len; i++) {
                        a[i] = arrayOfPair[i].value();
                    }
                }

                toIndex = a.length;
            }
        }, closeHandlers, sorted, cmp);
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

        return new IteratorStream<>(new ExIterator<T>() {
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
                    N.sort(a);
                } else {
                    N.sort(a, comparator);
                }
            }
        }, closeHandlers, true, comparator);
    }

    @Override
    public Stream<T> peek(final Consumer<? super T> action) {
        return new IteratorStream<>(new ExIterator<T>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public T next() {
                final T next = elements.next();
                action.accept(next);
                return next;
            }
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new IteratorStream<>(new ExIterator<T>() {
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
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorStream<>(new ExIterator<T>() {
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
        }, closeHandlers, sorted, cmp);
    }

    @Override
    public void forEach(Consumer<? super T> action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    @Override
    public <R> R forEach(R seed, BiFunction<R, ? super T, R> accumulator, BiPredicate<? super R, ? super T> conditionToBreak) {
        R result = seed;
        T next = null;

        while (elements.hasNext()) {
            next = elements.next();
            result = accumulator.apply(result, next);

            if (conditionToBreak.test(result, next)) {
                break;
            }
        }

        return result;
    }

    @Override
    public void forEachPair(final BiConsumer<? super T, ? super T> action, final int increment) {
        final int windowSize = 2;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        T prev = (T) NONE;

        while (elements.hasNext()) {
            if (increment > windowSize && prev != NONE) {
                int skipNum = increment - windowSize;

                while (skipNum-- > 0 && elements.hasNext()) {
                    elements.next();
                }

                if (elements.hasNext() == false) {
                    break;
                }

                prev = (T) NONE;
            }

            if (increment == 1) {
                action.accept(prev == NONE ? elements.next() : prev, (prev = (elements.hasNext() ? elements.next() : null)));
            } else {
                action.accept(elements.next(), (prev = (elements.hasNext() ? elements.next() : null)));
            }
        }
    }

    @Override
    public void forEachTriple(final TriConsumer<? super T, ? super T, ? super T> action, final int increment) {
        final int windowSize = 3;

        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        T prev = (T) NONE;
        T prev2 = (T) NONE;

        while (elements.hasNext()) {
            if (increment > windowSize && prev != NONE) {
                int skipNum = increment - windowSize;

                while (skipNum-- > 0 && elements.hasNext()) {
                    elements.next();
                }

                if (elements.hasNext() == false) {
                    break;
                }

                prev = (T) NONE;
            }

            if (increment == 1) {
                action.accept(prev2 == NONE ? elements.next() : prev2, (prev2 = (prev == NONE ? (elements.hasNext() ? elements.next() : null) : prev)),
                        (prev = (elements.hasNext() ? elements.next() : null)));

            } else if (increment == 2) {
                action.accept(prev == NONE ? elements.next() : prev, (prev2 = (elements.hasNext() ? elements.next() : null)),
                        (prev = (elements.hasNext() ? elements.next() : null)));
            } else {
                action.accept(elements.next(), (prev2 = (elements.hasNext() ? elements.next() : null)), (prev = (elements.hasNext() ? elements.next() : null)));
            }
        }
    }

    @Override
    public Object[] toArray() {
        return toArray(N.EMPTY_OBJECT_ARRAY);
    }

    <A> A[] toArray(A[] a) {
        return elements.toArray(a);
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
        final M result = mapFactory.get();
        T element = null;

        while (elements.hasNext()) {
            element = elements.next();
            Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        T element = null;

        while (elements.hasNext()) {
            element = elements.next();
            key = N.requireNonNull(classifier.apply(element), "element cannot be mapped to a null key");

            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, element);
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
    public <K, U, V extends Collection<U>, M extends Multimap<K, U, V>> M toMultimap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        T element = null;

        while (elements.hasNext()) {
            element = elements.next();
            result.put(keyExtractor.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public T reduce(T identity, BinaryOperator<T> accumulator) {
        T result = identity;

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return result;
    }

    @Override
    public NullabLe<T> reduce(BinaryOperator<T> accumulator) {
        if (elements.hasNext() == false) {
            NullabLe.empty();
        }

        T result = elements.next();

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return NullabLe.of(result);
    }

    @Override
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator, BinaryOperator<U> combiner) {
        U result = identity;

        while (elements.hasNext()) {
            result = accumulator.apply(result, elements.next());
        }

        return result;
    }

    @Override
    public <R> R collect(Supplier<R> supplier, BiConsumer<R, ? super T> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public <R, A> R collect(Collector<? super T, A, R> collector) {
        final A container = collector.supplier().get();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();

        while (elements.hasNext()) {
            accumulator.accept(container, elements.next());
        }

        return collector.finisher().apply(container);
    }

    @Override
    public NullabLe<T> head() {
        if (head == null) {
            head = elements.hasNext() ? NullabLe.of(elements.next()) : NullabLe.<T> empty();
            tail = new IteratorStream<>(elements, closeHandlers, sorted, cmp);
        }

        return head;
    }

    @Override
    public Stream<T> tail() {
        if (tail == null) {
            head = elements.hasNext() ? NullabLe.of(elements.next()) : NullabLe.<T> empty();
            tail = new IteratorStream<>(elements, closeHandlers, sorted, cmp);
        }

        return tail;
    }

    @Override
    public Stream<T> head2() {
        if (head2 == null) {
            final Object[] a = this.toArray();
            head2 = new ArrayStream<>((T[]) a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, cmp);
            tail2 = a.length == 0 ? NullabLe.<T> empty() : NullabLe.of((T) a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public NullabLe<T> tail2() {
        if (tail2 == null) {
            final Object[] a = this.toArray();
            head2 = new ArrayStream<>((T[]) a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted, cmp);
            tail2 = a.length == 0 ? NullabLe.<T> empty() : NullabLe.of((T) a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public Stream<T> last(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative");

        if (n == 0) {
            return new IteratorStream<>(ExIterator.EMPTY, closeHandlers, sorted, cmp);
        }

        final Deque<T> dqueue = n <= 1024 ? new ArrayDeque<T>(n) : new LinkedList<T>();

        while (elements.hasNext()) {
            if (dqueue.size() >= n) {
                dqueue.pollFirst();
            }

            dqueue.offerLast(elements.next());
        }

        return new IteratorStream<>(dqueue.iterator(), closeHandlers, sorted, cmp);
    }

    @Override
    public Stream<T> skipLast(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative");

        if (n == 0) {
            return this;
        }

        return new IteratorStream<>(new ExIterator<T>() {
            private Deque<T> dqueue = null;

            @Override
            public boolean hasNext() {
                if (dqueue == null) {
                    dqueue = n <= 1024 ? new ArrayDeque<T>(n) : new LinkedList<T>();

                    while (dqueue.size() < n && elements.hasNext()) {
                        dqueue.offerLast(elements.next());
                    }
                }

                return elements.hasNext();
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                dqueue.offerLast(elements.next());

                return dqueue.pollFirst();
            }

        }, closeHandlers, sorted, cmp);
    }

    @Override
    public NullabLe<T> min(Comparator<? super T> comparator) {
        if (elements.hasNext() == false) {
            return NullabLe.empty();
        } else if (sorted && isSameComparator(comparator, cmp)) {
            return NullabLe.of(elements.next());
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;
        T candidate = elements.next();
        T next = null;

        while (elements.hasNext()) {
            next = elements.next();
            if (comparator.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return NullabLe.of(candidate);
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
        T candidate = elements.next();
        T next = null;

        while (elements.hasNext()) {
            next = elements.next();
            if (comparator.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return NullabLe.of(candidate);
    }

    @Override
    public NullabLe<T> kthLargest(int k, Comparator<? super T> comparator) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return NullabLe.empty();
        } else if (sorted && isSameComparator(comparator, cmp)) {
            final LinkedList<T> queue = new LinkedList<>();

            while (elements.hasNext()) {
                if (queue.size() >= k) {
                    queue.poll();
                }

                queue.offer(elements.next());
            }

            return queue.size() < k ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(queue.peek());
        }

        comparator = comparator == null ? OBJECT_COMPARATOR : comparator;
        final Queue<T> queue = new PriorityQueue<>(k, comparator);
        T e = null;

        while (elements.hasNext()) {
            e = elements.next();

            if (queue.size() < k) {
                queue.offer(e);
            } else {
                if (comparator.compare(e, queue.peek()) > 0) {
                    queue.poll();
                    queue.offer(e);
                }
            }
        }

        return queue.size() < k ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(queue.peek());
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public boolean anyMatch(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public NullabLe<T> findFirst(Predicate<? super T> predicate) {
        while (elements.hasNext()) {
            T e = elements.next();

            if (predicate.test(e)) {
                return NullabLe.of(e);
            }
        }

        return (NullabLe<T>) NullabLe.empty();
    }

    @Override
    public NullabLe<T> findLast(Predicate<? super T> predicate) {
        if (elements.hasNext() == false) {
            return (NullabLe<T>) NullabLe.empty();
        }

        boolean hasResult = false;
        T e = null;
        T result = null;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? NullabLe.of(result) : (NullabLe<T>) NullabLe.empty();
    }

    /**
     * Returns a Stream with elements from a temporary queue which is filled by reading the elements from the specified iterator asynchronously.
     * 
     * @param stream
     * @param queueSize Default value is 8
     * @return
     */
    @Override
    public Stream<T> queued(int queueSize) {
        final Iterator<T> iter = iterator();

        if (iter instanceof QueuedIterator && ((QueuedIterator<? extends T>) iter).max() >= queueSize) {
            return this;
        } else {
            return new IteratorStream<>(Stream.parallelConcat(Arrays.asList(iter), queueSize, asyncExecutor), closeHandlers, sorted, cmp);
        }
    }

    @Override
    ExIterator<T> exIterator() {
        return elements;
    }

    @Override
    public Stream<T> parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
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

        return new IteratorStream<>(elements, newCloseHandlers, sorted, cmp);
    }
}
