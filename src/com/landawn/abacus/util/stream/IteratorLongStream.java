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

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.PrimitiveIterator;
import java.util.Set;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.StreamSupport;

import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nullable;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.LongBinaryOperator;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.LongToDoubleFunction;
import com.landawn.abacus.util.function.LongToFloatFunction;
import com.landawn.abacus.util.function.LongToIntFunction;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToLongFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class IteratorLongStream extends AbstractLongStream {
    final LongIteratorEx elements;

    OptionalLong head;
    LongStream tail;

    LongStream head2;
    OptionalLong tail2;

    IteratorLongStream(final LongIterator values) {
        this(values, null);
    }

    IteratorLongStream(final LongIterator values, final Collection<Runnable> closeHandlers) {
        this(values, false, closeHandlers);
    }

    IteratorLongStream(final LongIterator values, final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);

        LongIteratorEx tmp = null;

        if (values instanceof LongIteratorEx) {
            tmp = (LongIteratorEx) values;
        } else {
            tmp = new LongIteratorEx() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public long nextLong() {
                    return values.nextLong();
                }
            };
        }

        this.elements = tmp;
    }

    @Override
    public LongStream filter(final LongPredicate predicate) {
        return newStream(new LongIteratorEx() {
            private boolean hasNext = false;
            private long next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    while (elements.hasNext()) {
                        next = elements.nextLong();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public long nextLong() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }
        }, sorted);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate) {
        return newStream(new LongIteratorEx() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private long next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.nextLong();

                    if (predicate.test(next)) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public long nextLong() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate) {
        return newStream(new LongIteratorEx() {
            private boolean hasNext = false;
            private long next = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.nextLong();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else if (elements.hasNext()) {
                        next = elements.nextLong();
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public long nextLong() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public LongStream map(final LongUnaryOperator mapper) {
        return newStream(new LongIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return mapper.applyAsLong(elements.nextLong());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false);
    }

    @Override
    public IntStream mapToInt(final LongToIntFunction mapper) {
        return newStream(new IntIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextLong());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false);
    }

    @Override
    public FloatStream mapToFloat(final LongToFloatFunction mapper) {
        return newStream(new FloatIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return mapper.applyAsFloat(elements.nextLong());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false);
    }

    @Override
    public DoubleStream mapToDouble(final LongToDoubleFunction mapper) {
        return newStream(new DoubleIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return mapper.applyAsDouble(elements.nextLong());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false);
    }

    @Override
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
        return newStream(new ObjIteratorEx<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextLong());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false, null);
    }

    @Override
    public LongStream flatMap(final LongFunction<? extends LongStream> mapper) {
        final LongIteratorEx iter = new LongIteratorEx() {
            private LongIterator cur = null;
            private LongStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextLong());

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
            public long nextLong() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorLongStream(iter, newCloseHandlers);
    }

    @Override
    public IntStream flatMapToInt(final LongFunction<? extends IntStream> mapper) {
        final IntIteratorEx iter = new IntIteratorEx() {
            private IntIterator cur = null;
            private IntStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextLong());

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
    public FloatStream flatMapToFloat(final LongFunction<? extends FloatStream> mapper) {
        final FloatIteratorEx iter = new FloatIteratorEx() {
            private FloatIterator cur = null;
            private FloatStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextLong());

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
            public float nextFloat() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorFloatStream(iter, newCloseHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(final LongFunction<? extends DoubleStream> mapper) {
        final DoubleIteratorEx iter = new DoubleIteratorEx() {
            private DoubleIterator cur = null;
            private DoubleStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextLong());

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
            public double nextDouble() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorDoubleStream(iter, newCloseHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final LongFunction<? extends Stream<T>> mapper) {
        final ObjIteratorEx<T> iter = new ObjIteratorEx<T>() {
            private Iterator<? extends T> cur = null;
            private Stream<? extends T> s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextLong());

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
    public Stream<LongList> splitToList(final int size) {
        N.checkArgPositive(size, "size");

        return newStream(new ObjIteratorEx<LongList>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public LongList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final LongList result = new LongList(size);

                while (result.size() < size && elements.hasNext()) {
                    result.add(elements.nextLong());
                }

                return result;
            }

            @Override
            public long count() {
                final long len = elements.count();
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) {
                elements.skip(n >= Long.MAX_VALUE / size ? Long.MAX_VALUE : n * size);
            }
        }, false, null);
    }

    @Override
    public Stream<LongList> splitToList(final LongPredicate predicate) {
        return newStream(new ObjIteratorEx<LongList>() {
            private long next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public LongList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final LongList result = new LongList();

                if (hasNext == false) {
                    next = elements.nextLong();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextLong() : 0;
                    } else if (predicate.test(next) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextLong() : 0;
                    } else {
                        break;
                    }
                }

                return result;
            }

        }, false, null);
    }

    @Override
    public <U> Stream<LongList> splitToList(final U seed, final BiPredicate<? super Long, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return newStream(new ObjIteratorEx<LongList>() {
            private long next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public LongList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final LongList result = new LongList();

                if (hasNext == false) {
                    next = elements.nextLong();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next, seed);
                        next = (hasNext = elements.hasNext()) ? elements.nextLong() : 0;
                    } else if (predicate.test(next, seed) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextLong() : 0;
                    } else {
                        if (seedUpdate != null) {
                            seedUpdate.accept(seed);
                        }

                        break;
                    }
                }

                return result;
            }

        }, false, null);
    }

    @Override
    public Stream<LongList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return newStream(new ObjIteratorEx<LongList>() {
            private LongList prev = null;

            @Override
            public boolean hasNext() {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.nextLong();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public LongList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                LongList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;

                    if (cnt <= 8) {
                        result = new LongList(windowSize);

                        for (int i = windowSize - cnt; i < windowSize; i++) {
                            result.add(prev.get(i));
                        }
                    } else {
                        final long[] dest = new long[windowSize];
                        N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                        result = LongList.of(dest, cnt);
                    }
                } else {
                    result = new LongList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.nextLong());
                }

                return prev = result;
            }
        }, false, null);
    }

    @Override
    public LongStream top(int n) {
        return top(n, LONG_COMPARATOR);
    }

    @Override
    public LongStream top(int n, Comparator<? super Long> comparator) {
        return boxed().top(n, comparator).mapToLong(ToLongFunction.UNBOX);
    }

    @Override
    public LongStream peek(final LongConsumer action) {
        return newStream(new LongIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                final long next = elements.nextLong();
                action.accept(next);
                return next;
            }
        }, sorted);
    }

    @Override
    public LongStream limit(final long maxSize) {
        N.checkArgNotNegative(maxSize, "maxSize");

        return newStream(new LongIteratorEx() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public long nextLong() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextLong();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted);
    }

    @Override
    public LongStream skip(final long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        return newStream(new LongIteratorEx() {
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
            public long nextLong() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextLong();
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
        }, sorted);
    }

    @Override
    public <E extends Exception> void forEach(final Try.LongConsumer<E> action) throws E {
        while (elements.hasNext()) {
            action.accept(elements.nextLong());
        }
    }

    @Override
    public long[] toArray() {
        return elements.toArray();
    }

    @Override
    public LongList toLongList() {
        return elements.toList();
    }

    @Override
    public List<Long> toList() {
        return toCollection(Fn.Suppliers.<Long> ofList());
    }

    @Override
    public Set<Long> toSet() {
        return toCollection(Fn.Suppliers.<Long> ofSet());
    }

    @Override
    public <R extends Collection<Long>> R toCollection(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset() {
        return toMultiset(Fn.Suppliers.<Long> ofMultiset());
    }

    @Override
    public Multiset<Long> toMultiset(Supplier<? extends Multiset<Long>> supplier) {
        final Multiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public LongMultiset<Long> toLongMultiset() {
        return toLongMultiset(Fn.Suppliers.<Long> ofLongMultiset());
    }

    @Override
    public LongMultiset<Long> toLongMultiset(Supplier<? extends LongMultiset<Long>> supplier) {
        final LongMultiset<Long> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyExtractor, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        long element = 0;

        while (elements.hasNext()) {
            element = elements.nextLong();
            Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final LongFunction<? extends K> classifier, final Collector<Long, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Long> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        long element = 0;

        while (elements.hasNext()) {
            element = elements.nextLong();
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
    public long reduce(long identity, LongBinaryOperator op) {
        long result = identity;

        while (elements.hasNext()) {
            result = op.applyAsLong(result, elements.nextLong());
        }

        return result;
    }

    @Override
    public OptionalLong reduce(LongBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        long result = elements.nextLong();

        while (elements.hasNext()) {
            result = op.applyAsLong(result, elements.nextLong());
        }

        return OptionalLong.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjLongConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.nextLong());
        }

        return result;
    }

    @Override
    public OptionalLong head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalLong.of(elements.nextLong()) : OptionalLong.empty();
            tail = new IteratorLongStream(elements, sorted, closeHandlers);
        }

        return head;
    }

    @Override
    public LongStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalLong.of(elements.nextLong()) : OptionalLong.empty();
            tail = new IteratorLongStream(elements, sorted, closeHandlers);
        }

        return tail;
    }

    @Override
    public LongStream headd() {
        if (head2 == null) {
            final long[] a = elements.toArray();
            head2 = new ArrayLongStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalLong.empty() : OptionalLong.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalLong taill() {
        if (tail2 == null) {
            final long[] a = elements.toArray();
            head2 = new ArrayLongStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalLong.empty() : OptionalLong.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalLong min() {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        } else if (sorted) {
            return OptionalLong.of(elements.nextLong());
        }

        long candidate = elements.nextLong();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.nextLong();

            if (next < candidate) {
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
                next = elements.nextLong();
            }

            return OptionalLong.of(next);
        }

        long candidate = elements.nextLong();
        long next = 0;

        while (elements.hasNext()) {
            next = elements.nextLong();

            if (next > candidate) {
                candidate = next;
            }
        }

        return OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong kthLargest(int k) {
        N.checkArgPositive(k, "k");

        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        final Nullable<Long> optional = boxed().kthLargest(k, LONG_COMPARATOR);

        return optional.isPresent() ? OptionalLong.of(optional.get()) : OptionalLong.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextLong();
        }

        return result;
    }

    @Override
    public OptionalDouble average() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        long sum = 0;
        long count = 0;

        while (elements.hasNext()) {
            sum += elements.nextLong();
            count++;
        }

        return OptionalDouble.of(((double) sum) / count);
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public LongSummaryStatistics summarize() {
        final LongSummaryStatistics result = new LongSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextLong());
        }

        return result;
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.LongPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextLong())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.LongPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextLong()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.LongPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextLong())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> OptionalLong findFirst(final Try.LongPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            long e = elements.nextLong();

            if (predicate.test(e)) {
                return OptionalLong.of(e);
            }
        }

        return OptionalLong.empty();
    }

    @Override
    public <E extends Exception> OptionalLong findLast(final Try.LongPredicate<E> predicate) throws E {
        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        boolean hasResult = false;
        long e = 0;
        long result = 0;

        while (elements.hasNext()) {
            e = elements.nextLong();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalLong.of(result) : OptionalLong.empty();
    }

    @Override
    public FloatStream asFloatStream() {
        return newStream(new FloatIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return elements.nextLong();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted);
    }

    @Override
    public DoubleStream asDoubleStream() {
        return newStream(new DoubleIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return elements.nextLong();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted);
    }

    @Override
    public java.util.stream.LongStream toJdkStream() {
        final PrimitiveIterator.OfLong spliterator = new PrimitiveIterator.OfLong() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return elements.nextLong();
            }
        };

        if (N.isNullOrEmpty(closeHandlers)) {
            return StreamSupport.longStream(Spliterators.spliteratorUnknownSize(spliterator, Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL),
                    isParallel());
        } else {
            return StreamSupport.longStream(Spliterators.spliteratorUnknownSize(spliterator, Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL),
                    isParallel()).onClose(() -> close(closeHandlers));
        }
    }

    @Override
    public Stream<Long> boxed() {
        return new IteratorStream<>(iterator(), sorted, sorted ? LONG_COMPARATOR : null, closeHandlers);
    }

    @Override
    LongIteratorEx iteratorEx() {
        return elements;
    }

    @Override
    public LongStream parallel(int maxThreadNum, Splitor splitor) {
        return new ParallelIteratorLongStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorLongStream(elements, sorted, newCloseHandlers);
    }
}
