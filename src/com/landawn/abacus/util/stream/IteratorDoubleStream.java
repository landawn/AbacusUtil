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
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleBinaryOperator;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleToFloatFunction;
import com.landawn.abacus.util.function.DoubleToIntFunction;
import com.landawn.abacus.util.function.DoubleToLongFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.ObjDoubleConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class IteratorDoubleStream extends AbstractDoubleStream {
    final DoubleIteratorEx elements;

    OptionalDouble head;
    DoubleStream tail;

    DoubleStream head2;
    OptionalDouble tail2;

    IteratorDoubleStream(final DoubleIterator values) {
        this(values, null);
    }

    IteratorDoubleStream(final DoubleIterator values, final Collection<Runnable> closeHandlers) {
        this(values, false, closeHandlers);
    }

    IteratorDoubleStream(final DoubleIterator values, final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);

        DoubleIteratorEx tmp = null;

        if (values instanceof DoubleIteratorEx) {
            tmp = (DoubleIteratorEx) values;
        } else {
            tmp = new DoubleIteratorEx() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public double nextDouble() {
                    return values.nextDouble();
                }
            };
        }

        this.elements = tmp;
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate) {
        return newStream(new DoubleIteratorEx() {
            private boolean hasNext = false;
            private double next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    while (elements.hasNext()) {
                        next = elements.nextDouble();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public double nextDouble() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }
        }, sorted);
    }

    @Override
    public DoubleStream takeWhile(final DoublePredicate predicate) {
        return newStream(new DoubleIteratorEx() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private double next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.nextDouble();

                    if (predicate.test(next)) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public double nextDouble() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate) {
        return newStream(new DoubleIteratorEx() {
            private boolean hasNext = false;
            private double next = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.nextDouble();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else if (elements.hasNext()) {
                        next = elements.nextDouble();
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public double nextDouble() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public DoubleStream map(final DoubleUnaryOperator mapper) {
        return newStream(new DoubleIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return mapper.applyAsDouble(elements.nextDouble());
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
    public IntStream mapToInt(final DoubleToIntFunction mapper) {
        return newStream(new IntIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextDouble());
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
    public LongStream mapToLong(final DoubleToLongFunction mapper) {
        return newStream(new LongIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return mapper.applyAsLong(elements.nextDouble());
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
    public FloatStream mapToFloat(final DoubleToFloatFunction mapper) {
        return newStream(new FloatIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return mapper.applyAsFloat(elements.nextDouble());
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
    public <U> Stream<U> mapToObj(final DoubleFunction<? extends U> mapper) {
        return newStream(new ObjIteratorEx<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextDouble());
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
    public DoubleStream flatMap(final DoubleFunction<? extends DoubleStream> mapper) {
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

                    s = mapper.apply(elements.nextDouble());

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
    public IntStream flatMapToInt(final DoubleFunction<? extends IntStream> mapper) {
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

                    s = mapper.apply(elements.nextDouble());

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
    public LongStream flatMapToLong(final DoubleFunction<? extends LongStream> mapper) {
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

                    s = mapper.apply(elements.nextDouble());

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
    public FloatStream flatMapToFloat(final DoubleFunction<? extends FloatStream> mapper) {
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

                    s = mapper.apply(elements.nextDouble());

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
    public <T> Stream<T> flatMapToObj(final DoubleFunction<? extends Stream<T>> mapper) {
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

                    s = mapper.apply(elements.nextDouble());

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
    public Stream<DoubleList> splitToList(final int size) {
        N.checkArgPositive(size, "size");

        return newStream(new ObjIteratorEx<DoubleList>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public DoubleList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final DoubleList result = new DoubleList(size);

                while (result.size() < size && elements.hasNext()) {
                    result.add(elements.nextDouble());
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
    public Stream<DoubleList> splitToList(final DoublePredicate predicate) {
        return newStream(new ObjIteratorEx<DoubleList>() {
            private double next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public DoubleList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final DoubleList result = new DoubleList();

                if (hasNext == false) {
                    next = elements.nextDouble();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextDouble() : 0;
                    } else if (predicate.test(next) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextDouble() : 0;
                    } else {
                        break;
                    }
                }

                return result;
            }

        }, false, null);
    }

    @Override
    public <U> Stream<DoubleList> splitToList(final U seed, final BiPredicate<? super Double, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return newStream(new ObjIteratorEx<DoubleList>() {
            private double next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public DoubleList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final DoubleList result = new DoubleList();

                if (hasNext == false) {
                    next = elements.nextDouble();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next, seed);
                        next = (hasNext = elements.hasNext()) ? elements.nextDouble() : 0;
                    } else if (predicate.test(next, seed) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextDouble() : 0;
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
    public Stream<DoubleList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return newStream(new ObjIteratorEx<DoubleList>() {
            private DoubleList prev = null;

            @Override
            public boolean hasNext() {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.nextDouble();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public DoubleList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                DoubleList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;

                    if (cnt <= 8) {
                        result = new DoubleList(windowSize);

                        for (int i = windowSize - cnt; i < windowSize; i++) {
                            result.add(prev.get(i));
                        }
                    } else {
                        final double[] dest = new double[windowSize];
                        N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                        result = DoubleList.of(dest, cnt);
                    }
                } else {
                    result = new DoubleList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.nextDouble());
                }

                return prev = result;
            }
        }, false, null);
    }

    @Override
    public DoubleStream top(int n) {
        return top(n, DOUBLE_COMPARATOR);
    }

    @Override
    public DoubleStream top(int n, Comparator<? super Double> comparator) {
        return boxed().top(n, comparator).mapToDouble(ToDoubleFunction.UNBOX);
    }

    @Override
    public DoubleStream peek(final DoubleConsumer action) {
        return newStream(new DoubleIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                final double next = elements.nextDouble();
                action.accept(next);
                return next;
            }
        }, sorted);
    }

    @Override
    public DoubleStream limit(final long maxSize) {
        N.checkArgNotNegative(maxSize, "maxSize");

        return newStream(new DoubleIteratorEx() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public double nextDouble() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextDouble();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted);
    }

    @Override
    public DoubleStream skip(final long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        return newStream(new DoubleIteratorEx() {
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
            public double nextDouble() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextDouble();
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
            public double[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, sorted);
    }

    @Override
    public <E extends Exception> void forEach(final Try.DoubleConsumer<E> action) throws E {
        while (elements.hasNext()) {
            action.accept(elements.nextDouble());
        }
    }

    @Override
    public double[] toArray() {
        return elements.toArray();
    }

    @Override
    public DoubleList toDoubleList() {
        return elements.toList();
    }

    @Override
    public List<Double> toList() {
        return toCollection(Fn.Suppliers.<Double> ofList());
    }

    @Override
    public Set<Double> toSet() {
        return toCollection(Fn.Suppliers.<Double> ofSet());
    }

    @Override
    public <C extends Collection<Double>> C toCollection(Supplier<? extends C> supplier) {
        final C result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset() {
        return toMultiset(Fn.Suppliers.<Double> ofMultiset());
    }

    @Override
    public Multiset<Double> toMultiset(Supplier<? extends Multiset<Double>> supplier) {
        final Multiset<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset() {
        return toLongMultiset(Fn.Suppliers.<Double> ofLongMultiset());
    }

    @Override
    public LongMultiset<Double> toLongMultiset(Supplier<? extends LongMultiset<Double>> supplier) {
        final LongMultiset<Double> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextDouble());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        double element = 0;

        while (elements.hasNext()) {
            element = elements.nextDouble();
            Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Double> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        double element = 0;

        while (elements.hasNext()) {
            element = elements.nextDouble();
            key = N.checkArgNotNull(classifier.apply(element), "element cannot be mapped to a null key");

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
    public double reduce(double identity, DoubleBinaryOperator op) {
        double result = identity;

        while (elements.hasNext()) {
            result = op.applyAsDouble(result, elements.nextDouble());
        }

        return result;
    }

    @Override
    public OptionalDouble reduce(DoubleBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        double result = elements.nextDouble();

        while (elements.hasNext()) {
            result = op.applyAsDouble(result, elements.nextDouble());
        }

        return OptionalDouble.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.nextDouble());
        }

        return result;
    }

    @Override
    public OptionalDouble head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalDouble.of(elements.nextDouble()) : OptionalDouble.empty();
            tail = new IteratorDoubleStream(elements, sorted, closeHandlers);
        }

        return head;
    }

    @Override
    public DoubleStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalDouble.of(elements.nextDouble()) : OptionalDouble.empty();
            tail = new IteratorDoubleStream(elements, sorted, closeHandlers);
        }

        return tail;
    }

    @Override
    public DoubleStream headd() {
        if (head2 == null) {
            final double[] a = elements.toArray();
            head2 = new ArrayDoubleStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalDouble.empty() : OptionalDouble.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalDouble taill() {
        if (tail2 == null) {
            final double[] a = elements.toArray();
            head2 = new ArrayDoubleStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalDouble.empty() : OptionalDouble.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalDouble min() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements.nextDouble());
        }

        double candidate = elements.nextDouble();
        double next = 0;

        while (elements.hasNext()) {
            next = elements.nextDouble();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalDouble.of(candidate);
    }

    @Override
    public OptionalDouble max() {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        } else if (sorted) {
            double next = 0;

            while (elements.hasNext()) {
                next = elements.nextDouble();
            }

            return OptionalDouble.of(next);
        }

        double candidate = elements.nextDouble();
        double next = 0;

        while (elements.hasNext()) {
            next = elements.nextDouble();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalDouble.of(candidate);
    }

    @Override
    public OptionalDouble kthLargest(int k) {
        N.checkArgPositive(k, "k");

        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        final Optional<Double> optional = boxed().kthLargest(k, DOUBLE_COMPARATOR);

        return optional.isPresent() ? OptionalDouble.of(optional.get()) : OptionalDouble.empty();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public DoubleSummaryStatistics summarize() {
        final DoubleSummaryStatistics result = new DoubleSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextDouble());
        }

        return result;
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.DoublePredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextDouble())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.DoublePredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextDouble()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.DoublePredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextDouble())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> OptionalDouble findFirst(final Try.DoublePredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            double e = elements.nextDouble();

            if (predicate.test(e)) {
                return OptionalDouble.of(e);
            }
        }

        return OptionalDouble.empty();
    }

    @Override
    public <E extends Exception> OptionalDouble findLast(final Try.DoublePredicate<E> predicate) throws E {
        if (elements.hasNext() == false) {
            return OptionalDouble.empty();
        }

        boolean hasResult = false;
        double e = 0;
        double result = 0;

        while (elements.hasNext()) {
            e = elements.nextDouble();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalDouble.of(result) : OptionalDouble.empty();
    }

    @Override
    public java.util.stream.DoubleStream toJdkStream() {
        final PrimitiveIterator.OfDouble spliterator = new PrimitiveIterator.OfDouble() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return elements.nextDouble();
            }
        };

        if (N.isNullOrEmpty(closeHandlers)) {
            return StreamSupport.doubleStream(
                    Spliterators.spliteratorUnknownSize(spliterator, Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL), isParallel());
        } else {
            return StreamSupport
                    .doubleStream(Spliterators.spliteratorUnknownSize(spliterator, Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL),
                            isParallel())
                    .onClose(() -> close(closeHandlers));
        }
    }

    @Override
    public Stream<Double> boxed() {
        return new IteratorStream<>(iterator(), sorted, sorted ? DOUBLE_COMPARATOR : null, closeHandlers);
    }

    @Override
    DoubleIteratorEx iteratorEx() {
        return elements;
    }

    @Override
    public DoubleStream parallel(int maxThreadNum, Splitor splitor) {
        return new ParallelIteratorDoubleStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorDoubleStream(elements, sorted, newCloseHandlers);
    }
}
