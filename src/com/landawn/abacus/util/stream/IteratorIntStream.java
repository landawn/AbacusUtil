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

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nullable;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.IntBinaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.IntToByteFunction;
import com.landawn.abacus.util.function.IntToCharFunction;
import com.landawn.abacus.util.function.IntToDoubleFunction;
import com.landawn.abacus.util.function.IntToFloatFunction;
import com.landawn.abacus.util.function.IntToLongFunction;
import com.landawn.abacus.util.function.IntToShortFunction;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.ObjIntConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class IteratorIntStream extends AbstractIntStream {
    final IntIteratorEx elements;

    OptionalInt head;
    IntStream tail;

    IntStream head2;
    OptionalInt tail2;

    IteratorIntStream(final IntIterator values) {
        this(values, null);
    }

    IteratorIntStream(final IntIterator values, final Collection<Runnable> closeHandlers) {
        this(values, false, closeHandlers);
    }

    IteratorIntStream(final IntIterator values, final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);

        IntIteratorEx tmp = null;

        if (values instanceof IntIteratorEx) {
            tmp = (IntIteratorEx) values;
        } else {
            tmp = new IntIteratorEx() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public int nextInt() {
                    return values.nextInt();
                }
            };
        }

        this.elements = tmp;
    }

    @Override
    public IntStream filter(final IntPredicate predicate) {
        return newStream(new IntIteratorEx() {
            private boolean hasNext = false;
            private int next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    while (elements.hasNext()) {
                        next = elements.nextInt();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public int nextInt() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }
        }, sorted);
    }

    @Override
    public IntStream takeWhile(final IntPredicate predicate) {
        return newStream(new IntIteratorEx() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private int next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.nextInt();

                    if (predicate.test(next)) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public int nextInt() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public IntStream dropWhile(final IntPredicate predicate) {
        return newStream(new IntIteratorEx() {
            private boolean hasNext = false;
            private int next = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.nextInt();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else if (elements.hasNext()) {
                        next = elements.nextInt();
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public int nextInt() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public IntStream map(final IntUnaryOperator mapper) {
        return newStream(new IntIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextInt());
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
    public CharStream mapToChar(final IntToCharFunction mapper) {
        return newStream(new CharIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char nextChar() {
                return mapper.applyAsChar(elements.nextInt());
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
    public ByteStream mapToByte(final IntToByteFunction mapper) {
        return newStream(new ByteIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public byte nextByte() {
                return mapper.applyAsByte(elements.nextInt());
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
    public ShortStream mapToShort(final IntToShortFunction mapper) {
        return newStream(new ShortIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short nextShort() {
                return mapper.applyAsShort(elements.nextInt());
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
    public LongStream mapToLong(final IntToLongFunction mapper) {
        return newStream(new LongIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return mapper.applyAsLong(elements.nextInt());
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
    public FloatStream mapToFloat(final IntToFloatFunction mapper) {
        return newStream(new FloatIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return mapper.applyAsFloat(elements.nextInt());
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
    public DoubleStream mapToDouble(final IntToDoubleFunction mapper) {
        return newStream(new DoubleIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return mapper.applyAsDouble(elements.nextInt());
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
    public <U> Stream<U> mapToObj(final IntFunction<? extends U> mapper) {
        return newStream(new ObjIteratorEx<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextInt());
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
    public IntStream flatMap(final IntFunction<? extends IntStream> mapper) {
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

                    s = mapper.apply(elements.nextInt());

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
    public CharStream flatMapToChar(final IntFunction<? extends CharStream> mapper) {
        final CharIteratorEx iter = new CharIteratorEx() {
            private CharIterator cur = null;
            private CharStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextInt());

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
            public char nextChar() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorCharStream(iter, newCloseHandlers);
    }

    @Override
    public ByteStream flatMapToByte(final IntFunction<? extends ByteStream> mapper) {
        final ByteIteratorEx iter = new ByteIteratorEx() {
            private ByteIterator cur = null;
            private ByteStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextInt());

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
            public byte nextByte() {
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
        };

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1)
                : new LocalLinkedHashSet<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorByteStream(iter, newCloseHandlers);
    }

    @Override
    public ShortStream flatMapToShort(final IntFunction<? extends ShortStream> mapper) {
        final ShortIteratorEx iter = new ShortIteratorEx() {
            private ShortIterator cur = null;
            private ShortStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextInt());

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
    public LongStream flatMapToLong(final IntFunction<? extends LongStream> mapper) {
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

                    s = mapper.apply(elements.nextInt());

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
    public FloatStream flatMapToFloat(final IntFunction<? extends FloatStream> mapper) {
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

                    s = mapper.apply(elements.nextInt());

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
    public DoubleStream flatMapToDouble(final IntFunction<? extends DoubleStream> mapper) {
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

                    s = mapper.apply(elements.nextInt());

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
    public <T> Stream<T> flatMapToObj(final IntFunction<? extends Stream<T>> mapper) {
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

                    s = mapper.apply(elements.nextInt());

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
    public Stream<IntList> splitToList(final int size) {
        N.checkArgPositive(size, "size");

        return newStream(new ObjIteratorEx<IntList>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public IntList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final IntList result = new IntList(size);

                while (result.size() < size && elements.hasNext()) {
                    result.add(elements.nextInt());
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
    public Stream<IntList> splitToList(final IntPredicate predicate) {
        return newStream(new ObjIteratorEx<IntList>() {
            private int next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public IntList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final IntList result = new IntList();

                if (hasNext == false) {
                    next = elements.nextInt();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextInt() : 0;
                    } else if (predicate.test(next) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextInt() : 0;
                    } else {
                        break;
                    }
                }

                return result;
            }

        }, false, null);
    }

    @Override
    public <U> Stream<IntList> splitToList(final U seed, final BiPredicate<? super Integer, ? super U> predicate, final Consumer<? super U> seedUpdate) {
        return newStream(new ObjIteratorEx<IntList>() {
            private int next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public IntList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final IntList result = new IntList();

                if (hasNext == false) {
                    next = elements.nextInt();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next, seed);
                        next = (hasNext = elements.hasNext()) ? elements.nextInt() : 0;
                    } else if (predicate.test(next, seed) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextInt() : 0;
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
    public Stream<IntList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return newStream(new ObjIteratorEx<IntList>() {
            private IntList prev = null;

            @Override
            public boolean hasNext() {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.nextInt();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public IntList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                IntList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;

                    if (cnt <= 8) {
                        result = new IntList(windowSize);

                        for (int i = windowSize - cnt; i < windowSize; i++) {
                            result.add(prev.get(i));
                        }
                    } else {
                        final int[] dest = new int[windowSize];
                        N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                        result = IntList.of(dest, cnt);
                    }
                } else {
                    result = new IntList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.nextInt());
                }

                return prev = result;
            }
        }, false, null);
    }

    @Override
    public IntStream top(int n) {
        return top(n, INT_COMPARATOR);
    }

    @Override
    public IntStream top(int n, Comparator<? super Integer> comparator) {
        return boxed().top(n, comparator).mapToInt(ToIntFunction.UNBOX);
    }

    @Override
    public IntStream peek(final IntConsumer action) {
        return newStream(new IntIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                final int next = elements.nextInt();
                action.accept(next);
                return next;
            }
        }, sorted);
    }

    @Override
    public IntStream limit(final long maxSize) {
        N.checkArgNotNegative(maxSize, "maxSize");

        return newStream(new IntIteratorEx() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public int nextInt() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextInt();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted);
    }

    @Override
    public IntStream skip(final long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        return newStream(new IntIteratorEx() {
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
            public int nextInt() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextInt();
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
            public int[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, sorted);
    }

    @Override
    public <E extends Exception> void forEach(final Try.IntConsumer<E> action) throws E {
        while (elements.hasNext()) {
            action.accept(elements.nextInt());
        }
    }

    @Override
    public int[] toArray() {
        return elements.toArray();
    }

    @Override
    public IntList toIntList() {
        return elements.toList();
    }

    @Override
    public List<Integer> toList() {
        return toCollection(Fn.Suppliers.<Integer> ofList());
    }

    @Override
    public Set<Integer> toSet() {
        return toCollection(Fn.Suppliers.<Integer> ofSet());
    }

    @Override
    public <C extends Collection<Integer>> C toCollection(Supplier<? extends C> supplier) {
        final C result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
    }

    @Override
    public Multiset<Integer> toMultiset() {
        return toMultiset(Fn.Suppliers.<Integer> ofMultiset());
    }

    @Override
    public Multiset<Integer> toMultiset(Supplier<? extends Multiset<Integer>> supplier) {
        final Multiset<Integer> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
    }

    @Override
    public LongMultiset<Integer> toLongMultiset() {
        return toLongMultiset(Fn.Suppliers.<Integer> ofLongMultiset());
    }

    @Override
    public LongMultiset<Integer> toLongMultiset(Supplier<? extends LongMultiset<Integer>> supplier) {
        final LongMultiset<Integer> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(IntFunction<? extends K> keyExtractor, IntFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        int element = 0;

        while (elements.hasNext()) {
            element = elements.nextInt();
            Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final IntFunction<? extends K> classifier, final Collector<Integer, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Integer> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        int element = 0;

        while (elements.hasNext()) {
            element = elements.nextInt();
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
    public int reduce(int identity, IntBinaryOperator op) {
        int result = identity;

        while (elements.hasNext()) {
            result = op.applyAsInt(result, elements.nextInt());
        }

        return result;
    }

    @Override
    public OptionalInt reduce(IntBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        int result = elements.nextInt();

        while (elements.hasNext()) {
            result = op.applyAsInt(result, elements.nextInt());
        }

        return OptionalInt.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjIntConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.nextInt());
        }

        return result;
    }

    @Override
    public OptionalInt head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalInt.of(elements.nextInt()) : OptionalInt.empty();
            tail = new IteratorIntStream(elements, sorted, closeHandlers);
        }

        return head;
    }

    @Override
    public IntStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalInt.of(elements.nextInt()) : OptionalInt.empty();
            tail = new IteratorIntStream(elements, sorted, closeHandlers);
        }

        return tail;
    }

    @Override
    public IntStream headd() {
        if (head2 == null) {
            final int[] a = elements.toArray();
            head2 = new ArrayIntStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalInt.empty() : OptionalInt.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalInt taill() {
        if (tail2 == null) {
            final int[] a = elements.toArray();
            head2 = new ArrayIntStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalInt.empty() : OptionalInt.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalInt min() {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        } else if (sorted) {
            return OptionalInt.of(elements.nextInt());
        }

        int candidate = elements.nextInt();
        int next = 0;

        while (elements.hasNext()) {
            next = elements.nextInt();

            if (next < candidate) {
                candidate = next;
            }
        }

        return OptionalInt.of(candidate);
    }

    @Override
    public OptionalInt max() {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        } else if (sorted) {
            int next = 0;

            while (elements.hasNext()) {
                next = elements.nextInt();
            }

            return OptionalInt.of(next);
        }

        int candidate = elements.nextInt();
        int next = 0;

        while (elements.hasNext()) {
            next = elements.nextInt();

            if (next > candidate) {
                candidate = next;
            }
        }

        return OptionalInt.of(candidate);
    }

    @Override
    public OptionalInt kthLargest(int k) {
        N.checkArgPositive(k, "k");

        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        final Nullable<Integer> optional = boxed().kthLargest(k, INT_COMPARATOR);

        return optional.isPresent() ? OptionalInt.of(optional.get()) : OptionalInt.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextInt();
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
            sum += elements.nextInt();
            count++;
        }

        return OptionalDouble.of(((double) sum) / count);
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public IntSummaryStatistics summarize() {
        final IntSummaryStatistics result = new IntSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextInt());
        }

        return result;
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.IntPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextInt())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.IntPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextInt()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.IntPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextInt())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public <E extends Exception> OptionalInt findFirst(final Try.IntPredicate<E> predicate) throws E {
        while (elements.hasNext()) {
            int e = elements.nextInt();

            if (predicate.test(e)) {
                return OptionalInt.of(e);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public <E extends Exception> OptionalInt findLast(final Try.IntPredicate<E> predicate) throws E {
        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        boolean hasResult = false;
        int e = 0;
        int result = 0;

        while (elements.hasNext()) {
            e = elements.nextInt();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalInt.of(result) : OptionalInt.empty();
    }

    @Override
    public LongStream asLongStream() {
        return newStream(new LongIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return elements.nextInt();
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
    public FloatStream asFloatStream() {
        return newStream(new FloatIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return elements.nextInt();
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
                return elements.nextInt();
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
    public java.util.stream.IntStream toJdkStream() {
        final PrimitiveIterator.OfInt spliterator = new PrimitiveIterator.OfInt() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return elements.nextInt();
            }
        };

        if (N.isNullOrEmpty(closeHandlers)) {
            return StreamSupport.intStream(Spliterators.spliteratorUnknownSize(spliterator, Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL),
                    isParallel());
        } else {
            return StreamSupport.intStream(Spliterators.spliteratorUnknownSize(spliterator, Spliterator.ORDERED | Spliterator.IMMUTABLE | Spliterator.NONNULL),
                    isParallel()).onClose(() -> close(closeHandlers));
        }
    }

    @Override
    public Stream<Integer> boxed() {
        return new IteratorStream<>(iterator(), sorted, sorted ? INT_COMPARATOR : null, closeHandlers);
    }

    @Override
    IntIteratorEx iteratorEx() {
        return elements;
    }

    @Override
    public IntStream parallel(int maxThreadNum, Splitor splitor) {
        return new ParallelIteratorIntStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorIntStream(elements, sorted, newCloseHandlers);
    }
}
