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

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.IntSummaryStatistics;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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
    final ExIntIterator elements;

    OptionalInt head;
    IntStream tail;

    IntStream head2;
    OptionalInt tail2;

    IteratorIntStream(final IntIterator values) {
        this(values, null);
    }

    IteratorIntStream(final IntIterator values, final Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorIntStream(final IntIterator values, final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);

        ExIntIterator tmp = null;

        if (values instanceof ExIntIterator) {
            tmp = (ExIntIterator) values;
        } else if (values instanceof SkippableIterator) {
            tmp = new ExIntIterator() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public int nextInt() {
                    return values.nextInt();
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
            tmp = new ExIntIterator() {
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
        return new IteratorIntStream(new ExIntIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream takeWhile(final IntPredicate predicate) {
        return new IteratorIntStream(new ExIntIterator() {
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

        }, closeHandlers, sorted);
    }

    @Override
    public IntStream dropWhile(final IntPredicate predicate) {
        return new IteratorIntStream(new ExIntIterator() {
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

        }, closeHandlers, sorted);
    }

    @Override
    public IntStream map(final IntUnaryOperator mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextInt());
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
    public CharStream mapToChar(final IntToCharFunction mapper) {
        return new IteratorCharStream(new ExCharIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char nextChar() {
                return mapper.applyAsChar(elements.nextInt());
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
    public ByteStream mapToByte(final IntToByteFunction mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public byte nextByte() {
                return mapper.applyAsByte(elements.nextInt());
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
    public ShortStream mapToShort(final IntToShortFunction mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short nextShort() {
                return mapper.applyAsShort(elements.nextInt());
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
    public LongStream mapToLong(final IntToLongFunction mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return mapper.applyAsLong(elements.nextInt());
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
    public FloatStream mapToFloat(final IntToFloatFunction mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return mapper.applyAsFloat(elements.nextInt());
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
    public DoubleStream mapToDouble(final IntToDoubleFunction mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return mapper.applyAsDouble(elements.nextInt());
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
    public <U> Stream<U> mapToObj(final IntFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ExIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextInt());
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
    public IntStream flatMap(final IntFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).exIterator();
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
    public CharStream flatMapToChar(final IntFunction<? extends CharStream> mapper) {
        return new IteratorCharStream(new ExCharIterator() {
            private CharIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).exIterator();
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
    public ByteStream flatMapToByte(final IntFunction<? extends ByteStream> mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            private ByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).exIterator();
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
    public ShortStream flatMapToShort(final IntFunction<? extends ShortStream> mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            private ShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).exIterator();
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
    public LongStream flatMapToLong(final IntFunction<? extends LongStream> mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            private LongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).exIterator();
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
    public FloatStream flatMapToFloat(final IntFunction<? extends FloatStream> mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            private FloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).exIterator();
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
    public DoubleStream flatMapToDouble(final IntFunction<? extends DoubleStream> mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            private DoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).exIterator();
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
    public <T> Stream<T> flatMapToObj(final IntFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ExIterator<T>() {
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextInt()).iterator();
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
        }, closeHandlers);
    }

    @Override
    public Stream<IntList> split2(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<IntList>(new ExIterator<IntList>() {
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

        }, closeHandlers);
    }

    @Override
    public <U> Stream<IntList> split2(final U identity, final BiFunction<? super Integer, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<IntList>(new ExIterator<IntList>() {
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
                        preCondition = predicate.apply(next, identity);
                        next = (hasNext = elements.hasNext()) ? elements.nextInt() : 0;
                    } else if (predicate.apply(next, identity) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextInt() : 0;
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
    public Stream<IntList> sliding2(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<IntList>(new ExIterator<IntList>() {
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
        }, closeHandlers);
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
    public IntStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorIntStream(new ExIntIterator() {
            int[] a = null;
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
            public int nextInt() {
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
            public int[] toArray() {
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

                N.sort(a);
            }
        }, closeHandlers, true);
    }

    @Override
    public IntStream peek(final IntConsumer action) {
        return new IteratorIntStream(new ExIntIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new IteratorIntStream(new ExIntIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public IntStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorIntStream(new ExIntIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(IntConsumer action) {
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
        return IntList.of(toArray());
    }

    @Override
    public List<Integer> toList() {
        final List<Integer> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
    }

    @Override
    public <R extends List<Integer>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
    }

    @Override
    public Set<Integer> toSet() {
        final Set<Integer> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
    }

    @Override
    public <R extends Set<Integer>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
    }

    @Override
    public Multiset<Integer> toMultiset() {
        final Multiset<Integer> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
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
        final LongMultiset<Integer> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextInt());
        }

        return result;
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
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(IntFunction<? extends K> keyExtractor, IntFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapFactory) {
        final Multimap<K, U, V> result = mapFactory.get();
        int element = 0;

        while (elements.hasNext()) {
            element = elements.nextInt();
            result.put(keyExtractor.apply(element), valueMapper.apply(element));
        }

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
            tail = new IteratorIntStream(elements, closeHandlers, sorted);
        }

        return head;
    }

    @Override
    public IntStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalInt.of(elements.nextInt()) : OptionalInt.empty();
            tail = new IteratorIntStream(elements, closeHandlers, sorted);
        }

        return tail;
    }

    @Override
    public IntStream head2() {
        if (head2 == null) {
            final int[] a = elements.toArray();
            head2 = new ArrayIntStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted);
            tail2 = a.length == 0 ? OptionalInt.empty() : OptionalInt.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalInt tail2() {
        if (tail2 == null) {
            final int[] a = elements.toArray();
            head2 = new ArrayIntStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted);
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

            if (N.compare(next, candidate) < 0) {
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

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalInt.of(candidate);
    }

    @Override
    public OptionalInt kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalInt.empty();
        }

        final NullabLe<Integer> optional = boxed().kthLargest(k, INT_COMPARATOR);

        return optional.isPresent() ? OptionalInt.of(optional.get()) : OptionalInt.empty();
    }

    @Override
    public Long sum() {
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
    public boolean anyMatch(IntPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextInt())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(IntPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextInt()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(IntPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextInt())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalInt findFirst(IntPredicate predicate) {
        while (elements.hasNext()) {
            int e = elements.nextInt();

            if (predicate.test(e)) {
                return OptionalInt.of(e);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public OptionalInt findLast(IntPredicate predicate) {
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
        return new IteratorLongStream(new ExLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream asFloatStream() {
        return new IteratorFloatStream(new ExFloatIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new IteratorDoubleStream(new ExDoubleIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public Stream<Integer> boxed() {
        return new IteratorStream<Integer>(iterator(), closeHandlers, sorted, sorted ? INT_COMPARATOR : null);
    }

    @Override
    ExIntIterator exIterator() {
        return elements;
    }

    @Override
    public IntStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelIteratorIntStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public IntStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorIntStream(elements, newCloseHandlers, sorted);
    }
}
