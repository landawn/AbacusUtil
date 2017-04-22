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

import com.landawn.abacus.util.DoubleIterator;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.LongSummaryStatistics;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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
    final ExLongIterator elements;

    long head;
    LongStream tail;

    LongStream head2;
    long tail2;

    IteratorLongStream(final LongIterator values) {
        this(values, null);
    }

    IteratorLongStream(final LongIterator values, final Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorLongStream(final LongIterator values, final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);

        this.elements = values instanceof ExLongIterator ? (ExLongIterator) values : new ExLongIterator() {
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

    @Override
    public LongStream filter(final LongPredicate predicate) {
        return new IteratorLongStream(new ExLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public LongStream takeWhile(final LongPredicate predicate) {
        return new IteratorLongStream(new ExLongIterator() {
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

        }, closeHandlers, sorted);
    }

    @Override
    public LongStream dropWhile(final LongPredicate predicate) {
        return new IteratorLongStream(new ExLongIterator() {
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

        }, closeHandlers, sorted);
    }

    @Override
    public LongStream map(final LongUnaryOperator mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long nextLong() {
                return mapper.applyAsLong(elements.nextLong());
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
    public IntStream mapToInt(final LongToIntFunction mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextLong());
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
    public FloatStream mapToFloat(final LongToFloatFunction mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float nextFloat() {
                return mapper.applyAsFloat(elements.nextLong());
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
    public DoubleStream mapToDouble(final LongToDoubleFunction mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double nextDouble() {
                return mapper.applyAsDouble(elements.nextLong());
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
    public <U> Stream<U> mapToObj(final LongFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ExIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextLong());
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
    public LongStream flatMap(final LongFunction<? extends LongStream> mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            private LongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextLong()).exIterator();
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
    public IntStream flatMapToInt(final LongFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextLong()).exIterator();
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
    public FloatStream flatMapToFloat(final LongFunction<? extends FloatStream> mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            private FloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextLong()).exIterator();
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
    public DoubleStream flatMapToDouble(final LongFunction<? extends DoubleStream> mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            private DoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextLong()).exIterator();
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
    public <T> Stream<T> flatMapToObj(final LongFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ExIterator<T>() {
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextLong()).iterator();
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
    public Stream<LongList> split0(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<LongList>(new ExIterator<LongList>() {
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

        }, closeHandlers);
    }

    @Override
    public <U> Stream<LongList> split0(final U identity, final BiFunction<? super Long, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<LongList>(new ExIterator<LongList>() {
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
                        preCondition = predicate.apply(next, identity);
                        next = (hasNext = elements.hasNext()) ? elements.nextLong() : 0;
                    } else if (predicate.apply(next, identity) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextLong() : 0;
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
    public Stream<LongList> sliding0(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<LongList>(new ExIterator<LongList>() {
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
        }, closeHandlers);
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
    public LongStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorLongStream(new ExLongIterator() {
            long[] a = null;
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
            public long nextLong() {
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
            public long[] toArray() {
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
    public LongStream peek(final LongConsumer action) {
        return new IteratorLongStream(new ExLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public LongStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new IteratorLongStream(new ExLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public LongStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorLongStream(new ExLongIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(LongConsumer action) {
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
        return LongList.of(toArray());
    }

    @Override
    public List<Long> toList() {
        final List<Long> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public <R extends List<Long>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public Set<Long> toSet() {
        final Set<Long> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public <R extends Set<Long>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
    }

    @Override
    public Multiset<Long> toMultiset() {
        final Multiset<Long> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
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
        final LongMultiset<Long> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextLong());
        }

        return result;
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
    public <K, U, M extends Map<K, U>> M toMap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        long element = 0;

        while (elements.hasNext()) {
            element = elements.nextLong();
            Collectors.merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(LongFunction<? extends K> keyMapper, LongFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapFactory) {
        final Multimap<K, U, V> result = mapFactory.get();
        long element = 0;

        while (elements.hasNext()) {
            element = elements.nextLong();
            result.put(keyMapper.apply(element), valueMapper.apply(element));
        }

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
    public long head() {
        if (tail == null) {
            if (elements.hasNext() == false) {
                throw new NoSuchElementException();
            }

            head = elements.nextLong();
            tail = new IteratorLongStream(elements, closeHandlers, sorted);
        }

        return head;
    }

    @Override
    public LongStream tail() {
        if (tail == null) {
            if (elements.hasNext() == false) {
                throw new IllegalStateException();
            }

            head = elements.nextLong();
            tail = new IteratorLongStream(elements, closeHandlers, sorted);
        }

        return tail;
    }

    @Override
    public LongStream head2() {
        if (head2 == null) {
            if (elements.hasNext() == false) {
                throw new IllegalStateException();
            }

            final long[] a = elements.toArray();
            head2 = new ArrayLongStream(a, 0, a.length - 1, closeHandlers, sorted);
            tail2 = a[a.length - 1];
        }

        return head2;
    }

    @Override
    public long tail2() {
        if (head2 == null) {
            if (elements.hasNext() == false) {
                throw new NoSuchElementException();
            }

            final long[] a = elements.toArray();
            head2 = new ArrayLongStream(a, 0, a.length - 1, closeHandlers, sorted);
            tail2 = a[a.length - 1];
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

            if (N.compare(next, candidate) < 0) {
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

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalLong.of(candidate);
    }

    @Override
    public OptionalLong kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalLong.empty();
        }

        final NullabLe<Long> optional = boxed().kthLargest(k, LONG_COMPARATOR);

        return optional.isPresent() ? OptionalLong.of(optional.get()) : OptionalLong.empty();
    }

    @Override
    public Long sum() {
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
    public boolean anyMatch(LongPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextLong())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(LongPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextLong()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(LongPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextLong())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalLong findFirst(LongPredicate predicate) {
        while (elements.hasNext()) {
            long e = elements.nextLong();

            if (predicate.test(e)) {
                return OptionalLong.of(e);
            }
        }

        return OptionalLong.empty();
    }

    @Override
    public OptionalLong findLast(LongPredicate predicate) {
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
        return new IteratorFloatStream(new ExFloatIterator() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public Stream<Long> boxed() {
        return new IteratorStream<Long>(iterator(), closeHandlers, sorted, sorted ? LONG_COMPARATOR : null);
    }

    @Override
    ExLongIterator exIterator() {
        return elements;
    }

    @Override
    public LongStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelIteratorLongStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public LongStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorLongStream(elements, newCloseHandlers, sorted);
    }
}
