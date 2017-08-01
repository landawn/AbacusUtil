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
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.ShortSummaryStatistics;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class IteratorShortStream extends AbstractShortStream {
    final ExShortIterator elements;

    OptionalShort head;
    ShortStream tail;

    ShortStream head2;
    OptionalShort tail2;

    IteratorShortStream(final ShortIterator values) {
        this(values, null);
    }

    IteratorShortStream(final ShortIterator values, final Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorShortStream(final ShortIterator values, final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);

        ExShortIterator tmp = null;

        if (values instanceof ExShortIterator) {
            tmp = (ExShortIterator) values;
        } else if (values instanceof SkippableIterator) {
            tmp = new ExShortIterator() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public short nextShort() {
                    return values.nextShort();
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
            tmp = new ExShortIterator() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public short nextShort() {
                    return values.nextShort();
                }
            };
        }

        this.elements = tmp;
    }

    @Override
    public ShortStream filter(final ShortPredicate predicate) {
        return new IteratorShortStream(new ExShortIterator() {
            private boolean hasNext = false;
            private short next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    while (elements.hasNext()) {
                        next = elements.nextShort();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
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

                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream takeWhile(final ShortPredicate predicate) {
        return new IteratorShortStream(new ExShortIterator() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private short next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.nextShort();

                    if (predicate.test(next)) {
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

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream dropWhile(final ShortPredicate predicate) {
        return new IteratorShortStream(new ExShortIterator() {
            private boolean hasNext = false;
            private short next = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.nextShort();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else if (elements.hasNext()) {
                        next = elements.nextShort();
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

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream map(final ShortUnaryOperator mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short nextShort() {
                return mapper.applyAsShort(elements.nextShort());
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
    public IntStream mapToInt(final ShortToIntFunction mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextShort());
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
    public <U> Stream<U> mapToObj(final ShortFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ExIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextShort());
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
    public ShortStream flatMap(final ShortFunction<? extends ShortStream> mapper) {
        return new IteratorShortStream(new ExShortIterator() {
            private ShortIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextShort()).exIterator();
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
    public IntStream flatMapToInt(final ShortFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextShort()).exIterator();
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
    public <T> Stream<T> flatMapToObj(final ShortFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ExIterator<T>() {
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextShort()).iterator();
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
    public Stream<ShortList> splitToList(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<ShortList>(new ExIterator<ShortList>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public ShortList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final ShortList result = new ShortList(size);

                while (result.size() < size && elements.hasNext()) {
                    result.add(elements.nextShort());
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<ShortList> splitToList(final ShortPredicate predicate) {
        return new IteratorStream<ShortList>(new ExIterator<ShortList>() {
            private short next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public ShortList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final ShortList result = new ShortList();

                if (hasNext == false) {
                    next = elements.nextShort();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextShort() : 0;
                    } else if (predicate.test(next) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextShort() : 0;
                    } else {
                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<ShortList> splitToList(final U identity, final BiFunction<? super Short, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<ShortList>(new ExIterator<ShortList>() {
            private short next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public ShortList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final ShortList result = new ShortList();

                if (hasNext == false) {
                    next = elements.nextShort();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.apply(next, identity);
                        next = (hasNext = elements.hasNext()) ? elements.nextShort() : 0;
                    } else if (predicate.apply(next, identity) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextShort() : 0;
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
    public Stream<ShortList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<ShortList>(new ExIterator<ShortList>() {
            private ShortList prev = null;

            @Override
            public boolean hasNext() {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.nextShort();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public ShortList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                ShortList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;

                    if (cnt <= 8) {
                        result = new ShortList(windowSize);

                        for (int i = windowSize - cnt; i < windowSize; i++) {
                            result.add(prev.get(i));
                        }
                    } else {
                        final short[] dest = new short[windowSize];
                        N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                        result = ShortList.of(dest, cnt);
                    }
                } else {
                    result = new ShortList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.nextShort());
                }

                return prev = result;
            }

        }, closeHandlers);
    }

    @Override
    public ShortStream top(int n) {
        return top(n, SHORT_COMPARATOR);
    }

    @Override
    public ShortStream top(int n, Comparator<? super Short> comparator) {
        return boxed().top(n, comparator).mapToShort(ToShortFunction.UNBOX);
    }

    @Override
    public ShortStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorShortStream(new ExShortIterator() {
            short[] a = null;
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
            public short nextShort() {
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
            public short[] toArray() {
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
    public ShortStream peek(final ShortConsumer action) {
        return new IteratorShortStream(new ExShortIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public short nextShort() {
                final short next = elements.nextShort();
                action.accept(next);
                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new IteratorShortStream(new ExShortIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public short nextShort() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextShort();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ShortStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorShortStream(new ExShortIterator() {
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
            public short nextShort() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextShort();
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
            public short[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(ShortConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.nextShort());
        }
    }

    @Override
    public short[] toArray() {
        return elements.toArray();
    }

    @Override
    public ShortList toShortList() {
        return ShortList.of(toArray());
    }

    @Override
    public List<Short> toList() {
        final List<Short> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public <R extends List<Short>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public Set<Short> toSet() {
        final Set<Short> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public <R extends Set<Short>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset() {
        final Multiset<Short> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public Multiset<Short> toMultiset(Supplier<? extends Multiset<Short>> supplier) {
        final Multiset<Short> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset() {
        final LongMultiset<Short> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public LongMultiset<Short> toLongMultiset(Supplier<? extends LongMultiset<Short>> supplier) {
        final LongMultiset<Short> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextShort());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ShortFunction<? extends K> keyExtractor, ShortFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        short element = 0;

        while (elements.hasNext()) {
            element = elements.nextShort();
            Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
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
        short element = 0;

        while (elements.hasNext()) {
            element = elements.nextShort();
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
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(ShortFunction<? extends K> keyExtractor, ShortFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapFactory) {
        final Multimap<K, U, V> result = mapFactory.get();
        short element = 0;

        while (elements.hasNext()) {
            element = elements.nextShort();
            result.put(keyExtractor.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public short reduce(short identity, ShortBinaryOperator op) {
        short result = identity;

        while (elements.hasNext()) {
            result = op.applyAsShort(result, elements.nextShort());
        }

        return result;
    }

    @Override
    public OptionalShort reduce(ShortBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalShort.empty();
        }

        short result = elements.nextShort();

        while (elements.hasNext()) {
            result = op.applyAsShort(result, elements.nextShort());
        }

        return OptionalShort.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjShortConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.nextShort());
        }

        return result;
    }

    @Override
    public OptionalShort head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalShort.of(elements.nextShort()) : OptionalShort.empty();
            tail = new IteratorShortStream(elements, closeHandlers, sorted);
        }

        return head;
    }

    @Override
    public ShortStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalShort.of(elements.nextShort()) : OptionalShort.empty();
            tail = new IteratorShortStream(elements, closeHandlers, sorted);
        }

        return tail;
    }

    @Override
    public ShortStream head2() {
        if (head2 == null) {
            final short[] a = elements.toArray();
            head2 = new ArrayShortStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted);
            tail2 = a.length == 0 ? OptionalShort.empty() : OptionalShort.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalShort tail2() {
        if (tail2 == null) {
            final short[] a = elements.toArray();
            head2 = new ArrayShortStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted);
            tail2 = a.length == 0 ? OptionalShort.empty() : OptionalShort.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalShort min() {
        if (elements.hasNext() == false) {
            return OptionalShort.empty();
        } else if (sorted) {
            return OptionalShort.of(elements.nextShort());
        }

        short candidate = elements.nextShort();
        short next = 0;

        while (elements.hasNext()) {
            next = elements.nextShort();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalShort.of(candidate);
    }

    @Override
    public OptionalShort max() {
        if (elements.hasNext() == false) {
            return OptionalShort.empty();
        } else if (sorted) {
            short next = 0;

            while (elements.hasNext()) {
                next = elements.nextShort();
            }

            return OptionalShort.of(next);
        }

        short candidate = elements.nextShort();
        short next = 0;

        while (elements.hasNext()) {
            next = elements.nextShort();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalShort.of(candidate);
    }

    @Override
    public OptionalShort kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalShort.empty();
        }

        final NullabLe<Short> optional = boxed().kthLargest(k, SHORT_COMPARATOR);

        return optional.isPresent() ? OptionalShort.of(optional.get()) : OptionalShort.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextShort();
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
            sum += elements.nextShort();
            count++;
        }

        return OptionalDouble.of(((double) sum) / count);
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public ShortSummaryStatistics summarize() {
        final ShortSummaryStatistics result = new ShortSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextShort());
        }

        return result;
    }

    @Override
    public boolean anyMatch(ShortPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextShort())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(ShortPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextShort()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(ShortPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextShort())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalShort findFirst(ShortPredicate predicate) {
        while (elements.hasNext()) {
            short e = elements.nextShort();

            if (predicate.test(e)) {
                return OptionalShort.of(e);
            }
        }

        return OptionalShort.empty();
    }

    @Override
    public OptionalShort findLast(ShortPredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalShort.empty();
        }

        boolean hasResult = false;
        short e = 0;
        short result = 0;

        while (elements.hasNext()) {
            e = elements.nextShort();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalShort.of(result) : OptionalShort.empty();
    }

    @Override
    public IntStream asIntStream() {
        return new IteratorIntStream(new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return elements.nextShort();
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
    public Stream<Short> boxed() {
        return new IteratorStream<Short>(iterator(), closeHandlers, sorted, sorted ? SHORT_COMPARATOR : null);
    }

    @Override
    ExShortIterator exIterator() {
        return elements;
    }

    @Override
    public ShortStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelIteratorShortStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public ShortStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorShortStream(elements, newCloseHandlers, sorted);
    }
}
