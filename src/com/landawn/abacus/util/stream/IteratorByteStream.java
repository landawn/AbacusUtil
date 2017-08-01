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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.ByteSummaryStatistics;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalByte;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteToIntFunction;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.ObjByteConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class IteratorByteStream extends AbstractByteStream {
    ExByteIterator elements;

    OptionalByte head;
    ByteStream tail;

    ByteStream head2;
    OptionalByte tail2;

    IteratorByteStream(final ByteIterator values) {
        this(values, null);
    }

    IteratorByteStream(final ByteIterator values, final Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorByteStream(final ByteIterator values, final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);

        ExByteIterator tmp = null;

        if (values instanceof ExByteIterator) {
            tmp = (ExByteIterator) values;
        } else if (values instanceof SkippableIterator) {
            tmp = new ExByteIterator() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public byte nextByte() {
                    return values.nextByte();
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
            tmp = new ExByteIterator() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public byte nextByte() {
                    return values.nextByte();
                }
            };
        }

        this.elements = tmp;
    }

    @Override
    public ByteStream filter(final BytePredicate predicate) {
        return new IteratorByteStream(new ExByteIterator() {
            private boolean hasNext = false;
            private byte next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    while (elements.hasNext()) {
                        next = elements.nextByte();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public byte nextByte() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream takeWhile(final BytePredicate predicate) {
        return new IteratorByteStream(new ExByteIterator() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private byte next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.nextByte();

                    if (predicate.test(next)) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public byte nextByte() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream dropWhile(final BytePredicate predicate) {
        return new IteratorByteStream(new ExByteIterator() {
            private boolean hasNext = false;
            private byte next = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.nextByte();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
                    } else if (elements.hasNext()) {
                        next = elements.nextByte();
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public byte nextByte() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream map(final ByteUnaryOperator mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public byte nextByte() {
                return mapper.applyAsByte(elements.nextByte());
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
    public IntStream mapToInt(final ByteToIntFunction mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextByte());
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
    public <U> Stream<U> mapToObj(final ByteFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ExIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextByte());
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
    public ByteStream flatMap(final ByteFunction<? extends ByteStream> mapper) {
        return new IteratorByteStream(new ExByteIterator() {
            private ByteIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextByte()).exIterator();
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
    public IntStream flatMapToInt(final ByteFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextByte()).exIterator();
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
    public <T> Stream<T> flatMapToObj(final ByteFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ExIterator<T>() {
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.nextByte()).iterator();
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
    public Stream<ByteList> splitToList(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<ByteList>(new ExIterator<ByteList>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public ByteList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final ByteList result = new ByteList(size);

                while (result.size() < size && elements.hasNext()) {
                    result.add(elements.nextByte());
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<ByteList> splitToList(final BytePredicate predicate) {
        return new IteratorStream<ByteList>(new ExIterator<ByteList>() {
            private byte next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public ByteList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final ByteList result = new ByteList();

                if (hasNext == false) {
                    next = elements.nextByte();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextByte() : 0;
                    } else if (predicate.test(next) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextByte() : 0;
                    } else {
                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<ByteList> splitToList(final U identity, final BiFunction<? super Byte, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<ByteList>(new ExIterator<ByteList>() {
            private byte next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public ByteList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final ByteList result = new ByteList();

                if (hasNext == false) {
                    next = elements.nextByte();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.apply(next, identity);
                        next = (hasNext = elements.hasNext()) ? elements.nextByte() : 0;
                    } else if (predicate.apply(next, identity) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextByte() : 0;
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
    public Stream<ByteList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<ByteList>(new ExIterator<ByteList>() {
            private ByteList prev = null;

            @Override
            public boolean hasNext() {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.nextByte();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public ByteList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                ByteList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;

                    if (cnt <= 8) {
                        result = new ByteList(windowSize);

                        for (int i = windowSize - cnt; i < windowSize; i++) {
                            result.add(prev.get(i));
                        }
                    } else {
                        final byte[] dest = new byte[windowSize];
                        N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                        result = ByteList.of(dest, cnt);
                    }
                } else {
                    result = new ByteList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.nextByte());
                }

                return prev = result;
            }

        }, closeHandlers);
    }

    @Override
    public ByteStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorByteStream(new ExByteIterator() {
            byte[] a = null;
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
            public byte nextByte() {
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
            public byte[] toArray() {
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
    public ByteStream peek(final ByteConsumer action) {
        return new IteratorByteStream(new ExByteIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public byte nextByte() {
                final byte next = elements.nextByte();

                action.accept(next);
                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new IteratorByteStream(new ExByteIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public byte nextByte() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextByte();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, closeHandlers, sorted);
    }

    @Override
    public ByteStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorByteStream(new ExByteIterator() {
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
            public byte nextByte() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextByte();
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
            public byte[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(ByteConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.nextByte());
        }
    }

    @Override
    public byte[] toArray() {
        return elements.toArray();
    }

    @Override
    public ByteList toByteList() {
        return ByteList.of(toArray());
    }

    @Override
    public List<Byte> toList() {
        final List<Byte> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public <R extends List<Byte>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public Set<Byte> toSet() {
        final Set<Byte> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public <R extends Set<Byte>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset() {
        final Multiset<Byte> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public Multiset<Byte> toMultiset(Supplier<? extends Multiset<Byte>> supplier) {
        final Multiset<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset() {
        final LongMultiset<Byte> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public LongMultiset<Byte> toLongMultiset(Supplier<? extends LongMultiset<Byte>> supplier) {
        final LongMultiset<Byte> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextByte());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        byte element = 0;

        while (elements.hasNext()) {
            element = elements.nextByte();
            Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final ByteFunction<? extends K> classifier, final Collector<Byte, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Byte> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        byte element = 0;

        while (elements.hasNext()) {
            element = elements.nextByte();
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
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(ByteFunction<? extends K> keyExtractor, ByteFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapFactory) {
        final Multimap<K, U, V> result = mapFactory.get();
        byte element = 0;

        while (elements.hasNext()) {
            element = elements.nextByte();
            result.put(keyExtractor.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public byte reduce(byte identity, ByteBinaryOperator op) {
        byte result = identity;

        while (elements.hasNext()) {
            result = op.applyAsByte(result, elements.nextByte());
        }

        return result;
    }

    @Override
    public OptionalByte reduce(ByteBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        byte result = elements.nextByte();

        while (elements.hasNext()) {
            result = op.applyAsByte(result, elements.nextByte());
        }

        return OptionalByte.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjByteConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.nextByte());
        }

        return result;
    }

    @Override
    public OptionalByte head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalByte.of(elements.nextByte()) : OptionalByte.empty();
            tail = new IteratorByteStream(elements, closeHandlers, sorted);
        }

        return head;
    }

    @Override
    public ByteStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalByte.of(elements.nextByte()) : OptionalByte.empty();
            tail = new IteratorByteStream(elements, closeHandlers, sorted);
        }

        return tail;
    }

    @Override
    public ByteStream head2() {
        if (head2 == null) {
            final byte[] a = elements.toArray();
            head2 = new ArrayByteStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted);
            tail2 = a.length == 0 ? OptionalByte.empty() : OptionalByte.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalByte tail2() {
        if (tail2 == null) {
            final byte[] a = elements.toArray();
            head2 = new ArrayByteStream(a, 0, a.length == 0 ? 0 : a.length - 1, closeHandlers, sorted);
            tail2 = a.length == 0 ? OptionalByte.empty() : OptionalByte.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalByte min() {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        } else if (sorted) {
            return OptionalByte.of(elements.nextByte());
        }

        byte candidate = elements.nextByte();
        byte next = 0;

        while (elements.hasNext()) {
            next = elements.nextByte();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalByte.of(candidate);
    }

    @Override
    public OptionalByte max() {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        } else if (sorted) {
            byte next = 0;

            while (elements.hasNext()) {
                next = elements.nextByte();
            }

            return OptionalByte.of(next);
        }

        byte candidate = elements.nextByte();
        byte next = 0;

        while (elements.hasNext()) {
            next = elements.nextByte();

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalByte.of(candidate);
    }

    @Override
    public OptionalByte kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        final NullabLe<Byte> optional = boxed().kthLargest(k, BYTE_COMPARATOR);

        return optional.isPresent() ? OptionalByte.of(optional.get()) : OptionalByte.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextByte();
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
            sum += elements.nextByte();
            count++;
        }

        return OptionalDouble.of(((double) sum) / count);
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public ByteSummaryStatistics summarize() {
        final ByteSummaryStatistics result = new ByteSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextByte());
        }

        return result;
    }

    @Override
    public boolean anyMatch(BytePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextByte())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(BytePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextByte()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(BytePredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextByte())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalByte findFirst(BytePredicate predicate) {
        while (elements.hasNext()) {
            byte e = elements.nextByte();

            if (predicate.test(e)) {
                return OptionalByte.of(e);
            }
        }

        return OptionalByte.empty();
    }

    @Override
    public OptionalByte findLast(BytePredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalByte.empty();
        }

        boolean hasResult = false;
        byte e = 0;
        byte result = 0;

        while (elements.hasNext()) {
            e = elements.nextByte();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalByte.of(result) : OptionalByte.empty();
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
                return elements.nextByte();
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
    public Stream<Byte> boxed() {
        return new IteratorStream<Byte>(iterator(), closeHandlers, sorted, sorted ? BYTE_COMPARATOR : null);
    }

    @Override
    ExByteIterator exIterator() {
        return elements;
    }

    @Override
    public ByteStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelIteratorByteStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public ByteStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorByteStream(elements, newCloseHandlers, sorted);
    }
}
