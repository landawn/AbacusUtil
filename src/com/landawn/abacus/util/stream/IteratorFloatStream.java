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

import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.FloatSummaryStatistics;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.FloatBinaryOperator;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.function.FloatToDoubleFunction;
import com.landawn.abacus.util.function.FloatToIntFunction;
import com.landawn.abacus.util.function.FloatToLongFunction;
import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.ObjFloatConsumer;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToFloatFunction;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
final class IteratorFloatStream extends AbstractFloatStream {
    private final ImmutableFloatIterator elements;

    IteratorFloatStream(ImmutableFloatIterator values) {
        this(values, null);
    }

    IteratorFloatStream(ImmutableFloatIterator values, Collection<Runnable> closeHandlers) {
        this(values, closeHandlers, false);
    }

    IteratorFloatStream(ImmutableFloatIterator values, Collection<Runnable> closeHandlers, boolean sorted) {
        super(closeHandlers, sorted);

        this.elements = values;
    }

    @Override
    public FloatStream filter(final FloatPredicate predicate) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private boolean hasNext = false;
            private float next = 0;

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
            public float next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream takeWhile(final FloatPredicate predicate) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private float next = 0;

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
            public float next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream dropWhile(final FloatPredicate predicate) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private boolean hasNext = false;
            private float next = 0;
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
                    } else {
                        if (elements.hasNext()) {
                            next = elements.next();
                            hasNext = true;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public float next() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream map(final FloatUnaryOperator mapper) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
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
    public IntStream mapToInt(final FloatToIntFunction mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int next() {
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
    public LongStream mapToLong(final FloatToLongFunction mapper) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public long next() {
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
    public DoubleStream mapToDouble(final FloatToDoubleFunction mapper) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
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
    public <U> Stream<U> mapToObj(final FloatFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ImmutableIterator<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
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
    public FloatStream flatMap(final FloatFunction<? extends FloatStream> mapper) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private ImmutableFloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).floatIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public float next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public IntStream flatMapToInt(final FloatFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ImmutableIntIterator() {
            private ImmutableIntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).intIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public LongStream flatMapToLong(final FloatFunction<? extends LongStream> mapper) {
        return new IteratorLongStream(new ImmutableLongIterator() {
            private ImmutableLongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).longIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public long next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public DoubleStream flatMapToDouble(final FloatFunction<? extends DoubleStream> mapper) {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            private ImmutableDoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).doubleIterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public double next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        }, closeHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final FloatFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ImmutableIterator<T>() {
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    cur = mapper.apply(elements.next()).iterator();
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
    public Stream<FloatStream> split(final int size) {
        return new IteratorStream<FloatStream>(new ImmutableIterator<FloatStream>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public FloatStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final float[] a = new float[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return new ArrayFloatStream(a, 0, cnt, null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<FloatList> split0(final int size) {
        return new IteratorStream<FloatList>(new ImmutableIterator<FloatList>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public FloatList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final float[] a = new float[size];
                int cnt = 0;

                while (cnt < size && elements.hasNext()) {
                    a[cnt++] = elements.next();
                }

                return FloatList.of(a, cnt);
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<FloatStream> split(final U boundary, final BiFunction<? super Float, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<FloatStream>(new ImmutableIterator<FloatStream>() {
            private float next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public FloatStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final FloatList result = FloatList.of(N.EMPTY_FLOAT_ARRAY);

                if (hasNext == false) {
                    next = elements.next();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        preCondition = predicate.apply(next, boundary);
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else if (predicate.apply(next, boundary) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }

                        break;
                    }
                }

                return FloatStream.of(result.array(), 0, result.size());
            }

        }, closeHandlers);
    }

    @Override
    public <U> Stream<FloatList> split0(final U boundary, final BiFunction<? super Float, ? super U, Boolean> predicate,
            final Consumer<? super U> boundaryUpdate) {
        return new IteratorStream<FloatList>(new ImmutableIterator<FloatList>() {
            private float next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public FloatList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final FloatList result = FloatList.of(N.EMPTY_FLOAT_ARRAY);

                if (hasNext == false) {
                    next = elements.next();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        preCondition = predicate.apply(next, boundary);
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else if (predicate.apply(next, boundary) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.next() : 0;
                    } else {
                        if (boundaryUpdate != null) {
                            boundaryUpdate.accept(boundary);
                        }

                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<FloatStream> sliding(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<FloatStream>(new ImmutableIterator<FloatStream>() {
            private FloatList prev = null;

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
            public FloatStream next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                FloatList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;
                    final float[] dest = new float[windowSize];
                    N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                    result = FloatList.of(dest, cnt);
                } else {
                    result = new FloatList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.next());
                }

                prev = result;

                return new ArrayFloatStream(result.array(), 0, result.size(), null, sorted);
            }

        }, closeHandlers);
    }

    @Override
    public Stream<FloatList> sliding0(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<FloatList>(new ImmutableIterator<FloatList>() {
            private FloatList prev = null;

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
            public FloatList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                FloatList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;
                    final float[] dest = new float[windowSize];
                    N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                    result = FloatList.of(dest, cnt);
                } else {
                    result = new FloatList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.next());
                }

                return prev = result;
            }
        }, closeHandlers);
    }

    @Override
    public FloatStream top(int n) {
        return top(n, FLOAT_COMPARATOR);
    }

    @Override
    public FloatStream top(int n, Comparator<? super Float> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        return boxed().top(n, comparator).mapToFloat(new ToFloatFunction<Float>() {
            @Override
            public float applyAsFloat(Float value) {
                return value.floatValue();
            }
        });
    }

    @Override
    public FloatStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorFloatStream(new ImmutableFloatIterator() {
            float[] a = null;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < a.length;
            }

            @Override
            public float next() {
                if (a == null) {
                    sort();
                }

                if (cursor >= a.length) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    sort();
                }

                return a.length - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    sort();
                }

                cursor = n >= a.length - cursor ? a.length : cursor + (int) n;
            }

            @Override
            public float[] toArray() {
                if (a == null) {
                    sort();
                }

                if (cursor == 0) {
                    return a;
                } else {
                    return N.copyOfRange(a, cursor, a.length);
                }
            }

            private void sort() {
                a = elements.toArray();

                N.sort(a);
            }
        }, closeHandlers, true);
    }

    @Override
    public FloatStream peek(final FloatConsumer action) {
        return new IteratorFloatStream(new ImmutableFloatIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public float next() {
                final float next = elements.next();
                action.accept(next);
                return next;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize == Long.MAX_VALUE) {
            return this;
        }

        return new IteratorFloatStream(new ImmutableFloatIterator() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public float next() {
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
        }, closeHandlers, sorted);
    }

    @Override
    public FloatStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorFloatStream(new ImmutableFloatIterator() {
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
            public float next() {
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
            public float[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, closeHandlers, sorted);
    }

    @Override
    public void forEach(FloatConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.next());
        }
    }

    @Override
    public float[] toArray() {
        return elements.toArray();
    }

    @Override
    public FloatList toFloatList() {
        return FloatList.of(toArray());
    }

    @Override
    public List<Float> toList() {
        final List<Float> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public List<Float> toList(Supplier<? extends List<Float>> supplier) {
        final List<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Float> toSet() {
        final Set<Float> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Set<Float> toSet(Supplier<? extends Set<Float>> supplier) {
        final Set<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset() {
        final Multiset<Float> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public Multiset<Float> toMultiset(Supplier<? extends Multiset<Float>> supplier) {
        final Multiset<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset() {
        final LongMultiset<Float> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public LongMultiset<Float> toLongMultiset(Supplier<? extends LongMultiset<Float>> supplier) {
        final LongMultiset<Float> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.next());
        }

        return result;
    }

    @Override
    public <K, D, A, M extends Map<K, D>> M toMap(final FloatFunction<? extends K> classifier, final Collector<Float, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Float> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        float element = 0;

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
    public <K, U, M extends Map<K, U>> M toMap(FloatFunction<? extends K> keyMapper, FloatFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapSupplier) {
        final M result = mapSupplier.get();

        float element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            Collectors.merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(FloatFunction<? extends K> keyMapper, FloatFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapSupplier) {
        final Multimap<K, U, V> result = mapSupplier.get();

        float element = 0;

        while (elements.hasNext()) {
            element = elements.next();
            result.put(keyMapper.apply(element), valueMapper.apply(element));
        }

        return result;
    }

    @Override
    public float reduce(float identity, FloatBinaryOperator op) {
        float result = identity;

        while (elements.hasNext()) {
            result = op.applyAsFloat(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalFloat reduce(FloatBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        float result = elements.next();

        while (elements.hasNext()) {
            result = op.applyAsFloat(result, elements.next());
        }

        return OptionalFloat.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjFloatConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.next());
        }

        return result;
    }

    @Override
    public OptionalFloat min() {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        float candidate = elements.next();
        float next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) > 0) {
                candidate = next;
            }
        }

        return OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat max() {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        float candidate = elements.next();
        float next = 0;

        while (elements.hasNext()) {
            next = elements.next();

            if (N.compare(candidate, next) < 0) {
                candidate = next;
            }
        }

        return OptionalFloat.of(candidate);
    }

    @Override
    public OptionalFloat kthLargest(int k) {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        final OptionalNullable<Float> optional = boxed().kthLargest(k, FLOAT_COMPARATOR);

        return optional.isPresent() ? OptionalFloat.of(optional.get()) : OptionalFloat.empty();
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public FloatSummaryStatistics summarize() {
        final FloatSummaryStatistics result = new FloatSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.next());
        }

        return result;
    }

    @Override
    public boolean anyMatch(FloatPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(FloatPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(FloatPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.next())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalFloat findFirst(FloatPredicate predicate) {
        while (elements.hasNext()) {
            float e = elements.next();

            if (predicate.test(e)) {
                return OptionalFloat.of(e);
            }
        }

        return OptionalFloat.empty();
    }

    @Override
    public OptionalFloat findLast(FloatPredicate predicate) {
        if (elements.hasNext() == false) {
            return OptionalFloat.empty();
        }

        boolean hasResult = false;
        float e = 0;
        float result = 0;

        while (elements.hasNext()) {
            e = elements.next();

            if (predicate.test(e)) {
                result = e;
                hasResult = true;
            }
        }

        return hasResult ? OptionalFloat.of(result) : OptionalFloat.empty();
    }

    @Override
    public OptionalFloat findAny(FloatPredicate predicate) {
        while (elements.hasNext()) {
            float e = elements.next();

            if (predicate.test(e)) {
                return OptionalFloat.of(e);
            }
        }

        return OptionalFloat.empty();
    }

    @Override
    public DoubleStream asDoubleStream() {
        return new IteratorDoubleStream(new ImmutableDoubleIterator() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public double next() {
                return elements.next();
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
    public Stream<Float> boxed() {
        return new IteratorStream<Float>(iterator(), closeHandlers, sorted, sorted ? FLOAT_COMPARATOR : null);
    }

    @Override
    public ImmutableIterator<Float> iterator() {
        return new ImmutableIterator<Float>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public Float next() {
                return elements.next();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        };
    }

    @Override
    public ImmutableFloatIterator floatIterator() {
        return elements;
    }

    @Override
    public FloatStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelIteratorFloatStream(elements, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public FloatStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorFloatStream(elements, newCloseHandlers, sorted);
    }
}
