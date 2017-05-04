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
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.DoubleSummaryStatistics;
import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
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

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class ArrayDoubleStream extends AbstractDoubleStream {
    final double[] elements;
    final int fromIndex;
    final int toIndex;

    ArrayDoubleStream(final double[] values) {
        this(values, 0, values.length);
    }

    ArrayDoubleStream(final double[] values, final Collection<Runnable> closeHandlers) {
        this(values, 0, values.length, closeHandlers);
    }

    ArrayDoubleStream(final double[] values, final Collection<Runnable> closeHandlers, final boolean sorted) {
        this(values, 0, values.length, closeHandlers, sorted);
    }

    ArrayDoubleStream(final double[] values, final int fromIndex, final int toIndex) {
        this(values, fromIndex, toIndex, null);
    }

    ArrayDoubleStream(final double[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers) {
        this(values, fromIndex, toIndex, closeHandlers, false);
    }

    ArrayDoubleStream(final double[] values, final int fromIndex, final int toIndex, final Collection<Runnable> closeHandlers, final boolean sorted) {
        super(closeHandlers, sorted);

        checkFromToIndex(fromIndex, toIndex, values.length);

        this.elements = values;
        this.fromIndex = fromIndex;
        this.toIndex = toIndex;
    }

    @Override
    public DoubleStream filter(final DoublePredicate predicate) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
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
            public double nextDouble() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream takeWhile(final DoublePredicate predicate) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
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
            public double nextDouble() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream dropWhile(final DoublePredicate predicate) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
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
            public double nextDouble() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return elements[cursor++];
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream map(final DoubleUnaryOperator mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double nextDouble() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsDouble(elements[cursor++]);
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
            public double[] toArray() {
                final double[] a = new double[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsDouble(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final DoubleToIntFunction mapper) {
        return new IteratorIntStream(new ExIntIterator() {
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
                    a[i] = mapper.applyAsInt(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public LongStream mapToLong(final DoubleToLongFunction mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long nextLong() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsLong(elements[cursor++]);
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
            public long[] toArray() {
                final long[] a = new long[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsLong(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public FloatStream mapToFloat(final DoubleToFloatFunction mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float nextFloat() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return mapper.applyAsFloat(elements[cursor++]);
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
            public float[] toArray() {
                final float[] a = new float[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    a[i] = mapper.applyAsFloat(elements[cursor++]);
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final DoubleFunction<? extends U> mapper) {
        return new IteratorStream<U>(new ExIterator<U>() {
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

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

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
    public DoubleStream flatMap(final DoubleFunction<? extends DoubleStream> mapper) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            private int cursor = fromIndex;
            private DoubleIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
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
    public IntStream flatMapToInt(final DoubleFunction<? extends IntStream> mapper) {
        return new IteratorIntStream(new ExIntIterator() {
            private int cursor = fromIndex;
            private IntIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
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
    public LongStream flatMapToLong(final DoubleFunction<? extends LongStream> mapper) {
        return new IteratorLongStream(new ExLongIterator() {
            private int cursor = fromIndex;
            private LongIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
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
    public FloatStream flatMapToFloat(final DoubleFunction<? extends FloatStream> mapper) {
        return new IteratorFloatStream(new ExFloatIterator() {
            private int cursor = fromIndex;
            private FloatIterator cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).exIterator();
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
    public <T> Stream<T> flatMapToObj(final DoubleFunction<? extends Stream<T>> mapper) {
        return new IteratorStream<T>(new ExIterator<T>() {
            private int cursor = fromIndex;
            private Iterator<? extends T> cur = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && cursor < toIndex) {
                    cur = mapper.apply(elements[cursor++]).iterator();
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
    public Stream<DoubleStream> split(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<DoubleStream>(new ExIterator<DoubleStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new ArrayDoubleStream(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex), null, sorted);
            }
        }, closeHandlers);
    }

    @Override
    public Stream<DoubleList> split0(final int size) {
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<DoubleList>(new ExIterator<DoubleList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return new DoubleList(N.copyOfRange(elements, cursor, (cursor = size < toIndex - cursor ? cursor + size : toIndex)));
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<DoubleStream> split(final U identity, final BiFunction<? super Double, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<DoubleStream>(new ExIterator<DoubleStream>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.apply(elements[from], identity);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], identity) == preCondition) {
                        cursor++;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return new ArrayDoubleStream(elements, from, cursor, null, sorted);
            }
        }, closeHandlers);
    }

    @Override
    public <U> Stream<DoubleList> split0(final U identity, final BiFunction<? super Double, ? super U, Boolean> predicate,
            final Consumer<? super U> identityUpdate) {
        return new IteratorStream<DoubleList>(new ExIterator<DoubleList>() {
            private int cursor = fromIndex;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int from = cursor;

                while (cursor < toIndex) {
                    if (from == cursor) {
                        preCondition = predicate.apply(elements[from], identity);
                        cursor++;
                    } else if (predicate.apply(elements[cursor], identity) == preCondition) {
                        cursor++;
                    } else {
                        if (identityUpdate != null) {
                            identityUpdate.accept(identity);
                        }

                        break;
                    }
                }

                return new DoubleList(N.copyOfRange(elements, from, cursor));
            }
        }, closeHandlers);
    }

    @Override
    public Stream<DoubleStream> splitAt(final int n) {
        if (n < 0) {
            throw new IllegalArgumentException("'n' can't be negative");
        }

        final DoubleStream[] a = new DoubleStream[2];
        final int middleIndex = n < toIndex - fromIndex ? fromIndex + n : toIndex;
        a[0] = middleIndex == fromIndex ? DoubleStream.empty() : new ArrayDoubleStream(elements, fromIndex, middleIndex, null, sorted);
        a[1] = middleIndex == toIndex ? DoubleStream.empty() : new ArrayDoubleStream(elements, middleIndex, toIndex, null, sorted);

        return new ArrayStream<>(a, closeHandlers);
    }

    @Override
    public Stream<DoubleStream> splitBy(DoublePredicate where) {
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
    public Stream<DoubleStream> sliding(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<DoubleStream>(new ExIterator<DoubleStream>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final ArrayDoubleStream result = new ArrayDoubleStream(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex, null,
                        sorted);

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<DoubleList> sliding0(final int windowSize, final int increment) {
        if (windowSize < 1 || increment < 1) {
            throw new IllegalArgumentException("'windowSize' and 'increment' must not be less than 1");
        }

        return new IteratorStream<DoubleList>(new ExIterator<DoubleList>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleList next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final DoubleList result = DoubleList.of(N.copyOfRange(elements, cursor, windowSize < toIndex - cursor ? cursor + windowSize : toIndex));

                cursor = increment < toIndex - cursor && windowSize < toIndex - cursor ? cursor + increment : toIndex;

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public DoubleStream top(int n) {
        return top(n, DOUBLE_COMPARATOR);
    }

    @Override
    public DoubleStream top(int n, Comparator<? super Double> comparator) {
        if (n < 1) {
            throw new IllegalArgumentException("'n' can not be less than 1");
        }

        if (n >= toIndex - fromIndex) {
            return this;
        } else if (sorted && isSameComparator(comparator, DOUBLE_COMPARATOR)) {
            return new ArrayDoubleStream(elements, toIndex - n, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayDoubleStream(N.top(elements, fromIndex, toIndex, n, comparator), closeHandlers, sorted);
        }
    }

    @Override
    public DoubleStream sorted() {
        if (sorted) {
            return this;
        }

        final double[] a = N.copyOfRange(elements, fromIndex, toIndex);
        N.sort(a);
        return new ArrayDoubleStream(a, closeHandlers, true);
    }

    @Override
    public DoubleStream peek(final DoubleConsumer action) {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double nextDouble() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                action.accept(elements[cursor]);

                return elements[cursor++];
            }

            @Override
            public double[] toArray() {
                final double[] a = new double[toIndex - cursor];

                for (int i = 0, len = toIndex - cursor; i < len; i++) {
                    action.accept(elements[cursor]);

                    a[i] = elements[cursor++];
                }

                return a;
            }
        }, closeHandlers, sorted);
    }

    @Override
    public DoubleStream limit(long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        } else if (maxSize >= toIndex - fromIndex) {
            return this;
        }

        return new ArrayDoubleStream(elements, fromIndex, (int) (fromIndex + maxSize), closeHandlers, sorted);
    }

    @Override
    public DoubleStream skip(long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        if (n >= toIndex - fromIndex) {
            return new ArrayDoubleStream(elements, toIndex, toIndex, closeHandlers, sorted);
        } else {
            return new ArrayDoubleStream(elements, (int) (fromIndex + n), toIndex, closeHandlers, sorted);
        }
    }

    @Override
    public void forEach(DoubleConsumer action) {
        for (int i = fromIndex; i < toIndex; i++) {
            action.accept(elements[i]);
        }
    }

    @Override
    public double[] toArray() {
        return N.copyOfRange(elements, fromIndex, toIndex);
    }

    @Override
    public DoubleList toDoubleList() {
        return DoubleList.of(N.copyOfRange(elements, fromIndex, toIndex));
    }

    @Override
    public List<Double> toList() {
        final List<Double> result = new ArrayList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends List<Double>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Set<Double> toSet() {
        final Set<Double> result = new HashSet<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <R extends Set<Double>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset() {
        final Multiset<Double> result = new Multiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public Multiset<Double> toMultiset(Supplier<? extends Multiset<Double>> supplier) {
        final Multiset<Double> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset() {
        final LongMultiset<Double> result = new LongMultiset<>(N.min(9, N.initHashCapacity(toIndex - fromIndex)));

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public LongMultiset<Double> toLongMultiset(Supplier<? extends LongMultiset<Double>> supplier) {
        final LongMultiset<Double> result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(elements[i]);
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            Collectors.merge(result, keyExtractor.apply(elements[i]), valueMapper.apply(elements[i]), mergeFunction);
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
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper,
            Supplier<Multimap<K, U, V>> mapFactory) {
        final Multimap<K, U, V> result = mapFactory.get();

        for (int i = fromIndex; i < toIndex; i++) {
            result.put(keyExtractor.apply(elements[i]), valueMapper.apply(elements[i]));
        }

        return result;
    }

    @Override
    public OptionalDouble first() {
        return fromIndex < toIndex ? OptionalDouble.of(elements[fromIndex]) : OptionalDouble.empty();
    }

    @Override
    public OptionalDouble last() {
        return fromIndex < toIndex ? OptionalDouble.of(elements[toIndex - 1]) : OptionalDouble.empty();
    }

    @Override
    public double reduce(double identity, DoubleBinaryOperator op) {
        double result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = op.applyAsDouble(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalDouble reduce(DoubleBinaryOperator op) {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        }

        double result = elements[fromIndex];

        for (int i = fromIndex + 1; i < toIndex; i++) {
            result = op.applyAsDouble(result, elements[i]);
        }

        return OptionalDouble.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjDoubleConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        for (int i = fromIndex; i < toIndex; i++) {
            accumulator.accept(result, elements[i]);
        }

        return result;
    }

    @Override
    public OptionalDouble head() {
        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(elements[fromIndex]);
    }

    @Override
    public DoubleStream tail() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ArrayDoubleStream(elements, fromIndex + 1, toIndex, closeHandlers, sorted);
    }

    @Override
    public DoubleStream head2() {
        if (fromIndex == toIndex) {
            return this;
        }

        return new ArrayDoubleStream(elements, fromIndex, toIndex - 1, closeHandlers, sorted);
    }

    @Override
    public OptionalDouble tail2() {
        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(elements[toIndex - 1]);
    }

    @Override
    public OptionalDouble min() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements[fromIndex]);
        }

        return OptionalDouble.of(N.min(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalDouble max() {
        if (fromIndex == toIndex) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements[toIndex - 1]);
        }

        return OptionalDouble.of(N.max(elements, fromIndex, toIndex));
    }

    @Override
    public OptionalDouble kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (k > toIndex - fromIndex) {
            return OptionalDouble.empty();
        } else if (sorted) {
            return OptionalDouble.of(elements[toIndex - k]);
        }

        return OptionalDouble.of(N.kthLargest(elements, fromIndex, toIndex, k));
    }

    @Override
    public long count() {
        return toIndex - fromIndex;
    }

    @Override
    public DoubleStream reversed() {
        return new IteratorDoubleStream(new ExDoubleIterator() {
            private int cursor = toIndex;

            @Override
            public boolean hasNext() {
                return cursor > fromIndex;
            }

            @Override
            public double nextDouble() {
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
            public double[] toArray() {
                final double[] a = new double[cursor - fromIndex];

                for (int i = 0, len = a.length; i < len; i++) {
                    a[i] = elements[cursor - i - 1];
                }

                return a;
            }
        }, closeHandlers);
    }

    @Override
    public DoubleSummaryStatistics summarize() {
        final DoubleSummaryStatistics result = new DoubleSummaryStatistics();

        for (int i = fromIndex; i < toIndex; i++) {
            result.accept(elements[i]);
        }

        return result;
    }

    @Override
    public boolean anyMatch(final DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(final DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i]) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(final DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalDouble findFirst(final DoublePredicate predicate) {
        for (int i = fromIndex; i < toIndex; i++) {
            if (predicate.test(elements[i])) {
                return OptionalDouble.of(elements[i]);
            }
        }

        return OptionalDouble.empty();
    }

    @Override
    public OptionalDouble findLast(final DoublePredicate predicate) {
        for (int i = toIndex - 1; i >= fromIndex; i--) {
            if (predicate.test(elements[i])) {
                return OptionalDouble.of(elements[i]);
            }
        }

        return OptionalDouble.empty();
    }

    @Override
    public Stream<Double> boxed() {
        return new IteratorStream<Double>(iterator(), closeHandlers, sorted, sorted ? DOUBLE_COMPARATOR : null);
    }

    @Override
    public DoubleStream cached() {
        return this;
    }

    @Override
    ExDoubleIterator exIterator() {
        return ExDoubleIterator.of(elements, fromIndex, toIndex);
    }

    @Override
    public DoubleStream parallel(int maxThreadNum, Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelArrayDoubleStream(elements, fromIndex, toIndex, closeHandlers, sorted, maxThreadNum, splitor);
    }

    @Override
    public DoubleStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new ArrayDoubleStream(elements, fromIndex, toIndex, newCloseHandlers, sorted);
    }
}
