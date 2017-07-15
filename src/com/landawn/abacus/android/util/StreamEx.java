/*
 * Copyright (C) 2017 HaiYang Li
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

package com.landawn.abacus.android.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.annimon.stream.Collector;
import com.annimon.stream.Collectors;
import com.annimon.stream.DoubleStream;
import com.annimon.stream.IntStream;
import com.annimon.stream.LongStream;
import com.annimon.stream.Optional;
import com.annimon.stream.Stream;
import com.annimon.stream.function.BiConsumer;
import com.annimon.stream.function.BiFunction;
import com.annimon.stream.function.Consumer;
import com.annimon.stream.function.Function;
import com.annimon.stream.function.IntFunction;
import com.annimon.stream.function.Predicate;
import com.annimon.stream.function.Supplier;
import com.annimon.stream.function.ToDoubleFunction;
import com.annimon.stream.function.ToIntFunction;
import com.annimon.stream.function.ToLongFunction;
import com.annimon.stream.function.UnaryOperator;
import com.annimon.stream.operator.ObjConcat;
import com.landawn.abacus.util.ImmutableIterator;
import com.landawn.abacus.util.Indexed;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Nth;
import com.landawn.abacus.util.Pair;

/**
 * A simple wrapper for <a href="https://github.com/aNNiMON/Lightweight-Stream-API">Lightweight-Stream-API</a>
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 * 
 */
public final class StreamEx<T> {
    private static final Object NONE = new Object();

    @SuppressWarnings({ "rawtypes", "unchecked" })
    private static final StreamEx EMPTY = new StreamEx(Stream.empty());

    private final Stream<T> s;

    StreamEx(final Stream<T> s) {
        this.s = s;
    }

    @SuppressWarnings("unchecked")
    public static <T> StreamEx<T> empty() {
        return EMPTY;
    }

    /**
     * 
     * @param map
     * @return an empty Stream if the specified <code>map</code> is null. 
     */
    public static <K, V> StreamEx<Map.Entry<K, V>> of(final Map<K, V> map) {
        return map == null ? StreamEx.<Map.Entry<K, V>> empty() : new StreamEx<>(Stream.of(map));
    }

    /**
     * 
     * @param iterator
     * @return an empty Stream if the specified <code>iterator</code> is null.  
     */
    public static <T> StreamEx<T> of(final Iterator<? extends T> iterator) {
        return iterator == null ? StreamEx.<T> empty() : new StreamEx<>(Stream.of(iterator));
    }

    /**
     * 
     * @param iterable
     * @return an empty Stream if the specified <code>iterable</code> is null. 
     */
    public static <T> StreamEx<T> of(final Iterable<? extends T> iterable) {
        return iterable == null ? StreamEx.<T> empty() : new StreamEx<>(Stream.of(iterable));
    }

    /**
     * 
     * @param elements
     * @return an empty Stream if the specified <code>elements</code> is null. 
     */
    @SafeVarargs
    public static <T> StreamEx<T> of(final T... elements) {
        return elements == null ? StreamEx.<T> empty() : new StreamEx<>(Stream.of(elements));
    }

    /**
     * 
     * @param element
     * @return an empty Stream if the specified <code>element</code> is null. 
     */
    public static <T> StreamEx<T> ofNullable(final T element) {
        return (element == null) ? StreamEx.<T> empty() : new StreamEx<>(Stream.ofNullable(element));
    }

    public static StreamEx<Integer> range(final int from, final int to) {
        return new StreamEx<>(Stream.range(from, to));
    }

    public static StreamEx<Long> range(final long from, final long to) {
        return new StreamEx<>(Stream.range(from, to));
    }

    public static StreamEx<Integer> rangeClosed(final int from, final int to) {
        return new StreamEx<>(Stream.rangeClosed(from, to));
    }

    public static StreamEx<Long> rangeClosed(final long from, final long to) {
        return new StreamEx<>(Stream.rangeClosed(from, to));
    }

    public static <T> StreamEx<T> generate(final Supplier<T> supplier) {
        return new StreamEx<>(Stream.generate(supplier));
    }

    public static <T> StreamEx<T> iterate(final T seed, final UnaryOperator<T> op) {
        return new StreamEx<>(Stream.iterate(seed, op));
    }

    public static <T> StreamEx<T> iterate(final T seed, final Predicate<? super T> predicate, final UnaryOperator<T> op) {
        return new StreamEx<>(Stream.iterate(seed, predicate, op));
    }

    public static <T> StreamEx<T> concat(final Collection<? extends T> coll1, final Collection<? extends T> coll2) {
        return concat(coll1 == null ? ImmutableIterator.EMPTY : coll1.iterator(), coll2 == null ? ImmutableIterator.EMPTY : coll2.iterator());
    }

    public static <T> StreamEx<T> concat(final Iterator<? extends T> iter1, final Iterator<? extends T> iter2) {
        return new StreamEx<>(Stream.of(new ObjConcat<T>(iter1 == null ? ImmutableIterator.EMPTY : iter1, iter2 == null ? ImmutableIterator.EMPTY : iter2)));
    }

    public static <T> StreamEx<T> merge(final Collection<? extends T> coll1, final Collection<? extends T> coll2,
            final BiFunction<? super T, ? super T, Nth> selector) {
        return merge(coll1 == null ? ImmutableIterator.EMPTY : coll1.iterator(), coll2 == null ? ImmutableIterator.EMPTY : coll2.iterator(), selector);
    }

    public static <T> StreamEx<T> merge(final Iterator<? extends T> iter1, final Iterator<? extends T> iter2,
            final BiFunction<? super T, ? super T, Nth> selector) {
        N.requireNonNull(selector);

        if (iter1 == null) {
            return iter2 == null ? StreamEx.<T> empty() : of(iter2);
        } else if (iter2 == null) {
            return of(iter1);
        }

        return of(new ImmutableIterator<T>() {
            private Object first = NONE;
            private Object second = NONE;

            @Override
            public boolean hasNext() {
                return first != NONE || second != NONE || iter1.hasNext() || iter2.hasNext();
            }

            @Override
            public T next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                if (first == NONE && iter1.hasNext()) {
                    first = iter1.next();
                }

                if (second == NONE && iter2.hasNext()) {
                    second = iter2.next();
                }

                T result = null;

                if (second == NONE || (first != NONE && selector.apply((T) first, (T) second) == Nth.FIRST)) {
                    result = (T) first;
                    first = NONE;
                } else {
                    result = (T) second;
                    second = NONE;
                }

                return result;
            }
        });
    }

    public static <F, S, R> StreamEx<R> zip(final Collection<? extends F> coll1, final Collection<? extends S> coll2,
            final BiFunction<? super F, ? super S, ? extends R> combiner) {
        return zip(coll1 == null ? ImmutableIterator.EMPTY : coll1.iterator(), coll2 == null ? ImmutableIterator.EMPTY : coll2.iterator(), combiner);
    }

    public static <F, S, R> StreamEx<R> zip(final Iterator<? extends F> iter1, final Iterator<? extends S> iter2,
            final BiFunction<? super F, ? super S, ? extends R> combiner) {
        return new StreamEx<>(Stream.zip(iter1 == null ? ImmutableIterator.EMPTY : iter1, iter2 == null ? ImmutableIterator.EMPTY : iter2, combiner));
    }

    /**
     * 
     * @param c
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R> Pair<StreamEx<L>, StreamEx<R>> unzip(final Collection<? extends T> c, final BiConsumer<? super T, Pair<L, R>> unzip) {
        if (N.isNullOrEmpty(c)) {
            return Pair.of(StreamEx.<L> empty(), StreamEx.<R> empty());
        }

        final int len = c.size();
        final List<L> l = new ArrayList<L>(len);
        final List<R> r = new ArrayList<R>(len);
        final Pair<L, R> p = new Pair<>();

        for (T e : c) {
            unzip.accept(e, p);

            l.add(p.left);
            r.add(p.right);
        }

        return Pair.of(StreamEx.of(l), StreamEx.of(r));
    }

    /**
     * 
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R> Pair<StreamEx<L>, StreamEx<R>> unzip(final Iterator<? extends T> iter, final BiConsumer<? super T, Pair<L, R>> unzip) {
        if (iter == null) {
            return Pair.of(StreamEx.<L> empty(), StreamEx.<R> empty());
        }

        final List<L> l = new ArrayList<L>();
        final List<R> r = new ArrayList<R>();
        final Pair<L, R> p = new Pair<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), p);

                l.add(p.left);
                r.add(p.right);
            }
        }

        return Pair.of(StreamEx.of(l), StreamEx.of(r));
    }

    public Iterator<? extends T> iterator() {
        return s.iterator();
    }

    public StreamEx<T> filter(final Predicate<? super T> predicate) {
        return new StreamEx<>(s.filter(predicate));
    }

    public StreamEx<T> filterNot(final Predicate<? super T> predicate) {
        return new StreamEx<>(s.filterNot(predicate));
    }

    public <R> StreamEx<R> map(final Function<? super T, ? extends R> mapper) {
        return new StreamEx<>(s.map(mapper));
    }

    public IntStream mapToInt(final ToIntFunction<? super T> mapper) {
        return s.mapToInt(mapper);
    }

    public LongStream mapToLong(final ToLongFunction<? super T> mapper) {
        return s.mapToLong(mapper);
    }

    public DoubleStream mapToDouble(final ToDoubleFunction<? super T> mapper) {
        return s.mapToDouble(mapper);
    }

    public <R> StreamEx<R> flatMap(final Function<? super T, ? extends StreamEx<? extends R>> mapper) {
        final Function<T, Stream<R>> mapper2 = new Function<T, Stream<R>>() {
            @Override
            public Stream<R> apply(T t) {
                return Stream.of(mapper.apply(t).iterator());
            }
        };

        return new StreamEx<>(s.flatMap(mapper2));
    }

    public <R> StreamEx<R> flatMap2(final Function<? super T, ? extends Collection<? extends R>> mapper) {
        final Function<T, Stream<R>> mapper2 = new Function<T, Stream<R>>() {
            @Override
            public Stream<R> apply(T t) {
                return Stream.of(mapper.apply(t));
            }
        };

        return new StreamEx<>(s.flatMap(mapper2));
    }

    public <R> StreamEx<R> flatMap3(final Function<? super T, ? extends R[]> mapper) {
        final Function<T, Stream<R>> mapper2 = new Function<T, Stream<R>>() {
            @Override
            public Stream<R> apply(T t) {
                return Stream.of((R[]) mapper.apply(t));
            }
        };

        return new StreamEx<>(s.flatMap(mapper2));
    }

    public IntStream flatMapToInt(final Function<? super T, ? extends IntStream> mapper) {
        return s.flatMapToInt(mapper);
    }

    public LongStream flatMapToLong(final Function<? super T, ? extends LongStream> mapper) {
        return s.flatMapToLong(mapper);
    }

    public DoubleStream flatMapToDouble(final Function<? super T, ? extends DoubleStream> mapper) {
        return s.flatMapToDouble(mapper);
    }

    public StreamEx<Indexed<T>> indexed() {
        final Function<T, Indexed<T>> mapper = new Function<T, Indexed<T>>() {
            private int index = 0;

            @Override
            public Indexed<T> apply(T t) {
                return Indexed.of(t, index++);
            }
        };

        return map(mapper);
    }

    public StreamEx<T> distinct() {
        final Set<Object> set = new HashSet<>();

        final Predicate<T> predicate = new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return set.add(value);
            }
        };

        return filter(predicate);
    }

    public StreamEx<T> distinctBy(final Function<? super T, ?> keyExtractor) {
        final Set<Object> set = new HashSet<>();

        final Predicate<T> predicate = new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return set.add(keyExtractor.apply(value));
            }
        };

        return filter(predicate);
    }

    public StreamEx<T> sorted() {
        return new StreamEx<>(s.sorted());
    }

    public StreamEx<T> sorted(final Comparator<? super T> comparator) {
        return new StreamEx<>(s.sorted(comparator));
    }

    @SuppressWarnings("rawtypes")
    public StreamEx<T> sortedBy(final Function<? super T, ? extends Comparable> keyExtractor) {
        final Comparator<? super T> comparator = new Comparator<T>() {
            @Override
            public int compare(T o1, T o2) {
                return N.compare(keyExtractor.apply(o1), keyExtractor.apply(o2));
            }
        };

        return sorted(comparator);
    }

    public <K> StreamEx<Map.Entry<K, List<T>>> groupBy(final Function<? super T, ? extends K> classifier) {
        return of(collect(Collectors.<T, K> groupingBy(classifier)));
    }

    public <K, D> StreamEx<Map.Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier, final Collector<? super T, ?, D> downstream) {
        return of(collect(Collectors.groupingBy(classifier, downstream)));
    }

    public <K, D, M extends Map<K, D>> StreamEx<Map.Entry<K, D>> groupBy(final Function<? super T, ? extends K> classifier,
            final Collector<? super T, ?, D> downstream, final Supplier<M> mapFactory) {
        return of(collect(Collectors.groupingBy(classifier, mapFactory, downstream)));
    }

    public <K> StreamEx<List<T>> chunkBy(final Function<? super T, ? extends K> classifier) {
        return new StreamEx<>(s.chunkBy(classifier));
    }

    public StreamEx<List<T>> split(final int size) {
        return new StreamEx<>(s.slidingWindow(size, size));
    }

    public StreamEx<List<T>> sliding(final int windowSize) {
        return new StreamEx<>(s.slidingWindow(windowSize));
    }

    public StreamEx<List<T>> sliding(final int windowSize, final int stepWidth) {
        return new StreamEx<>(s.slidingWindow(windowSize, stepWidth));
    }

    public StreamEx<T> peek(final Consumer<? super T> action) {
        return new StreamEx<>(s.peek(action));
    }

    public StreamEx<T> scan(final BiFunction<T, T, T> accumulator) {
        return new StreamEx<>(s.scan(accumulator));
    }

    public <R> StreamEx<R> scan(final R identity, final BiFunction<? super R, ? super T, ? extends R> accumulator) {
        return new StreamEx<>(s.scan(identity, accumulator));
    }

    public StreamEx<T> takeUntil(final Predicate<? super T> stopPredicate) {
        return new StreamEx<>(s.takeUntil(stopPredicate));
    }

    public StreamEx<T> takeWhile(final Predicate<? super T> predicate) {
        return new StreamEx<>(s.takeWhile(predicate));
    }

    public StreamEx<T> dropWhile(final Predicate<? super T> predicate) {
        return new StreamEx<>(s.dropWhile(predicate));
    }

    public StreamEx<T> limit(final long maxSize) {
        return new StreamEx<>(s.limit(maxSize));
    }

    public StreamEx<T> skip(final long n) {
        return new StreamEx<>(s.skip(n));
    }

    public <R> R reduce(final R identity, final BiFunction<? super R, ? super T, ? extends R> accumulator) {
        return s.reduce(identity, accumulator);
    }

    public Optional<T> reduce(final BiFunction<T, T, T> accumulator) {
        return s.reduce(accumulator);
    }

    public <R> R collect(final Supplier<R> supplier, BiConsumer<R, ? super T> accumulator) {
        return s.collect(supplier, accumulator);
    }

    public <R> R collect(final Collector<? super T, ?, R> collector) {
        return s.collect(collector);
    }

    public Object[] toArray() {
        return s.toArray();
    }

    public <R> R[] toArray(final IntFunction<R[]> generator) {
        return s.toArray(generator);
    }

    public List<T> toList() {
        return s.toList();
    }

    public Set<T> toSet() {
        final Collector<T, ?, Set<T>> collector = Collectors.toSet();

        return collect(collector);
    }

    public <K> Map<K, T> toMap(final Function<? super T, ? extends K> keyMapper) {
        return collect(Collectors.toMap(keyMapper));
    }

    public <K, V> Map<K, V> toMap(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends V> valueMapper) {
        return collect(Collectors.toMap(keyMapper, valueMapper));
    }

    public <K, V, M extends Map<K, V>> M toMap(final Function<? super T, ? extends K> keyMapper, final Function<? super T, ? extends V> valueMapper,
            final Supplier<M> mapFactory) {
        return collect(Collectors.toMap(keyMapper, valueMapper, mapFactory));
    }

    public Optional<T> min(final Comparator<? super T> comparator) {
        return s.min(comparator);
    }

    public Optional<T> max(final Comparator<? super T> comparator) {
        return s.max(comparator);
    }

    public Optional<T> first() {
        return s.findFirst();
    }

    public Optional<T> last() {
        final Iterator<? extends T> iter = s.iterator();

        if (iter.hasNext() == false) {
            return Optional.empty();
        }

        T last = null;

        while (iter.hasNext()) {
            last = iter.next();
        }

        return Optional.of(last);
    }

    public boolean anyMatch(final Predicate<? super T> predicate) {
        return s.anyMatch(predicate);
    }

    public boolean allMatch(final Predicate<? super T> predicate) {
        return s.allMatch(predicate);
    }

    public boolean noneMatch(final Predicate<? super T> predicate) {
        return s.noneMatch(predicate);
    }

    public long count() {
        return s.count();
    }

    public void forEach(final Consumer<? super T> action) {
        s.forEach(action);
    }

    public <U> StreamEx<U> __(final Function<? super Stream<T>, Stream<U>> transfer) {
        return new StreamEx<>(transfer.apply(s));
    }
}
