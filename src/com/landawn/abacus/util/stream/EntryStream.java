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

package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Random;

import com.landawn.abacus.util.Comparators;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.ImmutableIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;

/**
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 */
public final class EntryStream<K, V> {

    @SuppressWarnings("rawtypes")
    private static final EntryStream EMPTY = of(new Map.Entry[0]);

    private final Stream<Map.Entry<K, V>> s;

    EntryStream(final Stream<? extends Map.Entry<K, V>> s) {
        this.s = (Stream<Map.Entry<K, V>>) s;
    }

    public static <K, V> EntryStream<K, V> empty() {
        return EMPTY;
    }

    public static <K, V> EntryStream<K, V> of(final Stream<? extends Map.Entry<K, V>> s) {
        return new EntryStream<K, V>(s);
    }

    public static <K, V> EntryStream<K, V> of(final Iterator<? extends Map.Entry<K, V>> iterator) {
        return new EntryStream<K, V>(Stream.of(iterator));
    }

    public static <K, V> EntryStream<K, V> of(final Map<K, V> map) {
        return new EntryStream<K, V>(Stream.of(map));
    }

    public static <K, V> EntryStream<K, V> of(final Collection<? extends Map.Entry<K, V>> entries) {
        return new EntryStream<K, V>(Stream.of(entries));
    }

    @SafeVarargs
    public static <K, V> EntryStream<K, V> of(final Map.Entry<K, V>... entries) {
        return new EntryStream<K, V>(Stream.of(entries));
    }

    public static <E> EntryStream<E, Integer> of(final Multiset<E> multiset) {
        final Function<Map.Entry<E, Integer>, Map.Entry<E, Integer>> mapper = Fn.identity();

        return multiset == null ? EntryStream.<E, Integer> empty() : multiset.stream().mapToEntry(mapper);
    }

    public static <E> EntryStream<E, Long> of(final LongMultiset<E> multiset) {
        final Function<Map.Entry<E, Long>, Map.Entry<E, Long>> mapper = Fn.identity();

        return multiset == null ? EntryStream.<E, Long> empty() : multiset.stream().mapToEntry(mapper);
    }

    public static <K, E, V extends Collection<E>> EntryStream<K, V> of(final Multimap<K, E, V> mulitmap) {
        final Function<Map.Entry<K, V>, Map.Entry<K, V>> mapper = Fn.identity();

        return mulitmap == null ? EntryStream.<K, V> empty() : mulitmap.stream().mapToEntry(mapper);
    }

    public static <K, T> EntryStream<K, T> of(final Collection<? extends T> c, final Function<? super T, K> keyExtractor) {
        final Function<T, T> valueMapper = Fn.identity();

        return Stream.of(c).mapToEntry(keyExtractor, valueMapper);
    }

    public static <K, T> EntryStream<K, T> of(final T[] a, final Function<? super T, K> keyExtractor) {
        final Function<T, T> valueMapper = Fn.identity();

        return Stream.of(a).mapToEntry(keyExtractor, valueMapper);
    }

    public Stream<K> keys() {
        final Function<Map.Entry<K, V>, K> func = Fn.key();

        return s.map(func);
    }

    public Stream<V> values() {
        final Function<Map.Entry<K, V>, V> func = Fn.value();

        return s.map(func);
    }

    public Stream<Map.Entry<K, V>> entries() {
        return s;
    }

    public EntryStream<V, K> inversed() {
        final Function<Map.Entry<K, V>, Map.Entry<V, K>> mapper = new Function<Map.Entry<K, V>, Map.Entry<V, K>>() {
            @Override
            public Entry<V, K> apply(Entry<K, V> e) {
                return Pair.of(e.getValue(), e.getKey());
            }
        };

        return map(mapper);
    }

    public <KK> EntryStream<K, V> filter(final Predicate<Map.Entry<K, V>> predicate) {
        return of(s.filter(predicate));
    }

    public <KK> EntryStream<K, V> filterByKey(final Predicate<? super K> keyPredicate) {
        final Predicate<Map.Entry<K, V>> predicate = Fn.testByKey(keyPredicate);

        return of(s.filter(predicate));
    }

    public <KK> EntryStream<K, V> filterByValue(final Predicate<? super V> valuePredicate) {
        final Predicate<Map.Entry<K, V>> predicate = Fn.testByValue(valuePredicate);

        return of(s.filter(predicate));
    }

    public <KK, VV> EntryStream<KK, VV> map(final Function<? super Map.Entry<K, V>, Map.Entry<KK, VV>> mapper) {
        return of(s.map(mapper));
    }

    public <KK, VV> EntryStream<KK, VV> map(final Function<? super K, KK> keyMapper, final Function<? super V, VV> valueMapper) {
        final Function<Map.Entry<K, V>, Map.Entry<KK, VV>> mapper = new Function<Map.Entry<K, V>, Map.Entry<KK, VV>>() {
            @Override
            public Entry<KK, VV> apply(Entry<K, V> t) {
                return Pair.of(keyMapper.apply(t.getKey()), valueMapper.apply(t.getValue()));
            }
        };

        return map(mapper);
    }

    public <KK> EntryStream<KK, V> mapKey(final Function<? super K, KK> keyMapper) {
        final Function<Map.Entry<K, V>, Map.Entry<KK, V>> mapper = Fn.mapKey(keyMapper);

        return of(s.map(mapper));
    }

    public <VV> EntryStream<K, VV> mapValue(final Function<? super V, VV> valueMapper) {
        final Function<Map.Entry<K, V>, Map.Entry<K, VV>> mapper = Fn.mapValue(valueMapper);

        return of(s.map(mapper));
    }

    public <KK, VV> EntryStream<KK, VV> flatMap(final Function<? super Map.Entry<K, V>, EntryStream<KK, VV>> mapper) {
        final Function<Map.Entry<K, V>, Stream<Map.Entry<KK, VV>>> mapper2 = new Function<Map.Entry<K, V>, Stream<Map.Entry<KK, VV>>>() {
            @Override
            public Stream<Entry<KK, VV>> apply(Entry<K, V> t) {
                return mapper.apply(t).s;
            }
        };

        return flatMap2(mapper2);
    }

    public <KK, VV> EntryStream<KK, VV> flatMap2(final Function<? super Map.Entry<K, V>, Stream<Map.Entry<KK, VV>>> mapper) {
        return of(s.flatMap(mapper));
    }

    public <KK, VV> EntryStream<KK, VV> flatMap3(final Function<? super Map.Entry<K, V>, Map<KK, VV>> mapper) {
        final Function<Map.Entry<K, V>, Stream<Map.Entry<KK, VV>>> mapper2 = new Function<Map.Entry<K, V>, Stream<Map.Entry<KK, VV>>>() {
            @Override
            public Stream<Entry<KK, VV>> apply(Entry<K, V> t) {
                return Stream.of(mapper.apply(t));
            }
        };

        return flatMap2(mapper2);
    }

    public <KK> EntryStream<KK, V> flatMapKey(final Function<? super K, Stream<KK>> keyMapper) {
        final Function<Map.Entry<K, V>, Stream<Map.Entry<KK, V>>> mapper2 = new Function<Map.Entry<K, V>, Stream<Map.Entry<KK, V>>>() {
            @Override
            public Stream<Entry<KK, V>> apply(final Map.Entry<K, V> e) {
                return keyMapper.apply(e.getKey()).map(new Function<KK, Map.Entry<KK, V>>() {
                    @Override
                    public Map.Entry<KK, V> apply(KK kk) {
                        return Pair.of(kk, e.getValue());
                    }
                });
            }
        };

        return flatMap2(mapper2);
    }

    public <VV> EntryStream<K, VV> flatMapValue(final Function<? super V, Stream<VV>> valueMapper) {
        final Function<Map.Entry<K, V>, Stream<Map.Entry<K, VV>>> mapper2 = new Function<Map.Entry<K, V>, Stream<Map.Entry<K, VV>>>() {
            @Override
            public Stream<Entry<K, VV>> apply(final Entry<K, V> e) {
                return valueMapper.apply(e.getValue()).map(new Function<VV, Map.Entry<K, VV>>() {
                    @Override
                    public Map.Entry<K, VV> apply(VV vv) {
                        return Pair.of(e.getKey(), vv);
                    }
                });
            }
        };

        return flatMap2(mapper2);
    }

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public EntryStream<K, List<V>> groupBy() {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();
        final Function<? super Map.Entry<K, V>, V> valueMapper = Fn.value();

        return of(s.groupBy(classifier, valueMapper));
    }

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public EntryStream<K, List<V>> groupBy(final Supplier<Map<K, List<V>>> mapFactory) {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();
        final Function<? super Map.Entry<K, V>, V> valueMapper = Fn.value();

        return of(s.groupBy(classifier, valueMapper, mapFactory));
    }

    public <KK, VV> EntryStream<KK, List<VV>> groupBy(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper) {

        return of(s.groupBy(keyExtractor, valueMapper));
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <KK, VV> EntryStream<KK, List<VV>> groupBy(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final Supplier<Map<KK, List<VV>>> mapFactory) {

        return of(s.groupBy(keyExtractor, valueMapper, mapFactory));
    }

    /**
     * 
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public <A, D> EntryStream<K, D> groupBy(final Collector<? super Map.Entry<K, V>, A, D> downstream) {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();

        return of(s.groupBy(classifier, downstream));
    }

    /**
     * 
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public <A, D> EntryStream<K, D> groupBy(final Collector<? super Map.Entry<K, V>, A, D> downstream, final Supplier<Map<K, D>> mapFactory) {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();

        return of(s.groupBy(classifier, downstream, mapFactory));
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public <KK, A, D> EntryStream<KK, D> groupBy(final Function<? super Map.Entry<K, V>, ? extends KK> classifier,
            final Collector<? super Map.Entry<K, V>, A, D> downstream) {

        return of(s.groupBy(classifier, downstream));
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public <KK, A, D> EntryStream<KK, D> groupBy(final Function<? super Map.Entry<K, V>, ? extends KK> classifier,
            final Collector<? super Map.Entry<K, V>, A, D> downstream, final Supplier<Map<KK, D>> mapFactory) {

        return of(s.groupBy(classifier, downstream, mapFactory));
    }

    /**
     * 
     * @return
     */
    public EntryStream<K, V> groupBy2() {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();
        final Function<? super Map.Entry<K, V>, V> valueMapper = Fn.value();

        return of(s.groupBy2(classifier, valueMapper));
    }

    /**
     * 
     * @param mergeFunction
     * @return
     */
    public EntryStream<K, V> groupBy2(final BinaryOperator<V> mergeFunction) {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();
        final Function<? super Map.Entry<K, V>, V> valueMapper = Fn.value();

        return of(s.groupBy2(classifier, valueMapper, mergeFunction));
    }

    /**
     * 
     * @param mapFactory
     * @return
     */
    public EntryStream<K, V> groupBy2(final Supplier<Map<K, V>> mapFactory) {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();
        final Function<? super Map.Entry<K, V>, V> valueMapper = Fn.value();

        return of(s.groupBy2(classifier, valueMapper, mapFactory));
    }

    /**
     * 
     * @param mergeFunction
     * @param mapFactory
     * @return
     */
    public EntryStream<K, V> groupBy2(final BinaryOperator<V> mergeFunction, final Supplier<Map<K, V>> mapFactory) {
        final Function<? super Map.Entry<K, V>, K> classifier = Fn.key();
        final Function<? super Map.Entry<K, V>, V> valueMapper = Fn.value();

        return of(s.groupBy2(classifier, valueMapper, mergeFunction, mapFactory));
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#groupBy(Function, Function)
     */
    public <KK, VV> EntryStream<KK, VV> groupBy2(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper) {

        return of(s.groupBy2(keyExtractor, valueMapper));
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#groupBy(Function, Function, Supplier)
     */
    public <KK, VV> EntryStream<KK, VV> groupBy2(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final Supplier<Map<KK, VV>> mapFactory) {

        return of(s.groupBy2(keyExtractor, valueMapper, mapFactory));
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#groupBy(Function, Function, BinaryOperator)
     */
    public <KK, VV> EntryStream<KK, VV> groupBy2(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final BinaryOperator<VV> mergeFunction) {

        return of(s.groupBy2(keyExtractor, valueMapper, mergeFunction));
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @see Collectors#groupBy(Function, Function, BinaryOperator, Supplier)
     */
    public <KK, VV> EntryStream<KK, VV> groupBy2(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final BinaryOperator<VV> mergeFunction, final Supplier<Map<KK, VV>> mapFactory) {

        return of(s.groupBy2(keyExtractor, valueMapper, mergeFunction, mapFactory));
    }

    public EntryStream<K, V> sorted(final Comparator<? super Map.Entry<K, V>> comparator) {
        return of(s.sorted(comparator));
    }

    public EntryStream<K, V> sortedByKey(final Comparator<? super K> keyComparator) {
        final Comparator<Map.Entry<K, V>> comparator = Comparators.comparingByKey(keyComparator);

        return of(s.sorted(comparator));
    }

    public EntryStream<K, V> sortedByValue(final Comparator<? super V> valueComparator) {
        final Comparator<Map.Entry<K, V>> comparator = Comparators.comparingByValue(valueComparator);

        return of(s.sorted(comparator));
    }

    @SuppressWarnings("rawtypes")
    public EntryStream<K, V> sortedBy(Function<? super Map.Entry<K, V>, ? extends Comparable> keyExtractor) {
        return of(s.sortedBy(keyExtractor));
    }

    public EntryStream<K, V> distinct() {
        return of(s.distinct());
    }

    public EntryStream<K, V> distinctByKey() {
        final Function<? super Entry<K, V>, K> keyExtractor = Fn.key();

        return of(s.distinctBy(keyExtractor));
    }

    public EntryStream<K, V> distinctByValue() {
        final Function<? super Entry<K, V>, V> keyExtractor = Fn.value();

        return of(s.distinctBy(keyExtractor));
    }

    public EntryStream<K, V> distinctBy(final Function<? super Map.Entry<K, V>, ?> keyExtractor) {
        return of(s.distinctBy(keyExtractor));
    }

    public EntryStream<K, V> reversed() {
        return of(s.reversed());
    }

    public EntryStream<K, V> shuffled() {
        return of(s.shuffled());
    }

    public EntryStream<K, V> shuffled(final Random rnd) {
        return of(s.shuffled(rnd));
    }

    public EntryStream<K, V> rotated(final int distance) {
        return of(s.rotated(distance));
    }

    public EntryStream<K, V> skip(long n) {
        return of(s.skip(n));
    }

    public EntryStream<K, V> limit(long n) {
        return of(s.limit(n));
    }

    public EntryStream<K, V> peek(final Consumer<? super Map.Entry<K, V>> action) {
        return of(s.peek(action));
    }

    public void forEach(final Consumer<? super Map.Entry<K, V>> action) {
        s.forEach(action);
    }

    public long count() {
        return s.count();
    }

    public ImmutableIterator<Map.Entry<K, V>> iterator() {
        return s.iterator();
    }

    /**
     * 
     * @return
     */
    public Map<K, V> toMap() {
        final Collector<Map.Entry<K, V>, ?, Map<K, V>> collector = Collectors.toMap();

        return s.collect(collector);
    }

    /**
     * 
     * @param mergeFunction
     * @return
     */
    public Map<K, V> toMap(final BinaryOperator<V> mergeFunction) {
        final Collector<Map.Entry<K, V>, ?, Map<K, V>> collector = Collectors.toMap(mergeFunction);

        return s.collect(collector);
    }

    /**
     * 
     * @param mapFactory
     * @return
     */
    public <M extends Map<K, V>> M toMap(final Supplier<M> mapFactory) {
        final Collector<Map.Entry<K, V>, ?, M> collector = Collectors.toMap(mapFactory);

        return s.collect(collector);
    }

    /**
     * 
     * @param mergeFunction
     * @param mapFactory
     * @return
     */
    public <M extends Map<K, V>> M toMap(final BinaryOperator<V> mergeFunction, final Supplier<M> mapFactory) {
        final Function<Map.Entry<K, V>, K> keyExtractor = Fn.key();
        final Function<Map.Entry<K, V>, V> valueMapper = Fn.value();

        final Collector<Entry<K, V>, ?, M> collector = Collectors.toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);

        return s.collect(collector);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#toMap(Function, Function)
     */
    public <KK, VV> Map<KK, VV> toMap(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper) {
        return s.toMap(keyExtractor, valueMapper);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, Supplier)
     */
    public <KK, VV, M extends Map<KK, VV>> M toMap(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final Supplier<M> mapFactory) {
        return s.toMap(keyExtractor, valueMapper, mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator)
     */
    public <KK, VV> Map<KK, VV> toMap(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final BinaryOperator<VV> mergeFunction) {
        return s.toMap(keyExtractor, valueMapper, mergeFunction);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
     */
    public <KK, VV, M extends Map<KK, VV>> M toMap(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final BinaryOperator<VV> mergeFunction, final Supplier<M> mapFactory) {
        return s.toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    /**
     * 
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public <A, D> Map<K, D> toMap(final Collector<? super Map.Entry<K, V>, A, D> downstream) {
        final Function<Map.Entry<K, V>, K> keyExtractor = Fn.key();
        return s.toMap(keyExtractor, downstream);
    }

    /**
     * 
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public <A, D, M extends Map<K, D>> M toMap(final Collector<? super Map.Entry<K, V>, A, D> downstream, final Supplier<M> mapFactory) {
        final Function<Map.Entry<K, V>, K> keyExtractor = Fn.key();

        return s.toMap(keyExtractor, downstream, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public <KK, A, D> Map<KK, D> toMap(final Function<? super Map.Entry<K, V>, ? extends KK> classifier,
            final Collector<? super Map.Entry<K, V>, A, D> downstream) {
        return s.toMap(classifier, downstream);
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public <KK, A, D, M extends Map<KK, D>> M toMap(final Function<? super Map.Entry<K, V>, ? extends KK> classifier,
            final Collector<? super Map.Entry<K, V>, A, D> downstream, final Supplier<M> mapFactory) {
        return s.toMap(classifier, downstream, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public Map<K, List<V>> toMap2() {
        final Collector<Map.Entry<K, V>, ?, Map<K, List<V>>> collector = Collectors.toMap2();

        return s.collect(collector);
    }

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public <M extends Map<K, List<V>>> M toMap2(final Supplier<M> mapFactory) {
        final Function<Map.Entry<K, V>, K> keyExtractor = Fn.key();
        final Function<Map.Entry<K, V>, V> valueMapper = Fn.value();

        return s.toMap2(keyExtractor, valueMapper, mapFactory);
    }

    public <KK, VV> Map<KK, List<VV>> toMap2(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper) {
        return s.toMap2(keyExtractor, valueMapper);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <KK, VV, M extends Map<KK, List<VV>>> M toMap2(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final Supplier<M> mapFactory) {

        return s.toMap2(keyExtractor, valueMapper, mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public Multimap<K, V, List<V>> toMultimap() {
        final Function<Map.Entry<K, V>, K> keyExtractor = Fn.key();
        final Function<Map.Entry<K, V>, V> valueMapper = Fn.value();

        return s.toMultimap(keyExtractor, valueMapper);
    }

    /**
     * 
     * @param keyExtractor
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <C extends Collection<V>> Multimap<K, V, C> toMultimap(final Supplier<Multimap<K, V, C>> mapFactory) {
        final Function<Map.Entry<K, V>, K> keyExtractor = Fn.key();
        final Function<Map.Entry<K, V>, V> valueMapper = Fn.value();

        return s.toMultimap(keyExtractor, valueMapper, mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#toMultimap(Function, Function)
     */
    public <KK, VV> Multimap<KK, VV, List<VV>> toMultimap(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper) {
        return s.toMultimap(keyExtractor, valueMapper);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMultimap(Function, Function, Supplier)
     */
    public <KK, VV, C extends Collection<VV>> Multimap<KK, VV, C> toMultimap(final Function<? super Map.Entry<K, V>, ? extends KK> keyExtractor,
            final Function<? super Map.Entry<K, V>, ? extends VV> valueMapper, final Supplier<Multimap<KK, VV, C>> mapFactory) {
        return s.toMultimap(keyExtractor, valueMapper, mapFactory);
    }

    public Map.Entry<K, V> reduce(final Map.Entry<K, V> identity, final BinaryOperator<Map.Entry<K, V>> accumulator) {
        return s.reduce(identity, accumulator);
    }

    public NullabLe<Map.Entry<K, V>> reduce(final BinaryOperator<Map.Entry<K, V>> accumulator) {
        return s.reduce(accumulator);
    }

    public <U> U reduce(final U identity, final BiFunction<U, ? super Map.Entry<K, V>, U> accumulator, final BinaryOperator<U> combiner) {
        return s.reduce(identity, accumulator, combiner);
    }

    public <U> U reduce(final U identity, final BiFunction<U, ? super Map.Entry<K, V>, U> accumulator) {
        return s.reduce(identity, accumulator);
    }

    public <R> R collect(final Supplier<R> supplier, final BiConsumer<R, ? super Map.Entry<K, V>> accumulator, final BiConsumer<R, R> combiner) {
        return s.collect(supplier, accumulator, combiner);
    }

    public <R> R collect(final Supplier<R> supplier, final BiConsumer<R, ? super Map.Entry<K, V>> accumulator) {
        return s.collect(supplier, accumulator);
    }

    public <R, A> R collect(final Collector<? super Map.Entry<K, V>, A, R> collector) {
        return s.collect(collector);
    }

    public <R, A, RR> RR collectAndThen(final Collector<? super Map.Entry<K, V>, A, R> downstream, final Function<R, RR> finisher) {
        return s.collectAndThen(downstream, finisher);
    }

    public <K2, V2> EntryStream<K2, V2> __(Function<? super Stream<Map.Entry<K, V>>, ? extends Stream<Map.Entry<K2, V2>>> transfer) {
        return of(transfer.apply(s));
    }

    public EntryStream<K, V> onClose(Runnable closeHandler) {
        return of(s.onClose(closeHandler));
    }

    public void close() {
        s.close();
    }
}
