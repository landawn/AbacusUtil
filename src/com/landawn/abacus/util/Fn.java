/*
 * Copyright (c) 2017, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.landawn.abacus.util;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;

/**
 * It's designed for Stream<Entry<K, V>>
 * <pre>
 * <code>
 * 
 * Map<String, Integer> map = N.asMap("a", 1, "b", 2, "c", 3);
 * // Instead of
 * Stream.of(map).filter(e -> e.getKey().equals("a") || e.getKey().equals("b")).toMap(e -> e.getKey(), e -> e.getValue());
 * // Using Fn
 * Stream.of(map).filter(Fn.testByKey(k -> k.equals("a") || k.equals("b"))).collect(Fn.toMap());
 * 
 * </code>
 * </pre>
 * 
 * 
 * @author haiyang li
 *
 */
public final class Fn {
    @SuppressWarnings("rawtypes")
    public static final Function IDENTITY = Function.IDENTITY;

    @SuppressWarnings("rawtypes")
    private static final BinaryOperator<Collection> ADD_ALL = new BinaryOperator<Collection>() {
        @Override
        public Collection apply(Collection t, Collection u) {
            t.addAll(u);
            return t;
        }
    };

    private Fn() {
        // Singleton.
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testByKey(final Predicate<? super K> predicate) {
        return new Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getKey());
            }
        };
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testByVal(final Predicate<? super V> predicate) {
        return new Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getValue());
            }
        };
    }

    public static <K, V> Consumer<Map.Entry<K, V>> acceptByKey(final Consumer<? super K> consumer) {
        return new Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getKey());
            }
        };
    }

    public static <K, V> Consumer<Map.Entry<K, V>> acceptByVal(final Consumer<? super V> consumer) {
        return new Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getValue());
            }
        };
    }

    public static <K, V, R> Function<Map.Entry<K, V>, R> applyByKey(final Function<? super K, R> func) {
        return new Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getKey());
            }
        };
    }

    public static <K, V, R> Function<Map.Entry<K, V>, R> applyByVal(final Function<? super V, R> func) {
        return new Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getValue());
            }
        };
    }

    /**
     * 
     * @return
     * @throws IllegalStateException if there are duplicated keys.
     */
    public static <K, V, A> Collector<Map.Entry<K, V>, A, Map<K, V>> toMap() {
        return (Collector<Map.Entry<K, V>, A, Map<K, V>>) Collectors.toMap(new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Entry<K, V> entry) {
                return entry.getKey();
            }
        }, new Function<Map.Entry<K, V>, V>() {
            @Override
            public V apply(Entry<K, V> entry) {
                return entry.getValue();
            }
        });
    }

    public static <K, V, A> Collector<Map.Entry<K, V>, A, Map<K, V>> toMap(final BinaryOperator<V> mergeFunction) {
        return (Collector<Map.Entry<K, V>, A, Map<K, V>>) Collectors.toMap(new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Entry<K, V> entry) {
                return entry.getKey();
            }
        }, new Function<Map.Entry<K, V>, V>() {
            @Override
            public V apply(Entry<K, V> entry) {
                return entry.getValue();
            }
        }, mergeFunction);
    }

    public static <K, V, A> Collector<Map.Entry<K, V>, A, Map<K, List<V>>> toMap2() {
        @SuppressWarnings("rawtypes")
        final BinaryOperator<List<V>> mergeFunction = (BinaryOperator) ADD_ALL;

        return (Collector<Map.Entry<K, V>, A, Map<K, List<V>>>) Collectors.toMap(new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Entry<K, V> entry) {
                return entry.getKey();
            }
        }, new Function<Map.Entry<K, V>, List<V>>() {
            @Override
            public List<V> apply(Entry<K, V> entry) {
                return N.asList(entry.getValue());
            }
        }, mergeFunction);
    }

    public static <K, V, A> Collector<Map.Entry<K, V>, A, Multimap<K, V, List<V>>> toMultimap() {
        return (Collector<Map.Entry<K, V>, A, Multimap<K, V, List<V>>>) Collectors.toMultimap(new Function<Map.Entry<K, V>, K>() {
            @Override
            public K apply(Entry<K, V> entry) {
                return entry.getKey();
            }
        }, new Function<Map.Entry<K, V>, V>() {
            @Override
            public V apply(Entry<K, V> entry) {
                return entry.getValue();
            }
        });
    }
}
