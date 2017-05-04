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

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.regex.Pattern;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;

/**
 * Utility class for function creation and Stream&lt;Entry&lt;K, V&gt;&gt;
 * <pre>
 * <code>
 * 
 * Map<String, Integer> map = N.asMap("a", 1, "b", 2, "c", 3);
 * // Instead of
 * Stream.of(map).filter(e -> e.getKey().equals("a") || e.getKey().equals("b")).toMap(e -> e.getKey(), e -> e.getValue());
 * // Using Fn
 * Stream.of(map).filter(Fn.testByKey(k -> k.equals("a") || k.equals("b"))).collect(Collectors.toMap());
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
    public static final com.landawn.abacus.util.function.Supplier<Map<String, Object>> SUPPLIER_OF_MAP = (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.MAP;
    @SuppressWarnings("rawtypes")
    public static final com.landawn.abacus.util.function.Supplier<LinkedHashMap<String, Object>> SUPPLIER_OF_LINKED_HASH_MAP = (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.LINKED_HASH_MAP;
    @SuppressWarnings("rawtypes")
    public static final IntFunction<Map<String, Object>> FACTORY_OF_MAP = (IntFunction) IntFunction.MAP_FACTORY;
    @SuppressWarnings("rawtypes")
    public static final IntFunction<LinkedHashMap<String, Object>> FACTORY_OF_LINKED_HASH_MAP = (IntFunction) IntFunction.LINKED_HASH_MAP_FACTORY;

    @SuppressWarnings("rawtypes")
    private static final Consumer PRINTLN = new Consumer() {
        @Override
        public void accept(Object value) {
            N.println(value);
        }
    };

    private static final BinaryOperator<Collection<Object>> ADDING_ALL_MERGER = new BinaryOperator<Collection<Object>>() {
        @Override
        public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
            t.addAll(u);
            return t;
        }
    };

    private static final BinaryOperator<Collection<Object>> REMOVING_ALL_MERGER = new BinaryOperator<Collection<Object>>() {
        @Override
        public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
            t.removeAll(u);
            return t;
        }
    };

    private static final BinaryOperator<Map<Object, Object>> PUTTING_ALL_MERGER = new BinaryOperator<Map<Object, Object>>() {
        @Override
        public Map<Object, Object> apply(Map<Object, Object> t, Map<Object, Object> u) {
            t.putAll(u);
            return t;
        }
    };

    private static final BiConsumer<Collection<Object>, Collection<Object>> ADDING_ALL_COMBINER = new BiConsumer<Collection<Object>, Collection<Object>>() {
        @Override
        public void accept(Collection<Object> t, Collection<Object> u) {
            t.addAll(u);
        }
    };

    private static final BiConsumer<Collection<Object>, Collection<Object>> REMOVING_ALL_COMBINER = new BiConsumer<Collection<Object>, Collection<Object>>() {
        @Override
        public void accept(Collection<Object> t, Collection<Object> u) {
            t.removeAll(u);
        }
    };

    private static final BiConsumer<Map<Object, Object>, Map<Object, Object>> PUTTING_ALL_COMBINER = new BiConsumer<Map<Object, Object>, Map<Object, Object>>() {
        @Override
        public void accept(Map<Object, Object> t, Map<Object, Object> u) {
            t.putAll(u);
        }
    };

    private Fn() {
        // Singleton.
    }

    public static <T> Consumer<T> doNothing() {
        return Consumer.DO_NOTHING;
    }

    public static <T> Consumer<T> println() {
        return PRINTLN;
    }

    public static <T, U> BiConsumer<T, U> println(final String separator) {
        return new BiConsumer<T, U>() {
            @Override
            public void accept(T t, U u) {
                N.println(t + separator + u);
            }
        };
    }

    public static <T> Function<T, T> identity() {
        return Function.IDENTITY;
    }

    public static <T, U> Function<T, U> cast(final Class<U> clazz) {
        return new Function<T, U>() {
            @Override
            public U apply(T t) {
                return (U) t;
            }
        };
    }

    public static <T> Predicate<T> alwaysTrue() {
        return Predicate.ALWAYS_TRUE;
    }

    public static <T> Predicate<T> alwaysFalse() {
        return Predicate.ALWAYS_FALSE;
    }

    public static <T> Predicate<T> isNull() {
        return Predicate.IS_NULL;
    }

    public static <T> Predicate<T> notNull() {
        return Predicate.NOT_NULL;
    }

    public static <T> Predicate<T> equal(final Object target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.equals(value, target);
            }
        };
    }

    public static <T> Predicate<T> notEqual(final Object target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !N.equals(value, target);
            }
        };
    }

    public static <T extends Comparable<? super T>> Predicate<T> greaterThan(final T target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) > 0;
            }
        };
    }

    public static <T extends Comparable<? super T>> Predicate<T> greaterEqual(final T target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) >= 0;
            }
        };
    }

    public static <T extends Comparable<? super T>> Predicate<T> lessThan(final T target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) < 0;
            }
        };
    }

    public static <T extends Comparable<? super T>> Predicate<T> lessEqual(final T target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) <= 0;
            }
        };
    }

    public static <T> Predicate<T> in(final Collection<?> c) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return c.contains(value);
            }
        };
    }

    public static <T> Predicate<T> notIn(final Collection<?> c) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !c.contains(value);
            }
        };
    }

    public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return clazz.isInstance(value);
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static Predicate<Class> subtypeOf(final Class<?> clazz) {
        return new Predicate<Class>() {
            @Override
            public boolean test(Class value) {
                return clazz.isAssignableFrom(value);
            }
        };
    }

    public static Predicate<CharSequence> matches(final Pattern pattern) {
        return new Predicate<CharSequence>() {
            @Override
            public boolean test(CharSequence value) {
                return pattern.matcher(value).find();
            }
        };
    }

    public static <T, U> BiPredicate<T, U> equal() {
        return BiPredicate.EQUAL;
    }

    public static <T, U> BiPredicate<T, U> notEqual() {
        return BiPredicate.NOT_EQUAL;
    }

    public static <T extends Comparable<? super T>> BiPredicate<T, T> greaterThan() {
        return (BiPredicate<T, T>) BiPredicate.GREATER_THAN;
    }

    public static <T extends Comparable<? super T>> BiPredicate<T, T> greaterEqual() {
        return (BiPredicate<T, T>) BiPredicate.GREATER_EQUAL;
    }

    public static <T extends Comparable<? super T>> BiPredicate<T, T> lessThan() {
        return (BiPredicate<T, T>) BiPredicate.LESS_THAN;
    }

    public static <T extends Comparable<? super T>> BiPredicate<T, T> lessEqual() {
        return (BiPredicate<T, T>) BiPredicate.LESS_EQUAL;
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testByKey(final Predicate<? super K> predicate) {
        return new Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getKey());
            }
        };
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testByValue(final Predicate<? super V> predicate) {
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

    public static <K, V> Consumer<Map.Entry<K, V>> acceptByValue(final Consumer<? super V> consumer) {
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

    public static <K, V, R> Function<Map.Entry<K, V>, R> applyByValue(final Function<? super V, R> func) {
        return new Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getValue());
            }
        };
    }

    public static <T> BinaryOperator<T> throwingMerger() {
        return BinaryOperator.THROWING_MERGER;
    }

    public static <T> BinaryOperator<T> ignoringMerger() {
        return BinaryOperator.IGNORING_MERGER;
    }

    public static <T> BinaryOperator<T> replacingMerger() {
        return BinaryOperator.REPLACING_MERGER;
    }

    public static <T, C extends Collection<T>> BinaryOperator<C> addingAllMerge() {
        return (BinaryOperator<C>) ADDING_ALL_MERGER;
    }

    public static <T, C extends Collection<T>> BinaryOperator<C> removingAllMerge() {
        return (BinaryOperator<C>) REMOVING_ALL_MERGER;
    }

    public static <K, V, M extends Map<K, V>> BinaryOperator<M> puttingAllMerge() {
        return (BinaryOperator<M>) PUTTING_ALL_MERGER;
    }

    public static <T, C extends Collection<T>> BiConsumer<C, C> addingAllCombiner() {
        return (BiConsumer<C, C>) ADDING_ALL_COMBINER;
    }

    public static <T, C extends Collection<T>> BiConsumer<C, C> removingAllCombiner() {
        return (BiConsumer<C, C>) REMOVING_ALL_COMBINER;
    }

    public static <K, V, M extends Map<K, V>> BiConsumer<M, M> puttingAllCombiner() {
        return (BiConsumer<M, M>) PUTTING_ALL_COMBINER;
    }

    public static ToByteFunction<Byte> unboxB() {
        return ToByteFunction.UNBOX;
    }

    public static ToCharFunction<Character> unboxC() {
        return ToCharFunction.UNBOX;
    }

    public static ToShortFunction<Short> unboxS() {
        return ToShortFunction.UNBOX;
    }

    public static ToIntFunction<Integer> unboxI() {
        return ToIntFunction.UNBOX;
    }

    public static ToLongFunction<Long> unboxL() {
        return ToLongFunction.UNBOX;
    }

    public static ToFloatFunction<Float> unboxF() {
        return ToFloatFunction.UNBOX;
    }

    public static ToDoubleFunction<Double> unboxD() {
        return ToDoubleFunction.UNBOX;
    }

    public static final class Supplier {
        public static final com.landawn.abacus.util.function.Supplier<String> UUID = com.landawn.abacus.util.function.Supplier.UUID;
        public static final com.landawn.abacus.util.function.Supplier<String> GUID = com.landawn.abacus.util.function.Supplier.GUID;
        private Supplier() {
            // singleton.
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<ExList<T>> ofExList() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.EX_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<List<T>> ofList() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<LinkedList<T>> ofLinkedList() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.LINKED_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<Set<T>> ofSet() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<LinkedHashSet<T>> ofLinkedHashSet() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.LINKED_HASH_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<TreeSet<T>> ofTreeSet() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.TREE_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> com.landawn.abacus.util.function.Supplier<Map<K, V>> ofMap() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> com.landawn.abacus.util.function.Supplier<LinkedHashMap<K, V>> ofLinkedHashMap() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.LINKED_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> com.landawn.abacus.util.function.Supplier<TreeMap<K, V>> ofTreeMap() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.TREE_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<Queue<T>> ofQueue() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<ArrayDeque<T>> ofArrayDeque() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.ARRAY_DEQUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.LINKED_BLOCKING_QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.CONCURRENT_LINKED_QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<PriorityQueue<T>> ofPriorityQueue() {
            return (com.landawn.abacus.util.function.Supplier) com.landawn.abacus.util.function.Supplier.PRIORITY_QUEUE;
        }
    }

    public static final class Factory {
        private Factory() {
            // singleton.
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<ExList<T>> ofExList() {
            return (IntFunction) IntFunction.EX_LIST_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<List<T>> ofList() {
            return (IntFunction) IntFunction.LIST_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<LinkedList<T>> ofLinkedList() {
            return (IntFunction) IntFunction.LINKED_LIST_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<Set<T>> ofSet() {
            return (IntFunction) IntFunction.SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<LinkedHashSet<T>> ofLinkedHashSet() {
            return (IntFunction) IntFunction.LINKED_HASH_SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<TreeSet<T>> ofTreeSet() {
            return (IntFunction) IntFunction.TREE_SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<Map<K, V>> ofMap() {
            return (IntFunction) IntFunction.MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<LinkedHashMap<K, V>> ofLinkedHashMap() {
            return (IntFunction) IntFunction.LINKED_HASH_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<TreeMap<K, V>> ofTreeMap() {
            return (IntFunction) IntFunction.TREE_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<Queue<T>> ofQueue() {
            return (IntFunction) IntFunction.QUEUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<ArrayDeque<T>> ofArrayDeque() {
            return (IntFunction) IntFunction.ARRAY_DEQUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
            return (IntFunction) IntFunction.LINKED_BLOCKING_QUEUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
            return (IntFunction) IntFunction.CONCURRENT_LINKED_QUEUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<PriorityQueue<T>> ofPriorityQueue() {
            return (IntFunction) IntFunction.PRIORITY_QUEUE_FACTORY;
        }
    }

}
