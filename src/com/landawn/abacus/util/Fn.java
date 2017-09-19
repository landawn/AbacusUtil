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

import java.util.AbstractMap;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

import com.landawn.abacus.util.Tuple.Tuple2;
import com.landawn.abacus.util.Tuple.Tuple3;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedBiConsumer;
import com.landawn.abacus.util.function.IndexedBiFunction;
import com.landawn.abacus.util.function.IndexedBiPredicate;
import com.landawn.abacus.util.function.IndexedConsumer;
import com.landawn.abacus.util.function.IndexedFunction;
import com.landawn.abacus.util.function.IndexedPredicate;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriConsumer;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.TriPredicate;
import com.landawn.abacus.util.function.UnaryOperator;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;

/**
 * Factory utility class for functional interfaces.
 * 
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
    public static final IntFunction<Map<String, Object>> FACTORY_OF_MAP = (IntFunction) Factory.MAP_FACTORY;
    @SuppressWarnings("rawtypes")
    public static final IntFunction<LinkedHashMap<String, Object>> FACTORY_OF_LINKED_HASH_MAP = (IntFunction) Factory.LINKED_HASH_MAP_FACTORY;
    @SuppressWarnings("rawtypes")
    public static final Supplier<Map<String, Object>> SUPPLIER_OF_MAP = (Supplier) Suppliers.MAP;
    @SuppressWarnings("rawtypes")
    public static final Supplier<LinkedHashMap<String, Object>> SUPPLIER_OF_LINKED_HASH_MAP = (Supplier) Suppliers.LINKED_HASH_MAP;

    private static final Runnable EMPTY_ACTION = new Runnable() {
        @Override
        public void run() {
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Consumer DO_NOTHING = new Consumer() {
        @Override
        public void accept(Object value) {
            // do nothing.
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Consumer PRINTLN = new Consumer() {
        @Override
        public void accept(Object value) {
            N.println(value);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Function IDENTITY = new Function() {
        @Override
        public Object apply(Object t) {
            return t;
        }
    };

    private static final Function<Map.Entry<Object, Object>, Object> KEY = new Function<Map.Entry<Object, Object>, Object>() {
        @Override
        public Object apply(Map.Entry<Object, Object> t) {
            return t.getKey();
        }
    };

    private static final Function<Map.Entry<Object, Object>, Object> VALUE = new Function<Map.Entry<Object, Object>, Object>() {
        @Override
        public Object apply(Map.Entry<Object, Object> t) {
            return t.getValue();
        }
    };

    private static final BiFunction<Object, Object, Map.Entry<Object, Object>> ENTRY = new BiFunction<Object, Object, Map.Entry<Object, Object>>() {
        @Override
        public Map.Entry<Object, Object> apply(Object key, Object value) {
            return new AbstractMap.SimpleImmutableEntry<>(key, value);
        }
    };

    private static final Function<String, String> TRIM = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return t == null ? null : t.trim();
        }
    };

    private static final Function<String, String> TRIM_TO_EMPTY = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return t == null ? "" : t.trim();
        }
    };

    private static final Function<String, String> TRIM_TO_NULL = new Function<String, String>() {
        @Override
        public String apply(String t) {
            if (t == null || (t = t.trim()).length() == 0) {
                return null;
            }

            return t;
        }
    };

    private static final BiFunction<Object, Object, Pair<Object, Object>> PAIR = new BiFunction<Object, Object, Pair<Object, Object>>() {
        @Override
        public Pair<Object, Object> apply(Object key, Object value) {
            return Pair.of(key, value);
        }
    };

    private static final TriFunction<Object, Object, Object, Triple<Object, Object, Object>> TRIPLE = new TriFunction<Object, Object, Object, Triple<Object, Object, Object>>() {
        @Override
        public Triple<Object, Object, Object> apply(Object a, Object b, Object c) {
            return Triple.of(a, b, c);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Predicate ALWAYS_TRUE = new Predicate() {
        @Override
        public boolean test(Object value) {
            return true;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Predicate ALWAYS_FALSE = new Predicate() {
        @Override
        public boolean test(Object value) {
            return false;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Predicate IS_NULL = new Predicate() {
        @Override
        public boolean test(Object value) {
            return value == null;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Predicate NOT_NULL = new Predicate() {
        @Override
        public boolean test(Object value) {
            return value != null;
        }
    };

    private static final Object NULL = new Object();

    private Fn() {
        // Singleton.
    }

    public static <T> T get(final Supplier<T> supplier) {
        return supplier.get();
    }

    public static <T> Comparator<T> naturalOrder() {
        return Comparators.naturalOrder();
    }

    public static <T> Comparator<T> reversedOrder() {
        return Comparators.reversedOrder();
    }

    public static <T> Comparator<T> reversedOrder(final Comparator<T> cmp) {
        return Comparators.reversedOrder(cmp);
    }

    @SuppressWarnings("rawtypes")
    public static <T, U extends Comparable> Comparator<T> comparingBy(final Function<? super T, ? extends U> keyExtractor) {
        return Comparators.comparingBy(keyExtractor);
    }

    @SuppressWarnings("rawtypes")
    public static <T, U extends Comparable> Comparator<T> reversedComparingBy(final Function<? super T, ? extends U> keyExtractor) {
        return Comparators.reversedComparingBy(keyExtractor);
    }

    public static Runnable emptyAction() {
        return EMPTY_ACTION;
    }

    public static <T> Consumer<T> doNothing() {
        return DO_NOTHING;
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
        return IDENTITY;
    }

    public static <K, T> Function<T, Keyed<K, T>> keyed(final Function<? super T, K> keyExtractor) {
        return new Function<T, Keyed<K, T>>() {
            @Override
            public Keyed<K, T> apply(T t) {
                return Keyed.of(keyExtractor.apply(t), t);
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Function<Entry<K, V>, K> key() {
        return (Function) KEY;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Function<Entry<K, V>, V> value() {
        return (Function) VALUE;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> BiFunction<K, V, Map.Entry<K, V>> entry() {
        return (BiFunction) ENTRY;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> BiFunction<K, V, Pair<K, V>> pair() {
        return (BiFunction) PAIR;
    }

    @SuppressWarnings("rawtypes")
    public static <A, B, C> TriFunction<A, B, C, Triple<A, B, C>> triple() {
        return (TriFunction) TRIPLE;
    }

    public static Function<String, String> trim() {
        return TRIM;
    }

    public static Function<String, String> trimToEmpty() {
        return TRIM_TO_EMPTY;
    }

    public static Function<String, String> trimToNull() {
        return TRIM_TO_NULL;
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
        return ALWAYS_TRUE;
    }

    public static <T> Predicate<T> alwaysFalse() {
        return ALWAYS_FALSE;
    }

    public static <T> Predicate<T> isNull() {
        return IS_NULL;
    }

    public static <T> Predicate<T> notNull() {
        return NOT_NULL;
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

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Predicate<T> greaterThan(final T target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) > 0;
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Predicate<T> greaterEqual(final T target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) >= 0;
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Predicate<T> lessThan(final T target) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) < 0;
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Predicate<T> lessEqual(final T target) {
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

    public static Predicate<String> startsWith(final String prefix) {
        return new Predicate<String>() {
            @Override
            public boolean test(String value) {
                return value.startsWith(prefix);
            }
        };
    }

    public static Predicate<String> endsWith(final String suffix) {
        return new Predicate<String>() {
            @Override
            public boolean test(String value) {
                return value.endsWith(suffix);
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
        return BiPredicates.EQUAL;
    }

    public static <T, U> BiPredicate<T, U> notEqual() {
        return BiPredicates.NOT_EQUAL;
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> BiPredicate<T, T> greaterThan() {
        return (BiPredicate<T, T>) BiPredicates.GREATER_THAN;
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> BiPredicate<T, T> greaterEqual() {
        return (BiPredicate<T, T>) BiPredicates.GREATER_EQUAL;
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> BiPredicate<T, T> lessThan() {
        return (BiPredicate<T, T>) BiPredicates.LESS_THAN;
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> BiPredicate<T, T> lessEqual() {
        return (BiPredicate<T, T>) BiPredicates.LESS_EQUAL;
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

    public static <K, V, KK> Function<Map.Entry<K, V>, Map.Entry<KK, V>> mapKey(final Function<? super K, KK> func) {
        return new Function<Map.Entry<K, V>, Map.Entry<KK, V>>() {
            @Override
            public Map.Entry<KK, V> apply(Entry<K, V> entry) {
                return Pair.of(func.apply(entry.getKey()), entry.getValue());
            }
        };
    }

    public static <K, V, VV> Function<Map.Entry<K, V>, Map.Entry<K, VV>> mapValue(final Function<? super V, VV> func) {
        return new Function<Map.Entry<K, V>, Map.Entry<K, VV>>() {
            @Override
            public Map.Entry<K, VV> apply(Entry<K, V> entry) {
                return Pair.of(entry.getKey(), func.apply(entry.getValue()));
            }
        };
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testKeyVal(final BiPredicate<? super K, ? super V> predicate) {
        return new Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getKey(), entry.getValue());
            }
        };
    }

    public static <K, V> Consumer<Map.Entry<K, V>> acceptKeyVal(final BiConsumer<? super K, ? super V> consumer) {
        return new Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getKey(), entry.getValue());
            }
        };
    }

    public static <K, V, R> Function<Map.Entry<K, V>, R> applyKeyVal(final BiFunction<? super K, ? super V, R> func) {
        return new Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getKey(), entry.getValue());
            }
        };
    }

    public static <T> BinaryOperator<T> throwingMerger() {
        return BinaryOperators.THROWING_MERGER;
    }

    public static <T> BinaryOperator<T> ignoringMerger() {
        return BinaryOperators.IGNORING_MERGER;
    }

    public static <T> BinaryOperator<T> replacingMerger() {
        return BinaryOperators.REPLACING_MERGER;
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

    /**
     * 
     * @param predicate
     * @param limit
     * @return
     */
    public static <T> Predicate<T> limited(final Predicate<T> predicate, final int limit) {
        Objects.requireNonNull(predicate);

        return new Predicate<T>() {
            private final AtomicInteger counter = new AtomicInteger(limit);

            @Override
            public boolean test(T t) {
                return predicate.test(t) && counter.decrementAndGet() >= 0;
            }
        };
    }

    /**
     * 
     * @param predicate
     * @param limit
     * @return
     */
    public static <T> Predicate<T> limited(final Predicate<T> predicate, final long limit) {
        Objects.requireNonNull(predicate);

        return new Predicate<T>() {
            private final AtomicLong counter = new AtomicLong(limit);

            @Override
            public boolean test(T t) {
                return predicate.test(t) && counter.decrementAndGet() >= 0;
            }
        };
    }

    /** 
     * 
     * @param predicate
     * @param limit
     * @return
     */
    public static <T, U> BiPredicate<T, U> limited(final BiPredicate<T, U> predicate, final int limit) {
        Objects.requireNonNull(predicate);

        return new BiPredicate<T, U>() {
            private final AtomicInteger counter = new AtomicInteger(limit);

            @Override
            public boolean test(T t, U u) {
                return predicate.test(t, u) && counter.decrementAndGet() >= 0;
            }
        };
    }

    /**
     * 
     * @param predicate
     * @param limit
     * @return
     */
    public static <T, U> BiPredicate<T, U> limited(final BiPredicate<T, U> predicate, final long limit) {
        Objects.requireNonNull(predicate);

        return new BiPredicate<T, U>() {
            private final AtomicLong counter = new AtomicLong(limit);

            @Override
            public boolean test(T t, U u) {
                return predicate.test(t, u) && counter.decrementAndGet() >= 0;
            }
        };
    }

    /**
     * 
     * @param predicate
     * @param limit
     * @return
     */
    public static <A, B, C> TriPredicate<A, B, C> limited(final TriPredicate<A, B, C> predicate, final int limit) {
        Objects.requireNonNull(predicate);

        return new TriPredicate<A, B, C>() {
            private final AtomicInteger counter = new AtomicInteger(limit);

            @Override
            public boolean test(A a, B b, C c) {
                return predicate.test(a, b, c) && counter.decrementAndGet() >= 0;
            }
        };
    }

    /**
     * 
     * @param predicate
     * @param limit
     * @return
     */
    public static <A, B, C> TriPredicate<A, B, C> limited(final TriPredicate<A, B, C> predicate, final long limit) {
        Objects.requireNonNull(predicate);

        return new TriPredicate<A, B, C>() {
            private final AtomicLong counter = new AtomicLong(limit);

            @Override
            public boolean test(A a, B b, C c) {
                return predicate.test(a, b, c) && counter.decrementAndGet() >= 0;
            }
        };
    }

    /**
     * Returns a stateful <code>Predicate</code> which should not be used in parallel stream.
     * 
     * @param predicate
     * @return
     */
    public static <T> Predicate<T> indexed(final IndexedPredicate<T> predicate) {
        Objects.requireNonNull(predicate);

        return new Predicate<T>() {
            private final AtomicInteger idx = new AtomicInteger(0);

            @Override
            public boolean test(T t) {
                return predicate.test(idx.getAndIncrement(), t);
            }
        };
    }

    /**
     * Returns a stateful <code>BiPredicate</code> which should not be used in parallel stream.
     * 
     * @param predicate
     * @return
     */
    public static <U, T> BiPredicate<U, T> indexed(final IndexedBiPredicate<U, T> predicate) {
        Objects.requireNonNull(predicate);

        return new BiPredicate<U, T>() {
            private final AtomicInteger idx = new AtomicInteger(0);

            @Override
            public boolean test(U u, T t) {
                return predicate.test(u, idx.getAndIncrement(), t);
            }
        };
    }

    /**
     * Returns a stateful <code>Function</code> which should not be used in parallel stream.
     * 
     * @param func
     * @return
     */
    public static <T, R> Function<T, R> indexeD(final IndexedFunction<T, R> func) {
        Objects.requireNonNull(func);

        return new Function<T, R>() {
            private final AtomicInteger idx = new AtomicInteger(0);

            @Override
            public R apply(T t) {
                return func.apply(idx.getAndIncrement(), t);
            }
        };
    }

    /**
     * Returns a stateful <code>BiFunction</code> which should not be used in parallel stream.
     * 
     * @param func
     * @return
     */
    public static <U, T, R> BiFunction<U, T, R> indexeD(final IndexedBiFunction<U, T, R> func) {
        Objects.requireNonNull(func);

        return new BiFunction<U, T, R>() {
            private final AtomicInteger idx = new AtomicInteger(0);

            @Override
            public R apply(U u, T t) {
                return func.apply(u, idx.getAndIncrement(), t);
            }
        };
    }

    /**
     * Returns a stateful <code>Consumer</code> which should not be used in parallel stream.
     * 
     * @param action
     * @return
     */
    public static <T> Consumer<T> indeXed(final IndexedConsumer<T> action) {
        Objects.requireNonNull(action);

        return new Consumer<T>() {
            private final AtomicInteger idx = new AtomicInteger(0);

            @Override
            public void accept(T t) {
                action.accept(idx.getAndIncrement(), t);
            }
        };
    }

    /**
     * Returns a stateful <code>BiConsumer</code> which should not be used in parallel stream.
     * 
     * @param action
     * @return
     */
    public static <U, T> BiConsumer<U, T> indeXed(final IndexedBiConsumer<U, T> action) {
        Objects.requireNonNull(action);

        return new BiConsumer<U, T>() {
            private final AtomicInteger idx = new AtomicInteger(0);

            @Override
            public void accept(U u, T t) {
                action.accept(u, idx.getAndIncrement(), t);
            }
        };
    }

    public static <T, C extends Collection<T>> BiConsumer<C, C> addAll() {
        return BiConsumers.<T, C> ofAddAll();
    }

    public static <K, V, M extends Map<K, V>> BiConsumer<M, M> putAll() {
        return BiConsumers.<K, V, M> ofPutAll();
    }

    /**
     * 
     * @return
     * @see Collectors#toList()
     */
    public static <T> Collector<T, ?, List<T>> toList() {
        return Collectors.toList();
    }

    /**
     * 
     * @return
     * @see Collectors#toImmutableList()
     */
    public static <T> Collector<T, ?, ImmutableList<T>> toImmutableList() {
        return Collectors.toImmutableList();
    }

    /**
     * 
     * @return
     * @see Collectors#toSet()
     */
    public static <T> Collector<T, ?, Set<T>> toSet() {
        return Collectors.toSet();
    }

    /**
     * 
     * @return
     * @see Collectors#toImmutableSet()
     */
    public static <T> Collector<T, ?, ImmutableSet<T>> toImmutableSet() {
        return Collectors.toImmutableSet();
    }

    /**
     * 
     * @return
     * @see Collectors#toMap()
     */
    public static <K, V> Collector<Map.Entry<K, V>, ?, Map<K, V>> toMap() {
        return Collectors.toMap();
    }

    /**
     * 
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(BinaryOperator)
     */
    public static <K, V> Collector<Map.Entry<K, V>, ?, Map<K, V>> toMap(final BinaryOperator<V> mergeFunction) {
        return Collectors.toMap(mergeFunction);
    }

    /**
     * 
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Supplier)
     */
    public static <K, V, M extends Map<K, V>> Collector<Map.Entry<K, V>, ?, M> toMap(final Supplier<M> mapFactory) {
        return Collectors.toMap(mapFactory);
    }

    /**
     * 
     * @param mergeFunction
     * @param mapFactory
     * @return
     * @see Collectors#toMap(BinaryOperator, Supplier)
     */
    public static <K, V, M extends Map<K, V>> Collector<Map.Entry<K, V>, ?, M> toMap(final BinaryOperator<V> mergeFunction, final Supplier<M> mapFactory) {
        return Collectors.toMap(mergeFunction, mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#toMap(Function, Function)
     */
    public static <T, K, U> Collector<T, ?, Map<K, U>> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        return Collectors.toMap(keyExtractor, valueMapper);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toMap(Function, Function, BinaryOperator)
     */
    public static <T, K, U> Collector<T, ?, Map<K, U>> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        return Collectors.toMap(keyExtractor, valueMapper, mergeFunction);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mapFactory
     * @return
     * @see Collectors#toMap(Function, Function, Supplier)
     */
    public static <T, K, U, M extends Map<K, U>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final Supplier<M> mapFactory) {
        return Collectors.toMap(keyExtractor, valueMapper, mapFactory);
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
    public static <T, K, U, M extends Map<K, U>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends U> valueMapper, final BinaryOperator<U> mergeFunction, final Supplier<M> mapFactory) {
        return Collectors.toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    /**
     * 
     * @return
     * @see Collectors#toImmutableMap()
     */
    public static <K, V> Collector<Map.Entry<K, V>, ?, ImmutableMap<K, V>> toImmutableMap() {
        return Collectors.toImmutableMap(Fn.IDENTITY, Fn.IDENTITY);
    }

    /**
     * 
     * @param mergeFunction
     * @return
     * @see Collectors#toImmutableMap(BinaryOperator)
     */
    public static <K, V> Collector<Map.Entry<K, V>, ?, ImmutableMap<K, V>> toImmutableMap(final BinaryOperator<V> mergeFunction) {
        return Collectors.toImmutableMap(Fn.IDENTITY, Fn.IDENTITY, mergeFunction);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     * @see Collectors#toImmutableMap(Function, Function)
     */
    public static <T, K, U> Collector<T, ?, ImmutableMap<K, U>> toImmutableMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper) {
        return Collectors.toImmutableMap(keyExtractor, valueMapper);
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @param mergeFunction
     * @return
     * @see Collectors#toImmutableMap(Function, Function, BinaryOperator)
     */
    public static <T, K, U> Collector<T, ?, ImmutableMap<K, U>> toImmutableMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        return Collectors.toImmutableMap(keyExtractor, valueMapper, mergeFunction);
    }

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public static <T, K> Collector<T, ?, Map<K, List<T>>> groupingBy(Function<? super T, ? extends K> classifier) {
        return Collectors.groupingBy(classifier);
    }

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public static <T, K, M extends Map<K, List<T>>> Collector<T, ?, M> groupingBy(final Function<? super T, ? extends K> classifier,
            final Supplier<M> mapFactory) {
        return Collectors.groupingBy(classifier, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    @SuppressWarnings("hiding")
    public static <T, K, A, D> Collector<T, ?, Map<K, D>> groupingBy(final Function<? super T, ? extends K> classifier,
            final Collector<? super T, A, D> downstream) {
        return Collectors.groupingBy(classifier, downstream);
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    @SuppressWarnings("hiding")
    public static <T, K, A, D, M extends Map<K, D>> Collector<T, ?, M> groupingBy(final Function<? super T, ? extends K> classifier,
            final Collector<? super T, A, D> downstream, final Supplier<M> mapFactory) {
        return Collectors.groupingBy(classifier, downstream, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public static <T, K> Collector<T, ?, Map<K, Long>> countingBy(Function<? super T, ? extends K> classifier) {
        return groupingBy(classifier, Collectors.counting());
    }

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public static <T, K, M extends Map<K, Long>> Collector<T, ?, M> countingBy(final Function<? super T, ? extends K> classifier,
            final Supplier<M> mapFactory) {
        return groupingBy(classifier, Collectors.counting(), mapFactory);
    }

    /**
     * 
     * @param classifier
     * @return
     * @see Collectors#groupingBy(Function)
     */
    public static <T, K> Collector<T, ?, Map<K, Integer>> countingIntBy(Function<? super T, ? extends K> classifier) {
        return groupingBy(classifier, Collectors.countingInt());
    }

    /**
     * 
     * @param classifier
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Supplier)
     */
    public static <T, K, M extends Map<K, Integer>> Collector<T, ?, M> countingIntBy(final Function<? super T, ? extends K> classifier,
            final Supplier<M> mapFactory) {
        return groupingBy(classifier, Collectors.countingInt(), mapFactory);
    }

    /**
     * 
     * @return
     * @see Collectors#counting()
     */
    public static <T> Collector<T, ?, Long> counting() {
        return Collectors.counting();
    }

    /**
     * 
     * @return
     * @see Collectors#countingInt()
     */
    public static <T> Collector<T, ?, Integer> countingInt() {
        return Collectors.countingInt();
    }

    /**
     * 
     * @param mapper
     * @return
     * @see Collectors#summingInt(ToIntFunction)
     */
    public static <T> Collector<T, ?, Integer> summingInt(final ToIntFunction<? super T> mapper) {
        return Collectors.summingInt(mapper);
    }

    /**
     * 
     * @param mapper
     * @return
     * @see Collectors#summingLong(ToLongFunction)
     */
    public static <T> Collector<T, ?, Long> summingLong(final ToLongFunction<? super T> mapper) {
        return Collectors.summingLong(mapper);
    }

    /**
     * 
     * @param mapper
     * @return
     * @see Collectors#summarizingDouble(ToDoubleFunction)
     */
    public static <T> Collector<T, ?, Double> summingDouble(final ToDoubleFunction<? super T> mapper) {
        return Collectors.summingDouble(mapper);
    }

    /**
     * 
     * @param mapper
     * @return
     * @see Collectors#averagingInt(ToIntFunction)
     */
    public static <T> Collector<T, ?, Double> averagingInt(final ToIntFunction<? super T> mapper) {
        return Collectors.averagingInt(mapper);
    }

    /**
     * 
     * @param mapper
     * @return
     * @see Collectors#averagingLong(ToLongFunction)
     */
    public static <T> Collector<T, ?, Double> averagingLong(final ToLongFunction<? super T> mapper) {
        return Collectors.averagingLong(mapper);
    }

    /**
     * 
     * @param mapper
     * @return
     * @see Collectors#averagingDouble(ToDoubleFunction)
     */
    public static <T> Collector<T, ?, Double> averagingDouble(final ToDoubleFunction<? super T> mapper) {
        return Collectors.averagingDouble(mapper);
    }

    public static final class Factory {

        private static final IntFunction<boolean[]> BOOLEAN_ARRAY = new IntFunction<boolean[]>() {
            @Override
            public boolean[] apply(int len) {
                return new boolean[len];
            }
        };

        private static final IntFunction<char[]> CHAR_ARRAY = new IntFunction<char[]>() {
            @Override
            public char[] apply(int len) {
                return new char[len];
            }
        };

        private static final IntFunction<byte[]> BYTE_ARRAY = new IntFunction<byte[]>() {
            @Override
            public byte[] apply(int len) {
                return new byte[len];
            }
        };

        private static final IntFunction<short[]> SHORT_ARRAY = new IntFunction<short[]>() {
            @Override
            public short[] apply(int len) {
                return new short[len];
            }
        };

        private static final IntFunction<int[]> INT_ARRAY = new IntFunction<int[]>() {
            @Override
            public int[] apply(int len) {
                return new int[len];
            }
        };

        private static final IntFunction<long[]> LONG_ARRAY = new IntFunction<long[]>() {
            @Override
            public long[] apply(int len) {
                return new long[len];
            }
        };

        private static final IntFunction<float[]> FLOAT_ARRAY = new IntFunction<float[]>() {
            @Override
            public float[] apply(int len) {
                return new float[len];
            }
        };

        private static final IntFunction<double[]> DOUBLE_ARRAY = new IntFunction<double[]>() {
            @Override
            public double[] apply(int len) {
                return new double[len];
            }
        };

        private static final IntFunction<String[]> STRING_ARRAY = new IntFunction<String[]>() {
            @Override
            public String[] apply(int len) {
                return new String[len];
            }
        };

        private static final IntFunction<Object[]> OBJECT_ARRAY = new IntFunction<Object[]>() {
            @Override
            public Object[] apply(int len) {
                return new Object[len];
            }
        };

        private static final IntFunction<BooleanList> BOOLEAN_LIST = new IntFunction<BooleanList>() {
            @Override
            public BooleanList apply(int len) {
                return new BooleanList(len);
            }
        };

        private static final IntFunction<CharList> CHAR_LIST = new IntFunction<CharList>() {
            @Override
            public CharList apply(int len) {
                return new CharList(len);
            }
        };

        private static final IntFunction<ByteList> BYTE_LIST = new IntFunction<ByteList>() {
            @Override
            public ByteList apply(int len) {
                return new ByteList(len);
            }
        };

        private static final IntFunction<ShortList> SHORT_LIST = new IntFunction<ShortList>() {
            @Override
            public ShortList apply(int len) {
                return new ShortList(len);
            }
        };

        private static final IntFunction<IntList> INT_LIST = new IntFunction<IntList>() {
            @Override
            public IntList apply(int len) {
                return new IntList(len);
            }
        };

        private static final IntFunction<LongList> LONG_LIST = new IntFunction<LongList>() {
            @Override
            public LongList apply(int len) {
                return new LongList(len);
            }
        };

        private static final IntFunction<FloatList> FLOAT_LIST = new IntFunction<FloatList>() {
            @Override
            public FloatList apply(int len) {
                return new FloatList(len);
            }
        };

        private static final IntFunction<DoubleList> DOUBLE_LIST = new IntFunction<DoubleList>() {
            @Override
            public DoubleList apply(int len) {
                return new DoubleList(len);
            }
        };
        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super List> LIST_FACTORY = new IntFunction<List>() {
            @Override
            public List apply(int len) {
                return new ArrayList<>(len);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super LinkedList> LINKED_LIST_FACTORY = new IntFunction<LinkedList>() {
            @Override
            public LinkedList apply(int len) {
                return new LinkedList<>();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super Set> SET_FACTORY = new IntFunction<Set>() {
            @Override
            public Set apply(int len) {
                return new HashSet<>(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super LinkedHashSet> LINKED_HASH_SET_FACTORY = new IntFunction<LinkedHashSet>() {
            @Override
            public LinkedHashSet apply(int len) {
                return new LinkedHashSet<>(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super TreeSet> TREE_SET_FACTORY = new IntFunction<TreeSet>() {
            @Override
            public TreeSet apply(int len) {
                return new TreeSet<>();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super Map> MAP_FACTORY = new IntFunction<Map>() {
            @Override
            public Map apply(int len) {
                return new HashMap<>(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super LinkedHashMap> LINKED_HASH_MAP_FACTORY = new IntFunction<LinkedHashMap>() {
            @Override
            public LinkedHashMap apply(int len) {
                return new LinkedHashMap<>(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super TreeMap> TREE_MAP_FACTORY = new IntFunction<TreeMap>() {
            @Override
            public TreeMap apply(int len) {
                return new TreeMap<>();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super ConcurrentHashMap> CONCURRENT_HASH_MAP_FACTORY = new IntFunction<ConcurrentHashMap>() {
            @Override
            public ConcurrentHashMap apply(int len) {
                return new ConcurrentHashMap(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super Queue> QUEUE_FACTORY = new IntFunction<Queue>() {
            @Override
            public Queue apply(int len) {
                return new LinkedList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super ArrayDeque> ARRAY_DEQUE_FACTORY = new IntFunction<ArrayDeque>() {
            @Override
            public ArrayDeque apply(int len) {
                return new ArrayDeque(len);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super LinkedBlockingQueue> LINKED_BLOCKING_QUEUE_FACTORY = new IntFunction<LinkedBlockingQueue>() {
            @Override
            public LinkedBlockingQueue apply(int len) {
                return new LinkedBlockingQueue(len);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super ConcurrentLinkedQueue> CONCURRENT_LINKED_QUEUE_FACTORY = new IntFunction<ConcurrentLinkedQueue>() {
            @Override
            public ConcurrentLinkedQueue apply(int len) {
                return new ConcurrentLinkedQueue();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super PriorityQueue> PRIORITY_QUEUE_FACTORY = new IntFunction<PriorityQueue>() {
            @Override
            public PriorityQueue apply(int len) {
                return new PriorityQueue(len);
            }
        };

        private Factory() {
            // singleton.
        }

        public static IntFunction<boolean[]> ofBooleanArray() {
            return BOOLEAN_ARRAY;
        }

        public static IntFunction<char[]> ofCharArray() {
            return CHAR_ARRAY;
        }

        public static IntFunction<byte[]> ofByteArray() {
            return BYTE_ARRAY;
        }

        public static IntFunction<short[]> ofShortArray() {
            return SHORT_ARRAY;
        }

        public static IntFunction<int[]> ofIntArray() {
            return INT_ARRAY;
        }

        public static IntFunction<long[]> ofLongArray() {
            return LONG_ARRAY;
        }

        public static IntFunction<float[]> ofFloatArray() {
            return FLOAT_ARRAY;
        }

        public static IntFunction<double[]> ofDoubleArray() {
            return DOUBLE_ARRAY;
        }

        public static IntFunction<String[]> ofStringArray() {
            return STRING_ARRAY;
        }

        public static IntFunction<Object[]> ofObjectArray() {
            return OBJECT_ARRAY;
        }

        public static IntFunction<BooleanList> ofBooleanList() {
            return BOOLEAN_LIST;
        }

        public static IntFunction<CharList> ofCharList() {
            return CHAR_LIST;
        }

        public static IntFunction<ByteList> ofByteList() {
            return BYTE_LIST;
        }

        public static IntFunction<ShortList> ofShortList() {
            return SHORT_LIST;
        }

        public static IntFunction<IntList> ofIntList() {
            return INT_LIST;
        }

        public static IntFunction<LongList> ofLongList() {
            return LONG_LIST;
        }

        public static IntFunction<FloatList> ofFloatList() {
            return FLOAT_LIST;
        }

        public static IntFunction<DoubleList> ofDoubleList() {
            return DOUBLE_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<List<T>> ofList() {
            return (IntFunction) LIST_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<LinkedList<T>> ofLinkedList() {
            return (IntFunction) LINKED_LIST_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<Set<T>> ofSet() {
            return (IntFunction) SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<LinkedHashSet<T>> ofLinkedHashSet() {
            return (IntFunction) LINKED_HASH_SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<TreeSet<T>> ofTreeSet() {
            return (IntFunction) TREE_SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<Map<K, V>> ofMap() {
            return (IntFunction) MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<LinkedHashMap<K, V>> ofLinkedHashMap() {
            return (IntFunction) LINKED_HASH_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<TreeMap<K, V>> ofTreeMap() {
            return (IntFunction) TREE_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
            return (IntFunction) CONCURRENT_HASH_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<Queue<T>> ofQueue() {
            return (IntFunction) QUEUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<ArrayDeque<T>> ofArrayDeque() {
            return (IntFunction) ARRAY_DEQUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
            return (IntFunction) LINKED_BLOCKING_QUEUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
            return (IntFunction) CONCURRENT_LINKED_QUEUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<PriorityQueue<T>> ofPriorityQueue() {
            return (IntFunction) PRIORITY_QUEUE_FACTORY;
        }
    }

    public static final class Suppliers {
        private static final Supplier<String> UUID = new Supplier<String>() {
            @Override
            public String get() {
                return N.uuid();
            }
        };

        private static final Supplier<String> GUID = new Supplier<String>() {
            @Override
            public String get() {
                return N.guid();
            }
        };

        private static final Supplier<boolean[]> EMPTY_BOOLEAN_ARRAY = new Supplier<boolean[]>() {
            @Override
            public boolean[] get() {
                return N.EMPTY_BOOLEAN_ARRAY;
            }
        };

        private static final Supplier<char[]> EMPTY_CHAR_ARRAY = new Supplier<char[]>() {
            @Override
            public char[] get() {
                return N.EMPTY_CHAR_ARRAY;
            }
        };

        private static final Supplier<byte[]> EMPTY_BYTE_ARRAY = new Supplier<byte[]>() {
            @Override
            public byte[] get() {
                return N.EMPTY_BYTE_ARRAY;
            }
        };

        private static final Supplier<short[]> EMPTY_SHORT_ARRAY = new Supplier<short[]>() {
            @Override
            public short[] get() {
                return N.EMPTY_SHORT_ARRAY;
            }
        };

        private static final Supplier<int[]> EMPTY_INT_ARRAY = new Supplier<int[]>() {
            @Override
            public int[] get() {
                return N.EMPTY_INT_ARRAY;
            }
        };

        private static final Supplier<long[]> EMPTY_LONG_ARRAY = new Supplier<long[]>() {
            @Override
            public long[] get() {
                return N.EMPTY_LONG_ARRAY;
            }
        };

        private static final Supplier<float[]> EMPTY_FLOAT_ARRAY = new Supplier<float[]>() {
            @Override
            public float[] get() {
                return N.EMPTY_FLOAT_ARRAY;
            }
        };

        private static final Supplier<double[]> EMPTY_DOUBLE_ARRAY = new Supplier<double[]>() {
            @Override
            public double[] get() {
                return N.EMPTY_DOUBLE_ARRAY;
            }
        };

        private static final Supplier<String[]> EMPTY_STRING_ARRAY = new Supplier<String[]>() {
            @Override
            public String[] get() {
                return N.EMPTY_STRING_ARRAY;
            }
        };

        private static final Supplier<Object[]> EMPTY_OBJECT_ARRAY = new Supplier<Object[]>() {
            @Override
            public Object[] get() {
                return N.EMPTY_OBJECT_ARRAY;
            }
        };

        private static final Supplier<BooleanList> BOOLEAN_LIST = new Supplier<BooleanList>() {
            @Override
            public BooleanList get() {
                return new BooleanList();
            }
        };

        private static final Supplier<CharList> CHAR_LIST = new Supplier<CharList>() {
            @Override
            public CharList get() {
                return new CharList();
            }
        };

        private static final Supplier<ByteList> BYTE_LIST = new Supplier<ByteList>() {
            @Override
            public ByteList get() {
                return new ByteList();
            }
        };

        private static final Supplier<ShortList> SHORT_LIST = new Supplier<ShortList>() {
            @Override
            public ShortList get() {
                return new ShortList();
            }
        };

        private static final Supplier<IntList> INT_LIST = new Supplier<IntList>() {
            @Override
            public IntList get() {
                return new IntList();
            }
        };

        private static final Supplier<LongList> LONG_LIST = new Supplier<LongList>() {
            @Override
            public LongList get() {
                return new LongList();
            }
        };

        private static final Supplier<FloatList> FLOAT_LIST = new Supplier<FloatList>() {
            @Override
            public FloatList get() {
                return new FloatList();
            }
        };

        private static final Supplier<DoubleList> DOUBLE_LIST = new Supplier<DoubleList>() {
            @Override
            public DoubleList get() {
                return new DoubleList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super List> LIST = new Supplier<List>() {
            @Override
            public List get() {
                return new ArrayList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super LinkedList> LINKED_LIST = new Supplier<LinkedList>() {
            @Override
            public LinkedList get() {
                return new LinkedList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super Set> SET = new Supplier<Set>() {
            @Override
            public Set get() {
                return new HashSet();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super LinkedHashSet> LINKED_HASH_SET = new Supplier<LinkedHashSet>() {
            @Override
            public LinkedHashSet get() {
                return new LinkedHashSet();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super TreeSet> TREE_SET = new Supplier<TreeSet>() {
            @Override
            public TreeSet get() {
                return new TreeSet();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super Map> MAP = new Supplier<Map>() {
            @Override
            public Map get() {
                return new HashMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super LinkedHashMap> LINKED_HASH_MAP = new Supplier<LinkedHashMap>() {
            @Override
            public LinkedHashMap get() {
                return new LinkedHashMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super TreeMap> TREE_MAP = new Supplier<TreeMap>() {
            @Override
            public TreeMap get() {
                return new TreeMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super ConcurrentHashMap> CONCURRENT_HASH_MAP = new Supplier<ConcurrentHashMap>() {
            @Override
            public ConcurrentHashMap get() {
                return new ConcurrentHashMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super Queue> QUEUE = new Supplier<Queue>() {
            @Override
            public Queue get() {
                return new LinkedList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super ArrayDeque> ARRAY_DEQUE = new Supplier<ArrayDeque>() {
            @Override
            public ArrayDeque get() {
                return new ArrayDeque();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super LinkedBlockingQueue> LINKED_BLOCKING_QUEUE = new Supplier<LinkedBlockingQueue>() {
            @Override
            public LinkedBlockingQueue get() {
                return new LinkedBlockingQueue();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super ConcurrentLinkedQueue> CONCURRENT_LINKED_QUEUE = new Supplier<ConcurrentLinkedQueue>() {
            @Override
            public ConcurrentLinkedQueue get() {
                return new ConcurrentLinkedQueue();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super PriorityQueue> PRIORITY_QUEUE = new Supplier<PriorityQueue>() {
            @Override
            public PriorityQueue get() {
                return new PriorityQueue();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super Multiset> MULTISET = new Supplier<Multiset>() {
            @Override
            public Multiset get() {
                return new Multiset();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super LongMultiset> LONG_MULTISET = new Supplier<LongMultiset>() {
            @Override
            public LongMultiset get() {
                return new LongMultiset();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super ListMultimap> MULTIMAP = new Supplier<ListMultimap>() {
            @Override
            public ListMultimap get() {
                return N.newListMultimap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super BiMap> BI_MAP = new Supplier<BiMap>() {
            @Override
            public BiMap get() {
                return new BiMap();
            }
        };

        private static final Supplier<StringBuilder> STRING_BUILDER = new Supplier<StringBuilder>() {
            @Override
            public StringBuilder get() {
                return new StringBuilder();
            }
        };

        private Suppliers() {
            // singleton.
        }

        public static Supplier<String> ofUUID() {
            return UUID;
        }

        public static Supplier<String> ofGUID() {
            return GUID;
        }

        public static Supplier<boolean[]> ofEmptyBooleanArray() {
            return EMPTY_BOOLEAN_ARRAY;
        }

        public static Supplier<char[]> ofEmptyCharArray() {
            return EMPTY_CHAR_ARRAY;
        }

        public static Supplier<byte[]> ofEmptyByteArray() {
            return EMPTY_BYTE_ARRAY;
        }

        public static Supplier<short[]> ofEmptyShortArray() {
            return EMPTY_SHORT_ARRAY;
        }

        public static Supplier<int[]> ofEmptyIntArray() {
            return EMPTY_INT_ARRAY;
        }

        public static Supplier<long[]> ofEmptyLongArray() {
            return EMPTY_LONG_ARRAY;
        }

        public static Supplier<float[]> ofEmptyFloatArray() {
            return EMPTY_FLOAT_ARRAY;
        }

        public static Supplier<double[]> ofEmptyDoubleArray() {
            return EMPTY_DOUBLE_ARRAY;
        }

        public static Supplier<String[]> ofEmptyStringArray() {
            return EMPTY_STRING_ARRAY;
        }

        public static Supplier<Object[]> ofEmptyObjectArray() {
            return EMPTY_OBJECT_ARRAY;
        }

        public static Supplier<BooleanList> ofBooleanList() {
            return BOOLEAN_LIST;
        }

        public static Supplier<CharList> ofCharList() {
            return CHAR_LIST;
        }

        public static Supplier<ByteList> ofByteList() {
            return BYTE_LIST;
        }

        public static Supplier<ShortList> ofShortList() {
            return SHORT_LIST;
        }

        public static Supplier<IntList> ofIntList() {
            return INT_LIST;
        }

        public static Supplier<LongList> ofLongList() {
            return LONG_LIST;
        }

        public static Supplier<FloatList> ofFloatList() {
            return FLOAT_LIST;
        }

        public static Supplier<DoubleList> ofDoubleList() {
            return DOUBLE_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<List<T>> ofList() {
            return (Supplier) LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<LinkedList<T>> ofLinkedList() {
            return (Supplier) LINKED_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<Set<T>> ofSet() {
            return (Supplier) SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<LinkedHashSet<T>> ofLinkedHashSet() {
            return (Supplier) LINKED_HASH_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<TreeSet<T>> ofTreeSet() {
            return (Supplier) TREE_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<Map<K, V>> ofMap() {
            return (Supplier) MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<LinkedHashMap<K, V>> ofLinkedHashMap() {
            return (Supplier) LINKED_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<TreeMap<K, V>> ofTreeMap() {
            return (Supplier) TREE_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<ConcurrentMap<K, V>> ofConcurrentMap() {
            return (Supplier) CONCURRENT_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
            return (Supplier) CONCURRENT_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<Queue<T>> ofQueue() {
            return (Supplier) QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<Deque<T>> ofDeque() {
            return (Supplier) ARRAY_DEQUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<ArrayDeque<T>> ofArrayDeque() {
            return (Supplier) ARRAY_DEQUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
            return (Supplier) LINKED_BLOCKING_QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
            return (Supplier) CONCURRENT_LINKED_QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<PriorityQueue<T>> ofPriorityQueue() {
            return (Supplier) PRIORITY_QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<Multiset<T>> ofMultiset() {
            return (Supplier) MULTISET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<LongMultiset<T>> ofLongMultiset() {
            return (Supplier) LONG_MULTISET;
        }

        @SuppressWarnings("rawtypes")
        public static <K, v> Supplier<ListMultimap<K, v>> ofMultimap() {
            return (Supplier) MULTIMAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<BiMap<K, V>> ofBiMap() {
            return (Supplier) BI_MAP;
        }

        public static Supplier<StringBuilder> ofStringBuilder() {
            return STRING_BUILDER;
        }
    }

    public static final class Predicates {

        private Predicates() {
            // singleton.
        }

        public static <T, U> Predicate<T> of(final U u, final BiPredicate<? super T, ? super U> predicate) {
            Objects.requireNonNull(predicate);

            return new Predicate<T>() {
                @Override
                public boolean test(T t) {
                    return predicate.test(t, u);
                }
            };
        }

        /**
         * Remove the continuous repeat elements.
         * Returns a stateful predicate which should not be used in parallel stream.
         * 
         * @return
         */
        public static <T> Predicate<T> skipRepeats() {
            return new Predicate<T>() {
                private T pre = (T) NULL;

                @Override
                public boolean test(T value) {
                    boolean res = pre == NULL || N.equals(value, pre) == false;
                    pre = value;
                    return res;
                }
            };
        }

        public static <T> Predicate<T> indexed(final IndexedPredicate<T> predicate) {
            return Fn.indexed(predicate);
        }
    }

    public static final class BiPredicates {

        @SuppressWarnings("rawtypes")
        private static final BiPredicate ALWAYS_TRUE = new BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return true;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiPredicate ALWAYS_FALSE = new BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return false;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiPredicate EQUAL = new BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return N.equals(t, u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiPredicate NOT_EQUAL = new BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return !N.equals(t, u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiPredicate<? extends Comparable, ? extends Comparable> GREATER_THAN = new BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) > 0;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiPredicate<? extends Comparable, ? extends Comparable> GREATER_EQUAL = new BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) >= 0;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiPredicate<? extends Comparable, ? extends Comparable> LESS_THAN = new BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) < 0;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiPredicate<? extends Comparable, ? extends Comparable> LESS_EQUAL = new BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) <= 0;
            }
        };

        private BiPredicates() {
            // singleton.
        }

        public static <T, U> BiPredicate<T, U> alwaysTrue() {
            return ALWAYS_TRUE;
        }

        public static <T, U> BiPredicate<T, U> alwaysFalse() {
            return ALWAYS_FALSE;
        }

        public static <U, T> BiPredicate<U, T> indexed(final IndexedBiPredicate<U, T> predicate) {
            return Fn.indexed(predicate);
        }
    }

    public static final class TriPredicates {

        @SuppressWarnings("rawtypes")
        private static final TriPredicate ALWAYS_TRUE = new TriPredicate() {
            @Override
            public boolean test(Object a, Object b, Object c) {
                return true;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final TriPredicate ALWAYS_FALSE = new TriPredicate() {
            @Override
            public boolean test(Object a, Object b, Object c) {
                return false;
            }
        };

        private TriPredicates() {
            // singleton.
        }

        public static <A, B, C> TriPredicate<A, B, C> alwaysTrue() {
            return ALWAYS_TRUE;
        }

        public static <A, B, C> TriPredicate<A, B, C> alwaysFalse() {
            return ALWAYS_FALSE;
        }

    }

    public static final class Consumers {

        private Consumers() {
            // singleton.
        }

        public static <T, U> Consumer<T> of(final U u, final BiConsumer<? super T, ? super U> action) {
            Objects.requireNonNull(action);

            return new Consumer<T>() {
                @Override
                public void accept(T t) {
                    action.accept(t, u);
                }
            };
        }

        /**
         * Returns a <code>Consumer</code> which calls the specified <code>func</code>.
         * 
         * @param func
         * @return
         */
        public static <T> Consumer<T> of(final Function<? super T, ?> func) {
            return new Consumer<T>() {
                @Override
                public void accept(T t) {
                    func.apply(t);
                }
            };
        }

        public static <T> Consumer<T> indexed(final IndexedConsumer<T> action) {
            return Fn.indeXed(action);
        }
    }

    public static final class BiConsumers {

        @SuppressWarnings("rawtypes")
        private static final BiConsumer DO_NOTHING = new BiConsumer() {
            @Override
            public void accept(Object t, Object u) {
                // do nothing.
            }
        };

        private static final BiConsumer<Collection<Object>, Object> ADD = new BiConsumer<Collection<Object>, Object>() {
            @Override
            public void accept(Collection<Object> t, Object u) {
                t.add(u);
            }
        };

        private static final BiConsumer<Collection<Object>, Collection<Object>> ADD_ALL = new BiConsumer<Collection<Object>, Collection<Object>>() {
            @Override
            public void accept(Collection<Object> t, Collection<Object> u) {
                t.addAll(u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiConsumer<PrimitiveList, PrimitiveList> ADD_ALL_2 = new BiConsumer<PrimitiveList, PrimitiveList>() {
            @Override
            public void accept(PrimitiveList t, PrimitiveList u) {
                t.addAll(u);
            }
        };

        private static final BiConsumer<Collection<Object>, Object> REMOVE = new BiConsumer<Collection<Object>, Object>() {
            @Override
            public void accept(Collection<Object> t, Object u) {
                t.remove(u);
            }
        };

        private static final BiConsumer<Collection<Object>, Collection<Object>> REMOVE_ALL = new BiConsumer<Collection<Object>, Collection<Object>>() {
            @Override
            public void accept(Collection<Object> t, Collection<Object> u) {
                t.removeAll(u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiConsumer<PrimitiveList, PrimitiveList> REMOVE_ALL_2 = new BiConsumer<PrimitiveList, PrimitiveList>() {
            @Override
            public void accept(PrimitiveList t, PrimitiveList u) {
                t.removeAll(u);
            }
        };

        private static final BiConsumer<Map<Object, Object>, Map.Entry<Object, Object>> PUT = new BiConsumer<Map<Object, Object>, Map.Entry<Object, Object>>() {
            @Override
            public void accept(Map<Object, Object> t, Map.Entry<Object, Object> u) {
                t.put(u.getKey(), u.getValue());
            }
        };

        private static final BiConsumer<Map<Object, Object>, Map<Object, Object>> PUT_ALL = new BiConsumer<Map<Object, Object>, Map<Object, Object>>() {
            @Override
            public void accept(Map<Object, Object> t, Map<Object, Object> u) {
                t.putAll(u);
            }
        };

        private static final BiConsumer<Map<Object, Object>, Object> REMOVE_BY_KEY = new BiConsumer<Map<Object, Object>, Object>() {
            @Override
            public void accept(Map<Object, Object> t, Object u) {
                t.remove(u);
            }
        };

        private static final BiConsumer<Joiner, Joiner> MERGE = new BiConsumer<Joiner, Joiner>() {
            @Override
            public void accept(Joiner t, Joiner u) {
                t.merge(u);
            }
        };

        private static final BiConsumer<StringBuilder, Object> APPEND = new BiConsumer<StringBuilder, Object>() {
            @Override
            public void accept(StringBuilder t, Object u) {
                t.append(u);
            }
        };

        private BiConsumers() {
            // singleton.
        }

        public static <T, U> BiConsumer<T, U> doNothing() {
            return DO_NOTHING;
        }

        public static <T, C extends Collection<? super T>> BiConsumer<C, T> ofAdd() {
            return (BiConsumer<C, T>) ADD;
        }

        public static <T, C extends Collection<T>> BiConsumer<C, C> ofAddAll() {
            return (BiConsumer<C, C>) ADD_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends PrimitiveList> BiConsumer<T, T> ofAddAll2() {
            return (BiConsumer<T, T>) ADD_ALL_2;
        }

        public static <T, C extends Collection<? super T>> BiConsumer<C, T> ofRemove() {
            return (BiConsumer<C, T>) REMOVE;
        }

        public static <T, C extends Collection<T>> BiConsumer<C, C> ofRemoveAll() {
            return (BiConsumer<C, C>) REMOVE_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends PrimitiveList> BiConsumer<T, T> ofRemoveAll2() {
            return (BiConsumer<T, T>) REMOVE_ALL_2;
        }

        public static <K, V, M extends Map<K, V>, E extends Map.Entry<K, V>> BiConsumer<M, E> ofPut() {
            return (BiConsumer<M, E>) PUT;
        }

        public static <K, V, M extends Map<K, V>> BiConsumer<M, M> ofPutAll() {
            return (BiConsumer<M, M>) PUT_ALL;
        }

        public static <K, V, M extends Map<K, V>> BiConsumer<M, K> ofRemoveByKey() {
            return (BiConsumer<M, K>) REMOVE_BY_KEY;
        }

        public static BiConsumer<Joiner, Joiner> ofMerge() {
            return MERGE;
        }

        public static <T> BiConsumer<StringBuilder, T> ofAppend() {
            return (BiConsumer<StringBuilder, T>) APPEND;
        }

        /**
         * Returns a <code>BiConsumer</code> which calls the specified <code>func</code>.
         * 
         * @param func
         * @return
         */
        public static <T, U> BiConsumer<T, U> of(final BiFunction<? super T, ? super U, ?> func) {
            return new BiConsumer<T, U>() {
                @Override
                public void accept(T t, U u) {
                    func.apply(t, u);
                }
            };
        }

        public static <U, T> BiConsumer<U, T> indexed(final IndexedBiConsumer<U, T> action) {
            return Fn.indeXed(action);
        }
    }

    public static final class TriConsumers {
        private TriConsumers() {
            // singleton.
        }

        /**
         * Returns a <code>TriConsumer</code> which calls the specified <code>func</code>.
         * 
         * @param func
         * @return
         */
        public static <A, B, C> TriConsumer<A, B, C> of(final TriFunction<? super A, ? super B, ? super C, ?> func) {
            return new TriConsumer<A, B, C>() {
                @Override
                public void accept(A a, B b, C c) {
                    func.apply(a, b, c);
                }
            };
        }
    }

    public static final class Functions {

        private Functions() {
            // singleton.
        }

        public static <T, U, R> Function<T, R> of(final U u, final BiFunction<? super T, ? super U, R> func) {
            Objects.requireNonNull(func);

            return new Function<T, R>() {
                @Override
                public R apply(T t) {
                    return func.apply(t, u);
                }
            };
        }

        /**
         * Returns a <code>Function</code> which calls the specified <code>action</code> and always return an empty<code>Optional</code> if <code>action</code> is executed successfully.
         * 
         * @param action
         * @return
         */
        public static <T, R> Function<T, Optional<R>> of(final Consumer<? super T> action) {
            return new Function<T, Optional<R>>() {
                @Override
                public Optional<R> apply(T t) {
                    action.accept(t);
                    return Optional.empty();
                }
            };
        }

        public static <T, R> Function<T, R> indexed(final IndexedFunction<T, R> func) {
            return Fn.indexeD(func);
        }
    }

    public static final class BiFunctions {

        private static final BiFunction<Object, Object, Object> RETURN_FIRST = new BiFunction<Object, Object, Object>() {
            @Override
            public Object apply(Object t, Object u) {
                return t;
            }
        };

        private static final BiFunction<Object, Object, Object> RETURN_SECOND = new BiFunction<Object, Object, Object>() {
            @Override
            public Object apply(Object t, Object u) {
                return u;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiFunction<Object, Object, Tuple2> TUPLE = new BiFunction<Object, Object, Tuple2>() {
            @Override
            public Tuple2 apply(Object t, Object u) {
                return Tuple.of(t, u);
            }
        };

        private static final BiFunction<Collection<Object>, Object, Collection<Object>> ADD = new BiFunction<Collection<Object>, Object, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Object u) {
                t.add(u);
                return t;
            }
        };

        private static final BiFunction<Collection<Object>, Collection<Object>, Collection<Object>> ADD_ALL = new BiFunction<Collection<Object>, Collection<Object>, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.addAll(u);
                return t;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiFunction<PrimitiveList, PrimitiveList, PrimitiveList> ADD_ALL_2 = new BiFunction<PrimitiveList, PrimitiveList, PrimitiveList>() {
            @Override
            public PrimitiveList apply(PrimitiveList t, PrimitiveList u) {
                t.addAll(u);
                return t;
            }
        };

        private static final BiFunction<Collection<Object>, Object, Collection<Object>> REMOVE = new BiFunction<Collection<Object>, Object, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Object u) {
                t.remove(u);
                return t;
            }
        };

        private static final BiFunction<Collection<Object>, Collection<Object>, Collection<Object>> REMOVE_ALL = new BiFunction<Collection<Object>, Collection<Object>, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.removeAll(u);
                return t;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BiFunction<PrimitiveList, PrimitiveList, PrimitiveList> REMOVE_ALL_2 = new BiFunction<PrimitiveList, PrimitiveList, PrimitiveList>() {
            @Override
            public PrimitiveList apply(PrimitiveList t, PrimitiveList u) {
                t.removeAll(u);
                return t;
            }
        };

        private static final BiFunction<Map<Object, Object>, Map.Entry<Object, Object>, Map<Object, Object>> PUT = new BiFunction<Map<Object, Object>, Map.Entry<Object, Object>, Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map.Entry<Object, Object> u) {
                t.put(u.getKey(), u.getValue());
                return t;
            }
        };

        private static final BiFunction<Map<Object, Object>, Map<Object, Object>, Map<Object, Object>> PUT_ALL = new BiFunction<Map<Object, Object>, Map<Object, Object>, Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map<Object, Object> u) {
                t.putAll(u);
                return t;
            }
        };

        private static final BiFunction<Map<Object, Object>, Object, Map<Object, Object>> REMOVE_BY_KEY = new BiFunction<Map<Object, Object>, Object, Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Object u) {
                t.remove(u);
                return t;
            }
        };

        private static final BiFunction<Joiner, Joiner, Joiner> MERGE = new BiFunction<Joiner, Joiner, Joiner>() {
            @Override
            public Joiner apply(Joiner t, Joiner u) {
                return t.merge(u);
            }
        };

        private static final BiFunction<StringBuilder, Object, StringBuilder> APPEND = new BiFunction<StringBuilder, Object, StringBuilder>() {
            @Override
            public StringBuilder apply(StringBuilder t, Object u) {
                return t.append(u);
            }
        };

        private BiFunctions() {
            // singleton.
        }

        public static <T, U> BiFunction<T, U, T> returnFirst() {
            return (BiFunction<T, U, T>) RETURN_FIRST;
        }

        public static <T, U> BiFunction<T, U, U> returnSecond() {
            return (BiFunction<T, U, U>) RETURN_SECOND;
        }

        @SuppressWarnings("rawtypes")
        public static <T, U> BiFunction<T, U, Tuple2<T, U>> ofTuple() {
            return (BiFunction) TUPLE;
        }

        public static <T, C extends Collection<? super T>> BiFunction<C, T, C> ofAdd() {
            return (BiFunction<C, T, C>) ADD;
        }

        public static <T, C extends Collection<T>> BiFunction<C, C, C> ofAddAll() {
            return (BiFunction<C, C, C>) ADD_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends PrimitiveList> BiFunction<T, T, T> ofAddAll2() {
            return (BiFunction<T, T, T>) ADD_ALL_2;
        }

        public static <T, C extends Collection<? super T>> BiFunction<C, T, C> ofRemove() {
            return (BiFunction<C, T, C>) REMOVE;
        }

        public static <T, C extends Collection<T>> BiFunction<C, C, C> ofRemoveAll() {
            return (BiFunction<C, C, C>) REMOVE_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends PrimitiveList> BiFunction<T, T, T> ofRemoveAll2() {
            return (BiFunction<T, T, T>) REMOVE_ALL_2;
        }

        public static <K, V, M extends Map<K, V>, E extends Map.Entry<K, V>> BiFunction<M, E, M> ofPut() {
            return (BiFunction<M, E, M>) PUT;
        }

        public static <K, V, M extends Map<K, V>> BiFunction<M, M, M> ofPutAll() {
            return (BiFunction<M, M, M>) PUT_ALL;
        }

        public static <K, V, M extends Map<K, V>, U> BiFunction<M, K, M> ofRemoveByKey() {
            return (BiFunction<M, K, M>) REMOVE_BY_KEY;
        }

        public static BiFunction<Joiner, Joiner, Joiner> ofMerge() {
            return MERGE;
        }

        public static <T> BiFunction<StringBuilder, T, StringBuilder> ofAppend() {
            return (BiFunction<StringBuilder, T, StringBuilder>) APPEND;
        }

        /**
         * Returns a <code>BiFunction</code> which calls the specified <code>action</code> and always return an empty<code>Optional</code> if <code>action</code> is executed successfully.
         * 
         * @param action
         * @return
         */
        public static <T, U, R> BiFunction<T, U, Optional<R>> of(final BiConsumer<? super T, ? super U> action) {
            return new BiFunction<T, U, Optional<R>>() {
                @Override
                public Optional<R> apply(T t, U u) {
                    action.accept(t, u);
                    return Optional.empty();
                }
            };
        }

        public static <U, T, R> BiFunction<U, T, R> indexed(final IndexedBiFunction<U, T, R> func) {
            return Fn.indexeD(func);
        }
    }

    public static final class TriFunctions {

        @SuppressWarnings("rawtypes")
        private static final TriFunction<Object, Object, Object, Tuple3> TUPLE = new TriFunction<Object, Object, Object, Tuple3>() {
            @Override
            public Tuple3 apply(Object a, Object b, Object c) {
                return Tuple.of(a, b, c);
            }
        };

        private TriFunctions() {
            // singleton.
        }

        @SuppressWarnings("rawtypes")
        public static <A, B, C> TriFunction<A, B, C, Tuple3<A, B, C>> ofTuple() {
            return (TriFunction) TUPLE;
        }

        /**
         * Returns a <code>TriFunction</code> which calls the specified <code>action</code> and always return an empty<code>Optional</code> if <code>action</code> is executed successfully.
         * 
         * @param action
         * @return
         */
        public static <A, B, C, R> TriFunction<A, B, C, Optional<R>> of(final TriConsumer<? super A, ? super B, ? super C> action) {
            return new TriFunction<A, B, C, Optional<R>>() {
                @Override
                public Optional<R> apply(A a, B b, C c) {
                    action.accept(a, b, c);
                    return Optional.empty();
                }
            };
        }
    }

    public static final class BinaryOperators {
        @SuppressWarnings("rawtypes")
        private static final BinaryOperator THROWING_MERGER = new BinaryOperator() {
            @Override
            public Object apply(Object t, Object u) {
                throw new IllegalStateException(String.format("Duplicate key (attempted merging values %s and %s)", t, u));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BinaryOperator IGNORING_MERGER = new BinaryOperator() {
            @Override
            public Object apply(Object t, Object u) {
                return t;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BinaryOperator REPLACING_MERGER = new BinaryOperator() {
            @Override
            public Object apply(Object t, Object u) {
                return u;
            }
        };

        private static final BinaryOperator<Collection<Object>> ADD_ALL = new BinaryOperator<Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.addAll(u);
                return t;
            }
        };

        private static final BinaryOperator<Collection<Object>> REMOVE_ALL = new BinaryOperator<Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.removeAll(u);
                return t;
            }
        };

        private static final BinaryOperator<Map<Object, Object>> PUT_ALL = new BinaryOperator<Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map<Object, Object> u) {
                t.putAll(u);
                return t;
            }
        };

        private BinaryOperators() {
            // singleton.
        }

        public static <T, C extends Collection<T>> BinaryOperator<C> ofAddAll() {
            return (BinaryOperator<C>) ADD_ALL;
        }

        public static <T, C extends Collection<T>> BinaryOperator<C> ofRemoveAll() {
            return (BinaryOperator<C>) REMOVE_ALL;
        }

        public static <K, V, M extends Map<K, V>> BinaryOperator<M> ofPutAll() {
            return (BinaryOperator<M>) PUT_ALL;
        }

        public static BinaryOperator<Joiner> ofMerge() {
            return new BinaryOperator<Joiner>() {
                @Override
                public Joiner apply(Joiner t, Joiner u) {
                    return t.merge(u);
                }
            };
        }

        public static BinaryOperator<StringBuilder> ofAppend() {
            return new BinaryOperator<StringBuilder>() {
                @Override
                public StringBuilder apply(StringBuilder t, StringBuilder u) {
                    return t.append(u);
                }
            };
        }

        public static BinaryOperator<String> ofJoin(final String seperator) {
            return new BinaryOperator<String>() {
                @Override
                public String apply(String t, String u) {
                    return t + seperator + u;
                }
            };
        }

        public static <T> BinaryOperator<T> minBy(final Comparator<? super T> comparator) {
            Objects.requireNonNull(comparator);

            return new BinaryOperator<T>() {
                @Override
                public T apply(T t, T u) {
                    return comparator.compare(t, u) <= 0 ? t : u;
                }
            };
        }

        public static <T> BinaryOperator<T> maxBy(final Comparator<? super T> comparator) {
            Objects.requireNonNull(comparator);

            return new BinaryOperator<T>() {
                @Override
                public T apply(T t, T u) {
                    return comparator.compare(t, u) >= 0 ? t : u;
                }
            };
        }
    }

    public static final class UnaryOperators {
        @SuppressWarnings("rawtypes")
        private static final UnaryOperator IDENTITY = new UnaryOperator() {
            @Override
            public Object apply(Object t) {
                return t;
            }
        };

        private UnaryOperators() {
            // singleton.
        }

        public static <T> UnaryOperator<T> identity() {
            return IDENTITY;
        }
    }
}
