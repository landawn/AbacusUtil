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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.Tuple.Tuple1;
import com.landawn.abacus.util.Tuple.Tuple2;
import com.landawn.abacus.util.Tuple.Tuple3;
import com.landawn.abacus.util.Tuple.Tuple4;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.BooleanSupplier;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleBiPredicate;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedBiConsumer;
import com.landawn.abacus.util.function.IndexedBiFunction;
import com.landawn.abacus.util.function.IndexedBiPredicate;
import com.landawn.abacus.util.function.IndexedConsumer;
import com.landawn.abacus.util.function.IndexedFunction;
import com.landawn.abacus.util.function.IndexedPredicate;
import com.landawn.abacus.util.function.IntBiPredicate;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.LongBiPredicate;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.QuadFunction;
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
import com.landawn.abacus.util.stream.Stream;

/**
 * Factory utility class for functional interfaces.
 * 
 * <br>
 * Note: Don't save and reuse any Function/Predicat/Consumer/... created by calling the methods in this class.
 * The method should be called every time.
 * </br>
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
 * 
 * @author haiyang li
 *
 */
public final class Fn extends Comparators {

    private static final Object NONE = new Object();

    @SuppressWarnings("rawtypes")
    public static final IntFunction<Map<String, Object>> FACTORY_OF_MAP = (IntFunction) Factory.MAP_FACTORY;
    @SuppressWarnings("rawtypes")
    public static final IntFunction<LinkedHashMap<String, Object>> FACTORY_OF_LINKED_HASH_MAP = (IntFunction) Factory.LINKED_HASH_MAP_FACTORY;
    @SuppressWarnings("rawtypes")
    public static final Supplier<Map<String, Object>> SUPPLIER_OF_MAP = (Supplier) Suppliers.MAP;
    @SuppressWarnings("rawtypes")
    public static final Supplier<LinkedHashMap<String, Object>> SUPPLIER_OF_LINKED_HASH_MAP = (Supplier) Suppliers.LINKED_HASH_MAP;

    private static final com.landawn.abacus.util.function.Runnable EMPTY_ACTION = new com.landawn.abacus.util.function.Runnable() {
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

    private static final Consumer<AutoCloseable> CLOSE = new Consumer<AutoCloseable>() {
        @Override
        public void accept(AutoCloseable value) {
            IOUtil.close(value);
        }
    };

    private static final Consumer<AutoCloseable> CLOSE_QUIETLY = new Consumer<AutoCloseable>() {
        @Override
        public void accept(AutoCloseable value) {
            IOUtil.closeQuietly(value);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_EQUAL = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), "=", N.toString(value)));
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_HYPHEN = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), "-", N.toString(value)));
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_UNDERSCORE = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), "_", N.toString(value)));
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_COLON = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), ":", N.toString(value)));
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_COLON_SPACE = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), ": ", N.toString(value)));
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_COMMA = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), ",", N.toString(value)));
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_COMMA_SPACE = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), ", ", N.toString(value)));
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiConsumer PRINTLN_EMPTY = new BiConsumer() {
        @Override
        public void accept(Object key, Object value) {
            N.println(StringUtil.concat(N.toString(key), N.toString(value)));
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
    private static final Function TO_STRING = new Function() {
        @Override
        public Object apply(Object t) {
            return N.toString(t);
        }
    };

    private static final Function<String, String> TO_CAMEL_CASE = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return StringUtil.toCamelCase(t);
        }
    };

    private static final Function<String, String> TO_LOWER_CASE = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return StringUtil.toLowerCase(t);
        }
    };

    private static final Function<String, String> TO_LOWER_CASE_WITH_UNDERSCORE = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return StringUtil.toLowerCaseWithUnderscore(t);
        }
    };

    private static final Function<String, String> TO_UPPER_CASE = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return StringUtil.toUpperCase(t);
        }
    };

    private static final Function<String, String> TO_UPPER_CASE_WITH_UNDERSCORE = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return StringUtil.toUpperCaseWithUnderscore(t);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final BiFunction COMPARE = new BiFunction<Comparable, Comparable, Integer>() {
        @Override
        public Integer apply(Comparable a, Comparable b) {
            return N.compare(a, b);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Function IDENTITY = new Function() {
        @Override
        public Object apply(Object t) {
            return t;
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

    private static final Function<String, String> NULL_TO_EMPTY = new Function<String, String>() {
        @Override
        public String apply(String t) {
            return t == null ? N.EMPTY_STRING : t;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Function<List, List> NULL_TO_EMPTY_L = new Function<List, List>() {
        @Override
        public List apply(List t) {
            return t == null ? N.emptyList() : t;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Function<Set, Set> NULL_TO_EMPTY_S = new Function<Set, Set>() {
        @Override
        public Set apply(Set t) {
            return t == null ? N.emptySet() : t;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Function<Map, Map> NULL_TO_EMPTY_M = new Function<Map, Map>() {
        @Override
        public Map apply(Map t) {
            return t == null ? N.emptyMap() : t;
        }
    };

    private static final Function<String, Integer> LENGTH = new Function<String, Integer>() {
        @Override
        public Integer apply(String t) {
            return N.len(t);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Function<Collection, Integer> SIZE = new Function<Collection, Integer>() {
        @Override
        public Integer apply(Collection t) {
            return N.len(t);
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

    private static final Function<Map.Entry<Object, Object>, Map.Entry<Object, Object>> INVERSE = new Function<Map.Entry<Object, Object>, Map.Entry<Object, Object>>() {
        @Override
        public Map.Entry<Object, Object> apply(Map.Entry<Object, Object> t) {
            return new SimpleImmutableEntry<>(t.getValue(), t.getKey());
        }
    };

    private static final BiFunction<Object, Object, Map.Entry<Object, Object>> ENTRY = new BiFunction<Object, Object, Map.Entry<Object, Object>>() {
        @Override
        public Map.Entry<Object, Object> apply(Object key, Object value) {
            return new SimpleImmutableEntry<>(key, value);
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

    private static final Function<Object, Tuple1<Object>> TUPLE_1 = new Function<Object, Tuple1<Object>>() {
        @Override
        public Tuple1<Object> apply(Object t) {
            return Tuple.of(t);
        }
    };

    private static final BiFunction<Object, Object, Tuple2<Object, Object>> TUPLE_2 = new BiFunction<Object, Object, Tuple2<Object, Object>>() {
        @Override
        public Tuple2<Object, Object> apply(Object t, Object u) {
            return Tuple.of(t, u);
        }
    };

    private static final TriFunction<Object, Object, Object, Tuple3<Object, Object, Object>> TUPLE_3 = new TriFunction<Object, Object, Object, Tuple3<Object, Object, Object>>() {
        @Override
        public Tuple3<Object, Object, Object> apply(Object a, Object b, Object c) {
            return Tuple.of(a, b, c);
        }
    };

    private static final QuadFunction<Object, Object, Object, Object, Tuple4<Object, Object, Object, Object>> TUPLE_4 = new QuadFunction<Object, Object, Object, Object, Tuple4<Object, Object, Object, Object>>() {
        @Override
        public Tuple4<Object, Object, Object, Object> apply(Object a, Object b, Object c, Object d) {
            return Tuple.of(a, b, c, d);
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

    private static final Predicate<CharSequence> IS_NULL_OR_EMPTY = new Predicate<CharSequence>() {
        @Override
        public boolean test(CharSequence value) {
            return value == null || value.length() == 0;
        }
    };

    private static final Predicate<CharSequence> IS_NULL_OR_EMPTY_OR_BLANK = new Predicate<CharSequence>() {
        @Override
        public boolean test(CharSequence value) {
            return N.isNullOrEmptyOrBlank(value);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Predicate NOT_NULL = new Predicate() {
        @Override
        public boolean test(Object value) {
            return value != null;
        }
    };

    private static final Predicate<CharSequence> NOT_NULL_OR_EMPTY = new Predicate<CharSequence>() {
        @Override
        public boolean test(CharSequence value) {
            return value != null && value.length() > 0;
        }
    };

    private static final Predicate<CharSequence> NOT_NULL_OR_EMPTY_OR_BLANK = new Predicate<CharSequence>() {
        @Override
        public boolean test(CharSequence value) {
            return N.notNullOrEmptyOrBlank(value);
        }
    };

    private static final ToIntFunction<Number> NUM_TO_INT_FUNC = new ToIntFunction<Number>() {
        @Override
        public int applyAsInt(Number value) {
            return value == null ? 0 : value.intValue();
        }
    };

    private static final ToLongFunction<Number> NUM_TO_LONG_FUNC = new ToLongFunction<Number>() {
        @Override
        public long applyAsLong(Number value) {
            return value == null ? 0 : value.longValue();
        }
    };

    private static final ToDoubleFunction<Number> NUM_TO_DOUBLE_FUNC = new ToDoubleFunction<Number>() {
        @Override
        public double applyAsDouble(Number value) {
            return value == null ? 0d : value.doubleValue();
        }
    };

    private Fn() {
        super();
        // Singleton.
    }

    public static <T> T get(final Supplier<T> supplier) {
        return supplier.get();
    }

    /**
     * Returns a {@code Supplier} which returns a single instance created by calling the specified {@code supplier.get()}.
     * 
     * @param supplier
     * @return
     */
    public static <T> Supplier<T> single(final Supplier<T> supplier) {
        return new Supplier<T>() {
            private T instance = (T) NONE;

            @Override
            public T get() {
                synchronized (this) {
                    if (instance == NONE) {
                        instance = supplier.get();
                    }

                    return instance;
                }
            }
        };
    }

    public static com.landawn.abacus.util.function.Runnable close(final AutoCloseable closeable) {
        return new com.landawn.abacus.util.function.Runnable() {
            private volatile boolean isClosed = false;

            @Override
            public void run() {
                if (isClosed) {
                    return;
                }

                isClosed = true;
                IOUtil.close(closeable);
            }
        };
    }

    @SafeVarargs
    public static com.landawn.abacus.util.function.Runnable closeAll(final AutoCloseable... a) {
        return new com.landawn.abacus.util.function.Runnable() {
            private volatile boolean isClosed = false;

            @Override
            public void run() {
                if (isClosed) {
                    return;
                }

                isClosed = true;
                IOUtil.closeAll(a);
            }
        };
    }

    public static com.landawn.abacus.util.function.Runnable closeQuietly(final AutoCloseable closeable) {
        return new com.landawn.abacus.util.function.Runnable() {
            private volatile boolean isClosed = false;

            @Override
            public void run() {
                if (isClosed) {
                    return;
                }

                isClosed = true;
                IOUtil.closeQuietly(closeable);
            }
        };
    }

    @SafeVarargs
    public static com.landawn.abacus.util.function.Runnable closeAllQuietly(final AutoCloseable... a) {
        return new com.landawn.abacus.util.function.Runnable() {
            private volatile boolean isClosed = false;

            @Override
            public void run() {
                if (isClosed) {
                    return;
                }

                isClosed = true;
                IOUtil.closeAllQuietly(a);
            }
        };
    }

    public static com.landawn.abacus.util.function.Runnable emptyAction() {
        return EMPTY_ACTION;
    }

    public static <T> Consumer<T> doNothing() {
        return DO_NOTHING;
    }

    public static <T extends AutoCloseable> Consumer<T> close() {
        return (Consumer<T>) CLOSE;
    }

    public static <T extends AutoCloseable> Consumer<T> closeQuietly() {
        return (Consumer<T>) CLOSE_QUIETLY;
    }

    public static <T> Consumer<T> println() {
        return PRINTLN;
    }

    public static <T, U> BiConsumer<T, U> println(final String separator) {
        N.checkArgNotNull(separator);

        switch (separator) {
            case "=":
                return PRINTLN_EQUAL;

            case ":":
                return PRINTLN_COLON;

            case ": ":
                return PRINTLN_COLON_SPACE;

            case "-":
                return PRINTLN_HYPHEN;

            case "_":
                return PRINTLN_UNDERSCORE;

            case ",":
                return PRINTLN_COMMA;

            case ", ":
                return PRINTLN_COMMA_SPACE;

            case "":
                return PRINTLN_EMPTY;

            default:
                return new BiConsumer<T, U>() {
                    @Override
                    public void accept(T t, U u) {
                        N.println(t + separator + u);
                    }
                };
        }
    }

    public static <T> Function<T, String> toStr() {
        return TO_STRING;
    }

    public static Function<String, String> toCamelCase() {
        return TO_CAMEL_CASE;
    }

    public static Function<String, String> toLowerCase() {
        return TO_LOWER_CASE;
    }

    public static Function<String, String> toLowerCaseWithUnderscore() {
        return TO_LOWER_CASE_WITH_UNDERSCORE;
    }

    public static Function<String, String> toUpperCase() {
        return TO_UPPER_CASE;
    }

    public static Function<String, String> toUpperCaseWithUnderscore() {
        return TO_UPPER_CASE_WITH_UNDERSCORE;
    }

    public static <T> Function<T, T> identity() {
        return IDENTITY;
    }

    public static <K, T> Function<T, Keyed<K, T>> keyed(final Function<? super T, K> keyExtractor) {
        N.checkArgNotNull(keyExtractor);

        return new Function<T, Keyed<K, T>>() {
            @Override
            public Keyed<K, T> apply(T t) {
                return Keyed.of(keyExtractor.apply(t), t);
            }
        };
    }

    private static final Function<Keyed<?, Object>, Object> VAL = new Function<Keyed<?, Object>, Object>() {
        @Override
        public Object apply(Keyed<?, Object> t) {
            return t.val();
        }
    };

    @SuppressWarnings("rawtypes")
    public static <K, T> Function<Keyed<K, T>, T> val() {
        return (Function) VAL;
    }

    private static final Function<Map.Entry<Keyed<Object, Object>, Object>, Object> KK = new Function<Map.Entry<Keyed<Object, Object>, Object>, Object>() {
        @Override
        public Object apply(Map.Entry<Keyed<Object, Object>, Object> t) {
            return t.getKey().val();
        }
    };

    @SuppressWarnings("rawtypes")
    public static <T, K, V> Function<Map.Entry<Keyed<K, T>, Long>, T> kk() {
        return (Function) KK;
    }

    private static final Function<Object, Wrapper<Object>> WRAP = new Function<Object, Wrapper<Object>>() {
        @Override
        public Wrapper<Object> apply(Object t) {
            return Wrapper.of(t);
        }
    };

    @SuppressWarnings("rawtypes")
    public static <T> Function<T, Wrapper<T>> wrap() {
        return (Function) WRAP;
    }

    public static <T> Function<T, Wrapper<T>> wrap(final ToIntFunction<? super T> hashFunction, final BiPredicate<? super T, ? super T> equalsFunction) {
        N.checkArgNotNull(hashFunction);
        N.checkArgNotNull(equalsFunction);

        return new Function<T, Wrapper<T>>() {
            @Override
            public Wrapper<T> apply(T t) {
                return Wrapper.of(t, hashFunction, equalsFunction);
            }
        };
    }

    private static final Function<Wrapper<Object>, Object> UNWRAP = new Function<Wrapper<Object>, Object>() {
        @Override
        public Object apply(Wrapper<Object> t) {
            return t.value();
        }
    };

    @SuppressWarnings("rawtypes")
    public static <K, T> Function<Wrapper<T>, T> unwrap() {
        return (Function) UNWRAP;
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
    public static <K, V> Function<Entry<K, V>, Entry<V, K>> inverse() {
        return (Function) INVERSE;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> BiFunction<K, V, Map.Entry<K, V>> entry() {
        return (BiFunction) ENTRY;
    }

    public static <K, T> Function<T, Map.Entry<K, T>> entry(final K key) {
        return new Function<T, Map.Entry<K, T>>() {
            @Override
            public Entry<K, T> apply(T t) {
                return new SimpleImmutableEntry<>(key, t);
            }
        };
    }

    public static <K, T> Function<T, Map.Entry<K, T>> entry(final Function<? super T, K> keyExtractor) {
        N.checkArgNotNull(keyExtractor);

        return new Function<T, Map.Entry<K, T>>() {
            @Override
            public Entry<K, T> apply(T t) {
                return new SimpleImmutableEntry<>(keyExtractor.apply(t), t);
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static <L, R> BiFunction<L, R, Pair<L, R>> pair() {
        return (BiFunction) PAIR;
    }

    @SuppressWarnings("rawtypes")
    public static <L, M, R> TriFunction<L, M, R, Triple<L, M, R>> triple() {
        return (TriFunction) TRIPLE;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Function<T, Tuple1<T>> tuple1() {
        return (Function) TUPLE_1;
    }

    @SuppressWarnings("rawtypes")
    public static <T, U> BiFunction<T, U, Tuple2<T, U>> tuple2() {
        return (BiFunction) TUPLE_2;
    }

    @SuppressWarnings("rawtypes")
    public static <A, B, C> TriFunction<A, B, C, Tuple3<A, B, C>> tuple3() {
        return (TriFunction) TUPLE_3;
    }

    @SuppressWarnings({ "rawtypes" })
    public static <A, B, C, D> QuadFunction<A, B, C, D, Tuple4<A, B, C, D>> tuple4() {
        return (QuadFunction) TUPLE_4;
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

    public static Function<String, String> nullToEmpty() {
        return NULL_TO_EMPTY;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Function<List<T>, List<T>> nullToEmptyL() {
        return (Function) NULL_TO_EMPTY_L;
    }

    @SuppressWarnings("rawtypes")
    public static <T> Function<Set<T>, Set<T>> nullToEmptyS() {
        return (Function) NULL_TO_EMPTY_S;
    }

    @SuppressWarnings("rawtypes")
    public static <K, V> Function<Map<K, V>, Map<K, V>> nullToEmptyM() {
        return (Function) NULL_TO_EMPTY_M;
    }

    public static Function<String, Integer> length() {
        return LENGTH;
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Collection> Function<T, Integer> size() {
        return (Function<T, Integer>) SIZE;
    }

    public static <T, U> Function<T, U> cast(final Class<U> clazz) {
        N.checkArgNotNull(clazz);

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

    public static <T extends CharSequence> Predicate<T> isNullOrEmpty() {
        return (Predicate<T>) IS_NULL_OR_EMPTY;
    }

    public static <T extends CharSequence> Predicate<T> isNullOrEmptyOrBlank() {
        return (Predicate<T>) IS_NULL_OR_EMPTY_OR_BLANK;
    }

    public static <T> Predicate<T> notNull() {
        return NOT_NULL;
    }

    public static <T extends CharSequence> Predicate<T> notNullOrEmpty() {
        return (Predicate<T>) NOT_NULL_OR_EMPTY;
    }

    public static <T extends CharSequence> Predicate<T> notNullOrEmptyOrBlank() {
        return (Predicate<T>) NOT_NULL_OR_EMPTY_OR_BLANK;
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

    /**
     * Checks if the value/element: {@code minValue < e < maxValue}.
     * 
     * @param minValue
     * @param maxValue
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Predicate<T> between(final T minValue, final T maxValue) {
        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, minValue) > 0 && N.compare(value, maxValue) < 0;
            }
        };
    }

    public static <T> Predicate<T> in(final Collection<?> c) {
        N.checkArgNotNull(c);

        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return c.contains(value);
            }
        };
    }

    public static <T> Predicate<T> notIn(final Collection<?> c) {
        N.checkArgNotNull(c);

        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !c.contains(value);
            }
        };
    }

    public static <T> Predicate<T> instanceOf(final Class<?> clazz) {
        N.checkArgNotNull(clazz);

        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return clazz.isInstance(value);
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static Predicate<Class> subtypeOf(final Class<?> clazz) {
        N.checkArgNotNull(clazz);

        return new Predicate<Class>() {
            @Override
            public boolean test(Class value) {
                return clazz.isAssignableFrom(value);
            }
        };
    }

    public static Predicate<String> startsWith(final String prefix) {
        N.checkArgNotNull(prefix);

        return new Predicate<String>() {
            @Override
            public boolean test(String value) {
                return value.startsWith(prefix);
            }
        };
    }

    public static Predicate<String> endsWith(final String suffix) {
        N.checkArgNotNull(suffix);

        return new Predicate<String>() {
            @Override
            public boolean test(String value) {
                return value.endsWith(suffix);
            }
        };
    }

    public static Predicate<String> contains(final String str) {
        N.checkArgNotNull(str);

        return new Predicate<String>() {
            @Override
            public boolean test(String value) {
                return value.contains(str);
            }
        };
    }

    public static Predicate<CharSequence> matches(final Pattern pattern) {
        N.checkArgNotNull(pattern);

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

    public static <T> Predicate<T> not(final Predicate<T> predicate) {
        N.checkArgNotNull(predicate);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return !predicate.test(t);
            }
        };
    }

    public static <T, U> BiPredicate<T, U> not(final BiPredicate<T, U> biPredicate) {
        N.checkArgNotNull(biPredicate);

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                return !biPredicate.test(t, u);
            }
        };
    }

    public static <A, B, C> TriPredicate<A, B, C> not(final TriPredicate<A, B, C> triPredicate) {
        N.checkArgNotNull(triPredicate);

        return new TriPredicate<A, B, C>() {
            @Override
            public boolean test(A a, B b, C c) {
                return !triPredicate.test(a, b, c);
            }
        };
    }

    public static BooleanSupplier and(final BooleanSupplier first, final BooleanSupplier second) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);

        return new BooleanSupplier() {
            @Override
            public boolean getAsBoolean() {
                return first.getAsBoolean() && second.getAsBoolean();
            }
        };
    }

    public static BooleanSupplier and(final BooleanSupplier first, final BooleanSupplier second, final BooleanSupplier third) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);
        N.checkArgNotNull(third);

        return new BooleanSupplier() {
            @Override
            public boolean getAsBoolean() {
                return first.getAsBoolean() && second.getAsBoolean() && third.getAsBoolean();
            }
        };
    }

    public static <T> Predicate<T> and(final Predicate<? super T> first, final Predicate<? super T> second) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return first.test(t) && second.test(t);
            }
        };
    }

    public static <T> Predicate<T> and(final Predicate<? super T> first, final Predicate<? super T> second, final Predicate<? super T> third) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);
        N.checkArgNotNull(third);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return first.test(t) && second.test(t) && third.test(t);
            }
        };
    }

    public static <T> Predicate<T> and(final Collection<Predicate<? super T>> c) {
        N.checkArgNotNullOrEmpty(c, "c");

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                for (Predicate<? super T> p : c) {
                    if (p.test(t) == false) {
                        return false;
                    }
                }

                return true;
            }
        };
    }

    public static <T, U> BiPredicate<T, U> and(final BiPredicate<? super T, ? super U> first, final BiPredicate<? super T, ? super U> second) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                return first.test(t, u) && second.test(t, u);
            }
        };
    }

    public static <T, U> BiPredicate<T, U> and(final BiPredicate<? super T, ? super U> first, final BiPredicate<? super T, ? super U> second,
            final BiPredicate<? super T, ? super U> third) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);
        N.checkArgNotNull(third);

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                return first.test(t, u) && second.test(t, u) && third.test(t, u);
            }
        };
    }

    public static <T, U> BiPredicate<T, U> and(final List<BiPredicate<? super T, ? super U>> c) {
        N.checkArgNotNullOrEmpty(c, "c");

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                for (BiPredicate<? super T, ? super U> p : c) {
                    if (p.test(t, u) == false) {
                        return false;
                    }
                }

                return true;
            }
        };
    }

    public static BooleanSupplier or(final BooleanSupplier first, final BooleanSupplier second) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);

        return new BooleanSupplier() {
            @Override
            public boolean getAsBoolean() {
                return first.getAsBoolean() || second.getAsBoolean();
            }
        };
    }

    public static BooleanSupplier or(final BooleanSupplier first, final BooleanSupplier second, final BooleanSupplier third) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);
        N.checkArgNotNull(third);

        return new BooleanSupplier() {
            @Override
            public boolean getAsBoolean() {
                return first.getAsBoolean() || second.getAsBoolean() || third.getAsBoolean();
            }
        };
    }

    public static <T> Predicate<T> or(final Predicate<? super T> first, final Predicate<? super T> second) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return first.test(t) || second.test(t);
            }
        };
    }

    public static <T> Predicate<T> or(final Predicate<? super T> first, final Predicate<? super T> second, final Predicate<? super T> third) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);
        N.checkArgNotNull(third);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return first.test(t) || second.test(t) || third.test(t);
            }
        };
    }

    public static <T> Predicate<T> or(final Collection<Predicate<? super T>> c) {
        N.checkArgNotNullOrEmpty(c, "c");

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                for (Predicate<? super T> p : c) {
                    if (p.test(t)) {
                        return true;
                    }
                }

                return false;
            }
        };
    }

    public static <T, U> BiPredicate<T, U> or(final BiPredicate<? super T, ? super U> first, final BiPredicate<? super T, ? super U> second) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                return first.test(t, u) || second.test(t, u);
            }
        };
    }

    public static <T, U> BiPredicate<T, U> or(final BiPredicate<? super T, ? super U> first, final BiPredicate<? super T, ? super U> second,
            final BiPredicate<? super T, ? super U> third) {
        N.checkArgNotNull(first);
        N.checkArgNotNull(second);
        N.checkArgNotNull(third);

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                return first.test(t, u) || second.test(t, u) || third.test(t, u);
            }
        };
    }

    public static <T, U> BiPredicate<T, U> or(final List<BiPredicate<? super T, ? super U>> c) {
        N.checkArgNotNullOrEmpty(c, "c");

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                for (BiPredicate<? super T, ? super U> p : c) {
                    if (p.test(t, u)) {
                        return true;
                    }
                }

                return false;
            }
        };
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testByKey(final Predicate<? super K> predicate) {
        N.checkArgNotNull(predicate);

        return new Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getKey());
            }
        };
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testByValue(final Predicate<? super V> predicate) {
        N.checkArgNotNull(predicate);

        return new Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getValue());
            }
        };
    }

    /**
     * Returns the specified instance.
     * 
     * @param predicate
     * @return
     * @deprecated replaced by {@link Fn#p(Predicate)}.
     */
    @Deprecated
    static <T> Predicate<T> test(final Predicate<T> predicate) {
        return predicate;
    }

    /**
     * Returns the specified instance.
     * 
     * @param predicate
     * @return
     * @deprecated replaced by {@link Fn#p(BiPredicate)}.
     */
    @Deprecated
    static <T, U> BiPredicate<T, U> test(final BiPredicate<T, U> predicate) {
        return predicate;
    }

    /**
     * Returns the specified instance.
     * 
     * @param predicate
     * @return
     * @deprecated
     */
    @Deprecated
    static <A, B, C> TriPredicate<A, B, C> test(final TriPredicate<A, B, C> predicate) {
        return predicate;
    }

    public static <K, V> Consumer<Map.Entry<K, V>> acceptByKey(final Consumer<? super K> consumer) {
        N.checkArgNotNull(consumer);

        return new Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getKey());
            }
        };
    }

    public static <K, V> Consumer<Map.Entry<K, V>> acceptByValue(final Consumer<? super V> consumer) {
        N.checkArgNotNull(consumer);

        return new Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getValue());
            }
        };
    }

    public static <K, V, R> Function<Map.Entry<K, V>, R> applyByKey(final Function<? super K, R> func) {
        N.checkArgNotNull(func);

        return new Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getKey());
            }
        };
    }

    public static <K, V, R> Function<Map.Entry<K, V>, R> applyByValue(final Function<? super V, R> func) {
        N.checkArgNotNull(func);

        return new Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getValue());
            }
        };
    }

    public static <K, V, KK> Function<Map.Entry<K, V>, Map.Entry<KK, V>> mapKey(final Function<? super K, KK> func) {
        N.checkArgNotNull(func);

        return new Function<Map.Entry<K, V>, Map.Entry<KK, V>>() {
            @Override
            public Map.Entry<KK, V> apply(Entry<K, V> entry) {
                return new SimpleImmutableEntry<>(func.apply(entry.getKey()), entry.getValue());
            }
        };
    }

    public static <K, V, VV> Function<Map.Entry<K, V>, Map.Entry<K, VV>> mapValue(final Function<? super V, VV> func) {
        N.checkArgNotNull(func);

        return new Function<Map.Entry<K, V>, Map.Entry<K, VV>>() {
            @Override
            public Map.Entry<K, VV> apply(Entry<K, V> entry) {
                return new SimpleImmutableEntry<>(entry.getKey(), func.apply(entry.getValue()));
            }
        };
    }

    public static <K, V> Predicate<Map.Entry<K, V>> testKeyVal(final BiPredicate<? super K, ? super V> predicate) {
        N.checkArgNotNull(predicate);

        return new Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getKey(), entry.getValue());
            }
        };
    }

    public static <K, V> Consumer<Map.Entry<K, V>> acceptKeyVal(final BiConsumer<? super K, ? super V> consumer) {
        N.checkArgNotNull(consumer);

        return new Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getKey(), entry.getValue());
            }
        };
    }

    public static <K, V, R> Function<Map.Entry<K, V>, R> applyKeyVal(final BiFunction<? super K, ? super V, R> func) {
        N.checkArgNotNull(func);

        return new Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getKey(), entry.getValue());
            }
        };
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

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static <T extends Number> ToIntFunction<T> numToInt() {
        return (ToIntFunction) NUM_TO_INT_FUNC;
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static <T extends Number> ToLongFunction<T> numToLong() {
        return (ToLongFunction) NUM_TO_LONG_FUNC;
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static <T extends Number> ToDoubleFunction<T> numToDouble() {
        return (ToDoubleFunction) NUM_TO_DOUBLE_FUNC;
    }

    /**
     * 
     * @param limit
     * @param predicate
     * @return
     */
    public static <T> Predicate<T> limitThenFilter(final int limit, final Predicate<T> predicate) {
        N.checkArgNotNull(predicate);

        return new Predicate<T>() {
            private final AtomicInteger counter = new AtomicInteger(limit);

            @Override
            public boolean test(T t) {
                return counter.getAndDecrement() > 0 && predicate.test(t);
            }
        };
    }

    public static <T, U> BiPredicate<T, U> limitThenFilter(final int limit, final BiPredicate<T, U> predicate) {
        N.checkArgNotNull(predicate);

        return new BiPredicate<T, U>() {
            private final AtomicInteger counter = new AtomicInteger(limit);

            @Override
            public boolean test(T t, U u) {
                return counter.getAndDecrement() > 0 && predicate.test(t, u);
            }
        };
    }

    public static <T> Predicate<T> filterThenLimit(final Predicate<T> predicate, final int limit) {
        N.checkArgNotNull(predicate);

        return new Predicate<T>() {
            private final AtomicInteger counter = new AtomicInteger(limit);

            @Override
            public boolean test(T t) {
                return predicate.test(t) && counter.getAndDecrement() > 0;
            }
        };
    }

    public static <T, U> BiPredicate<T, U> filterThenLimit(final BiPredicate<T, U> predicate, final int limit) {
        N.checkArgNotNull(predicate);

        return new BiPredicate<T, U>() {
            private final AtomicInteger counter = new AtomicInteger(limit);

            @Override
            public boolean test(T t, U u) {
                return predicate.test(t, u) && counter.getAndDecrement() > 0;
            }
        };
    }

    /**
     * Returns a stateful <code>Function</code> which should not be used in parallel stream.
     * 
     * @return
     */
    public static <T> Function<T, Indexed<T>> indexed() {
        return new Function<T, Indexed<T>>() {
            private final MutableLong idx = new MutableLong(0);

            @Override
            public Indexed<T> apply(T t) {
                return Indexed.of(t, idx.getAndIncrement());
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
        return Predicates.indexed(predicate);
    }

    /**
     * Returns a stateful <code>BiPredicate</code> which should not be used in parallel stream.
     * 
     * @param predicate
     * @return
     * @deprecated replaced by {@code BiPredicates#indexed(IndexedBiPredicate)}.
     */
    @Deprecated
    static <U, T> BiPredicate<U, T> indexed(final IndexedBiPredicate<U, T> predicate) {
        return BiPredicates.indexed(predicate);
    }

    /**
     * Returns a stateful <code>Function</code> which should not be used in parallel stream.
     * 
     * @param func
     * @return
     * @deprecated replaced by {@code Functions#indexed(IndexedFunction)}.
     */
    @Deprecated
    static <T, R> Function<T, R> indexedd(final IndexedFunction<T, R> func) {
        return Functions.indexed(func);
    }

    /**
     * Returns a stateful <code>BiFunction</code> which should not be used in parallel stream.
     * 
     * @param func
     * @return
     * @deprecated replaced by {@code BiFunctions#indexed(IndexedBiFunction)}.
     */
    @Deprecated
    static <U, T, R> BiFunction<U, T, R> indexedd(final IndexedBiFunction<U, T, R> func) {
        return BiFunctions.indexed(func);
    }

    /**
     * Returns a stateful <code>Consumer</code> which should not be used in parallel stream.
     * 
     * @param action
     * @return
     * @deprecated replaced by {@code Consumers#indexed(IndexedConsumer)}.
     */
    @Deprecated
    static <T> Consumer<T> indexeed(final IndexedConsumer<T> action) {
        return Consumers.indexed(action);
    }

    /**
     * Returns a stateful <code>BiConsumer</code> which should not be used in parallel stream.
     * 
     * @param action
     * @return
     * @deprecated replaced by {@code BiConsumers#indexed(IndexedBiConsumer)}.
     */
    @Deprecated
    static <U, T> BiConsumer<U, T> indexeed(final IndexedBiConsumer<U, T> action) {
        return BiConsumers.indexed(action);
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> Function<T, Integer> compareTo(final T target) {
        return new Function<T, Integer>() {
            @Override
            public Integer apply(T t) {
                return N.compare(t, target);
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static <T> Function<T, Integer> compareTo(final T target, final Comparator<? super T> cmp) {
        // N.checkArgNotNull(cmp);

        if (cmp == null || cmp == Comparators.naturalOrder()) {
            return (Function) compareTo((Comparable) target);
        }

        return new Function<T, Integer>() {
            @Override
            public Integer apply(T t) {
                return N.compare(t, target, cmp);
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static <T extends Comparable> BiFunction<T, T, Integer> compare() {
        return COMPARE;
    }

    public static <T> BiFunction<T, T, Integer> compare(final Comparator<? super T> cmp) {
        // N.checkArgNotNull(cmp);

        if (cmp == null || cmp == Comparators.naturalOrder()) {
            return COMPARE;
        }

        return new BiFunction<T, T, Integer>() {
            @Override
            public Integer apply(T a, T b) {
                return N.compare(a, b, cmp);
            }
        };
    }

    @Beta
    public static <T> Predicate<T> p(final Predicate<T> predicate) {
        return predicate;
    }

    @Beta
    public static <U, T> Predicate<T> p(final U u, final BiPredicate<T, U> biPredicate) {
        return Predicates.create(u, biPredicate);
    }

    @Beta
    public static <A, B, T> Predicate<T> p(final A a, final B b, final TriPredicate<T, A, B> triPredicate) {
        N.checkArgNotNull(triPredicate);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                return triPredicate.test(t, a, b);
            }
        };
    }

    @Beta
    public static <T, U> BiPredicate<T, U> p(final BiPredicate<T, U> biPredicate) {
        return biPredicate;
    }

    @Beta
    public static <T> Consumer<T> c(final Consumer<T> consumer) {
        return consumer;
    }

    @Beta
    public static <U, T> Consumer<T> c(final U u, final BiConsumer<T, U> biConsumer) {
        return Consumers.create(u, biConsumer);
    }

    @Beta
    public static <T, U> BiConsumer<T, U> c(final BiConsumer<T, U> biConsumer) {
        return biConsumer;
    }

    @Beta
    public static <T, R> Function<T, R> f(final Function<T, R> function) {
        return function;
    }

    @Beta
    public static <U, T, R> Function<T, R> f(final U u, final BiFunction<T, U, R> biFunction) {
        return Functions.create(u, biFunction);
    }

    @Beta
    public static <T, U, R> BiFunction<T, U, R> f(final BiFunction<T, U, R> biFunction) {
        return biFunction;
    }

    @Beta
    public static <T, E extends Exception> Predicate<T> pp(final Try.Predicate<T, E> predicate) {
        N.checkArgNotNull(predicate);

        return new Predicate<T>() {
            @Override
            public boolean test(T value) {
                try {
                    return predicate.test(value);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <U, T, E extends Exception> Predicate<T> pp(final U u, final Try.BiPredicate<T, U, E> biPredicate) {
        N.checkArgNotNull(biPredicate);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                try {
                    return biPredicate.test(t, u);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <A, B, T, E extends Exception> Predicate<T> pp(final A a, final B b, final Try.TriPredicate<T, A, B, E> triPredicate) {
        N.checkArgNotNull(triPredicate);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                try {
                    return triPredicate.test(t, a, b);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <T, U, E extends Exception> BiPredicate<T, U> pp(final Try.BiPredicate<T, U, E> biPredicate) {
        N.checkArgNotNull(biPredicate);

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                try {
                    return biPredicate.test(t, u);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <T, E extends Exception> Consumer<T> cc(final Try.Consumer<T, E> consumer) {
        N.checkArgNotNull(consumer);

        return new Consumer<T>() {
            @Override
            public void accept(T value) {
                try {
                    consumer.accept(value);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <U, T, E extends Exception> Consumer<T> cc(final U u, final Try.BiConsumer<T, U, E> biConsumer) {
        N.checkArgNotNull(biConsumer);

        return new Consumer<T>() {
            @Override
            public void accept(T t) {
                try {
                    biConsumer.accept(t, u);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <T, U, E extends Exception> BiConsumer<T, U> cc(final Try.BiConsumer<T, U, E> biConsumer) {
        N.checkArgNotNull(biConsumer);

        return new BiConsumer<T, U>() {
            @Override
            public void accept(T t, U u) {
                try {
                    biConsumer.accept(t, u);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <T, R, E extends Exception> Function<T, R> ff(final Try.Function<T, R, E> function) {
        N.checkArgNotNull(function);

        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                try {
                    return function.apply(t);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <U, T, R, E extends Exception> Function<T, R> ff(final U u, final Try.BiFunction<T, U, R, E> biFunction) {
        N.checkArgNotNull(biFunction);

        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                try {
                    return biFunction.apply(t, u);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <T, U, R, E extends Exception> BiFunction<T, U, R> ff(final Try.BiFunction<T, U, R, E> biFunction) {
        N.checkArgNotNull(biFunction);

        return new BiFunction<T, U, R>() {
            @Override
            public R apply(T t, U u) {
                try {
                    return biFunction.apply(t, u);
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        };
    }

    @Beta
    public static <T, E extends Exception> Try.Predicate<T, E> ep(final Try.Predicate<T, E> predicate) {
        return predicate;
    }

    @Beta
    public static <U, T, E extends Exception> Try.Predicate<T, E> ep(final U u, final Try.BiPredicate<T, U, E> biPredicate) {
        N.checkArgNotNull(biPredicate);

        return new Try.Predicate<T, E>() {
            @Override
            public boolean test(T t) throws E {
                return biPredicate.test(t, u);
            }
        };
    }

    @Beta
    public static <A, B, T, E extends Exception> Try.Predicate<T, E> ep(final A a, final B b, final Try.TriPredicate<T, A, B, E> triPredicate) {
        N.checkArgNotNull(triPredicate);

        return new Try.Predicate<T, E>() {
            @Override
            public boolean test(T t) throws E {
                return triPredicate.test(t, a, b);
            }
        };
    }

    @Beta
    public static <T, U, E extends Exception> Try.BiPredicate<T, U, E> ep(final Try.BiPredicate<T, U, E> biPredicate) {
        return biPredicate;
    }

    @Beta
    public static <T, E extends Exception> Try.Consumer<T, E> ec(final Try.Consumer<T, E> consumer) {
        return consumer;
    }

    @Beta
    public static <U, T, E extends Exception> Try.Consumer<T, E> ec(final U u, final Try.BiConsumer<T, U, E> biConsumer) {
        N.checkArgNotNull(biConsumer);

        return new Try.Consumer<T, E>() {
            @Override
            public void accept(T t) throws E {
                biConsumer.accept(t, u);
            }
        };
    }

    @Beta
    public static <T, U, E extends Exception> Try.BiConsumer<T, U, E> ec(final Try.BiConsumer<T, U, E> biConsumer) {
        return biConsumer;
    }

    @Beta
    public static <T, R, E extends Exception> Try.Function<T, R, E> ef(final Try.Function<T, R, E> function) {
        return function;
    }

    @Beta
    public static <U, T, R, E extends Exception> Try.Function<T, R, E> ef(final U u, final Try.BiFunction<T, U, R, E> biFunction) {
        N.checkArgNotNull(biFunction);

        return new Try.Function<T, R, E>() {
            @Override
            public R apply(T t) throws E {
                return biFunction.apply(t, u);
            }
        };
    }

    @Beta
    public static <U, T, R, E extends Exception> Try.BiFunction<T, U, R, E> ef(final Try.BiFunction<T, U, R, E> biFunction) {
        return biFunction;
    }

    /**
     * Synchronized {@code Predicate}
     * 
     * @param target to synchronized on
     * @param predicate 
     * @return
     */
    @Beta
    public static <T> Predicate<T> sp(final Object target, final Predicate<T> predicate) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(predicate, "predicate");

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                synchronized (target) {
                    return predicate.test(t);
                }
            }
        };
    }

    /**
     * Synchronized {@code Predicate}
     * 
     * @param target to synchronized on
     * @param u
     * @param biPredicate
     * @return
     */
    @Beta
    public static <U, T> Predicate<T> sp(final Object target, final U u, final BiPredicate<T, U> biPredicate) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(biPredicate, "biPredicate");

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                synchronized (target) {
                    return biPredicate.test(t, u);
                }
            }
        };
    }

    /**
     * Synchronized {@code Predicate}
     * 
     * @param target to synchronized on
     * @param a
     * @param b
     * @param triPredicate
     * @return
     */
    @Beta
    public static <A, B, T> Predicate<T> sp(final Object target, final A a, final B b, final TriPredicate<T, A, B> triPredicate) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(triPredicate, "triPredicate");

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                synchronized (target) {
                    return triPredicate.test(t, a, b);
                }
            }
        };
    }

    /**
     * Synchronized {@code BiPredicate}
     * 
     * @param target to synchronized on
     * @param biPredicate
     * @return
     */
    @Beta
    public static <T, U> BiPredicate<T, U> sp(final Object target, final BiPredicate<T, U> biPredicate) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(biPredicate, "biPredicate");

        return new BiPredicate<T, U>() {
            @Override
            public boolean test(T t, U u) {
                synchronized (target) {
                    return biPredicate.test(t, u);
                }
            }
        };
    }

    /**
     * Synchronized {@code Consumer}
     * 
     * @param target to synchronized on
     * @param consumer
     * @return
     */
    @Beta
    public static <T> Consumer<T> sc(final Object target, final Consumer<T> consumer) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(consumer, "consumer");

        return new Consumer<T>() {
            @Override
            public void accept(T t) {
                synchronized (target) {
                    consumer.accept(t);
                }
            }
        };
    }

    /**
     * Synchronized {@code Consumer}
     * 
     * @param target to synchronized on
     * @param u
     * @param biConsumer
     * @return
     */
    @Beta
    public static <U, T> Consumer<T> sc(final Object target, final U u, final BiConsumer<T, U> biConsumer) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(biConsumer, "biConsumer");

        return new Consumer<T>() {
            @Override
            public void accept(T t) {
                synchronized (target) {
                    biConsumer.accept(t, u);
                }
            }
        };
    }

    /**
     * Synchronized {@code BiConsumer}
     * 
     * @param target to synchronized on
     * @param biConsumer
     * @return
     */
    @Beta
    public static <T, U> BiConsumer<T, U> sc(final Object target, final BiConsumer<T, U> biConsumer) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(biConsumer, "biConsumer");

        return new BiConsumer<T, U>() {
            @Override
            public void accept(T t, U u) {
                synchronized (target) {
                    biConsumer.accept(t, u);
                }
            }
        };
    }

    /**
     * Synchronized {@code Function}
     * 
     * @param target to synchronized on
     * @param function
     * @return
     */
    @Beta
    public static <T, R> Function<T, R> sf(final Object target, final Function<T, R> function) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(function, "function");

        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                synchronized (target) {
                    return function.apply(t);
                }
            }
        };
    }

    /**
     * Synchronized {@code Function}
     * 
     * @param target to synchronized on
     * @param u
     * @param biFunction
     * @return
     */
    @Beta
    public static <U, T, R> Function<T, R> sf(final Object target, final U u, final BiFunction<T, U, R> biFunction) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(biFunction, "biFunction");

        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                synchronized (target) {
                    return biFunction.apply(t, u);
                }
            }
        };
    }

    /**
     * Synchronized {@code BiFunction}
     * 
     * @param target to synchronized on
     * @param biFunction
     * @return
     */
    @Beta
    public static <T, U, R> BiFunction<T, U, R> sf(final Object target, final BiFunction<T, U, R> biFunction) {
        N.checkArgNotNull(target, "target");
        N.checkArgNotNull(biFunction, "biFunction");

        return new BiFunction<T, U, R>() {
            @Override
            public R apply(T t, U u) {
                synchronized (target) {
                    return biFunction.apply(t, u);
                }
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

    /**
     * 
     * @return
     * @deprecated replaced by {@code BiConsumers#ofAddAll()}
     */
    @Deprecated
    static <T, C extends Collection<T>> BiConsumer<C, C> addAll() {
        return BiConsumers.<T, C> ofAddAll();
    }

    /**
     * 
     * @return
     * @deprecated replaced by {@code BiConsumers#ofPutAll()}
     */
    @Deprecated
    static <K, V, M extends Map<K, V>> BiConsumer<M, M> putAll() {
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

    public static <T, C extends Collection<T>> Collector<T, ?, C> toCollection(Supplier<? extends C> collectionFactory) {
        return Collectors.toCollection(collectionFactory);
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
    public static <T, K, V> Collector<T, ?, Map<K, V>> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends V> valueMapper) {
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
    public static <T, K, V> Collector<T, ?, Map<K, V>> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends V> valueMapper,
            BinaryOperator<V> mergeFunction) {
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
    public static <T, K, V, M extends Map<K, V>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends V> valueMapper, final Supplier<M> mapFactory) {
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
    public static <T, K, V, M extends Map<K, V>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> keyExtractor,
            final Function<? super T, ? extends V> valueMapper, final BinaryOperator<V> mergeFunction, final Supplier<M> mapFactory) {
        return Collectors.toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public static <T, K, A, D> Collector<T, ?, Map<K, D>> toMap(final Function<? super T, ? extends K> classifier,
            final Collector<? super T, A, D> downstream) {
        return Collectors.toMap(classifier, downstream);
    }

    /**
     * 
     * @param classifier
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public static <T, K, A, D, M extends Map<K, D>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> classifier,
            final Collector<? super T, A, D> downstream, final Supplier<M> mapFactory) {
        return Collectors.toMap(classifier, downstream, mapFactory);
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @param downstream
     * @return
     * @see Collectors#groupingBy(Function, Collector)
     */
    public static <T, K, U, A, D> Collector<T, ?, Map<K, D>> toMap(final Function<? super T, ? extends K> classifier,
            final Function<? super T, ? extends U> valueMapper, final Collector<? super U, A, D> downstream) {
        return Collectors.toMap(classifier, valueMapper, downstream);
    }

    /**
     * 
     * @param classifier
     * @param valueMapper
     * @param downstream
     * @param mapFactory
     * @return
     * @see Collectors#groupingBy(Function, Collector, Supplier)
     */
    public static <T, K, U, A, D, M extends Map<K, D>> Collector<T, ?, M> toMap(final Function<? super T, ? extends K> classifier,
            final Function<? super T, ? extends U> valueMapper, final Collector<? super U, A, D> downstream, final Supplier<M> mapFactory) {
        return Collectors.toMap(classifier, valueMapper, downstream, mapFactory);
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
    public static <T, K, V> Collector<T, ?, ImmutableMap<K, V>> toImmutableMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends V> valueMapper) {
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
    public static <T, K, V> Collector<T, ?, ImmutableMap<K, V>> toImmutableMap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends V> valueMapper, BinaryOperator<V> mergeFunction) {
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

    public static <T> Collector<T, ?, List<T>> filtering(final Predicate<? super T> predicate) {
        return Collectors.filtering(predicate);
    }

    public static <T, A, R> Collector<T, ?, R> filtering(final Predicate<? super T> predicate, final Collector<? super T, A, R> downstream) {
        return Collectors.filtering(predicate, downstream);
    }

    public static <T, U> Collector<T, ?, List<U>> mapping(final Function<? super T, ? extends U> mapper) {
        return Collectors.mapping(mapper);
    }

    public static <T, U, A, R> Collector<T, ?, R> mapping(final Function<? super T, ? extends U> mapper, final Collector<? super U, A, R> downstream) {
        return Collectors.mapping(mapper, downstream);
    }

    public static <T, U> Collector<T, ?, List<U>> flatMapping(final Function<? super T, ? extends Stream<? extends U>> mapper) {
        return Collectors.flatMapping(mapper);
    }

    public static <T, U, A, R> Collector<T, ?, R> flatMapping(final Function<? super T, ? extends Stream<? extends U>> mapper,
            final Collector<? super U, A, R> downstream) {
        return Collectors.flatMapping(mapper, downstream);
    }

    public static <T, U> Collector<T, ?, List<U>> flattMapping(final Function<? super T, ? extends Collection<? extends U>> mapper) {
        return Collectors.flattMapping(mapper);
    }

    public static <T, U, A, R> Collector<T, ?, R> flattMapping(final Function<? super T, ? extends Collection<? extends U>> mapper,
            final Collector<? super U, A, R> downstream) {
        return Collectors.flattMapping(mapper, downstream);
    }

    public static <T, A1, A2, R1, R2> Collector<T, Tuple2<A1, A2>, Tuple2<R1, R2>> combine(final Collector<? super T, A1, R1> collector1,
            final Collector<? super T, A2, R2> collector2) {
        return Collectors.combine(collector1, collector2);
    }

    public static <T, A1, A2, A3, R1, R2, R3> Collector<T, Tuple3<A1, A2, A3>, Tuple3<R1, R2, R3>> combine(final Collector<? super T, A1, R1> collector1,
            final Collector<? super T, A2, R2> collector2, final Collector<? super T, A3, R3> collector3) {
        return Collectors.combine(collector1, collector2, collector3);
    }

    public static <T, A1, A2, R1, R2> Collector<T, Tuple2<A1, A2>, Tuple2<R1, R2>> combine(final java.util.stream.Collector<? super T, A1, R1> collector1,
            final java.util.stream.Collector<? super T, A2, R2> collector2) {
        return Collectors.combine(collector1, collector2);
    }

    public static <T, A1, A2, A3, R1, R2, R3> Collector<T, Tuple3<A1, A2, A3>, Tuple3<R1, R2, R3>> combine(
            final java.util.stream.Collector<? super T, A1, R1> collector1, final java.util.stream.Collector<? super T, A2, R2> collector2,
            final java.util.stream.Collector<? super T, A3, R3> collector3) {
        return Collectors.combine(collector1, collector2, collector3);
    }

    public static class Factory {
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
        private static final IntFunction<? super Queue> QUEUE_FACTORY = new IntFunction<Queue>() {
            @Override
            public Queue apply(int len) {
                return new LinkedList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super Deque> DEQUE_FACTORY = new IntFunction<Deque>() {
            @Override
            public Deque apply(int len) {
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
        private static final IntFunction<? super IdentityHashMap> IDENTITY_HASH_MAP_FACTORY = new IntFunction<IdentityHashMap>() {
            @Override
            public IdentityHashMap apply(int len) {
                return new IdentityHashMap<>(N.initHashCapacity(len));
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
        private static final IntFunction<? super BiMap> BI_MAP_FACTORY = new IntFunction<BiMap>() {
            @Override
            public BiMap apply(int len) {
                return new BiMap(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super Multiset> MULTISET_FACTORY = new IntFunction<Multiset>() {
            @Override
            public Multiset apply(int len) {
                return new Multiset(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super LongMultiset> LONG_MULTISET_FACTORY = new IntFunction<LongMultiset>() {
            @Override
            public LongMultiset apply(int len) {
                return new LongMultiset(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super ListMultimap> LIST_MULTIMAP_FACTORY = new IntFunction<ListMultimap>() {
            @Override
            public ListMultimap apply(int len) {
                return new ListMultimap(N.initHashCapacity(len));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final IntFunction<? super SetMultimap> SET_MULTIMAP_FACTORY = new IntFunction<SetMultimap>() {
            @Override
            public SetMultimap apply(int len) {
                return new SetMultimap(N.initHashCapacity(len));
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
        public static <T> IntFunction<SortedSet<T>> ofSortedSet() {
            return (IntFunction) TREE_SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<NavigableSet<T>> ofNavigableSet() {
            return (IntFunction) TREE_SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<TreeSet<T>> ofTreeSet() {
            return (IntFunction) TREE_SET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<Queue<T>> ofQueue() {
            return (IntFunction) QUEUE_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<Deque<T>> ofDeque() {
            return (IntFunction) DEQUE_FACTORY;
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

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<Map<K, V>> ofMap() {
            return (IntFunction) MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<LinkedHashMap<K, V>> ofLinkedHashMap() {
            return (IntFunction) LINKED_HASH_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<IdentityHashMap<K, V>> ofIdentityHashMap() {
            return (IntFunction) IDENTITY_HASH_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<SortedMap<K, V>> ofSortedMap() {
            return (IntFunction) TREE_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<NavigableMap<K, V>> ofNavigableMap() {
            return (IntFunction) TREE_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<TreeMap<K, V>> ofTreeMap() {
            return (IntFunction) TREE_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<ConcurrentMap<K, V>> ofConcurrentMap() {
            return (IntFunction) CONCURRENT_HASH_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
            return (IntFunction) CONCURRENT_HASH_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> IntFunction<BiMap<K, V>> ofBiMap() {
            return (IntFunction) BI_MAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<Multiset<T>> ofMultiset() {
            return (IntFunction) MULTISET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <T> IntFunction<LongMultiset<T>> ofLongMultiset() {
            return (IntFunction) LONG_MULTISET_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, E> IntFunction<ListMultimap<K, E>> ofListMultimap() {
            return (IntFunction) LIST_MULTIMAP_FACTORY;
        }

        @SuppressWarnings("rawtypes")
        public static <K, E> IntFunction<SetMultimap<K, E>> ofSetMultimap() {
            return (IntFunction) SET_MULTIMAP_FACTORY;
        }

        /**
         * Won't work.
         * 
         * @return
         * @throws UnsupportedOperationException
         * 
         */
        @Deprecated
        public static IntFunction<ImmutableList<?>> ofImmutableList() {
            throw new UnsupportedOperationException();
        }

        /**
         * Won't work.
         * 
         * @return
         * @throws UnsupportedOperationException
         * 
         */
        @Deprecated
        public static IntFunction<ImmutableSet<?>> ofImmutableSet() {
            throw new UnsupportedOperationException();
        }

        /**
         * Won't work.
         * 
         * @return
         * @throws UnsupportedOperationException
         * 
         */
        @Deprecated
        public static IntFunction<ImmutableMap<?, ?>> ofImmutableMap() {
            throw new UnsupportedOperationException();
        }

        @Deprecated
        public static <T, C extends Collection<T>> IntFunction<C> single(final IntFunction<C> supplier) {
            return new IntFunction<C>() {
                private C c = null;

                @Override
                public C apply(int t) {
                    if (c == null) {
                        c = supplier.apply(t);
                    } else {
                        c.clear();
                    }

                    return c;
                }
            };
        }

        @Deprecated
        public static final class IntFunctions extends Factory {
            private IntFunctions() {
                // singleton.
            }
        }
    }

    public static final class IntFunctions extends Factory {
        private IntFunctions() {
            // singleton.
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
        private static final Supplier<? super Queue> QUEUE = new Supplier<Queue>() {
            @Override
            public Queue get() {
                return new LinkedList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super Deque> DEQUE = new Supplier<Deque>() {
            @Override
            public Deque get() {
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
        private static final Supplier<? super IdentityHashMap> IDENTITY_HASH_MAP = new Supplier<IdentityHashMap>() {
            @Override
            public IdentityHashMap get() {
                return new IdentityHashMap();
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
        private static final Supplier<? super BiMap> BI_MAP = new Supplier<BiMap>() {
            @Override
            public BiMap get() {
                return new BiMap();
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
        private static final Supplier<? super ListMultimap> LIST_MULTIMAP = new Supplier<ListMultimap>() {
            @Override
            public ListMultimap get() {
                return N.newListMultimap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final Supplier<? super SetMultimap> SET_MULTIMAP = new Supplier<SetMultimap>() {
            @Override
            public SetMultimap get() {
                return N.newSetMultimap();
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
        public static <T> Supplier<SortedSet<T>> ofSortedSet() {
            return (Supplier) TREE_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<NavigableSet<T>> ofNavigableSet() {
            return (Supplier) TREE_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<TreeSet<T>> ofTreeSet() {
            return (Supplier) TREE_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<Queue<T>> ofQueue() {
            return (Supplier) QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Supplier<Deque<T>> ofDeque() {
            return (Supplier) DEQUE;
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
        public static <K, V> Supplier<Map<K, V>> ofMap() {
            return (Supplier) MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<LinkedHashMap<K, V>> ofLinkedHashMap() {
            return (Supplier) LINKED_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<IdentityHashMap<K, V>> ofIdentityHashMap() {
            return (Supplier) IDENTITY_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<SortedMap<K, V>> ofSortedMap() {
            return (Supplier) TREE_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> Supplier<NavigableMap<K, V>> ofNavigableMap() {
            return (Supplier) TREE_MAP;
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
        public static <K, V> Supplier<BiMap<K, V>> ofBiMap() {
            return (Supplier) BI_MAP;
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
        public static <K, E> Supplier<ListMultimap<K, E>> ofListMultimap() {
            return (Supplier) LIST_MULTIMAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, E> Supplier<SetMultimap<K, E>> ofSetMultimap() {
            return (Supplier) SET_MULTIMAP;
        }

        public static Supplier<StringBuilder> ofStringBuilder() {
            return STRING_BUILDER;
        }

        /**
         * Won't work.
         * 
         * @return
         * @throws UnsupportedOperationException
         * 
         */
        @Deprecated
        public static Supplier<ImmutableList<?>> ofImmutableList() {
            throw new UnsupportedOperationException();
        }

        /**
         * Won't work.
         * 
         * @return
         * @throws UnsupportedOperationException
         * 
         */
        @Deprecated
        public static Supplier<ImmutableSet<?>> ofImmutableSet() {
            throw new UnsupportedOperationException();
        }

        /**
         * Won't work.
         * 
         * @return
         * @throws UnsupportedOperationException
         * 
         */
        @Deprecated
        public static Supplier<ImmutableMap<?, ?>> ofImmutableMap() {
            throw new UnsupportedOperationException();
        }

        @Deprecated
        public static <T, C extends Collection<T>> Supplier<C> single(final Supplier<C> supplier) {
            return new Supplier<C>() {
                private C c = null;

                @Override
                public C get() {
                    if (c == null) {
                        c = supplier.get();
                    } else {
                        c.clear();
                    }

                    return c;
                }
            };
        }
    }

    public static final class Predicates {

        private Predicates() {
            // singleton.
        }

        public static <T, U> Predicate<T> create(final U u, final BiPredicate<? super T, ? super U> predicate) {
            N.checkArgNotNull(predicate);

            return new Predicate<T>() {
                @Override
                public boolean test(T t) {
                    return predicate.test(t, u);
                }
            };
        }

        public static <T> Predicate<T> indexed(final IndexedPredicate<T> predicate) {
            N.checkArgNotNull(predicate);

            return new Predicate<T>() {
                private final MutableInt idx = new MutableInt(0);

                @Override
                public boolean test(T t) {
                    return predicate.test(idx.getAndIncrement(), t);
                }
            };
        }

        public static <T> Predicate<T> distinct() {
            return new Predicate<T>() {
                private final Set<Object> set = new HashSet<Object>();

                @Override
                public boolean test(T value) {
                    return set.add(value);
                }
            };
        }

        public static <T> Predicate<T> distinctBy(final Function<? super T, ?> mapper) {
            return new Predicate<T>() {
                private final Set<Object> set = new HashSet<Object>();

                @Override
                public boolean test(T value) {
                    return set.add(mapper.apply(value));
                }
            };
        }

        public static <T> Predicate<T> concurrentDistinct() {
            return new Predicate<T>() {
                private final Map<Object, Object> map = new ConcurrentHashMap<>();

                @Override
                public boolean test(T value) {
                    return map.put(value, NONE) == null;
                }
            };
        }

        public static <T> Predicate<T> concurrentDistinctBy(final Function<? super T, ?> mapper) {
            return new Predicate<T>() {
                private final Map<Object, Object> map = new ConcurrentHashMap<>();

                @Override
                public boolean test(T value) {
                    return map.put(mapper.apply(value), NONE) == null;
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
                private T pre = (T) NONE;

                @Override
                public boolean test(T value) {
                    boolean res = pre == NONE || N.equals(value, pre) == false;
                    pre = value;
                    return res;
                }
            };
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
            N.checkArgNotNull(predicate);

            return new BiPredicate<U, T>() {
                private final MutableInt idx = new MutableInt(0);

                @Override
                public boolean test(U u, T t) {
                    return predicate.test(u, idx.getAndIncrement(), t);
                }
            };
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

        public static <T, U> Consumer<T> create(final U u, final BiConsumer<? super T, ? super U> action) {
            N.checkArgNotNull(action);

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
        public static <T> Consumer<T> create(final Function<? super T, ?> func) {
            N.checkArgNotNull(func);

            return new Consumer<T>() {
                @Override
                public void accept(T t) {
                    func.apply(t);
                }
            };
        }

        public static <T> Consumer<T> indexed(final IndexedConsumer<T> action) {
            N.checkArgNotNull(action);

            return new Consumer<T>() {
                private final MutableInt idx = new MutableInt(0);

                @Override
                public void accept(T t) {
                    action.accept(idx.getAndIncrement(), t);
                }
            };
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
        public static <T extends PrimitiveList> BiConsumer<T, T> ofAddAlll() {
            return (BiConsumer<T, T>) ADD_ALL_2;
        }

        public static <T, C extends Collection<? super T>> BiConsumer<C, T> ofRemove() {
            return (BiConsumer<C, T>) REMOVE;
        }

        public static <T, C extends Collection<T>> BiConsumer<C, C> ofRemoveAll() {
            return (BiConsumer<C, C>) REMOVE_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends PrimitiveList> BiConsumer<T, T> ofRemoveAlll() {
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
        public static <T, U> BiConsumer<T, U> create(final BiFunction<? super T, ? super U, ?> func) {
            N.checkArgNotNull(func);

            return new BiConsumer<T, U>() {
                @Override
                public void accept(T t, U u) {
                    func.apply(t, u);
                }
            };
        }

        public static <U, T> BiConsumer<U, T> indexed(final IndexedBiConsumer<U, T> action) {
            N.checkArgNotNull(action);

            return new BiConsumer<U, T>() {
                private final MutableInt idx = new MutableInt(0);

                @Override
                public void accept(U u, T t) {
                    action.accept(u, idx.getAndIncrement(), t);
                }
            };
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
        public static <A, B, C> TriConsumer<A, B, C> create(final TriFunction<? super A, ? super B, ? super C, ?> func) {
            N.checkArgNotNull(func);

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

        public static <T, U, R> Function<T, R> create(final U u, final BiFunction<? super T, ? super U, R> func) {
            N.checkArgNotNull(func);

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
        public static <T, R> Function<T, Optional<R>> create(final Consumer<? super T> action) {
            N.checkArgNotNull(action);

            return new Function<T, Optional<R>>() {
                @Override
                public Optional<R> apply(T t) {
                    action.accept(t);
                    return Optional.empty();
                }
            };
        }

        public static <T, R> Function<T, R> indexed(final IndexedFunction<T, R> func) {
            N.checkArgNotNull(func);

            return new Function<T, R>() {
                private final MutableInt idx = new MutableInt(0);

                @Override
                public R apply(T t) {
                    return func.apply(idx.getAndIncrement(), t);
                }
            };
        }

        /**
         * 
         * @return
         * @deprecated replaced by PairFn#toList();
         */
        @Deprecated
        public static <T> Function<Pair<T, T>, List<T>> pairToList() {
            return Pairs.toList();
        }

        /**
         * 
         * @return
         * @deprecated replaced by PairFn#toSet();
         */
        @Deprecated
        public static <T> Function<Pair<T, T>, Set<T>> pairToSet() {
            return Pairs.toSet();
        }

        /**
         * 
         * @return
         * @deprecated replaced by TripleFn#toList();
         */
        @Deprecated
        public static <T> Function<Triple<T, T, T>, List<T>> tripleToList() {
            return Triples.toList();
        }

        /**
         * 
         * @deprecated replaced by TripleFn#toSet();
         */
        @Deprecated
        public static <T> Function<Triple<T, T, T>, Set<T>> tripleToSet() {
            return Triples.toSet();
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

        public static <T, C extends Collection<? super T>> BiFunction<C, T, C> ofAdd() {
            return (BiFunction<C, T, C>) ADD;
        }

        public static <T, C extends Collection<T>> BiFunction<C, C, C> ofAddAll() {
            return (BiFunction<C, C, C>) ADD_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends PrimitiveList> BiFunction<T, T, T> ofAddAlll() {
            return (BiFunction<T, T, T>) ADD_ALL_2;
        }

        public static <T, C extends Collection<? super T>> BiFunction<C, T, C> ofRemove() {
            return (BiFunction<C, T, C>) REMOVE;
        }

        public static <T, C extends Collection<T>> BiFunction<C, C, C> ofRemoveAll() {
            return (BiFunction<C, C, C>) REMOVE_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends PrimitiveList> BiFunction<T, T, T> ofRemoveAlll() {
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
        public static <T, U, R> BiFunction<T, U, Optional<R>> create(final BiConsumer<? super T, ? super U> action) {
            N.checkArgNotNull(action);

            return new BiFunction<T, U, Optional<R>>() {
                @Override
                public Optional<R> apply(T t, U u) {
                    action.accept(t, u);
                    return Optional.empty();
                }
            };
        }

        public static <U, T, R> BiFunction<U, T, R> indexed(final IndexedBiFunction<U, T, R> func) {
            N.checkArgNotNull(func);

            return new BiFunction<U, T, R>() {
                private final MutableInt idx = new MutableInt(0);

                @Override
                public R apply(U u, T t) {
                    return func.apply(u, idx.getAndIncrement(), t);
                }
            };
        }
    }

    public static final class TriFunctions {

        private TriFunctions() {
            // singleton.
        }

        /**
         * Returns a <code>TriFunction</code> which calls the specified <code>action</code> and always return an empty<code>Optional</code> if <code>action</code> is executed successfully.
         * 
         * @param action
         * @return
         */
        public static <A, B, C, R> TriFunction<A, B, C, Optional<R>> create(final TriConsumer<? super A, ? super B, ? super C> action) {
            N.checkArgNotNull(action);

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

        private static final BinaryOperator<Collection<Object>> ADD_ALL_TO_FIRST = new BinaryOperator<Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.addAll(u);
                return t;
            }
        };

        private static final BinaryOperator<Collection<Object>> ADD_ALL_TO_BIGGER = new BinaryOperator<Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                if (t.size() >= u.size()) {
                    t.addAll(u);
                    return t;
                } else {
                    u.addAll(t);
                    return u;
                }
            }
        };

        private static final BinaryOperator<Collection<Object>> REMOVE_ALL_FROM_FIRST = new BinaryOperator<Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.removeAll(u);
                return t;
            }
        };

        private static final BinaryOperator<Map<Object, Object>> PUT_ALL_TO_FIRST = new BinaryOperator<Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map<Object, Object> u) {
                t.putAll(u);
                return t;
            }
        };

        private static final BinaryOperator<Map<Object, Object>> PUT_ALL_TO_BIGGER = new BinaryOperator<Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map<Object, Object> u) {
                if (t.size() >= u.size()) {
                    t.putAll(u);
                    return t;
                } else {
                    u.putAll(t);
                    return u;
                }
            }
        };

        private static final BinaryOperator<Joiner> MERGE_TO_FIRST = new BinaryOperator<Joiner>() {
            @Override
            public Joiner apply(Joiner t, Joiner u) {
                return t.merge(u);
            }
        };

        private static final BinaryOperator<Joiner> MERGE_TO_BIGGER = new BinaryOperator<Joiner>() {
            @Override
            public Joiner apply(Joiner t, Joiner u) {
                if (t.length() >= u.length()) {
                    return t.merge(u);
                } else {
                    return u.merge(t);
                }
            }
        };

        private static final BinaryOperator<StringBuilder> APPEND_TO_FIRST = new BinaryOperator<StringBuilder>() {
            @Override
            public StringBuilder apply(StringBuilder t, StringBuilder u) {
                return t.append(u);
            }
        };

        private static final BinaryOperator<StringBuilder> APPEND_TO_BIGGER = new BinaryOperator<StringBuilder>() {
            @Override
            public StringBuilder apply(StringBuilder t, StringBuilder u) {
                if (t.length() >= u.length()) {
                    return t.append(u);
                } else {
                    return u.append(t);
                }
            }
        };

        private static final BinaryOperator<String> CONCAT = new BinaryOperator<String>() {
            @Override
            public String apply(String t, String u) {
                return t + u;
            }
        };

        private static final BinaryOperator<Integer> ADD_INTEGER = new BinaryOperator<Integer>() {
            @Override
            public Integer apply(Integer t, Integer u) {
                return t.intValue() + u.intValue();
            }
        };

        private static final BinaryOperator<Long> ADD_LONG = new BinaryOperator<Long>() {
            @Override
            public Long apply(Long t, Long u) {
                return t.longValue() + u.longValue();
            }
        };

        private static final BinaryOperator<Double> ADD_DOUBLE = new BinaryOperator<Double>() {
            @Override
            public Double apply(Double t, Double u) {
                return t.doubleValue() + u.doubleValue();
            }
        };

        private static final BinaryOperator<BigInteger> ADD_BIG_INTEGER = new BinaryOperator<BigInteger>() {
            @Override
            public BigInteger apply(BigInteger t, BigInteger u) {
                return t.add(u);
            }
        };

        private static final BinaryOperator<BigDecimal> ADD_BIG_DECIMAL = new BinaryOperator<BigDecimal>() {
            @Override
            public BigDecimal apply(BigDecimal t, BigDecimal u) {
                return t.add(u);
            }
        };

        @SuppressWarnings({ "rawtypes" })
        private static final BinaryOperator<Comparable> MIN = new BinaryOperator<Comparable>() {
            @Override
            public Comparable apply(Comparable t, Comparable u) {
                return N.compare(t, u) <= 0 ? t : u;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final BinaryOperator<Comparable> MAX = new BinaryOperator<Comparable>() {
            @Override
            public Comparable apply(Comparable t, Comparable u) {
                return N.compare(t, u) >= 0 ? t : u;
            }
        };

        private BinaryOperators() {
            // singleton.
        }

        /**
         * 
         * @return
         * @deprecated replaced by {@code #ofAddAllToFirst()}
         */
        @Deprecated
        @SuppressWarnings("unchecked")
        public static <T, C extends Collection<T>> BinaryOperator<C> ofAddAll() {
            return (BinaryOperator<C>) ADD_ALL_TO_FIRST;
        }

        @SuppressWarnings("unchecked")
        public static <T, C extends Collection<T>> BinaryOperator<C> ofAddAllToFirst() {
            return (BinaryOperator<C>) ADD_ALL_TO_FIRST;
        }

        @SuppressWarnings("unchecked")
        public static <T, C extends Collection<T>> BinaryOperator<C> ofAddAllToBigger() {
            return (BinaryOperator<C>) ADD_ALL_TO_BIGGER;
        }

        /**
         * 
         * @return
         * @deprecated replaced by {@code #ofRemoveAllFromFirst()}.
         */
        @Deprecated
        @SuppressWarnings("unchecked")
        public static <T, C extends Collection<T>> BinaryOperator<C> ofRemoveAll() {
            return (BinaryOperator<C>) REMOVE_ALL_FROM_FIRST;
        }

        @SuppressWarnings("unchecked")
        public static <T, C extends Collection<T>> BinaryOperator<C> ofRemoveAllFromFirst() {
            return (BinaryOperator<C>) REMOVE_ALL_FROM_FIRST;
        }

        /**
         * 
         * @return
         * @deprecated replaced by {@code #ofPutAllToFirst()}
         */
        @Deprecated
        @SuppressWarnings("unchecked")
        public static <K, V, M extends Map<K, V>> BinaryOperator<M> ofPutAll() {
            return (BinaryOperator<M>) PUT_ALL_TO_FIRST;
        }

        @SuppressWarnings("unchecked")
        public static <K, V, M extends Map<K, V>> BinaryOperator<M> ofPutAllToFirst() {
            return (BinaryOperator<M>) PUT_ALL_TO_FIRST;
        }

        @SuppressWarnings("unchecked")
        public static <K, V, M extends Map<K, V>> BinaryOperator<M> ofPutAllToBigger() {
            return (BinaryOperator<M>) PUT_ALL_TO_BIGGER;
        }

        /**
         * 
         * @return
         * @deprecated replaced by {@code #ofMergeToFirst}.
         */
        @Deprecated
        public static BinaryOperator<Joiner> ofMerge() {
            return MERGE_TO_FIRST;
        }

        public static BinaryOperator<Joiner> ofMergeToFirst() {
            return MERGE_TO_FIRST;
        }

        public static BinaryOperator<Joiner> ofMergeToBigger() {
            return MERGE_TO_BIGGER;
        }

        /**
         * 
         * @return
         * @deprecated replaced by {@code #ofAppendToFirst()}
         */
        @Deprecated
        public static BinaryOperator<StringBuilder> ofAppend() {
            return APPEND_TO_FIRST;
        }

        public static BinaryOperator<StringBuilder> ofAppendToFirst() {
            return APPEND_TO_FIRST;
        }

        public static BinaryOperator<StringBuilder> ofAppendToBigger() {
            return APPEND_TO_BIGGER;
        }

        public static BinaryOperator<String> ofConcat() {
            return CONCAT;
        }

        public static BinaryOperator<Integer> ofAddInt() {
            return ADD_INTEGER;
        }

        public static BinaryOperator<Long> ofAddLong() {
            return ADD_LONG;
        }

        public static BinaryOperator<Double> ofAddDouble() {
            return ADD_DOUBLE;
        }

        public static BinaryOperator<BigInteger> ofAddBigInteger() {
            return ADD_BIG_INTEGER;
        }

        public static BinaryOperator<BigDecimal> ofAddBigDecimal() {
            return ADD_BIG_DECIMAL;
        }

        @SuppressWarnings({ "unchecked", "rawtypes" })
        public static <T extends Comparable<? super T>> BinaryOperator<T> min() {
            return (BinaryOperator) MIN;
        }

        @SuppressWarnings({ "unchecked", "rawtypes" })
        public static <T extends Comparable<? super T>> BinaryOperator<T> max() {
            return (BinaryOperator) MAX;
        }

        public static <T> BinaryOperator<T> minBy(final Comparator<? super T> comparator) {
            N.checkArgNotNull(comparator);

            return new BinaryOperator<T>() {
                @Override
                public T apply(T t, T u) {
                    return comparator.compare(t, u) <= 0 ? t : u;
                }
            };
        }

        public static <T> BinaryOperator<T> maxBy(final Comparator<? super T> comparator) {
            N.checkArgNotNull(comparator);

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

    public static final class Ints {

        private static final IntPredicate POSITIVE = new IntPredicate() {
            @Override
            public boolean test(int t) {
                return t > 0;
            }
        };

        private static final IntPredicate NOT_NEGATIVE = new IntPredicate() {
            @Override
            public boolean test(int t) {
                return t >= 0;
            }
        };

        private static final IntBiPredicate EQUAL = new IntBiPredicate() {
            @Override
            public boolean test(int t, int u) {
                return t == u;
            }
        };

        private static final IntBiPredicate NOT_EQUAL = new IntBiPredicate() {
            @Override
            public boolean test(int t, int u) {
                return t != u;
            }
        };

        private static final IntBiPredicate GREATER_THAN = new IntBiPredicate() {
            @Override
            public boolean test(int t, int u) {
                return t > u;
            }
        };

        private static final IntBiPredicate GREATER_EQUAL = new IntBiPredicate() {
            @Override
            public boolean test(int t, int u) {
                return t >= u;
            }
        };

        private static final IntBiPredicate LESS_THAN = new IntBiPredicate() {
            @Override
            public boolean test(int t, int u) {
                return t < u;
            }
        };

        private static final IntBiPredicate LESS_EQUAL = new IntBiPredicate() {
            @Override
            public boolean test(int t, int u) {
                return t <= u;
            }
        };

        private Ints() {
            // Utility class.
        }

        public static IntPredicate positve() {
            return POSITIVE;
        }

        public static IntPredicate notNegative() {
            return NOT_NEGATIVE;
        }

        public static IntBiPredicate equal() {
            return EQUAL;
        }

        public static IntBiPredicate notEqual() {
            return NOT_EQUAL;
        }

        public static IntBiPredicate greaterThan() {
            return GREATER_THAN;
        }

        public static IntBiPredicate greaterEqual() {
            return GREATER_EQUAL;
        }

        public static IntBiPredicate lessThan() {
            return LESS_THAN;
        }

        public static IntBiPredicate lessEqual() {
            return LESS_EQUAL;
        }
    }

    public static final class Longs {

        private static final LongPredicate POSITIVE = new LongPredicate() {
            @Override
            public boolean test(long t) {
                return t > 0;
            }
        };

        private static final LongPredicate NOT_NEGATIVE = new LongPredicate() {
            @Override
            public boolean test(long t) {
                return t >= 0;
            }
        };

        private static final LongBiPredicate EQUAL = new LongBiPredicate() {
            @Override
            public boolean test(long t, long u) {
                return t == u;
            }
        };

        private static final LongBiPredicate NOT_EQUAL = new LongBiPredicate() {
            @Override
            public boolean test(long t, long u) {
                return t != u;
            }
        };

        private static final LongBiPredicate GREATER_THAN = new LongBiPredicate() {
            @Override
            public boolean test(long t, long u) {
                return t > u;
            }
        };

        private static final LongBiPredicate GREATER_EQUAL = new LongBiPredicate() {
            @Override
            public boolean test(long t, long u) {
                return t >= u;
            }
        };

        private static final LongBiPredicate LESS_THAN = new LongBiPredicate() {
            @Override
            public boolean test(long t, long u) {
                return t < u;
            }
        };

        private static final LongBiPredicate LESS_EQUAL = new LongBiPredicate() {
            @Override
            public boolean test(long t, long u) {
                return t <= u;
            }
        };

        private Longs() {
            // Utility class.
        }

        public static LongPredicate positve() {
            return POSITIVE;
        }

        public static LongPredicate notNegative() {
            return NOT_NEGATIVE;
        }

        public static LongBiPredicate equal() {
            return EQUAL;
        }

        public static LongBiPredicate notEqual() {
            return NOT_EQUAL;
        }

        public static LongBiPredicate greaterThan() {
            return GREATER_THAN;
        }

        public static LongBiPredicate greaterEqual() {
            return GREATER_EQUAL;
        }

        public static LongBiPredicate lessThan() {
            return LESS_THAN;
        }

        public static LongBiPredicate lessEqual() {
            return LESS_EQUAL;
        }
    }

    public static final class Doubles {

        private static final DoublePredicate POSITIVE = new DoublePredicate() {
            @Override
            public boolean test(double t) {
                return t > 0;
            }
        };

        private static final DoublePredicate NOT_NEGATIVE = new DoublePredicate() {
            @Override
            public boolean test(double t) {
                return t >= 0;
            }
        };

        private static final DoubleBiPredicate EQUAL = new DoubleBiPredicate() {
            @Override
            public boolean test(double t, double u) {
                return N.equals(t, u);
            }
        };

        private static final DoubleBiPredicate NOT_EQUAL = new DoubleBiPredicate() {
            @Override
            public boolean test(double t, double u) {
                return N.compare(t, u) != 0;
            }
        };

        private static final DoubleBiPredicate GREATER_THAN = new DoubleBiPredicate() {
            @Override
            public boolean test(double t, double u) {
                return N.compare(t, u) > 0;
            }
        };

        private static final DoubleBiPredicate GREATER_EQUAL = new DoubleBiPredicate() {
            @Override
            public boolean test(double t, double u) {
                return N.compare(t, u) >= 0;
            }
        };

        private static final DoubleBiPredicate LESS_THAN = new DoubleBiPredicate() {
            @Override
            public boolean test(double t, double u) {
                return N.compare(t, u) < 0;
            }
        };

        private static final DoubleBiPredicate LESS_EQUAL = new DoubleBiPredicate() {
            @Override
            public boolean test(double t, double u) {
                return N.compare(t, u) <= 0;
            }
        };

        private Doubles() {
            // Utility class.
        }

        public static DoublePredicate positve() {
            return POSITIVE;
        }

        public static DoublePredicate notNegative() {
            return NOT_NEGATIVE;
        }

        public static DoubleBiPredicate equal() {
            return EQUAL;
        }

        public static DoubleBiPredicate notEqual() {
            return NOT_EQUAL;
        }

        public static DoubleBiPredicate greaterThan() {
            return GREATER_THAN;
        }

        public static DoubleBiPredicate greaterEqual() {
            return GREATER_EQUAL;
        }

        public static DoubleBiPredicate lessThan() {
            return LESS_THAN;
        }

        public static DoubleBiPredicate lessEqual() {
            return LESS_EQUAL;
        }
    }

    public static final class Pairs {
        @SuppressWarnings("rawtypes")
        private static final Function<Pair, List> PAIR_TO_LIST = new Function<Pair, List>() {
            @Override
            public List apply(Pair t) {
                return N.asList(t.getLeft(), t.getRight());
            }

        };

        @SuppressWarnings("rawtypes")
        private static final Function<Pair, Set> PAIR_TO_SET = new Function<Pair, Set>() {
            @Override
            public Set apply(Pair t) {
                return N.asSet(t.getLeft(), t.getRight());
            }

        };

        private Pairs() {
            // Utility class.
        }

        @SuppressWarnings("rawtypes")
        public static <T> Function<Pair<T, T>, List<T>> toList() {
            return (Function) PAIR_TO_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Function<Pair<T, T>, Set<T>> toSet() {
            return (Function) PAIR_TO_SET;
        }

    }

    public static final class Triples {

        @SuppressWarnings("rawtypes")
        private static final Function<Triple, List> TRIPLE_TO_LIST = new Function<Triple, List>() {
            @Override
            public List apply(Triple t) {
                return N.asList(t.getLeft(), t.getMiddle(), t.getRight());
            }

        };

        @SuppressWarnings("rawtypes")
        private static final Function<Triple, Set> TRIPLE_TO_SET = new Function<Triple, Set>() {
            @Override
            public Set apply(Triple t) {
                return N.asSet(t.getLeft(), t.getMiddle(), t.getRight());
            }

        };

        private Triples() {
            // Utility class.
        }

        @SuppressWarnings("rawtypes")
        public static <T> Function<Triple<T, T, T>, List<T>> toList() {
            return (Function) TRIPLE_TO_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> Function<Triple<T, T, T>, Set<T>> toSet() {
            return (Function) TRIPLE_TO_SET;
        }
    }
}
