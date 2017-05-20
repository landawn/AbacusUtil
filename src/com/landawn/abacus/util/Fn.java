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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
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
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.regex.Pattern;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriConsumer;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;

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
    public static final IntFunction<Map<String, Object>> FACTORY_OF_MAP = (IntFunction) Factory.MAP_FACTORY;
    @SuppressWarnings("rawtypes")
    public static final IntFunction<LinkedHashMap<String, Object>> FACTORY_OF_LINKED_HASH_MAP = (IntFunction) Factory.LINKED_HASH_MAP_FACTORY;
    @SuppressWarnings("rawtypes")
    public static final com.landawn.abacus.util.function.Supplier<Map<String, Object>> SUPPLIER_OF_MAP = (com.landawn.abacus.util.function.Supplier) Suppliers.MAP;
    @SuppressWarnings("rawtypes")
    public static final com.landawn.abacus.util.function.Supplier<LinkedHashMap<String, Object>> SUPPLIER_OF_LINKED_HASH_MAP = (com.landawn.abacus.util.function.Supplier) Suppliers.LINKED_HASH_MAP;

    @SuppressWarnings("rawtypes")
    private static final com.landawn.abacus.util.function.Consumer DO_NOTHING = new com.landawn.abacus.util.function.Consumer() {
        @Override
        public void accept(Object value) {
            // do nothing.
        }
    };

    @SuppressWarnings("rawtypes")
    private static final com.landawn.abacus.util.function.Consumer PRINTLN = new com.landawn.abacus.util.function.Consumer() {
        @Override
        public void accept(Object value) {
            N.println(value);
        }
    };

    @SuppressWarnings("rawtypes")
    private static final com.landawn.abacus.util.function.Function IDENTITY = new com.landawn.abacus.util.function.Function() {
        @Override
        public Object apply(Object t) {
            return t;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final com.landawn.abacus.util.function.Predicate ALWAYS_TRUE = new com.landawn.abacus.util.function.Predicate() {
        @Override
        public boolean test(Object value) {
            return true;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final com.landawn.abacus.util.function.Predicate ALWAYS_FALSE = new com.landawn.abacus.util.function.Predicate() {
        @Override
        public boolean test(Object value) {
            return false;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final com.landawn.abacus.util.function.Predicate IS_NULL = new com.landawn.abacus.util.function.Predicate() {
        @Override
        public boolean test(Object value) {
            return value == null;
        }
    };

    @SuppressWarnings("rawtypes")
    private static final com.landawn.abacus.util.function.Predicate NOT_NULL = new com.landawn.abacus.util.function.Predicate() {
        @Override
        public boolean test(Object value) {
            return value != null;
        }
    };

    private static final Object NULL = new Object();

    private Fn() {
        // Singleton.
    }

    public static <T> Comparator<T> naturalOrder() {
        return Comparators.naturalOrder();
    }

    public static <T> Comparator<T> reverseOrder() {
        return Comparators.reverseOrder();
    }

    public static <T> Comparator<T> reverseOrder(final Comparator<T> cmp) {
        return Comparators.reverseOrder(cmp);
    }

    public static <T> com.landawn.abacus.util.function.Consumer<T> doNothing() {
        return DO_NOTHING;
    }

    public static <T> com.landawn.abacus.util.function.Consumer<T> println() {
        return PRINTLN;
    }

    public static <T, U> com.landawn.abacus.util.function.BiConsumer<T, U> println(final String separator) {
        return new com.landawn.abacus.util.function.BiConsumer<T, U>() {
            @Override
            public void accept(T t, U u) {
                N.println(t + separator + u);
            }
        };
    }

    public static <T> com.landawn.abacus.util.function.Function<T, T> identity() {
        return IDENTITY;
    }

    public static <T, U> com.landawn.abacus.util.function.Function<T, U> cast(final Class<U> clazz) {
        return new com.landawn.abacus.util.function.Function<T, U>() {
            @Override
            public U apply(T t) {
                return (U) t;
            }
        };
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> alwaysTrue() {
        return ALWAYS_TRUE;
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> alwaysFalse() {
        return ALWAYS_FALSE;
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> isNull() {
        return IS_NULL;
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> notNull() {
        return NOT_NULL;
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> equal(final Object target) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.equals(value, target);
            }
        };
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> notEqual(final Object target) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !N.equals(value, target);
            }
        };
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.Predicate<T> greaterThan(final T target) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) > 0;
            }
        };
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.Predicate<T> greaterEqual(final T target) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) >= 0;
            }
        };
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.Predicate<T> lessThan(final T target) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) < 0;
            }
        };
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.Predicate<T> lessEqual(final T target) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return N.compare(value, target) <= 0;
            }
        };
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> in(final Collection<?> c) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return c.contains(value);
            }
        };
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> notIn(final Collection<?> c) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !c.contains(value);
            }
        };
    }

    public static <T> com.landawn.abacus.util.function.Predicate<T> instanceOf(final Class<?> clazz) {
        return new com.landawn.abacus.util.function.Predicate<T>() {
            @Override
            public boolean test(T value) {
                return clazz.isInstance(value);
            }
        };
    }

    @SuppressWarnings("rawtypes")
    public static com.landawn.abacus.util.function.Predicate<Class> subtypeOf(final Class<?> clazz) {
        return new com.landawn.abacus.util.function.Predicate<Class>() {
            @Override
            public boolean test(Class value) {
                return clazz.isAssignableFrom(value);
            }
        };
    }

    public static com.landawn.abacus.util.function.Predicate<CharSequence> matches(final Pattern pattern) {
        return new com.landawn.abacus.util.function.Predicate<CharSequence>() {
            @Override
            public boolean test(CharSequence value) {
                return pattern.matcher(value).find();
            }
        };
    }

    public static <T, U> com.landawn.abacus.util.function.BiPredicate<T, U> equal() {
        return BiPredicates.EQUAL;
    }

    public static <T, U> com.landawn.abacus.util.function.BiPredicate<T, U> notEqual() {
        return BiPredicates.NOT_EQUAL;
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.BiPredicate<T, T> greaterThan() {
        return (com.landawn.abacus.util.function.BiPredicate<T, T>) BiPredicates.GREATER_THAN;
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.BiPredicate<T, T> greaterEqual() {
        return (com.landawn.abacus.util.function.BiPredicate<T, T>) BiPredicates.GREATER_EQUAL;
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.BiPredicate<T, T> lessThan() {
        return (com.landawn.abacus.util.function.BiPredicate<T, T>) BiPredicates.LESS_THAN;
    }

    public static <T extends Comparable<? super T>> com.landawn.abacus.util.function.BiPredicate<T, T> lessEqual() {
        return (com.landawn.abacus.util.function.BiPredicate<T, T>) BiPredicates.LESS_EQUAL;
    }

    public static <K, V> com.landawn.abacus.util.function.Predicate<Map.Entry<K, V>> testByKey(
            final com.landawn.abacus.util.function.Predicate<? super K> predicate) {
        return new com.landawn.abacus.util.function.Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getKey());
            }
        };
    }

    public static <K, V> com.landawn.abacus.util.function.Predicate<Map.Entry<K, V>> testByValue(
            final com.landawn.abacus.util.function.Predicate<? super V> predicate) {
        return new com.landawn.abacus.util.function.Predicate<Map.Entry<K, V>>() {
            @Override
            public boolean test(Entry<K, V> entry) {
                return predicate.test(entry.getValue());
            }
        };
    }

    public static <K, V> com.landawn.abacus.util.function.Consumer<Map.Entry<K, V>> acceptByKey(
            final com.landawn.abacus.util.function.Consumer<? super K> consumer) {
        return new com.landawn.abacus.util.function.Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getKey());
            }
        };
    }

    public static <K, V> com.landawn.abacus.util.function.Consumer<Map.Entry<K, V>> acceptByValue(
            final com.landawn.abacus.util.function.Consumer<? super V> consumer) {
        return new com.landawn.abacus.util.function.Consumer<Map.Entry<K, V>>() {
            @Override
            public void accept(Entry<K, V> entry) {
                consumer.accept(entry.getValue());
            }
        };
    }

    public static <K, V, R> com.landawn.abacus.util.function.Function<Map.Entry<K, V>, R> applyByKey(
            final com.landawn.abacus.util.function.Function<? super K, R> func) {
        return new com.landawn.abacus.util.function.Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getKey());
            }
        };
    }

    public static <K, V, R> com.landawn.abacus.util.function.Function<Map.Entry<K, V>, R> applyByValue(
            final com.landawn.abacus.util.function.Function<? super V, R> func) {
        return new com.landawn.abacus.util.function.Function<Map.Entry<K, V>, R>() {
            @Override
            public R apply(Entry<K, V> entry) {
                return func.apply(entry.getValue());
            }
        };
    }

    public static <T> com.landawn.abacus.util.function.BinaryOperator<T> throwingMerger() {
        return BinaryOperators.THROWING_MERGER;
    }

    public static <T> com.landawn.abacus.util.function.BinaryOperator<T> ignoringMerger() {
        return BinaryOperators.IGNORING_MERGER;
    }

    public static <T> com.landawn.abacus.util.function.BinaryOperator<T> replacingMerger() {
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

    public static <T> Collector<T, ?, List<T>> toList() {
        return Collectors.toList();
    }

    public static <T> Collector<T, ?, Set<T>> toSet() {
        return Collectors.toSet();
    }

    public static <T> Collector<T, ?, Multiset<T>> toMultiset() {
        return Collectors.toMultiset();
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
        private static final IntFunction<ExList> EX_LIST_FACTORY = new IntFunction<ExList>() {
            @Override
            public ExList apply(int len) {
                return new ExList(len);
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
        public static <T> IntFunction<ExList<T>> ofExList() {
            return (IntFunction) EX_LIST_FACTORY;
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
        private static final com.landawn.abacus.util.function.Supplier<String> UUID = new com.landawn.abacus.util.function.Supplier<String>() {
            @Override
            public String get() {
                return N.uuid();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<String> GUID = new com.landawn.abacus.util.function.Supplier<String>() {
            @Override
            public String get() {
                return N.guid();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<boolean[]> EMPTY_BOOLEAN_ARRAY = new com.landawn.abacus.util.function.Supplier<boolean[]>() {
            @Override
            public boolean[] get() {
                return N.EMPTY_BOOLEAN_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<char[]> EMPTY_CHAR_ARRAY = new com.landawn.abacus.util.function.Supplier<char[]>() {
            @Override
            public char[] get() {
                return N.EMPTY_CHAR_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<byte[]> EMPTY_BYTE_ARRAY = new com.landawn.abacus.util.function.Supplier<byte[]>() {
            @Override
            public byte[] get() {
                return N.EMPTY_BYTE_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<short[]> EMPTY_SHORT_ARRAY = new com.landawn.abacus.util.function.Supplier<short[]>() {
            @Override
            public short[] get() {
                return N.EMPTY_SHORT_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<int[]> EMPTY_INT_ARRAY = new com.landawn.abacus.util.function.Supplier<int[]>() {
            @Override
            public int[] get() {
                return N.EMPTY_INT_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<long[]> EMPTY_LONG_ARRAY = new com.landawn.abacus.util.function.Supplier<long[]>() {
            @Override
            public long[] get() {
                return N.EMPTY_LONG_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<float[]> EMPTY_FLOAT_ARRAY = new com.landawn.abacus.util.function.Supplier<float[]>() {
            @Override
            public float[] get() {
                return N.EMPTY_FLOAT_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<double[]> EMPTY_DOUBLE_ARRAY = new com.landawn.abacus.util.function.Supplier<double[]>() {
            @Override
            public double[] get() {
                return N.EMPTY_DOUBLE_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<String[]> EMPTY_STRING_ARRAY = new com.landawn.abacus.util.function.Supplier<String[]>() {
            @Override
            public String[] get() {
                return N.EMPTY_STRING_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<Object[]> EMPTY_OBJECT_ARRAY = new com.landawn.abacus.util.function.Supplier<Object[]>() {
            @Override
            public Object[] get() {
                return N.EMPTY_OBJECT_ARRAY;
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<BooleanList> BOOLEAN_LIST = new com.landawn.abacus.util.function.Supplier<BooleanList>() {
            @Override
            public BooleanList get() {
                return new BooleanList();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<CharList> CHAR_LIST = new com.landawn.abacus.util.function.Supplier<CharList>() {
            @Override
            public CharList get() {
                return new CharList();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<ByteList> BYTE_LIST = new com.landawn.abacus.util.function.Supplier<ByteList>() {
            @Override
            public ByteList get() {
                return new ByteList();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<ShortList> SHORT_LIST = new com.landawn.abacus.util.function.Supplier<ShortList>() {
            @Override
            public ShortList get() {
                return new ShortList();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<IntList> INT_LIST = new com.landawn.abacus.util.function.Supplier<IntList>() {
            @Override
            public IntList get() {
                return new IntList();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<LongList> LONG_LIST = new com.landawn.abacus.util.function.Supplier<LongList>() {
            @Override
            public LongList get() {
                return new LongList();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<FloatList> FLOAT_LIST = new com.landawn.abacus.util.function.Supplier<FloatList>() {
            @Override
            public FloatList get() {
                return new FloatList();
            }
        };

        private static final com.landawn.abacus.util.function.Supplier<DoubleList> DOUBLE_LIST = new com.landawn.abacus.util.function.Supplier<DoubleList>() {
            @Override
            public DoubleList get() {
                return new DoubleList();
            }
        };
        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<ExList> EX_LIST = new com.landawn.abacus.util.function.Supplier<ExList>() {
            @Override
            public ExList get() {
                return new ExList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super List> LIST = new com.landawn.abacus.util.function.Supplier<List>() {
            @Override
            public List get() {
                return new ArrayList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super LinkedList> LINKED_LIST = new com.landawn.abacus.util.function.Supplier<LinkedList>() {
            @Override
            public LinkedList get() {
                return new LinkedList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super Set> SET = new com.landawn.abacus.util.function.Supplier<Set>() {
            @Override
            public Set get() {
                return new HashSet();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super LinkedHashSet> LINKED_HASH_SET = new com.landawn.abacus.util.function.Supplier<LinkedHashSet>() {
            @Override
            public LinkedHashSet get() {
                return new LinkedHashSet();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super TreeSet> TREE_SET = new com.landawn.abacus.util.function.Supplier<TreeSet>() {
            @Override
            public TreeSet get() {
                return new TreeSet();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super Map> MAP = new com.landawn.abacus.util.function.Supplier<Map>() {
            @Override
            public Map get() {
                return new HashMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super LinkedHashMap> LINKED_HASH_MAP = new com.landawn.abacus.util.function.Supplier<LinkedHashMap>() {
            @Override
            public LinkedHashMap get() {
                return new LinkedHashMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super TreeMap> TREE_MAP = new com.landawn.abacus.util.function.Supplier<TreeMap>() {
            @Override
            public TreeMap get() {
                return new TreeMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super ConcurrentHashMap> CONCURRENT_HASH_MAP = new com.landawn.abacus.util.function.Supplier<ConcurrentHashMap>() {
            @Override
            public ConcurrentHashMap get() {
                return new ConcurrentHashMap();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super Queue> QUEUE = new com.landawn.abacus.util.function.Supplier<Queue>() {
            @Override
            public Queue get() {
                return new LinkedList();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super ArrayDeque> ARRAY_DEQUE = new com.landawn.abacus.util.function.Supplier<ArrayDeque>() {
            @Override
            public ArrayDeque get() {
                return new ArrayDeque();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super LinkedBlockingQueue> LINKED_BLOCKING_QUEUE = new com.landawn.abacus.util.function.Supplier<LinkedBlockingQueue>() {
            @Override
            public LinkedBlockingQueue get() {
                return new LinkedBlockingQueue();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super ConcurrentLinkedQueue> CONCURRENT_LINKED_QUEUE = new com.landawn.abacus.util.function.Supplier<ConcurrentLinkedQueue>() {
            @Override
            public ConcurrentLinkedQueue get() {
                return new ConcurrentLinkedQueue();
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.Supplier<? super PriorityQueue> PRIORITY_QUEUE = new com.landawn.abacus.util.function.Supplier<PriorityQueue>() {
            @Override
            public PriorityQueue get() {
                return new PriorityQueue();
            }
        };

        private Suppliers() {
            // singleton.
        }

        public static com.landawn.abacus.util.function.Supplier<String> ofUUID() {
            return UUID;
        }

        public static com.landawn.abacus.util.function.Supplier<String> ofGUID() {
            return GUID;
        }

        public static com.landawn.abacus.util.function.Supplier<boolean[]> ofEmptyBooleanArray() {
            return EMPTY_BOOLEAN_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<char[]> ofEmptyCharArray() {
            return EMPTY_CHAR_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<byte[]> ofEmptyByteArray() {
            return EMPTY_BYTE_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<short[]> ofEmptyShortArray() {
            return EMPTY_SHORT_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<int[]> ofEmptyIntArray() {
            return EMPTY_INT_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<long[]> ofEmptyLongArray() {
            return EMPTY_LONG_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<float[]> ofEmptyFloatArray() {
            return EMPTY_FLOAT_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<double[]> ofEmptyDoubleArray() {
            return EMPTY_DOUBLE_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<String[]> ofEmptyStringArray() {
            return EMPTY_STRING_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<Object[]> ofEmptyObjectArray() {
            return EMPTY_OBJECT_ARRAY;
        }

        public static com.landawn.abacus.util.function.Supplier<BooleanList> ofBooleanList() {
            return BOOLEAN_LIST;
        }

        public static com.landawn.abacus.util.function.Supplier<CharList> ofCharList() {
            return CHAR_LIST;
        }

        public static com.landawn.abacus.util.function.Supplier<ByteList> ofByteList() {
            return BYTE_LIST;
        }

        public static com.landawn.abacus.util.function.Supplier<ShortList> ofShortList() {
            return SHORT_LIST;
        }

        public static com.landawn.abacus.util.function.Supplier<IntList> ofIntList() {
            return INT_LIST;
        }

        public static com.landawn.abacus.util.function.Supplier<LongList> ofLongList() {
            return LONG_LIST;
        }

        public static com.landawn.abacus.util.function.Supplier<FloatList> ofFloatList() {
            return FLOAT_LIST;
        }

        public static com.landawn.abacus.util.function.Supplier<DoubleList> ofDoubleList() {
            return DOUBLE_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<ExList<T>> ofExList() {
            return (com.landawn.abacus.util.function.Supplier) EX_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<List<T>> ofList() {
            return (com.landawn.abacus.util.function.Supplier) LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<LinkedList<T>> ofLinkedList() {
            return (com.landawn.abacus.util.function.Supplier) LINKED_LIST;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<Set<T>> ofSet() {
            return (com.landawn.abacus.util.function.Supplier) SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<LinkedHashSet<T>> ofLinkedHashSet() {
            return (com.landawn.abacus.util.function.Supplier) LINKED_HASH_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<TreeSet<T>> ofTreeSet() {
            return (com.landawn.abacus.util.function.Supplier) TREE_SET;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> com.landawn.abacus.util.function.Supplier<Map<K, V>> ofMap() {
            return (com.landawn.abacus.util.function.Supplier) MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> com.landawn.abacus.util.function.Supplier<LinkedHashMap<K, V>> ofLinkedHashMap() {
            return (com.landawn.abacus.util.function.Supplier) LINKED_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> com.landawn.abacus.util.function.Supplier<TreeMap<K, V>> ofTreeMap() {
            return (com.landawn.abacus.util.function.Supplier) TREE_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <K, V> com.landawn.abacus.util.function.Supplier<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
            return (com.landawn.abacus.util.function.Supplier) CONCURRENT_HASH_MAP;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<Queue<T>> ofQueue() {
            return (com.landawn.abacus.util.function.Supplier) QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<ArrayDeque<T>> ofArrayDeque() {
            return (com.landawn.abacus.util.function.Supplier) ARRAY_DEQUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
            return (com.landawn.abacus.util.function.Supplier) LINKED_BLOCKING_QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
            return (com.landawn.abacus.util.function.Supplier) CONCURRENT_LINKED_QUEUE;
        }

        @SuppressWarnings("rawtypes")
        public static <T> com.landawn.abacus.util.function.Supplier<PriorityQueue<T>> ofPriorityQueue() {
            return (com.landawn.abacus.util.function.Supplier) PRIORITY_QUEUE;
        }
    }

    public static final class Predicates {

        private Predicates() {
            // singleton.
        }

        public static <T, U> com.landawn.abacus.util.function.Predicate<T> of(final U u,
                final com.landawn.abacus.util.function.BiPredicate<? super T, ? super U> predicate) {
            N.requireNonNull(predicate);

            return new com.landawn.abacus.util.function.Predicate<T>() {
                @Override
                public boolean test(T t) {
                    return predicate.test(t, u);
                }
            };
        }

        /**
         * Remove the continuous repeat elements.
         * 
         * @return
         */
        public static <T> com.landawn.abacus.util.function.Predicate<T> removeRepeats() {
            return new com.landawn.abacus.util.function.Predicate<T>() {
                private T pre = (T) NULL;

                @Override
                public boolean test(T value) {
                    boolean res = pre == NULL || N.equals(value, pre) == false;
                    pre = value;
                    return res;
                }
            };
        }

        public static <T> com.landawn.abacus.util.function.Predicate<T> withLimit(final com.landawn.abacus.util.function.Predicate<T> predicate,
                final int limit) {
            N.requireNonNull(predicate);

            return new com.landawn.abacus.util.function.Predicate<T>() {
                private final AtomicInteger counter = new AtomicInteger(limit);

                @Override
                public boolean test(T t) {
                    return predicate.test(t) && counter.decrementAndGet() >= 0;
                }
            };
        }

        public static <T> com.landawn.abacus.util.function.Predicate<T> withLimit(final com.landawn.abacus.util.function.Predicate<T> predicate,
                final long limit) {
            N.requireNonNull(predicate);

            return new com.landawn.abacus.util.function.Predicate<T>() {
                private final AtomicLong counter = new AtomicLong(limit);

                @Override
                public boolean test(T t) {
                    return predicate.test(t) && counter.decrementAndGet() >= 0;
                }
            };
        }
    }

    public static final class BiPredicates {

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate ALWAYS_TRUE = new com.landawn.abacus.util.function.BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return true;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate ALWAYS_FALSE = new com.landawn.abacus.util.function.BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return false;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate EQUAL = new com.landawn.abacus.util.function.BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return N.equals(t, u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate NOT_EQUAL = new com.landawn.abacus.util.function.BiPredicate() {
            @Override
            public boolean test(Object t, Object u) {
                return !N.equals(t, u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate<? extends Comparable, ? extends Comparable> GREATER_THAN = new com.landawn.abacus.util.function.BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) > 0;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate<? extends Comparable, ? extends Comparable> GREATER_EQUAL = new com.landawn.abacus.util.function.BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) >= 0;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate<? extends Comparable, ? extends Comparable> LESS_THAN = new com.landawn.abacus.util.function.BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) < 0;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiPredicate<? extends Comparable, ? extends Comparable> LESS_EQUAL = new com.landawn.abacus.util.function.BiPredicate<Comparable, Comparable>() {
            @Override
            public boolean test(Comparable t, Comparable u) {
                return N.compare(t, u) <= 0;
            }
        };

        private BiPredicates() {
            // singleton.
        }

        public static <T, U> com.landawn.abacus.util.function.BiPredicate<T, U> alwaysTrue() {
            return ALWAYS_TRUE;
        }

        public static <T, U> com.landawn.abacus.util.function.BiPredicate<T, U> alwaysFalse() {
            return ALWAYS_FALSE;
        }

        public static <T, U> com.landawn.abacus.util.function.BiPredicate<T, U> withLimit(final com.landawn.abacus.util.function.BiPredicate<T, U> predicate,
                final int limit) {
            N.requireNonNull(predicate);

            return new com.landawn.abacus.util.function.BiPredicate<T, U>() {
                private final AtomicInteger counter = new AtomicInteger(limit);

                @Override
                public boolean test(T t, U u) {
                    return predicate.test(t, u) && counter.decrementAndGet() >= 0;
                }
            };
        }

        public static <T, U> com.landawn.abacus.util.function.BiPredicate<T, U> withLimit(final com.landawn.abacus.util.function.BiPredicate<T, U> predicate,
                final long limit) {
            N.requireNonNull(predicate);

            return new com.landawn.abacus.util.function.BiPredicate<T, U>() {
                private final AtomicLong counter = new AtomicLong(limit);

                @Override
                public boolean test(T t, U u) {
                    return predicate.test(t, u) && counter.decrementAndGet() >= 0;
                }
            };
        }
    }

    public static final class TriPredicates {

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.TriPredicate ALWAYS_TRUE = new com.landawn.abacus.util.function.TriPredicate() {
            @Override
            public boolean test(Object a, Object b, Object c) {
                return true;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.TriPredicate ALWAYS_FALSE = new com.landawn.abacus.util.function.TriPredicate() {
            @Override
            public boolean test(Object a, Object b, Object c) {
                return false;
            }
        };

        private TriPredicates() {
            // singleton.
        }

        public static <A, B, C> com.landawn.abacus.util.function.TriPredicate<A, B, C> alwaysTrue() {
            return ALWAYS_TRUE;
        }

        public static <A, B, C> com.landawn.abacus.util.function.TriPredicate<A, B, C> alwaysFalse() {
            return ALWAYS_FALSE;
        }

        public static <A, B, C> com.landawn.abacus.util.function.TriPredicate<A, B, C> withLimit(
                final com.landawn.abacus.util.function.TriPredicate<A, B, C> predicate, final int limit) {
            N.requireNonNull(predicate);

            return new com.landawn.abacus.util.function.TriPredicate<A, B, C>() {
                private final AtomicInteger counter = new AtomicInteger(limit);

                @Override
                public boolean test(A a, B b, C c) {
                    return predicate.test(a, b, c) && counter.decrementAndGet() >= 0;
                }
            };
        }

        public static <A, B, C> com.landawn.abacus.util.function.TriPredicate<A, B, C> withLimit(
                final com.landawn.abacus.util.function.TriPredicate<A, B, C> predicate, final long limit) {
            N.requireNonNull(predicate);

            return new com.landawn.abacus.util.function.TriPredicate<A, B, C>() {
                private final AtomicLong counter = new AtomicLong(limit);

                @Override
                public boolean test(A a, B b, C c) {
                    return predicate.test(a, b, c) && counter.decrementAndGet() >= 0;
                }
            };
        }

    }

    public static final class Consumers {

        private Consumers() {
            // singleton.
        }

        public static <T, U> com.landawn.abacus.util.function.Consumer<T> of(final U u,
                final com.landawn.abacus.util.function.BiConsumer<? super T, ? super U> action) {
            N.requireNonNull(action);

            return new com.landawn.abacus.util.function.Consumer<T>() {
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
    }

    public static final class BiConsumers {

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiConsumer DO_NOTHING = new com.landawn.abacus.util.function.BiConsumer() {
            @Override
            public void accept(Object t, Object u) {
                // do nothing.
            }
        };

        private static final com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Object> ADD = new com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Object>() {
            @Override
            public void accept(Collection<Object> t, Object u) {
                t.add(u);
            }
        };

        private static final com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Collection<Object>> ADD_ALL = new com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Collection<Object>>() {
            @Override
            public void accept(Collection<Object> t, Collection<Object> u) {
                t.addAll(u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiConsumer<AbstractList, AbstractList> ADD_ALL_2 = new com.landawn.abacus.util.function.BiConsumer<AbstractList, AbstractList>() {
            @Override
            public void accept(AbstractList t, AbstractList u) {
                t.addAll(u);
            }
        };

        private static final com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Object> REMOVE = new com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Object>() {
            @Override
            public void accept(Collection<Object> t, Object u) {
                t.remove(u);
            }
        };

        private static final com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Collection<Object>> REMOVE_ALL = new com.landawn.abacus.util.function.BiConsumer<Collection<Object>, Collection<Object>>() {
            @Override
            public void accept(Collection<Object> t, Collection<Object> u) {
                t.removeAll(u);
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiConsumer<AbstractList, AbstractList> REMOVE_ALL_2 = new com.landawn.abacus.util.function.BiConsumer<AbstractList, AbstractList>() {
            @Override
            public void accept(AbstractList t, AbstractList u) {
                t.removeAll(u);
            }
        };

        private static final com.landawn.abacus.util.function.BiConsumer<Map<Object, Object>, Map.Entry<Object, Object>> PUT = new com.landawn.abacus.util.function.BiConsumer<Map<Object, Object>, Map.Entry<Object, Object>>() {
            @Override
            public void accept(Map<Object, Object> t, Map.Entry<Object, Object> u) {
                t.put(u.getKey(), u.getValue());
            }
        };

        private static final com.landawn.abacus.util.function.BiConsumer<Map<Object, Object>, Map<Object, Object>> PUT_ALL = new com.landawn.abacus.util.function.BiConsumer<Map<Object, Object>, Map<Object, Object>>() {
            @Override
            public void accept(Map<Object, Object> t, Map<Object, Object> u) {
                t.putAll(u);
            }
        };

        private static final com.landawn.abacus.util.function.BiConsumer<Map<Object, Object>, Object> REMOVE_BY_KEY = new com.landawn.abacus.util.function.BiConsumer<Map<Object, Object>, Object>() {
            @Override
            public void accept(Map<Object, Object> t, Object u) {
                t.remove(u);
            }
        };

        private BiConsumers() {
            // singleton.
        }

        public static <T, U> com.landawn.abacus.util.function.BiConsumer<T, U> ofDoNothing() {
            return DO_NOTHING;
        }

        public static <T, C extends Collection<? super T>> com.landawn.abacus.util.function.BiConsumer<C, T> ofAdd() {
            return (com.landawn.abacus.util.function.BiConsumer<C, T>) ADD;
        }

        public static <T, C extends Collection<T>> com.landawn.abacus.util.function.BiConsumer<C, C> ofAddAll() {
            return (com.landawn.abacus.util.function.BiConsumer<C, C>) ADD_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends AbstractList> com.landawn.abacus.util.function.BiConsumer<T, T> ofAddAll2() {
            return (com.landawn.abacus.util.function.BiConsumer<T, T>) ADD_ALL_2;
        }

        public static <T, C extends Collection<? super T>> com.landawn.abacus.util.function.BiConsumer<C, T> ofRemove() {
            return (com.landawn.abacus.util.function.BiConsumer<C, T>) REMOVE;
        }

        public static <T, C extends Collection<T>> com.landawn.abacus.util.function.BiConsumer<C, C> ofRemoveAll() {
            return (com.landawn.abacus.util.function.BiConsumer<C, C>) REMOVE_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends AbstractList> com.landawn.abacus.util.function.BiConsumer<T, T> ofRemoveAll2() {
            return (com.landawn.abacus.util.function.BiConsumer<T, T>) REMOVE_ALL_2;
        }

        public static <K, V, M extends Map<K, V>, E extends Map.Entry<K, V>> com.landawn.abacus.util.function.BiConsumer<M, E> ofPut() {
            return (com.landawn.abacus.util.function.BiConsumer<M, E>) PUT;
        }

        public static <K, V, M extends Map<K, V>> com.landawn.abacus.util.function.BiConsumer<M, M> ofPutAll() {
            return (com.landawn.abacus.util.function.BiConsumer<M, M>) PUT_ALL;
        }

        public static <K, V, M extends Map<K, V>> com.landawn.abacus.util.function.BiConsumer<M, K> ofRemoveByKey() {
            return (com.landawn.abacus.util.function.BiConsumer<M, K>) REMOVE_BY_KEY;
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

        public static <T, U, R> com.landawn.abacus.util.function.Function<T, R> of(final U u,
                final com.landawn.abacus.util.function.BiFunction<? super T, ? super U, R> func) {
            N.requireNonNull(func);

            return new com.landawn.abacus.util.function.Function<T, R>() {
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

    }

    public static final class BiFunctions {

        private static final com.landawn.abacus.util.function.BiFunction<Collection<Object>, Object, Collection<Object>> ADD = new com.landawn.abacus.util.function.BiFunction<Collection<Object>, Object, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Object u) {
                t.add(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Collection<Object>, Collection<Object>, Collection<Object>> ADD_ALL = new com.landawn.abacus.util.function.BiFunction<Collection<Object>, Collection<Object>, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.addAll(u);
                return t;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiFunction<AbstractList, AbstractList, AbstractList> ADD_ALL_2 = new com.landawn.abacus.util.function.BiFunction<AbstractList, AbstractList, AbstractList>() {
            @Override
            public AbstractList apply(AbstractList t, AbstractList u) {
                t.addAll(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Collection<Object>, Object, Collection<Object>> REMOVE = new com.landawn.abacus.util.function.BiFunction<Collection<Object>, Object, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Object u) {
                t.remove(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Collection<Object>, Collection<Object>, Collection<Object>> REMOVE_ALL = new com.landawn.abacus.util.function.BiFunction<Collection<Object>, Collection<Object>, Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.removeAll(u);
                return t;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BiFunction<AbstractList, AbstractList, AbstractList> REMOVE_ALL_2 = new com.landawn.abacus.util.function.BiFunction<AbstractList, AbstractList, AbstractList>() {
            @Override
            public AbstractList apply(AbstractList t, AbstractList u) {
                t.removeAll(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Map<Object, Object>, Map.Entry<Object, Object>, Map<Object, Object>> PUT = new com.landawn.abacus.util.function.BiFunction<Map<Object, Object>, Map.Entry<Object, Object>, Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map.Entry<Object, Object> u) {
                t.put(u.getKey(), u.getValue());
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Map<Object, Object>, Map<Object, Object>, Map<Object, Object>> PUT_ALL = new com.landawn.abacus.util.function.BiFunction<Map<Object, Object>, Map<Object, Object>, Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map<Object, Object> u) {
                t.putAll(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Map<Object, Object>, Object, Map<Object, Object>> REMOVE_BY_KEY = new com.landawn.abacus.util.function.BiFunction<Map<Object, Object>, Object, Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Object u) {
                t.remove(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Object, Object, Object> RETURN_FIRST = new com.landawn.abacus.util.function.BiFunction<Object, Object, Object>() {
            @Override
            public Object apply(Object t, Object u) {
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BiFunction<Object, Object, Object> RETURN_SECOND = new com.landawn.abacus.util.function.BiFunction<Object, Object, Object>() {
            @Override
            public Object apply(Object t, Object u) {
                return u;
            }
        };

        private BiFunctions() {
            // singleton.
        }

        public static <T, C extends Collection<? super T>> com.landawn.abacus.util.function.BiFunction<C, T, C> ofAdd() {
            return (com.landawn.abacus.util.function.BiFunction<C, T, C>) ADD;
        }

        public static <T, C extends Collection<T>> com.landawn.abacus.util.function.BiFunction<C, C, C> ofAddAll() {
            return (com.landawn.abacus.util.function.BiFunction<C, C, C>) ADD_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends AbstractList> com.landawn.abacus.util.function.BiFunction<T, T, T> ofAddAll2() {
            return (com.landawn.abacus.util.function.BiFunction<T, T, T>) ADD_ALL_2;
        }

        public static <T, C extends Collection<? super T>> com.landawn.abacus.util.function.BiFunction<C, T, C> ofRemove() {
            return (com.landawn.abacus.util.function.BiFunction<C, T, C>) REMOVE;
        }

        public static <T, C extends Collection<T>> com.landawn.abacus.util.function.BiFunction<C, C, C> ofRemoveAll() {
            return (com.landawn.abacus.util.function.BiFunction<C, C, C>) REMOVE_ALL;
        }

        @SuppressWarnings("rawtypes")
        public static <T extends AbstractList> com.landawn.abacus.util.function.BiFunction<T, T, T> ofRemoveAll2() {
            return (com.landawn.abacus.util.function.BiFunction<T, T, T>) REMOVE_ALL_2;
        }

        public static <K, V, M extends Map<K, V>, E extends Map.Entry<K, V>> com.landawn.abacus.util.function.BiFunction<M, E, M> ofPut() {
            return (com.landawn.abacus.util.function.BiFunction<M, E, M>) PUT;
        }

        public static <K, V, M extends Map<K, V>> com.landawn.abacus.util.function.BiFunction<M, M, M> ofPutAll() {
            return (com.landawn.abacus.util.function.BiFunction<M, M, M>) PUT_ALL;
        }

        public static <K, V, M extends Map<K, V>, U> com.landawn.abacus.util.function.BiFunction<M, K, M> ofRemoveByKey() {
            return (com.landawn.abacus.util.function.BiFunction<M, K, M>) REMOVE_BY_KEY;
        }

        public static <T, U> com.landawn.abacus.util.function.BiFunction<T, U, T> ofReturnFirst() {
            return (com.landawn.abacus.util.function.BiFunction<T, U, T>) RETURN_FIRST;
        }

        public static <T, U> com.landawn.abacus.util.function.BiFunction<T, U, U> ofReturnSecond() {
            return (com.landawn.abacus.util.function.BiFunction<T, U, U>) RETURN_SECOND;
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
        private static final com.landawn.abacus.util.function.BinaryOperator THROWING_MERGER = new com.landawn.abacus.util.function.BinaryOperator() {
            @Override
            public Object apply(Object t, Object u) {
                throw new IllegalStateException(String.format("Duplicate key %s", u));
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BinaryOperator IGNORING_MERGER = new com.landawn.abacus.util.function.BinaryOperator() {
            @Override
            public Object apply(Object t, Object u) {
                return t;
            }
        };

        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.BinaryOperator REPLACING_MERGER = new com.landawn.abacus.util.function.BinaryOperator() {
            @Override
            public Object apply(Object t, Object u) {
                return u;
            }
        };

        private static final com.landawn.abacus.util.function.BinaryOperator<Collection<Object>> ADD_ALL = new com.landawn.abacus.util.function.BinaryOperator<Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.addAll(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BinaryOperator<Collection<Object>> REMOVE_ALL = new com.landawn.abacus.util.function.BinaryOperator<Collection<Object>>() {
            @Override
            public Collection<Object> apply(Collection<Object> t, Collection<Object> u) {
                t.removeAll(u);
                return t;
            }
        };

        private static final com.landawn.abacus.util.function.BinaryOperator<Map<Object, Object>> PUT_ALL = new com.landawn.abacus.util.function.BinaryOperator<Map<Object, Object>>() {
            @Override
            public Map<Object, Object> apply(Map<Object, Object> t, Map<Object, Object> u) {
                t.putAll(u);
                return t;
            }
        };

        private BinaryOperators() {
            // singleton.
        }

        public static <T, C extends Collection<T>> com.landawn.abacus.util.function.BinaryOperator<C> ofAddAll() {
            return (com.landawn.abacus.util.function.BinaryOperator<C>) ADD_ALL;
        }

        public static <T, C extends Collection<T>> com.landawn.abacus.util.function.BinaryOperator<C> ofRemoveAll() {
            return (com.landawn.abacus.util.function.BinaryOperator<C>) REMOVE_ALL;
        }

        public static <K, V, M extends Map<K, V>> com.landawn.abacus.util.function.BinaryOperator<M> ofPutAll() {
            return (com.landawn.abacus.util.function.BinaryOperator<M>) PUT_ALL;
        }

        public static com.landawn.abacus.util.function.BinaryOperator<String> ofJoin(final String seperator) {
            return new com.landawn.abacus.util.function.BinaryOperator<String>() {
                @Override
                public String apply(String t, String u) {
                    return t + seperator + u;
                }
            };
        }

        public static <T> com.landawn.abacus.util.function.BinaryOperator<T> minBy(final Comparator<? super T> comparator) {
            N.requireNonNull(comparator);

            return new com.landawn.abacus.util.function.BinaryOperator<T>() {
                @Override
                public T apply(T t, T u) {
                    return comparator.compare(t, u) <= 0 ? t : u;
                }
            };
        }

        public static <T> com.landawn.abacus.util.function.BinaryOperator<T> maxBy(final Comparator<? super T> comparator) {
            N.requireNonNull(comparator);

            return new com.landawn.abacus.util.function.BinaryOperator<T>() {
                @Override
                public T apply(T t, T u) {
                    return comparator.compare(t, u) >= 0 ? t : u;
                }
            };
        }
    }

    public static final class UnaryOperators {
        @SuppressWarnings("rawtypes")
        private static final com.landawn.abacus.util.function.UnaryOperator IDENTITY = new com.landawn.abacus.util.function.UnaryOperator() {
            @Override
            public Object apply(Object t) {
                return t;
            }
        };

        private UnaryOperators() {
            // singleton.
        }

        public static <T> com.landawn.abacus.util.function.UnaryOperator<T> identity() {
            return IDENTITY;
        }
    }
}
