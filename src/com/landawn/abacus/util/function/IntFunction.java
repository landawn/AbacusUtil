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

package com.landawn.abacus.util.function;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface IntFunction<R> extends java.util.function.IntFunction<R> {

    public static final IntFunction<boolean[]> BOOLEAN_ARRAY_FACTORY = new IntFunction<boolean[]>() {
        @Override
        public boolean[] apply(int value) {
            return new boolean[value];
        }
    };

    public static final IntFunction<char[]> CHAR_ARRAY_FACTORY = new IntFunction<char[]>() {
        @Override
        public char[] apply(int value) {
            return new char[value];
        }
    };

    public static final IntFunction<byte[]> BYTE_ARRAY_FACTORY = new IntFunction<byte[]>() {
        @Override
        public byte[] apply(int value) {
            return new byte[value];
        }
    };

    public static final IntFunction<short[]> SHORT_ARRAY_FACTORY = new IntFunction<short[]>() {
        @Override
        public short[] apply(int value) {
            return new short[value];
        }
    };

    public static final IntFunction<int[]> INT_ARRAY_FACTORY = new IntFunction<int[]>() {
        @Override
        public int[] apply(int value) {
            return new int[value];
        }
    };

    public static final IntFunction<long[]> LONG_ARRAY_FACTORY = new IntFunction<long[]>() {
        @Override
        public long[] apply(int value) {
            return new long[value];
        }
    };

    public static final IntFunction<float[]> FLOAT_ARRAY_FACTORY = new IntFunction<float[]>() {
        @Override
        public float[] apply(int value) {
            return new float[value];
        }
    };

    public static final IntFunction<double[]> DOUBLE_ARRAY_FACTORY = new IntFunction<double[]>() {
        @Override
        public double[] apply(int value) {
            return new double[value];
        }
    };

    public static final IntFunction<String[]> STRING_ARRAY_FACTORY = new IntFunction<String[]>() {
        @Override
        public String[] apply(int value) {
            return new String[value];
        }
    };

    public static final IntFunction<Object[]> OBJECT_ARRAY_FACTORY = new IntFunction<Object[]>() {
        @Override
        public Object[] apply(int value) {
            return new Object[value];
        }
    };

    @SuppressWarnings("rawtypes")
    public static final IntFunction<? super List> LIST_FACTORY = new IntFunction<List>() {
        @Override
        public List apply(int len) {
            return new ArrayList<>(len);
        }
    };

    @SuppressWarnings("rawtypes")
    public static final IntFunction<? super LinkedList> LINKED_LIST_FACTORY = new IntFunction<LinkedList>() {
        @Override
        public LinkedList apply(int len) {
            return new LinkedList<>();
        }
    };

    @SuppressWarnings("rawtypes")
    public static final IntFunction<? super Set> SET_FACTORY = new IntFunction<Set>() {
        @Override
        public Set apply(int len) {
            return new HashSet<>(N.initHashCapacity(len));
        }
    };

    @SuppressWarnings("rawtypes")
    public static final IntFunction<? super LinkedHashSet> LINKED_HASH_SET_FACTORY = new IntFunction<LinkedHashSet>() {
        @Override
        public LinkedHashSet apply(int len) {
            return new LinkedHashSet<>(N.initHashCapacity(len));
        }
    };

    @SuppressWarnings("rawtypes")
    public static final IntFunction<? super Map> MAP_FACTORY = new IntFunction<Map>() {
        @Override
        public Map apply(int len) {
            return new HashMap<>(N.initHashCapacity(len));
        }
    };

    @SuppressWarnings("rawtypes")
    public static final IntFunction<? super LinkedHashMap> LINKED_HASH_MAP_FACTORY = new IntFunction<LinkedHashMap>() {
        @Override
        public LinkedHashMap apply(int len) {
            return new LinkedHashMap<>(N.initHashCapacity(len));
        }
    };

    @Override
    R apply(int value);

    default <V> IntFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static IntFunction<Integer> identity() {
        return t -> t;
    }

    @SuppressWarnings("rawtypes")
    static <T> IntFunction<List<T>> ofListFactory() {
        return (IntFunction) LIST_FACTORY;
    }

    @SuppressWarnings("rawtypes")
    static <T> IntFunction<LinkedList<T>> ofLinkedListFactory() {
        return (IntFunction) LINKED_LIST_FACTORY;
    }

    @SuppressWarnings("rawtypes")
    static <T> IntFunction<Set<T>> ofSetFactory() {
        return (IntFunction) SET_FACTORY;
    }

    @SuppressWarnings("rawtypes")
    static <T> IntFunction<LinkedHashSet<T>> ofLinkedHashSetFactory() {
        return (IntFunction) LINKED_HASH_SET_FACTORY;
    }

    @SuppressWarnings("rawtypes")
    static <K, V> IntFunction<Map<K, V>> ofMapFactory() {
        return (IntFunction) MAP_FACTORY;
    }

    @SuppressWarnings("rawtypes")
    static <K, V> IntFunction<LinkedHashMap<K, V>> ofLinkedHashMapFactory() {
        return (IntFunction) LINKED_HASH_MAP_FACTORY;
    }
}
