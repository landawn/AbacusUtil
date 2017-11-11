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

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Objects;
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
import java.util.function.Function;

import com.landawn.abacus.util.BooleanList;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.Fn.Factory;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.ShortList;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface IntFunction<R> extends java.util.function.IntFunction<R> {
    static final IntFunction<Integer> BOX = new IntFunction<Integer>() {
        @Override
        public Integer apply(int value) {
            return value;
        }
    };

    @Override
    R apply(int value);

    default <V> IntFunction<V> andThen(Function<? super R, ? extends V> after) {
        Objects.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static IntFunction<Integer> identity() {
        return t -> t;
    }

    public static IntFunction<boolean[]> ofBooleanArray() {
        return Factory.ofBooleanArray();
    }

    public static IntFunction<char[]> ofCharArray() {
        return Factory.ofCharArray();
    }

    public static IntFunction<byte[]> ofByteArray() {
        return Factory.ofByteArray();
    }

    public static IntFunction<short[]> ofShortArray() {
        return Factory.ofShortArray();
    }

    public static IntFunction<int[]> ofIntArray() {
        return Factory.ofIntArray();
    }

    public static IntFunction<long[]> ofLongArray() {
        return Factory.ofLongArray();
    }

    public static IntFunction<float[]> ofFloatArray() {
        return Factory.ofFloatArray();
    }

    public static IntFunction<double[]> ofDoubleArray() {
        return Factory.ofDoubleArray();
    }

    public static IntFunction<String[]> ofStringArray() {
        return Factory.ofStringArray();
    }

    public static IntFunction<Object[]> ofObjectArray() {
        return Factory.ofObjectArray();
    }

    public static IntFunction<BooleanList> ofBooleanList() {
        return Factory.ofBooleanList();
    }

    public static IntFunction<CharList> ofCharList() {
        return Factory.ofCharList();
    }

    public static IntFunction<ByteList> ofByteList() {
        return Factory.ofByteList();
    }

    public static IntFunction<ShortList> ofShortList() {
        return Factory.ofShortList();
    }

    public static IntFunction<IntList> ofIntList() {
        return Factory.ofIntList();
    }

    public static IntFunction<LongList> ofLongList() {
        return Factory.ofLongList();
    }

    public static IntFunction<FloatList> ofFloatList() {
        return Factory.ofFloatList();
    }

    public static IntFunction<DoubleList> ofDoubleList() {
        return Factory.ofDoubleList();
    }

    public static <T> IntFunction<List<T>> ofList() {
        return Factory.ofList();
    }

    public static <T> IntFunction<LinkedList<T>> ofLinkedList() {
        return Factory.ofLinkedList();
    }

    public static <T> IntFunction<Set<T>> ofSet() {
        return Factory.ofSet();
    }

    public static <T> IntFunction<LinkedHashSet<T>> ofLinkedHashSet() {
        return Factory.ofLinkedHashSet();
    }

    public static <T> IntFunction<SortedSet<T>> ofSortedSet() {
        return Factory.ofSortedSet();
    }

    public static <T> IntFunction<NavigableSet<T>> ofNavigableSet() {
        return Factory.ofNavigableSet();
    }

    public static <T> IntFunction<TreeSet<T>> ofTreeSet() {
        return Factory.ofTreeSet();
    }

    public static <T> IntFunction<Queue<T>> ofQueue() {
        return Factory.ofQueue();
    }

    public static <T> IntFunction<Deque<T>> ofDeque() {
        return Factory.ofDeque();
    }

    public static <T> IntFunction<ArrayDeque<T>> ofArrayDeque() {
        return Factory.ofArrayDeque();
    }

    public static <T> IntFunction<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
        return Factory.ofLinkedBlockingQueue();
    }

    public static <T> IntFunction<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
        return Factory.ofConcurrentLinkedQueue();
    }

    public static <T> IntFunction<PriorityQueue<T>> ofPriorityQueue() {
        return Factory.ofPriorityQueue();
    }

    public static <K, V> IntFunction<Map<K, V>> ofMap() {
        return Factory.ofMap();
    }

    public static <K, V> IntFunction<LinkedHashMap<K, V>> ofLinkedHashMap() {
        return Factory.ofLinkedHashMap();
    }

    public static <K, V> IntFunction<SortedMap<K, V>> ofSortedMap() {
        return Factory.ofSortedMap();
    }

    public static <K, V> IntFunction<NavigableMap<K, V>> ofNavigableMap() {
        return Factory.ofNavigableMap();
    }

    public static <K, V> IntFunction<TreeMap<K, V>> ofTreeMap() {
        return Factory.ofTreeMap();
    }

    public static <K, V> IntFunction<ConcurrentMap<K, V>> ofConcurrentMap() {
        return Factory.ofConcurrentMap();
    }

    public static <K, V> IntFunction<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
        return Factory.ofConcurrentHashMap();
    }
}
