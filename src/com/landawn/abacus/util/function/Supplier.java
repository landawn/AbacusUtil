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

import com.landawn.abacus.util.BiMap;
import com.landawn.abacus.util.BooleanList;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.DoubleList;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.Fn.Suppliers;
import com.landawn.abacus.util.ImmutableList;
import com.landawn.abacus.util.ImmutableMap;
import com.landawn.abacus.util.ImmutableSet;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.ListMultimap;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.SetMultimap;
import com.landawn.abacus.util.ShortList;
import com.landawn.abacus.util.Try;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface Supplier<T> extends java.util.function.Supplier<T>, Try.Supplier<T, RuntimeException> {

    @Override
    T get();

    public static Supplier<BooleanList> ofBooleanList() {
        return Suppliers.ofBooleanList();
    }

    public static Supplier<CharList> ofCharList() {
        return Suppliers.ofCharList();
    }

    public static Supplier<ByteList> ofByteList() {
        return Suppliers.ofByteList();
    }

    public static Supplier<ShortList> ofShortList() {
        return Suppliers.ofShortList();
    }

    public static Supplier<IntList> ofIntList() {
        return Suppliers.ofIntList();
    }

    public static Supplier<LongList> ofLongList() {
        return Suppliers.ofLongList();
    }

    public static Supplier<FloatList> ofFloatList() {
        return Suppliers.ofFloatList();
    }

    public static Supplier<DoubleList> ofDoubleList() {
        return Suppliers.ofDoubleList();
    }

    static <T> Supplier<List<T>> ofList() {
        return Suppliers.ofList();
    }

    static <T> Supplier<LinkedList<T>> ofLinkedList() {
        return Suppliers.ofLinkedList();
    }

    static <T> Supplier<Set<T>> ofSet() {
        return Suppliers.ofSet();
    }

    static <T> Supplier<LinkedHashSet<T>> ofLinkedHashSet() {
        return Suppliers.ofLinkedHashSet();
    }

    static <T> Supplier<SortedSet<T>> ofSortedSet() {
        return Suppliers.ofSortedSet();
    }

    static <T> Supplier<NavigableSet<T>> ofNavigableSet() {
        return Suppliers.ofNavigableSet();
    }

    static <T> Supplier<TreeSet<T>> ofTreeSet() {
        return Suppliers.ofTreeSet();
    }

    static <T> Supplier<Queue<T>> ofQueue() {
        return Suppliers.ofQueue();
    }

    static <T> Supplier<Deque<T>> ofDeque() {
        return Suppliers.ofDeque();
    }

    static <T> Supplier<ArrayDeque<T>> ofArrayDeque() {
        return Suppliers.ofArrayDeque();
    }

    static <T> Supplier<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
        return Suppliers.ofLinkedBlockingQueue();
    }

    static <T> Supplier<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
        return Suppliers.ofConcurrentLinkedQueue();
    }

    static <T> Supplier<PriorityQueue<T>> ofPriorityQueue() {
        return Suppliers.ofPriorityQueue();
    }

    static <K, V> Supplier<Map<K, V>> ofMap() {
        return Suppliers.ofMap();
    }

    static <K, V> Supplier<LinkedHashMap<K, V>> ofLinkedHashMap() {
        return Suppliers.ofLinkedHashMap();
    }

    static <K, V> Supplier<SortedMap<K, V>> ofSortedMap() {
        return Suppliers.ofSortedMap();
    }

    static <K, V> Supplier<NavigableMap<K, V>> ofNavigableMap() {
        return Suppliers.ofNavigableMap();
    }

    static <K, V> Supplier<TreeMap<K, V>> ofTreeMap() {
        return Suppliers.ofTreeMap();
    }

    static <K, V> Supplier<ConcurrentMap<K, V>> ofConcurrentMap() {
        return Suppliers.ofConcurrentMap();
    }

    static <K, V> Supplier<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
        return Suppliers.ofConcurrentHashMap();
    }

    public static <K, V> Supplier<BiMap<K, V>> ofBiMap() {
        return Suppliers.ofBiMap();
    }

    public static <T> Supplier<Multiset<T>> ofMultiset() {
        return Suppliers.ofMultiset();
    }

    public static <T> Supplier<LongMultiset<T>> ofLongMultiset() {
        return Suppliers.ofLongMultiset();
    }

    public static <K, v> Supplier<ListMultimap<K, v>> ofListMultimap() {
        return Suppliers.ofListMultimap();
    }

    public static <K, v> Supplier<SetMultimap<K, v>> ofSetMultimap() {
        return Suppliers.ofSetMultimap();
    }

    public static Supplier<StringBuilder> ofStringBuilder() {
        return Suppliers.ofStringBuilder();
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
}
