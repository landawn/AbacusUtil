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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;

import com.landawn.abacus.util.Fn;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface Supplier<T> extends java.util.function.Supplier<T> {

    @Override
    T get();

    static <T> Supplier<List<T>> ofList() {
        return Fn.Suppliers.ofList();
    }

    static <T> Supplier<LinkedList<T>> ofLinkedList() {
        return Fn.Suppliers.ofLinkedList();
    }

    static <T> Supplier<Set<T>> ofSet() {
        return Fn.Suppliers.ofSet();
    }

    static <T> Supplier<LinkedHashSet<T>> ofLinkedHashSet() {
        return Fn.Suppliers.ofLinkedHashSet();
    }

    static <T> Supplier<TreeSet<T>> ofTreeSet() {
        return Fn.Suppliers.ofTreeSet();
    }

    static <K, V> Supplier<Map<K, V>> ofMap() {
        return Fn.Suppliers.ofMap();
    }

    static <K, V> Supplier<LinkedHashMap<K, V>> ofLinkedHashMap() {
        return Fn.Suppliers.ofLinkedHashMap();
    }

    static <K, V> Supplier<TreeMap<K, V>> ofTreeMap() {
        return Fn.Suppliers.ofTreeMap();
    }

    static <K, V> Supplier<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
        return Fn.Suppliers.ofConcurrentHashMap();
    }

    static <T> Supplier<Queue<T>> ofQueue() {
        return Fn.Suppliers.ofQueue();
    }

    static <T> Supplier<ArrayDeque<T>> ofArrayDeque() {
        return Fn.Suppliers.ofArrayDeque();
    }

    static <T> Supplier<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
        return Fn.Suppliers.ofLinkedBlockingQueue();
    }

    static <T> Supplier<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
        return Fn.Suppliers.ofConcurrentLinkedQueue();
    }

    static <T> Supplier<PriorityQueue<T>> ofPriorityQueue() {
        return Fn.Suppliers.ofPriorityQueue();
    }
}
