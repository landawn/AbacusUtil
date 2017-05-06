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
import java.util.function.Function;

import com.landawn.abacus.util.ExList;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.N;

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
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static IntFunction<Integer> identity() {
        return t -> t;
    }

    public static <T> IntFunction<ExList<T>> ofExList() {
        return Fn.Factory.ofExList();
    }

    public static <T> IntFunction<List<T>> ofList() {
        return Fn.Factory.ofList();
    }

    public static <T> IntFunction<LinkedList<T>> ofLinkedList() {
        return Fn.Factory.ofLinkedList();
    }

    public static <T> IntFunction<Set<T>> ofSet() {
        return Fn.Factory.ofSet();
    }

    public static <T> IntFunction<LinkedHashSet<T>> ofLinkedHashSet() {
        return Fn.Factory.ofLinkedHashSet();
    }

    public static <T> IntFunction<TreeSet<T>> ofTreeSet() {
        return Fn.Factory.ofTreeSet();
    }

    public static <K, V> IntFunction<Map<K, V>> ofMap() {
        return Fn.Factory.ofMap();
    }

    public static <K, V> IntFunction<LinkedHashMap<K, V>> ofLinkedHashMap() {
        return Fn.Factory.ofLinkedHashMap();
    }

    public static <K, V> IntFunction<TreeMap<K, V>> ofTreeMap() {
        return Fn.Factory.ofTreeMap();
    }

    public static <K, V> IntFunction<ConcurrentHashMap<K, V>> ofConcurrentHashMap() {
        return Fn.Factory.ofConcurrentHashMap();
    }

    public static <T> IntFunction<Queue<T>> ofQueue() {
        return Fn.Factory.ofQueue();
    }

    public static <T> IntFunction<ArrayDeque<T>> ofArrayDeque() {
        return Fn.Factory.ofArrayDeque();
    }

    public static <T> IntFunction<LinkedBlockingQueue<T>> ofLinkedBlockingQueue() {
        return Fn.Factory.ofLinkedBlockingQueue();
    }

    public static <T> IntFunction<ConcurrentLinkedQueue<T>> ofConcurrentLinkedQueue() {
        return Fn.Factory.ofConcurrentLinkedQueue();
    }

    public static <T> IntFunction<PriorityQueue<T>> ofPriorityQueue() {
        return Fn.Factory.ofPriorityQueue();
    }
}
