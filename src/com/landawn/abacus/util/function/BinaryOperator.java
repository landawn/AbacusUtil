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

import java.util.Collection;
import java.util.Comparator;
import java.util.Map;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface BinaryOperator<T> extends BiFunction<T, T, T>, java.util.function.BinaryOperator<T> {

    @SuppressWarnings("rawtypes")
    static final BinaryOperator THROWING_MERGER = (r, u) -> {
        throw new IllegalStateException(String.format("Duplicate key %s", u));
    };

    @SuppressWarnings("rawtypes")
    static final BinaryOperator IGNORING_MERGER = (t, u) -> t;

    @SuppressWarnings("rawtypes")
    static final BinaryOperator REPLACING_MERGER = (t, u) -> u;

    static final BinaryOperator<Collection<Object>> ADD_ALL = (t, u) -> {
        t.addAll(u);
        return t;
    };

    static final BinaryOperator<Collection<?>> REMOVE_ALL = (t, u) -> {
        t.removeAll(u);
        return t;
    };

    static final BinaryOperator<Map<Object, Object>> PUT_ALL = (t, u) -> {
        t.putAll(u);
        return t;
    };

    static <T, C extends Collection<T>> BinaryOperator<C> ofAddAll() {
        return (BinaryOperator<C>) ADD_ALL;
    }

    static <T, C extends Collection<T>> BinaryOperator<C> ofRemoveAll() {
        return (BinaryOperator<C>) REMOVE_ALL;
    }

    static <K, V, M extends Map<K, V>> BinaryOperator<M> ofPutAll() {
        return (BinaryOperator<M>) PUT_ALL;
    }

    static <T> BinaryOperator<T> minBy(Comparator<? super T> comparator) {
        N.requireNonNull(comparator);

        return (a, b) -> comparator.compare(a, b) <= 0 ? a : b;
    }

    static <T> BinaryOperator<T> maxBy(Comparator<? super T> comparator) {
        N.requireNonNull(comparator);

        return (a, b) -> comparator.compare(a, b) >= 0 ? a : b;
    }

    static <T> BinaryOperator<T> throwingMerger() {
        return THROWING_MERGER;
    }
}
