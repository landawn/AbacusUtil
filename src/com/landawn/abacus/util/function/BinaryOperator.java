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

import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.Try;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface BinaryOperator<T> extends BiFunction<T, T, T>, java.util.function.BinaryOperator<T>, Try.BinaryOperator<T, RuntimeException> {

    static <T, C extends Collection<T>> BinaryOperator<C> ofAddAll() {
        return Fn.BinaryOperators.ofAddAll();
    }

    static <T, C extends Collection<T>> BinaryOperator<C> ofRemoveAll() {
        return Fn.BinaryOperators.ofRemoveAll();
    }

    static <K, V, M extends Map<K, V>> BinaryOperator<M> ofPutAll() {
        return Fn.BinaryOperators.ofPutAll();
    }

    static <T> BinaryOperator<T> minBy(Comparator<? super T> comparator) {
        return Fn.BinaryOperators.minBy(comparator);
    }

    static <T> BinaryOperator<T> maxBy(Comparator<? super T> comparator) {
        return Fn.BinaryOperators.maxBy(comparator);
    }
}
