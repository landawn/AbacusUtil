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

import java.util.Map;
import java.util.function.Function;

import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface TriFunction<A, B, C, R> {

    static final TriFunction<Map<Object, Object>, Object, Object, Map<Object, Object>> PUT = (m, k, v) -> {
        m.put(k, v);
        return m;
    };

    @SuppressWarnings("rawtypes")
    static final TriFunction JUST_RETURN_FIRST = (a, b, c) -> a;
    @SuppressWarnings("rawtypes")
    static final TriFunction JUST_RETURN_SECOND = (a, b, c) -> b;
    @SuppressWarnings("rawtypes")
    static final TriFunction JUST_RETURN_THIRD = (a, b, c) -> c;

    R apply(A a, B b, C c);

    default <V> TriFunction<A, B, C, V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (a, b, c) -> after.apply(apply(a, b, c));
    }

    static <K, V, M extends Map<? super K, ? super V>> TriFunction<M, K, V, M> ofPut() {
        return (TriFunction<M, K, V, M>) PUT;
    }

    static <A, B, C> TriFunction<A, B, C, A> ofJustReturnFirst() {
        return JUST_RETURN_FIRST;
    }

    static <A, B, C> TriFunction<A, B, C, B> ofJustReturnSecond() {
        return JUST_RETURN_SECOND;
    }

    static <A, B, C> TriFunction<A, B, C, C> ofJustReturnThird() {
        return JUST_RETURN_THIRD;
    }
}
