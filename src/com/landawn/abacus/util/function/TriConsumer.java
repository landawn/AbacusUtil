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

import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface TriConsumer<A, B, C> {

    static final TriConsumer<Map<Object, Object>, Object, Object> PUT = (m, k, v) -> m.put(k, v);

    @SuppressWarnings("rawtypes")
    static final TriConsumer DO_NOTHING = (a, b, c) -> {
    };

    void accept(A a, B b, C c);

    default TriConsumer<A, B, C> andThen(TriConsumer<? super A, ? super B, ? super C> after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }

    static <K, V, M extends Map<? super K, ? super V>> TriConsumer<M, K, V> ofPut() {
        return (TriConsumer<M, K, V>) PUT;
    }

    static <A, B, C> TriConsumer<A, B, C> ofDoNothing() {
        return DO_NOTHING;
    }
}
