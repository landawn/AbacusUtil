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

import java.util.Objects;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Try;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface TriFunction<A, B, C, R> extends Try.TriFunction<A, B, C, R, RuntimeException> {

    @Override
    R apply(A a, B b, C c);

    default <V> TriFunction<A, B, C, V> andThen(java.util.function.Function<? super R, ? extends V> after) {
        Objects.requireNonNull(after);

        return (a, b, c) -> after.apply(apply(a, b, c));
    }

    /**
     * Returns the specified instance
     * 
     * @param func
     * @return
     */
    public static <A, B, C, R> TriFunction<A, B, C, R> of(final TriFunction<A, B, C, R> func) {
        N.requireNonNull(func);

        return func;
    }

    public static <A, B, C> TriFunction<A, B, C, Void> create(final TriConsumer<A, B, C> triConsumer) {
        N.requireNonNull(triConsumer);

        return new TriFunction<A, B, C, Void>() {
            @Override
            public Void apply(A a, B b, C c) {
                triConsumer.accept(a, b, c);

                return null;
            }
        };
    }
}
