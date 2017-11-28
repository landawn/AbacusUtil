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
import java.util.Map;

import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Try;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface BiFunction<T, U, R> extends java.util.function.BiFunction<T, U, R>, Try.BiFunction<T, U, R, RuntimeException> {

    @Override
    R apply(T t, U u);

    public static <T, U, R> BiFunction<T, U, R> of(final BiFunction<T, U, R> func) {
        N.requireNonNull(func);

        return func;
    }

    public static <T, U> BiFunction<T, U, Void> create(final BiConsumer<T, U> biConsumer) {
        N.requireNonNull(biConsumer);

        return new BiFunction<T, U, Void>() {
            @Override
            public Void apply(T t, U u) {
                biConsumer.accept(t, u);

                return null;
            }
        };
    }

    static <T, R extends Collection<? super T>> BiFunction<R, T, R> ofAdd() {
        return Fn.BiFunctions.ofAdd();
    }

    static <T, C extends Collection<T>> BiFunction<C, C, C> ofAddAll() {
        return Fn.BiFunctions.ofAddAll();
    }

    static <T, R extends Collection<? super T>> BiFunction<R, T, R> ofRemove() {
        return Fn.BiFunctions.ofRemove();
    }

    static <T, C extends Collection<T>> BiFunction<C, C, C> ofRemoveAll() {
        return Fn.BiFunctions.ofRemoveAll();
    }

    static <K, V, M extends Map<K, V>> BiFunction<M, M, M> ofPutAll() {
        return Fn.BiFunctions.ofPutAll();
    }
}
