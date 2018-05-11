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
import java.util.Objects;

import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Try;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface BiConsumer<T, U> extends java.util.function.BiConsumer<T, U>, Try.BiConsumer<T, U, RuntimeException> {

    @Override
    default BiConsumer<T, U> andThen(java.util.function.BiConsumer<? super T, ? super U> after) {
        Objects.requireNonNull(after);

        return (l, r) -> {
            accept(l, r);
            after.accept(l, r);
        };
    }

    /**
     * Returns the specified instance
     * 
     * @param biConsumer
     * @return
     */
    public static <T, U> BiConsumer<T, U> of(final BiConsumer<T, U> biConsumer) {
        N.requireNonNull(biConsumer);

        return biConsumer;
    }

    public static <T, U, R> BiConsumer<T, U> create(final BiFunction<T, U, R> func) {
        N.requireNonNull(func);

        return new BiConsumer<T, U>() {
            @Override
            public void accept(T t, U u) {
                func.apply(t, u);
            }
        };
    }

    static <T, U> BiConsumer<T, U> doNothing() {
        return Fn.BiConsumers.doNothing();
    }

    static <T, C extends Collection<? super T>> BiConsumer<C, T> ofAdd() {
        return Fn.BiConsumers.ofAdd();
    }

    static <T, C extends Collection<T>> BiConsumer<C, C> ofAddAll() {
        return Fn.BiConsumers.ofAddAll();
    }

    static <T, C extends Collection<? super T>> BiConsumer<C, T> ofRemove() {
        return Fn.BiConsumers.ofRemove();
    }

    static <T, C extends Collection<T>> BiConsumer<C, C> ofRemoveAll() {
        return Fn.BiConsumers.ofRemoveAll();
    }

    static <K, V, M extends Map<K, V>> BiConsumer<M, M> ofPutAll() {
        return Fn.BiConsumers.ofPutAll();
    }
}
