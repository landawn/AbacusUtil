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

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface BiFunction<T, U, R> extends java.util.function.BiFunction<T, U, R> {

    @Override
    R apply(T t, U u);

    public static <T, R extends Collection<? super T>> BiFunction<R, T, R> ofAdd() {
        return (r, t) -> {
            r.add(t);
            return r;
        };
    }

    public static <T, U extends Collection<? extends T>, R extends Collection<? super T>> BiFunction<R, U, R> ofAddAll() {
        return (r, u) -> {
            r.addAll(u);
            return r;
        };
    }

    public static <T, R extends Collection<? super T>> BiFunction<R, T, R> ofRemove() {
        return (r, t) -> {
            r.remove(t);
            return r;
        };
    }

    public static <T, U extends Collection<?>, R extends Collection<? super T>> BiFunction<R, U, R> ofRemoveAll() {
        return (r, u) -> {
            r.removeAll(u);
            return r;
        };
    }

    public static <K, V, U extends Map<? extends K, ? extends V>, R extends Map<? super K, ? super V>> BiFunction<R, U, R> ofPutAll() {
        return (r, u) -> {
            r.putAll(u);
            return r;
        };
    }
}
