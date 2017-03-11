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

    static final BiFunction<Collection<Object>, Object, Collection<Object>> ADD = (r, t) -> {
        r.add(t);
        return r;
    };

    static final BiFunction<Collection<Object>, Collection<Object>, Collection<Object>> ADD_ALL = (r, t) -> {
        r.addAll(t);
        return r;
    };

    static final BiFunction<Collection<Object>, Object, Collection<Object>> REMOVE = (r, t) -> {
        r.remove(t);
        return r;
    };

    static final BiFunction<Collection<Object>, Collection<?>, Collection<Object>> REMOVE_ALL = (r, t) -> {
        r.removeAll(t);
        return r;
    };

    static final BiFunction<Map<Object, Object>, Map<Object, Object>, Map<Object, Object>> PUT_ALL = (r, t) -> {
        r.putAll(t);
        return r;
    };

    @SuppressWarnings("rawtypes")
    static final BiFunction JUST_RETURN_FIRST = (t, u) -> t;
    @SuppressWarnings("rawtypes")
    static final BiFunction JUST_RETURN_SECOND = (t, u) -> u;

    @Override
    R apply(T t, U u);

    static <T, R extends Collection<? super T>> BiFunction<R, T, R> ofAdd() {
        return (BiFunction<R, T, R>) ADD;
    }

    static <T, U extends Collection<? extends T>, R extends Collection<? super T>> BiFunction<R, U, R> ofAddAll() {
        return (BiFunction<R, U, R>) ADD_ALL;
    }

    static <T, R extends Collection<? super T>> BiFunction<R, T, R> ofRemove() {
        return (BiFunction<R, T, R>) REMOVE;
    }

    static <T, U extends Collection<?>, R extends Collection<? super T>> BiFunction<R, U, R> ofRemoveAll() {
        return (BiFunction<R, U, R>) REMOVE_ALL;
    }

    static <K, V, U extends Map<? extends K, ? extends V>, R extends Map<? super K, ? super V>> BiFunction<R, U, R> ofPutAll() {
        return (BiFunction<R, U, R>) PUT_ALL;
    }

    static <T, U> BiFunction<T, U, T> ofJustReturnFirst() {
        return JUST_RETURN_FIRST;
    }

    static <T, U> BiFunction<T, U, U> ofJustReturnSecond() {
        return JUST_RETURN_SECOND;
    }
}
