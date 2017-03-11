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
public interface BiConsumer<T, U> extends java.util.function.BiConsumer<T, U> {

    static final BiConsumer<Collection<Object>, Object> ADD = (c, t) -> c.add(t);
    static final BiConsumer<Collection<Object>, Collection<Object>> ADD_ALL = (c, t) -> c.addAll(t);
    static final BiConsumer<Collection<Object>, Object> REMOVE = (c, t) -> c.remove(t);
    static final BiConsumer<Collection<?>, Collection<?>> REMOVE_ALL = (c, t) -> c.removeAll(t);
    static final BiConsumer<Map<Object, Object>, Map<Object, Object>> PUT_ALL = (m, t) -> m.putAll(t);

    @SuppressWarnings("rawtypes")
    static final BiConsumer DO_NOTHING = (c, t) -> {
    };

    @Override
    void accept(T t, U u);

    static <T, C extends Collection<? super T>> BiConsumer<C, T> ofAdd() {
        return (BiConsumer<C, T>) ADD;
    }

    static <T, U extends Collection<? extends T>, C extends Collection<? super T>> BiConsumer<C, U> ofAddAll() {
        return (BiConsumer<C, U>) ADD_ALL;
    }

    static <T, C extends Collection<? super T>> BiConsumer<C, T> ofRemove() {
        return (BiConsumer<C, T>) REMOVE;
    }

    static <T, U extends Collection<?>, C extends Collection<? super T>> BiConsumer<C, U> ofRemoveAll() {
        return (BiConsumer<C, U>) REMOVE_ALL;
    }

    static <K, V, U extends Map<? extends K, ? extends V>, M extends Map<? super K, ? super V>> BiConsumer<M, U> ofPutAll() {
        return (BiConsumer<M, U>) PUT_ALL;
    }

    static <T, U> BiConsumer<T, U> ofDoNothing() {
        return DO_NOTHING;
    }
}
