/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.Map;

/**
 * It's designed to supported primitive/object array key.
 * The elements in the array must not be modified after the array is put into the map as key.
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public class ArrayHashMap<K, V> extends XHashMap<K, V> {

    public ArrayHashMap() {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction);
    }

    public ArrayHashMap(final int initialCapacity) {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction, initialCapacity);
    }

    @SuppressWarnings("rawtypes")
    public ArrayHashMap(final Class<? extends Map> mapType) {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction, mapType);
    }

    public ArrayHashMap(final Map<? extends K, ? extends V> m) {
        super(Wrapper.arrayHashFunction, Wrapper.arrayEqualsFunction, m == null ? 0 : N.initHashCapacity(m.size()));

        putAll(m);
    }
}
