/*
 * Copyright (c) 2017, Haiyang Li.
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

import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * 
 * @since 0.9.55
 * 
 * @author Haiyang Li
 */
public interface ExMap<K, V> extends Map<K, V> {

    static <K, V> ExMap<K, V> newHashMap() {
        return of(new HashMap<K, V>());
    }

    static <K, V> ExMap<K, V> newHashMap(int initialCapacity) {
        return of(new HashMap<K, V>(initialCapacity));
    }

    static <K, V> ExMap<K, V> newLinkedHashMap() {
        return of(new LinkedHashMap<K, V>());
    }

    static <K, V> ExMap<K, V> newLinkedHashMap(int initialCapacity) {
        return of(new LinkedHashMap<K, V>(initialCapacity));
    }

    static <K, V, k extends K, v extends V> ExMap<K, V> of(final k k1, final v v1) {
        final Map<k, v> map = N.asMap(k1, v1);
        return ExMap.of(map);
    }

    static <K, V, k extends K, v extends V> ExMap<K, V> of(final k k1, final v v1, final k k2, final v v2) {
        final Map<k, v> map = N.asMap(k1, v1, k2, v2);
        return ExMap.of(map);
    }

    static <K, V, k extends K, v extends V> ExMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3) {
        final Map<k, v> map = N.asMap(k1, v1, k2, v2, k3, v3);
        return ExMap.of(map);
    }

    static <K, V, k extends K, v extends V> ExMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4, final v v4) {
        final Map<k, v> map = N.asMap(k1, v1, k2, v2, k3, v3, k4, v4);
        return ExMap.of(map);
    }

    static <K, V, k extends K, v extends V> ExMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4, final v v4,
            final k k5, final v v5) {
        final Map<k, v> map = N.asMap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5);
        return ExMap.of(map);
    }

    static <K, V, k extends K, v extends V> ExMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4, final v v4,
            final k k5, final v v5, final k k6, final v v6) {
        final Map<k, v> map = N.asMap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6);
        return ExMap.of(map);
    }

    static <K, V, k extends K, v extends V> ExMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4, final v v4,
            final k k5, final v v5, final k k6, final v v6, final k k7, final v v7) {
        final Map<k, v> map = N.asMap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6, k7, v7);
        return ExMap.of(map);
    }

    static <K, V> ExMap<K, V> of(final Map<? extends K, ? extends V> m1) {
        return new ExMap<K, V>() {
            private final Map<K, V> map = (Map<K, V>) m1;

            @Override
            public int size() {
                return map.size();
            }

            @Override
            public boolean isEmpty() {
                return map.isEmpty();
            }

            @Override
            public boolean containsKey(Object key) {
                return map.containsKey(key);
            }

            @Override
            public boolean containsValue(Object value) {
                return map.containsValue(value);
            }

            @Override
            public V get(Object key) {
                return map.get(key);
            }

            @Override
            public V put(K key, V value) {
                return map.put(key, value);
            }

            @Override
            public V remove(Object key) {
                return map.remove(key);
            }

            @Override
            public void putAll(Map<? extends K, ? extends V> m) {
                map.putAll(m);
            }

            @Override
            public void clear() {
                map.clear();
            }

            @Override
            public Set<K> keySet() {
                return map.keySet();
            }

            @Override
            public Collection<V> values() {
                return map.values();
            }

            @Override
            public Set<Map.Entry<K, V>> entrySet() {
                return map.entrySet();
            }

            @Override
            public int hashCode() {
                return map.hashCode();
            }

            @Override
            public boolean equals(Object obj) {
                return map.equals(obj);
            }

            @Override
            public String toString() {
                return map.toString();
            }
        };
    }

    static <K, V> ExMap<K, V> from(final Map<? extends K, ? extends V> m) {
        return of(new HashMap<K, V>(m));
    }
}
