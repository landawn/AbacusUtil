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
import java.util.Map;
import java.util.Set;

/**
 * 
 * @since 0.9.55
 * 
 * @author Haiyang Li
 */
public interface ExMap<K, V> extends Map<K, V> {

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

    static <K, V> ExMap<K, V> of(final Map<? extends K, ? extends V> map) {
        return new ExMap<K, V>() {
            private final Map<K, V> m = (Map<K, V>) map;

            @Override
            public int size() {
                return this.m.size();
            }

            @Override
            public boolean isEmpty() {
                return this.m.isEmpty();
            }

            @Override
            public boolean containsKey(Object key) {
                return this.m.containsKey(key);
            }

            @Override
            public boolean containsValue(Object value) {
                return this.m.containsValue(value);
            }

            @Override
            public V get(Object key) {
                return this.m.get(key);
            }

            @Override
            public V put(K key, V value) {
                return this.m.put(key, value);
            }

            @Override
            public V remove(Object key) {
                return this.m.remove(key);
            }

            @Override
            public void putAll(Map<? extends K, ? extends V> m) {
                this.m.putAll(m);
            }

            @Override
            public void clear() {
                this.m.clear();
            }

            @Override
            public Set<K> keySet() {
                return this.m.keySet();
            }

            @Override
            public Collection<V> values() {
                return this.m.values();
            }

            @Override
            public Set<Map.Entry<K, V>> entrySet() {
                return this.m.entrySet();
            }

            @Override
            public int hashCode() {
                return this.m.hashCode();
            }

            @Override
            public boolean equals(Object obj) {
                return this.m.equals(obj);
            }

            @Override
            public String toString() {
                return this.m.toString();
            }
        };
    }

    static <K, V> ExMap<K, V> from(final Map<? extends K, ? extends V> map) {
        return of(new HashMap<K, V>(map));
    }
}
