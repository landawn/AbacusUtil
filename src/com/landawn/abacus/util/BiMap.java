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

import java.util.AbstractSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.annotation.Internal;

/**
 * A BiMap (or "bidirectional map") is a map that preserves the uniqueness of its values as well as that of its keys. 
 * This constraint enables BiMaps to support an "inverse view", which is another BiMap containing the same entries as this BiMap but with reversed keys and values.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class BiMap<K, V> implements Map<K, V> {
    /**
     * The maximum capacity, used if a higher value is implicitly specified by either of the constructors with
     * arguments. MUST be a power of two <= 1<<30.
     */
    static final int MAXIMUM_CAPACITY = 1 << 30;

    /**
     * The default initial capacity - MUST be a power of two.
     */
    static final int DEFAULT_INITIAL_CAPACITY = 1 << 4; // aka 16

    /**
     * The load factor used when none specified in constructor.
     */
    static final float DEFAULT_LOAD_FACTOR = 0.75f;
    private final Map<K, V> keyMap;
    private final Map<V, K> valueMap;
    private transient BiMap<V, K> inverse;

    public BiMap() {
        this(DEFAULT_INITIAL_CAPACITY);
    }

    public BiMap(int initialCapacity) {
        this(initialCapacity, DEFAULT_LOAD_FACTOR);
    }

    public BiMap(int initialCapacity, float loadFactor) {
        this(new HashMap<K, V>(initialCapacity, loadFactor), new HashMap<V, K>(initialCapacity, loadFactor));
    }

    @SuppressWarnings("rawtypes")
    public BiMap(final Class<? extends Map> keyMapType, final Class<? extends Map> valueMapType) {
        this(N.newInstance(keyMapType), N.newInstance(valueMapType));
    }

    /**
     * 
     * @param keyMap The keyMap and this BiMap share the same data; any changes to one will appear in the other.
     * @param valueMap The valueMap and this BiMap share the same data; any changes to one will appear in the other.
     */
    @Internal
    BiMap(final Map<K, V> keyMap, final Map<V, K> valueMap) {
        this.keyMap = keyMap;
        this.valueMap = valueMap;
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final k k1, final v v1) {
        return N.asBiMap(k1, v1);
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final k k1, final v v1, final k k2, final v v2) {
        return N.asBiMap(k1, v1, k2, v2);
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3) {
        return N.asBiMap(k1, v1, k2, v2, k3, v3);
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4) {
        return N.asBiMap(k1, v1, k2, v2, k3, v3, k4, v4);
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5) {
        return N.asBiMap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5);
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5, final k k6, final v v6) {
        return N.asBiMap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6);
    }

    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3, final k k4,
            final v v4, final k k5, final v v5, final k k6, final v v6, final k k7, final v v7) {
        return N.asBiMap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6, k7, v7);
    }

    @SafeVarargs
    public static <K, V, k extends K, v extends V> BiMap<K, V> of(final Object... a) {
        return N.asBiMap(a);
    }

    public static <K, V> BiMap<K, V> from(final Map<? extends K, ? extends V> map) {
        final BiMap<K, V> biMap = new BiMap<>(N.initHashCapacity(map.size()));

        biMap.putAll(map);

        return biMap;
    }

    @Override
    public V get(Object key) {
        return keyMap.get(key);
    }

    public K getByValue(Object value) {
        return valueMap.get(value);
    }

    /**
     * The existed value associated with the specified key or the existed key associated with the specified value will
     * removed/replaced with new value or new key.
     */
    @Override
    public V put(K key, V value) {
        if ((key == null) || (value == null)) {
            throw new NullPointerException("key or value can't be null");
        }

        V v = keyMap.remove(key);

        if (v != null) {
            valueMap.remove(v);
        }

        K k = valueMap.remove(value);

        if (k != null) {
            keyMap.remove(k);
        }

        keyMap.put(key, value);
        valueMap.put(value, key);

        return v;
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        for (Map.Entry<? extends K, ? extends V> e : m.entrySet()) {
            put(e.getKey(), e.getValue());
        }
    }

    @Override
    public V remove(Object key) {
        V value = keyMap.remove(key);

        if (value != null) {
            valueMap.remove(value);
        }

        return value;
    }

    public K removeByValue(Object value) {
        K key = valueMap.remove(value);

        if (key != null) {
            keyMap.remove(key);
        }

        return key;
    }

    @Override
    public boolean containsKey(Object key) {
        return keyMap.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return valueMap.containsKey(value);
    }

    @Override
    public Set<K> keySet() {
        return ImmutableSet.of(keyMap.keySet());
    }

    @Override
    public Set<V> values() {
        return ImmutableSet.of(valueMap.keySet());
    }

    /**
     * Returns a Set of Immutable entry.
     */
    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        return new AbstractSet<Map.Entry<K, V>>() {
            @Override
            public Iterator<Map.Entry<K, V>> iterator() {
                return new Iterator<Map.Entry<K, V>>() {
                    private final Iterator<Map.Entry<K, V>> keyValueEntryIter = keyMap.entrySet().iterator();

                    @Override
                    public boolean hasNext() {
                        return keyValueEntryIter.hasNext();
                    }

                    @Override
                    public Map.Entry<K, V> next() {
                        final Map.Entry<K, V> entry = keyValueEntryIter.next();

                        return new Map.Entry<K, V>() {
                            @Override
                            public K getKey() {
                                return entry.getKey();
                            }

                            @Override
                            public V getValue() {
                                return entry.getValue();
                            }

                            @Override
                            public V setValue(V value) {
                                //    if (N.equals(entry.getValue(), value)) {
                                //        return entry.getValue();
                                //    }
                                //
                                //    //    if (valueMap.containsKey(value)) {
                                //    //        throw new IllegalStateException("Value: " + N.toString(value) + " already existed.");
                                //    //    }
                                //
                                //    valueMap.remove(entry.getValue());
                                //    valueMap.put(value, entry.getKey());
                                //    return entry.setValue(value);

                                throw new UnsupportedOperationException();
                            }
                        };
                    }

                    @Override
                    public void remove() {
                        throw new UnsupportedOperationException();
                    }
                };
            }

            @Override
            public int size() {
                return keyMap.size();
            }
        };
    }

    @Override
    public void clear() {
        keyMap.clear();
        valueMap.clear();
    }

    /**
     * Returns the inverse view of this BiMap, which maps each of this bimap's values to its associated key. 
     * The two BiMaps are backed by the same data; any changes to one will appear in the other.
     * 
     * @return
     */
    public BiMap<V, K> inversed() {
        return (inverse == null) ? inverse = new BiMap<>(valueMap, keyMap) : inverse;
    }

    @Override
    public boolean isEmpty() {
        return keyMap.isEmpty();
    }

    @Override
    public int size() {
        return keyMap.size();
    }

    //    public Stream<Map.Entry<K, V>> stream() {
    //        return Stream.of(keyMap.entrySet());
    //    }

    @Override
    public int hashCode() {
        return keyMap.hashCode();
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof BiMap && keyMap.equals(((BiMap<K, V>) obj).keyMap));
    }

    @Override
    public String toString() {
        return keyMap.toString();
    }
}
