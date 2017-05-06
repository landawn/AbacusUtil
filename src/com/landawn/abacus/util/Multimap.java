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

import java.lang.reflect.Modifier;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import com.landawn.abacus.annotation.Internal;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.TriPredicate;
import com.landawn.abacus.util.stream.Stream;

/**
 * Similar to {@link Map}, but in which each key may be associated with <i>multiple</i> values.
 *
 * <ul>
 * <li>a ->1, 2
 * <li>b -> 3
 * </ul>
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public final class Multimap<K, E, V extends Collection<E>> {
    private final Map<K, V> valueMap;
    private final Class<V> valueType;
    private final Class<V> concreteValueType;

    /**
     * Returns a <code>Multimap<K, E, List<E>></code>
     */
    public Multimap() {
        this(HashMap.class, ArrayList.class);
    }

    /**
     * Returns a <code>Multimap<K, E, List<E>></code>
     * 
     * @param initialCapacity
     */
    public Multimap(int initialCapacity) {
        this(new HashMap<K, V>(initialCapacity), ArrayList.class);
    }

    @SuppressWarnings("rawtypes")
    public Multimap(final Class<? extends Collection> valueType) {
        this(HashMap.class, valueType);
    }

    @SuppressWarnings("rawtypes")
    public Multimap(final Class<? extends Map> mapType, final Class<? extends Collection> valueType) {
        this(N.newInstance(mapType), valueType);
    }

    /**
     *
     * @param valueMap The valueMap and this Multimap share same data; any changes to one will appear in the other.
     * @param collectionType
     */
    @SuppressWarnings("rawtypes")
    @Internal
    Multimap(final Map<K, V> valueMap, final Class<? extends Collection> collectionType) {
        this.valueMap = valueMap;
        this.valueType = (Class) collectionType;

        if (Modifier.isAbstract(collectionType.getModifiers())) {
            if (List.class.isAssignableFrom(collectionType)) {
                concreteValueType = (Class) ArrayList.class;
            } else if (Set.class.isAssignableFrom(collectionType)) {
                concreteValueType = (Class) HashSet.class;
            } else if (Queue.class.isAssignableFrom(collectionType)) {
                concreteValueType = (Class) ArrayDeque.class;
            } else {
                throw new IllegalArgumentException("Unsupported collection type: " + collectionType.getCanonicalName());
            }
        } else {
            concreteValueType = (Class) collectionType;
        }
    }

    Multimap(final Map<? extends K, ? extends E> m) {
        this();

        putAll(m);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final k k1, final v v1) {
        return N.asListMultimap(k1, v1);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final k k1, final v v1, final k k2, final v v2) {
        return N.asListMultimap(k1, v1, k2, v2);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3) {
        return N.asListMultimap(k1, v1, k2, v2, k3, v3);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4) {
        return N.asListMultimap(k1, v1, k2, v2, k3, v3, k4, v4);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4, final k k5, final v v5) {
        return N.asListMultimap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4, final k k5, final v v5, final k k6, final v v6) {
        return N.asListMultimap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3,
            final k k4, final v v4, final k k5, final v v5, final k k6, final v v6, final k k7, final v v7) {
        return N.asListMultimap(k1, v1, k2, v2, k3, v3, k4, v4, k5, v5, k6, v6, k7, v7);
    }

    public static <K, V, k extends K, v extends V> Multimap<K, V, List<V>> of(final Object... a) {
        return N.asListMultimap(a);
    }

    public static <K, E> Multimap<K, E, List<E>> from(final Map<? extends K, ? extends E> map) {
        final Multimap<K, E, List<E>> multimap = new Multimap<>(N.initHashCapacity(map == null ? 0 : map.size()));

        if (N.notNullOrEmpty(map)) {
            multimap.putAll(map);
        }

        return multimap;
    }

    public static <K, E> Multimap<K, E, List<E>> from2(final Map<? extends K, ? extends Collection<? extends E>> map) {
        final Multimap<K, E, List<E>> multimap = new Multimap<>(N.initHashCapacity(map == null ? 0 : map.size()));

        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<? extends K, ? extends Collection<? extends E>> entry : map.entrySet()) {
                multimap.putAll(entry.getKey(), entry.getValue());
            }
        }

        return multimap;
    }

    public static <K, E> Multimap<K, E, List<E>> from(final Collection<? extends E> c, final Function<? super E, ? extends K> keyExtractor) {
        final Multimap<K, E, List<E>> multimap = N.newListMultimap(N.initHashCapacity(N.min(9, c == null ? 0 : c.size())));

        if (N.notNullOrEmpty(c)) {
            for (E e : c) {
                multimap.put(keyExtractor.apply(e), e);
            }
        }

        return multimap;

    }

    public V get(final Object key) {
        return valueMap.get(key);
    }

    public V getOrDefault(final Object key, V defaultValue) {
        final V value = valueMap.get(key);

        if (value == null) {
            return defaultValue;
        }

        return value;
    }

    /**
     * Replace the value with the specified element. 
     * 
     * @param key
     * @param e
     * @return
     */
    public Multimap<K, E, V> set(final K key, final E e) {
        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteValueType);
            valueMap.put(key, val);
        } else {
            val.clear();
        }

        val.add(e);

        return this;
    }

    /**
     * Replace the value with the specified <code>Collection</code>. Remove the key and all values if the specified <code>Collection</code> is null or empty. 
     * 
     * @param key
     * @param c
     * @return
     */
    public Multimap<K, E, V> setAll(final K key, final Collection<? extends E> c) {
        if (N.isNullOrEmpty(c)) {
            valueMap.remove(key);
        } else {
            V val = valueMap.get(key);

            if (val == null) {
                val = N.newInstance(concreteValueType);
                valueMap.put(key, val);
            } else {
                val.clear();
            }

            val.addAll(c);
        }

        return this;
    }

    /**
     * Replace the values with the values in the specified <code>Map</code>. 
     * 
     * @param m
     * @return
     */
    public Multimap<K, E, V> setAll(final Map<? extends K, ? extends E> m) {
        if (N.isNullOrEmpty(m)) {
            return this;
        }

        K key = null;
        V val = null;

        for (Map.Entry<? extends K, ? extends E> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (val == null) {
                val = N.newInstance(concreteValueType);
                valueMap.put(key, val);
            } else {
                val.clear();
            }

            val.add(e.getValue());
        }

        return this;
    }

    /**
     * Replace the values with the values in specified <code>Multimap</code>.
     * Remove the key and all values if the values in the specified <code>Multimap</code> is null or empty.
     * This should not happen because all the values in <code>Multimap</code> must not be null or empty. 
     * 
     * @param m
     * @return
     */
    public Multimap<K, E, V> setAll(final Multimap<? extends K, ? extends E, ? extends Collection<? extends E>> m) {
        if (N.isNullOrEmpty(m)) {
            return this;
        }

        K key = null;
        V val = null;

        for (Map.Entry<? extends K, ? extends Collection<? extends E>> e : m.entrySet()) {
            if (N.isNullOrEmpty(e.getValue())) {
                valueMap.remove(e.getKey());
                continue;
            }

            key = e.getKey();
            val = valueMap.get(key);

            if (val == null) {
                val = N.newInstance(concreteValueType);
                valueMap.put(key, val);
            } else {
                val.clear();
            }

            val.addAll(e.getValue());
        }

        return this;
    }

    public boolean put(final K key, final E e) {
        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteValueType);
            valueMap.put(key, val);
        }

        return val.add(e);
    }

    public boolean putIfAbsent(final K key, final E e) {
        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteValueType);
            valueMap.put(key, val);
        } else if (val.contains(e)) {
            return false;
        }

        return val.add(e);
    }

    public boolean putAll(final K key, final Collection<? extends E> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteValueType);
            valueMap.put(key, val);
        }

        return val.addAll(c);
    }

    public boolean putAll(final Collection<? extends E> c, final Function<? super E, K> keyExtractor) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        boolean result = false;

        for (E e : c) {
            result |= put(keyExtractor.apply(e), e);
        }

        return result;
    }

    public boolean putAll(final Map<? extends K, ? extends E> m) {
        if (N.isNullOrEmpty(m)) {
            return false;
        }

        boolean result = false;
        K key = null;
        V val = null;

        for (Map.Entry<? extends K, ? extends E> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (val == null) {
                val = N.newInstance(concreteValueType);
                valueMap.put(key, val);
            }

            result |= val.add(e.getValue());
        }

        return result;
    }

    public boolean putAll(final Multimap<? extends K, ? extends E, ? extends Collection<? extends E>> m) {
        if (N.isNullOrEmpty(m)) {
            return false;
        }

        boolean result = false;
        K key = null;
        V val = null;

        for (Map.Entry<? extends K, ? extends Collection<? extends E>> e : m.entrySet()) {
            if (N.isNullOrEmpty(e.getValue())) {
                continue;
            }

            key = e.getKey();
            val = valueMap.get(key);

            if (val == null) {
                val = N.newInstance(concreteValueType);
                valueMap.put(key, val);
            }

            result |= val.addAll(e.getValue());
        }

        return result;
    }

    public boolean remove(final Object key, final Object e) {
        V val = valueMap.get(key);

        if (val != null && val.remove(e)) {
            if (val.isEmpty()) {
                valueMap.remove(key);
            }

            return true;
        }

        return false;
    }

    /**
     * 
     * @param key
     * @return values associated with specified key.
     */
    public V removeAll(final Object key) {
        return valueMap.remove(key);
    }

    public boolean removeAll(final Object key, final Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        boolean result = false;
        final V val = valueMap.get(key);

        if (N.notNullOrEmpty(val)) {
            result = val.removeAll(c);

            if (val.isEmpty()) {
                valueMap.remove(key);
            }
        }

        return result;
    }

    public boolean removeAll(final Map<? extends K, ? extends E> m) {
        if (N.isNullOrEmpty(m)) {
            return false;
        }

        boolean result = false;
        Object key = null;
        V val = null;

        for (Map.Entry<?, ?> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (N.notNullOrEmpty(val)) {
                if (result == false) {
                    result = val.remove(e.getValue());
                } else {
                    val.remove(e.getValue());
                }

                if (val.isEmpty()) {
                    valueMap.remove(key);
                }
            }
        }

        return result;
    }

    public boolean removeAll(final Multimap<?, ?, ?> m) {
        if (N.isNullOrEmpty(m)) {
            return false;
        }

        boolean result = false;
        Object key = null;
        V val = null;

        for (Map.Entry<?, ? extends Collection<?>> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (N.notNullOrEmpty(val) && N.notNullOrEmpty(e.getValue())) {
                if (result == false) {
                    result = val.removeAll(e.getValue());
                } else {
                    val.removeAll(e.getValue());
                }

                if (val.isEmpty()) {
                    valueMap.remove(key);
                }
            }
        }

        return result;
    }

    /**
     * Remove the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param value
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean removeIf(E value, Predicate<? super K> predicate) {
        Set<K> removingKeys = null;

        for (K key : this.valueMap.keySet()) {
            if (predicate.test(key)) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(key);
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        boolean modified = false;

        for (K k : removingKeys) {
            if (remove(k, value)) {
                modified = true;
            }
        }

        return modified;
    }

    /**
     * Remove the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param value
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean removeIf(E value, BiPredicate<? super K, ? super V> predicate) {
        Set<K> removingKeys = null;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue())) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(entry.getKey());
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        boolean modified = false;

        for (K k : removingKeys) {
            if (remove(k, value)) {
                modified = true;
            }
        }

        return modified;
    }

    /**
     * Remove the specified values (all occurrences) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param value
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean removeAllIf(Collection<?> values, Predicate<? super K> predicate) {
        Set<K> removingKeys = null;

        for (K key : this.valueMap.keySet()) {
            if (predicate.test(key)) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(key);
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        boolean modified = false;

        for (K k : removingKeys) {
            if (removeAll(k, values)) {
                modified = true;
            }
        }

        return modified;
    }

    /**
     * Remove the specified values (all occurrences) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param values
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean removeAllIf(Collection<?> values, BiPredicate<? super K, ? super V> predicate) {
        Set<K> removingKeys = null;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue())) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(entry.getKey());
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        boolean modified = false;

        for (K k : removingKeys) {
            if (removeAll(k, values)) {
                modified = true;
            }
        }

        return modified;
    }

    /**
     * Remove all the values associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean removeAllIf(Predicate<? super K> predicate) {
        Set<K> removingKeys = null;

        for (K key : this.valueMap.keySet()) {
            if (predicate.test(key)) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(key);
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        for (K k : removingKeys) {
            removeAll(k);
        }

        return true;
    }

    /**
     * Remove all the values associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean removeAllIf(BiPredicate<? super K, ? super V> predicate) {
        Set<K> removingKeys = null;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue())) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(entry.getKey());
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        for (K k : removingKeys) {
            removeAll(k);
        }

        return true;
    }

    /**
     * Replaces one of the specified <code>oldValue</code> with the specified <code>newValue</code>.
     * <code>False</code> is returned if no <code>oldValue</code> is found.
     *
     * @param key
     * @param oldValue
     * @param newValue
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean replace(final K key, final E oldValue, final E newValue) {
        V val = valueMap.get(key);

        if (val.remove(oldValue)) {
            val.add(newValue);
            return true;
        }

        return false;
    }

    /**
     * Replaces all of the specified <code>oldValue</code> with the specified <code>newValue</code>.
     * <code>False</code> is returned if no <code>oldValue</code> is found.
     *
     * @param key
     * @param oldValue
     * @param newValue
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean replaceAll(final K key, final E oldValue, final E newValue) {
        V val = valueMap.get(key);

        if (val.removeAll(Arrays.asList(oldValue))) {
            val.add(newValue);
            return true;
        }

        return false;
    }

    /**
     * Replace the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param value
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean replaceIf(E oldValue, E newValue, Predicate<? super K> predicate) {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey())) {
                if (entry.getValue().remove(oldValue)) {
                    entry.getValue().add(newValue);
                    modified = true;
                }
            }
        }

        return modified;
    }

    /**
     * Replace the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param value
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean replaceIf(E oldValue, E newValue, BiPredicate<? super K, ? super V> predicate) {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue())) {
                if (entry.getValue().remove(oldValue)) {
                    entry.getValue().add(newValue);
                    modified = true;
                }
            }
        }

        return modified;
    }

    /**
     * Replace the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param value
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean replaceAllIf(E oldValue, E newValue, Predicate<? super K> predicate) {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey())) {
                if (entry.getValue().removeAll(Arrays.asList(oldValue))) {
                    entry.getValue().add(newValue);
                    modified = true;
                }
            }
        }

        return modified;
    }

    /**
     * Replace the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * 
     * @param value
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean replaceAllIf(E oldValue, E newValue, BiPredicate<? super K, ? super V> predicate) {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue())) {
                if (entry.getValue().removeAll(Arrays.asList(oldValue))) {
                    entry.getValue().add(newValue);
                    modified = true;
                }
            }
        }

        return modified;
    }

    /**
     * The associated keys will be removed if null or empty values are returned by the specified <code>function</code>.
     * 
     * @param function
     */
    public void replaceAll(BiFunction<? super K, ? super V, ? extends V> function) {
        List<K> keyToRemove = null;
        V newVal = null;

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            newVal = function.apply(entry.getKey(), entry.getValue());

            if (N.isNullOrEmpty(newVal)) {
                if (keyToRemove == null) {
                    keyToRemove = new ArrayList<>();
                }

                keyToRemove.add(entry.getKey());
            } else {
                try {
                    entry.setValue(newVal);
                } catch (IllegalStateException ise) {
                    throw new ConcurrentModificationException(ise);
                }
            }
        }

        if (N.notNullOrEmpty(keyToRemove)) {
            for (K key : keyToRemove) {
                valueMap.remove(key);
            }
        }
    }

    public boolean contains(final Object key, final Object e) {
        final V val = valueMap.get(key);

        return val == null ? false : val.contains(e);
    }

    public boolean containsKey(final Object key) {
        return valueMap.containsKey(key);
    }

    public boolean containsValue(final Object e) {
        Collection<V> values = values();

        for (V val : values) {
            if (val.contains(e)) {
                return true;
            }
        }

        return false;
    }

    public boolean containsAll(final Object key, final Collection<?> c) {
        final V val = valueMap.get(key);

        return val == null ? false : (N.isNullOrEmpty(c) ? true : val.containsAll(c));
    }

    public Multimap<K, E, List<E>> filterByKey(Predicate<? super K> filter) {
        final Multimap<K, E, List<E>> result = new Multimap<>(valueMap instanceof IdentityHashMap ? IdentityHashMap.class : LinkedHashMap.class, List.class);

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey())) {
                result.putAll(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public Multimap<K, E, List<E>> filterByValue(Predicate<? super V> filter) {
        final Multimap<K, E, List<E>> result = new Multimap<>(valueMap instanceof IdentityHashMap ? IdentityHashMap.class : LinkedHashMap.class, List.class);

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            if (filter.test(entry.getValue())) {
                result.putAll(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public Multimap<K, E, List<E>> filter(BiPredicate<? super K, ? super V> filter) {
        final Multimap<K, E, List<E>> result = new Multimap<>(valueMap instanceof IdentityHashMap ? IdentityHashMap.class : LinkedHashMap.class, List.class);

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey(), entry.getValue())) {
                result.putAll(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public void forEach(BiConsumer<? super K, ? super V> action) {
        N.requireNonNull(action);

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            action.accept(entry.getKey(), entry.getValue());
        }
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public <R> R forEach(final R seed, TriFunction<R, ? super K, ? super V, R> accumulator,
            final TriPredicate<? super K, ? super V, ? super R> conditionToBreak) {
        N.requireNonNull(accumulator);
        N.requireNonNull(conditionToBreak);

        R result = seed;

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            result = accumulator.apply(result, entry.getKey(), entry.getValue());

            if (conditionToBreak.test(entry.getKey(), entry.getValue(), result)) {
                break;
            }
        }

        return result;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * 
     * if (N.notNullOrEmpty(oldValue)) {
     *     return oldValue;
     * }
     * 
     * final V newValue = mappingFunction.apply(key);
     * 
     * if (N.notNullOrEmpty(newValue)) {
     *     valueMap.put(key, newValue);
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param key
     * @param mappingFunction
     * @return
     */
    public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
        N.requireNonNull(mappingFunction);

        final V oldValue = get(key);

        if (N.notNullOrEmpty(oldValue)) {
            return oldValue;
        }

        final V newValue = mappingFunction.apply(key);

        if (N.notNullOrEmpty(newValue)) {
            valueMap.put(key, newValue);
        }

        return newValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * 
     * if (N.isNullOrEmpty(oldValue)) {
     *     return oldValue;
     * }
     * 
     * final V newValue = remappingFunction.apply(key, oldValue);
     * 
     * if (N.notNullOrEmpty(newValue)) {
     *     valueMap.put(key, newValue);
     * } else {
     *     valueMap.remove(key);
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param key
     * @param remappingFunction
     * @return
     */
    public V computeIfPresent(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
        N.requireNonNull(remappingFunction);

        final V oldValue = get(key);

        if (N.isNullOrEmpty(oldValue)) {
            return oldValue;
        }

        final V newValue = remappingFunction.apply(key, oldValue);

        if (N.notNullOrEmpty(newValue)) {
            valueMap.put(key, newValue);
        } else {
            valueMap.remove(key);
        }

        return newValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * final V newValue = remappingFunction.apply(key, oldValue);
     * 
     * if (N.notNullOrEmpty(newValue)) {
     *     valueMap.put(key, newValue);
     * } else {
     *     if (oldValue != null) {
     *         valueMap.remove(key);
     *     }
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param key
     * @param remappingFunction
     * @return
     */
    public V compute(K key, BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
        N.requireNonNull(remappingFunction);

        final V oldValue = get(key);
        final V newValue = remappingFunction.apply(key, oldValue);

        if (N.notNullOrEmpty(newValue)) {
            valueMap.put(key, newValue);
        } else {
            if (oldValue != null) {
                valueMap.remove(key);
            }
        }

        return newValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * final V newValue = oldValue == null ? value : remappingFunction.apply(oldValue, value);
     * 
     * if (N.notNullOrEmpty(newValue)) {
     *     valueMap.put(key, newValue);
     * } else {
     *     if (oldValue != null) {
     *         valueMap.remove(key);
     *     }
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param key
     * @param value
     * @param remappingFunction
     * @return
     */
    public V merge(K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
        N.requireNonNull(remappingFunction);
        N.requireNonNull(value);

        final V oldValue = get(key);
        final V newValue = oldValue == null ? value : remappingFunction.apply(oldValue, value);

        if (N.notNullOrEmpty(newValue)) {
            valueMap.put(key, newValue);
        } else {
            if (oldValue != null) {
                valueMap.remove(key);
            }
        }

        return newValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * 
     * if (N.isNullOrEmpty(oldValue)) {
     *     put(key, e);
     *     return get(key);
     * }
     * 
     * final V newValue = remappingFunction.apply(oldValue, e);
     * 
     * if (N.notNullOrEmpty(newValue)) {
     *     valueMap.put(key, newValue);
     * } else {
     *     if (oldValue != null) {
     *         valueMap.remove(key);
     *     }
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param key
     * @param e
     * @param remappingFunction
     * @return
     */
    public V merge(K key, E e, BiFunction<? super V, ? super E, ? extends V> remappingFunction) {
        N.requireNonNull(remappingFunction);
        N.requireNonNull(e);

        final V oldValue = get(key);

        if (N.isNullOrEmpty(oldValue)) {
            put(key, e);
            return get(key);
        }

        final V newValue = remappingFunction.apply(oldValue, e);

        if (N.notNullOrEmpty(newValue)) {
            valueMap.put(key, newValue);
        } else {
            if (oldValue != null) {
                valueMap.remove(key);
            }
        }

        return newValue;
    }

    public Set<K> keySet() {
        return valueMap.keySet();
    }

    public Collection<V> values() {
        return valueMap.values();
    }

    public Set<Map.Entry<K, V>> entrySet() {
        return valueMap.entrySet();
    }

    public Map<K, V> toMap() {
        if (valueMap instanceof IdentityHashMap) {
            return new IdentityHashMap<>(valueMap);
        } else {
            return new HashMap<>(valueMap);
        }
    }

    public <M extends Map<K, V>> M toMap(final IntFunction<M> supplier) {
        final M map = supplier.apply(size());
        map.putAll(valueMap);
        return map;
    }

    public Stream<Map.Entry<K, V>> stream() {
        return Stream.of(valueMap.entrySet());
    }

    public void clear() {
        valueMap.clear();
    }

    public int size() {
        return valueMap.size();
    }

    public boolean isEmpty() {
        return valueMap.isEmpty();
    }

    @Override
    public int hashCode() {
        return valueMap.hashCode();
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals(final Object obj) {
        return obj == this || (obj instanceof Multimap && valueMap.equals(((Multimap<K, E, V>) obj).valueMap));
    }

    @Override
    public String toString() {
        return valueMap.toString();
    }

    protected Class<V> getCollectionType() {
        return valueType;
    }
}
