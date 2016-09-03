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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import com.landawn.abacus.annotation.Internal;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;
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
    private final Class<V> collectionType;
    private final Class<V> concreteCollectionType;

    Multimap() {
        this(HashMap.class, ArrayList.class);
    }

    Multimap(int initialCapacity) {
        this(new HashMap<K, V>(initialCapacity), ArrayList.class);
    }

    @SuppressWarnings("rawtypes")
    public Multimap(final Class<? extends Collection> collectionType) {
        this(HashMap.class, collectionType);
    }

    @SuppressWarnings("rawtypes")
    public Multimap(final Class<? extends Map> valueMapType, final Class<? extends Collection> collectionType) {
        this(N.newInstance(valueMapType), collectionType);
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
        this.collectionType = (Class) collectionType;

        if (Modifier.isAbstract(collectionType.getModifiers())) {
            if (List.class.isAssignableFrom(collectionType)) {
                concreteCollectionType = (Class) ArrayList.class;
            } else if (Set.class.isAssignableFrom(collectionType)) {
                concreteCollectionType = (Class) HashSet.class;
            } else if (Queue.class.isAssignableFrom(collectionType)) {
                concreteCollectionType = (Class) ArrayDeque.class;
            } else {
                throw new IllegalArgumentException("Unsupported collection type: " + collectionType.getCanonicalName());
            }
        } else {
            concreteCollectionType = (Class) collectionType;
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

    //    static <K, E, V extends Collection<E>> Multimap<K, E, V> of(final Class<V> collectionType, final Object... a) {
    //        final Multimap<K, E, V> multimap = new Multimap<>(collectionType);
    //
    //        N.newMultimap(multimap, a);
    //
    //        return multimap;
    //    }
    //
    //    @SuppressWarnings("rawtypes")
    //    static <K, E, V extends Collection<E>> Multimap<K, E, V> of(final Class<? extends Map> valueMapType, final Class<? extends Collection> collectionType,
    //            final Object... a) {
    //        final Multimap<K, E, V> multimap = new Multimap<>(valueMapType, collectionType);
    //
    //        N.newMultimap(multimap, a);
    //
    //        return multimap;
    //    }

    public static <K, E> Multimap<K, E, List<E>> from(final Map<? extends K, ? extends E> map) {
        final Multimap<K, E, List<E>> multimap = new Multimap<>(N.initHashCapacity(map.size()));

        multimap.putAll(map);

        return multimap;
    }

    //    static <K, E, V extends Collection<E>> Multimap<K, E, V> of(final Class<V> collectionType, final Map<K, E> map) {
    //        final Multimap<K, E, V> multimap = new Multimap<>(collectionType);
    //
    //        multimap.putAll(map);
    //
    //        return multimap;
    //    }
    //
    //    @SuppressWarnings("rawtypes")
    //    static <K, E, V extends Collection<E>> Multimap<K, E, V> of(final Class<? extends Map> valueMapType, final Class<V> collectionType,
    //            final Map<K, E> map) {
    //        final Multimap<K, E, V> multimap = new Multimap<>(valueMapType, collectionType);
    //
    //        multimap.putAll(map);
    //
    //        return multimap;
    //    }

    public static <K, E> Multimap<K, E, List<E>> from2(final Map<? extends K, ? extends Collection<? extends E>> map) {
        final Multimap<K, E, List<E>> multimap = new Multimap<>(N.initHashCapacity(map.size()));

        for (Map.Entry<? extends K, ? extends Collection<? extends E>> entry : map.entrySet()) {
            multimap.putAll(entry.getKey(), entry.getValue());
        }

        return multimap;
    }

    //    public static <K, E, V extends Collection<E>> Multimap<K, E, V> from(final Class<V> collectionType, final Map<? extends K, ? extends Collection<E>> map) {
    //        final Multimap<K, E, V> multimap = new Multimap<>(collectionType);
    //
    //        for (Map.Entry<? extends K, ? extends Collection<E>> entry : map.entrySet()) {
    //            multimap.putAll(entry.getKey(), entry.getValue());
    //        }
    //
    //        return multimap;
    //    }
    //
    //    @SuppressWarnings("rawtypes")
    //    public static <K, E, V extends Collection<E>> Multimap<K, E, V> from(final Class<? extends Map> valueMapType, final Class<V> collectionType,
    //            final Map<? extends K, ? extends Collection<E>> map) {
    //        final Multimap<K, E, V> multimap = new Multimap<>(valueMapType, collectionType);
    //
    //        for (Map.Entry<? extends K, ? extends Collection<E>> entry : map.entrySet()) {
    //            multimap.putAll(entry.getKey(), entry.getValue());
    //        }
    //
    //        return multimap;
    //    }

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

    public boolean put(final K key, final E e) {
        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteCollectionType);
            valueMap.put(key, val);
        }

        return val.add(e);
    }

    public boolean putIfAbsent(final K key, final E e) {
        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteCollectionType);
            valueMap.put(key, val);
        } else if (val.contains(e)) {
            return false;
        }

        return val.add(e);
    }

    public void putAll(final K key, final Collection<? extends E> c) {
        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteCollectionType);
            valueMap.put(key, val);
        }

        if (N.notNullOrEmpty(c)) {
            val.addAll(c);
        }
    }

    public void putAll(final Map<? extends K, ? extends E> m) {
        if (N.isNullOrEmpty(m)) {
            return;
        }

        K key = null;
        V val = null;

        for (Map.Entry<? extends K, ? extends E> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (val == null) {
                val = N.newInstance(concreteCollectionType);
                valueMap.put(key, val);
            }

            val.add(e.getValue());
        }
    }

    public void putAll(final Multimap<? extends K, ? extends E, ? extends Collection<? extends E>> m) {
        if (N.isNullOrEmpty(m)) {
            return;
        }

        K key = null;
        V val = null;

        for (Map.Entry<? extends K, ? extends Collection<? extends E>> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (val == null) {
                val = N.newInstance(concreteCollectionType);
                valueMap.put(key, val);
            }

            if (N.notNullOrEmpty(e.getValue())) {
                val.addAll(e.getValue());
            }
        }
    }

    public boolean remove(final Object key, final Object e) {
        V val = valueMap.get(key);

        if (N.isNullOrEmpty(val)) {
            valueMap.remove(key);

            return false;
        }

        if (val.remove(e)) {
            if (N.isNullOrEmpty(val)) {
                valueMap.remove(key);
            }

            return true;
        } else {
            return false;
        }
    }

    public V removeAll(final Object key) {
        return valueMap.remove(key);
    }

    public void removeAll(final Object key, final Collection<? extends E> c) {
        if (N.isNullOrEmpty(c)) {
            return;
        }

        V val = valueMap.get(key);

        if (N.notNullOrEmpty(val)) {
            val.removeAll(c);
        }

        if (N.isNullOrEmpty(val)) {
            valueMap.remove(key);
        }
    }

    public void removeAll(final Map<? extends K, ? extends E> m) {
        if (N.isNullOrEmpty(m)) {
            return;
        }

        Object key = null;
        V val = null;

        for (Map.Entry<?, ?> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (N.notNullOrEmpty(val)) {
                val.remove(e.getValue());
            }

            if (N.isNullOrEmpty(val)) {
                valueMap.remove(key);
            }
        }
    }

    public void removeAll(final Multimap<? extends K, ? extends E, ? extends Collection<? extends E>> m) {
        if (N.isNullOrEmpty(m)) {
            return;
        }

        Object key = null;
        V val = null;

        for (Map.Entry<?, ? extends Collection<?>> e : m.entrySet()) {
            key = e.getKey();
            val = valueMap.get(key);

            if (N.notNullOrEmpty(val) && N.notNullOrEmpty(e.getValue())) {
                val.removeAll(e.getValue());
            }

            if (N.isNullOrEmpty(val)) {
                valueMap.remove(key);
            }
        }
    }

    /**
     * Replaces one of the specified <code>oldValue</code> with the specified <code>newValue</code>.
     * <code>False</code> is returned if no <code>oldValue</code> is found.
     *
     * @param key
     * @param oldValue
     * @param newValue
     * @return <code>true</code> only if the old value is removed from the value collection successfully and the new value is added to value collection successfully.
     */
    public boolean replace(final K key, final E oldValue, final E newValue) {
        V val = valueMap.get(key);

        if (N.isNullOrEmpty(val)) {
            return false;
        }

        if (val.remove(oldValue) == false) {
            return false;
        }

        return val.add(newValue);
    }

    /**
     * Replaces all of the specified <code>oldValue</code> with the specified <code>newValue</code>.
     * <code>False</code> is returned if no <code>oldValue</code> is found.
     *
     * @param key
     * @param oldValue
     * @param newValue
     * @return <code>true</code> only if the all of the old values are removed from the value collection successfully and the new value is added to value collection successfully.
     */
    public boolean replaceAll(final K key, final E oldValue, final E newValue) {
        V val = valueMap.get(key);

        if (N.isNullOrEmpty(val)) {
            return false;
        }

        if (val.removeAll(N.asList(oldValue)) == false) {
            return false;
        }

        return val.add(newValue);
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

    public boolean containsAll(final Object key, final Collection<? extends E> c) {
        final V val = valueMap.get(key);

        return val == null ? false : (N.isNullOrEmpty(c) ? true : val.containsAll(c));
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

    public void forEach(BiConsumer<? super K, ? super V> action) {
        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            action.accept(entry.getKey(), entry.getValue());
        }
    }

    /**
     * 
     * @param action break if the action returns false.
     * @return false if it breaks, otherwise true.
     */
    public boolean forEach2(BiFunction<? super K, ? super V, Boolean> action) {
        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            if (action.apply(entry.getKey(), entry.getValue()).booleanValue() == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * 
     * if (N.isNullOrEmpty(oldValue)) {
     *     final V newValue = mappingFunction.apply(key);
     * 
     *     if (N.notNullOrEmpty(newValue)) {
     *         valueMap.put(key, newValue);
     *         return newValue;
     *     }
     * }
     * 
     * return oldValue;
     * </pre>
     * 
     * @param key
     * @param mappingFunction
     * @return
     */
    public V computeIfAbsent(K key, Function<? super K, ? extends V> mappingFunction) {
        N.requireNonNull(mappingFunction);

        final V oldValue = get(key);

        if (N.isNullOrEmpty(oldValue)) {
            final V newValue = mappingFunction.apply(key);

            if (N.notNullOrEmpty(newValue)) {
                valueMap.put(key, newValue);
                return newValue;
            }
        }

        return oldValue;
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
     * if (N.isNullOrEmpty(newValue)) {
     *     valueMap.remove(key);
     *     return newValue;
     * } else {
     *     valueMap.put(key, newValue);
     *     return newValue;
     * }
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

        if (N.isNullOrEmpty(newValue)) {
            valueMap.remove(key);
            return newValue;
        } else {
            valueMap.put(key, newValue);
            return newValue;
        }
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * final V newValue = remappingFunction.apply(key, oldValue);
     * 
     * if (N.isNullOrEmpty(newValue)) {
     *     if (oldValue != null || containsKey(key)) {
     *         valueMap.remove(key);
     *     }
     * 
     *     return newValue;
     * } else {
     *     valueMap.put(key, newValue);
     *     return newValue;
     * }
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

        if (N.isNullOrEmpty(newValue)) {
            if (oldValue != null || containsKey(key)) {
                valueMap.remove(key);
            }

            return newValue;
        } else {
            valueMap.put(key, newValue);
            return newValue;
        }
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multimap:
     * 
     * <pre>
     * final V oldValue = get(key);
     * final V newValue = (N.isNullOrEmpty(oldValue)) ? value : remappingFunction.apply(oldValue, value);
     *     
     * if (N.isNullOrEmpty(newValue)) {
     *     if (oldValue != null || containsKey(key)) {
     *         valueMap.remove(key);
     *     }
     * } else {
     *     valueMap.put(key, newValue);
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
        final V newValue = (N.isNullOrEmpty(oldValue)) ? value : remappingFunction.apply(oldValue, value);

        if (N.isNullOrEmpty(newValue)) {
            if (oldValue != null || containsKey(key)) {
                valueMap.remove(key);
            }
        } else {
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
     *     put(key, e);
     *     return get(key);
     * }
     * 
     * final V newValue = remappingFunction.apply(oldValue, e);
     * 
     * if (N.isNullOrEmpty(newValue)) {
     *     if (oldValue != null || containsKey(key)) {
     *         valueMap.remove(key);
     *     }
     * } else {
     *     valueMap.put(key, newValue);
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

        if (N.isNullOrEmpty(newValue)) {
            if (oldValue != null || containsKey(key)) {
                valueMap.remove(key);
            }
        } else {
            valueMap.put(key, newValue);
        }

        return newValue;
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

    protected Class<V> getCollectionType() {
        return collectionType;
    }

    public Map<K, V> toMap() {
        return new LinkedHashMap<>(valueMap);
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
}
