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
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Queue;
import java.util.Set;

import com.landawn.abacus.annotation.Internal;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.EntryStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * Similar to {@link Map}, but in which each key may be associated with <i>multiple</i> values.
 *
 * <ul>
 * <li>a ->1, 2
 * <li>b -> 3
 * </ul>
 *
 * @see N#newListMultimap()
 * @see N#newListMultimap(Class, Class)
 * @see N#newSetMultimap()
 * @see N#newSetMultimap(Class, Class)
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public class Multimap<K, E, V extends Collection<E>> {
    final Map<K, V> valueMap;
    final Class<V> valueType;
    final Class<V> concreteValueType;

    /**
     * Returns a <code>Multimap<K, E, List<E>></code>
     */
    Multimap() {
        this(HashMap.class, ArrayList.class);
    }

    /**
     * Returns a <code>Multimap<K, E, List<E>></code>
     * 
     * @param initialCapacity
     */
    Multimap(int initialCapacity) {
        this(new HashMap<K, V>(initialCapacity), ArrayList.class);
    }

    @SuppressWarnings("rawtypes")
    Multimap(final Class<? extends Map> mapType, final Class<? extends Collection> valueType) {
        this(N.newInstance(mapType), valueType);
    }

    /**
     *
     * @param valueMap The valueMap and this Multimap share same data; any changes to one will appear in the other.
     * @param valueType
     */
    @SuppressWarnings("rawtypes")
    @Internal
    Multimap(final Map<K, V> valueMap, final Class<? extends Collection> valueType) {
        this.valueMap = valueMap;
        this.valueType = (Class) valueType;

        if (Modifier.isAbstract(valueType.getModifiers())) {
            if (List.class.isAssignableFrom(valueType)) {
                concreteValueType = (Class) ArrayList.class;
            } else if (Set.class.isAssignableFrom(valueType)) {
                concreteValueType = (Class) HashSet.class;
            } else if (Queue.class.isAssignableFrom(valueType)) {
                concreteValueType = (Class) ArrayDeque.class;
            } else {
                throw new IllegalArgumentException("Unsupported collection type: " + valueType.getCanonicalName());
            }
        } else {
            concreteValueType = (Class) valueType;
        }
    }

    public static <K, E, V extends Collection<E>, M extends Multimap<K, E, V>> M from(final Map<? extends K, ? extends E> map,
            final Supplier<M> multimapSupplier) {
        final M multimap = multimapSupplier.get();

        if (N.notNullOrEmpty(map)) {
            multimap.putAll(map);
        }

        return multimap;
    }

    public static <K, E, V extends Collection<E>, M extends Multimap<K, E, V>> M from2(final Map<? extends K, ? extends Collection<? extends E>> map,
            final Supplier<M> multimapSupplier) {
        final M multimap = multimapSupplier.get();

        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<? extends K, ? extends Collection<? extends E>> entry : map.entrySet()) {
                multimap.putAll(entry.getKey(), entry.getValue());
            }
        }

        return multimap;
    }

    public static <T, K, V extends Collection<T>, M extends Multimap<K, T, V>, X extends Exception> M from(final Collection<? extends T> c,
            final Try.Function<? super T, ? extends K, X> keyExtractor, final Supplier<M> multimapSupplier) throws X {
        final M multimap = multimapSupplier.get();

        if (N.notNullOrEmpty(c)) {
            for (T e : c) {
                multimap.put(keyExtractor.apply(e), e);
            }
        }

        return multimap;
    }

    public static <T, K, E, V extends Collection<E>, M extends Multimap<K, E, V>, X extends Exception, X2 extends Exception> M from(
            final Collection<? extends T> c, final Try.Function<? super T, ? extends K, X> keyExtractor,
            final Try.Function<? super T, ? extends E, X2> valueExtractor, final Supplier<M> multimapSupplier) throws X, X2 {
        final M multimap = multimapSupplier.get();

        if (N.notNullOrEmpty(c)) {
            for (T e : c) {
                multimap.put(keyExtractor.apply(e), valueExtractor.apply(e));
            }
        }

        return multimap;
    }

    /**
     * 
     * @param map
     * @param multimapSupplier
     * @return
     * @see ListMultimap#invertFrom(Map)
     * @see SetMultimap#invertFrom(Map)
     */
    public static <K, E, V extends Collection<K>, M extends Multimap<E, K, V>> M invertFrom(final Map<K, E> map, final Supplier<M> multimapSupplier) {
        final M multimap = multimapSupplier.get();

        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<K, E> entry : map.entrySet()) {
                multimap.put(entry.getValue(), entry.getKey());
            }
        }

        return multimap;
    }

    /**
     * 
     * @param map
     * @param multimapSupplier
     * @return
     * @see ListMultimap#flatInvertFrom(Map)
     * @see SetMultimap#flatInvertFrom(Map)
     */
    public static <K, E, V extends Collection<K>, M extends Multimap<E, K, V>> M flatInvertFrom(final Map<K, ? extends Collection<? extends E>> map,
            final Supplier<M> multimapSupplier) {
        final M multimap = multimapSupplier.get();

        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<K, ? extends Collection<? extends E>> entry : map.entrySet()) {
                final Collection<? extends E> c = entry.getValue();

                if (N.notNullOrEmpty(c)) {
                    for (E e : c) {
                        multimap.put(e, entry.getKey());
                    }
                }
            }
        }

        return multimap;
    }

    /**
     * 
     * @param multimap
     * @return
     */
    public static <K, E, V extends Collection<E>, VV extends Collection<K>, M extends Multimap<E, K, VV>> M invertFrom(final Multimap<K, E, V> multimap,
            final Supplier<M> multimapSupplier) {
        final M res = multimapSupplier.get();

        if (N.notNullOrEmpty(multimap)) {
            for (Map.Entry<K, V> entry : multimap.entrySet()) {
                final V c = entry.getValue();

                if (N.notNullOrEmpty(c)) {
                    for (E e : c) {
                        res.put(e, entry.getKey());
                    }
                }
            }
        }

        return res;
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
     * Replace the value with the specified element. 
     * 
     * @param key
     * @param e
     * @return
     */
    public Multimap<K, E, V> setIfAbsent(final K key, final E e) {
        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteValueType);
            val.add(e);
            valueMap.put(key, val);
        }

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
     * Replace the value with the specified <code>Collection</code>. Remove the key and all values if the specified <code>Collection</code> is null or empty. 
     * 
     * @param key
     * @param c
     * @return
     */
    public Multimap<K, E, V> setAllIfAbsent(final K key, final Collection<? extends E> c) {
        V val = valueMap.get(key);

        if (val == null && N.notNullOrEmpty(c)) {
            val = N.newInstance(concreteValueType);
            val.addAll(c);
            valueMap.put(key, val);
        }

        return this;
    }

    /**
     * Replace the values for the keys calculated by the specified {@code keyExtractor}.
     * 
     * @param c
     * @param keyExtractor
     * @return
     */
    public <X extends Exception> Multimap<K, E, V> setAll(final Collection<? extends E> c, final Try.Function<? super E, K, X> keyExtractor) throws X {
        if (N.notNullOrEmpty(c)) {
            for (E e : c) {
                set(keyExtractor.apply(e), e);
            }
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

    public boolean putAllIfAbsent(final K key, final Collection<? extends E> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        V val = valueMap.get(key);

        if (val == null) {
            val = N.newInstance(concreteValueType);
            val.addAll(c);
            valueMap.put(key, val);
            return true;
        }

        return false;
    }

    public <X extends Exception> boolean putAll(final Collection<? extends E> c, final Try.Function<? super E, K, X> keyExtractor) throws X {
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
    public <X extends Exception> boolean removeIf(E value, Try.Predicate<? super K, X> predicate) throws X {
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
    public <X extends Exception> boolean removeIf(E value, Try.BiPredicate<? super K, ? super V, X> predicate) throws X {
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
     * @param val
     * @param predicate
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public <X extends Exception> boolean removeAllIf(Collection<?> values, Try.Predicate<? super K, X> predicate) throws X {
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
    public <X extends Exception> boolean removeAllIf(Collection<?> values, Try.BiPredicate<? super K, ? super V, X> predicate) throws X {
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
    public <X extends Exception> boolean removeAllIf(Try.Predicate<? super K, X> predicate) throws X {
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
    public <X extends Exception> boolean removeAllIf(Try.BiPredicate<? super K, ? super V, X> predicate) throws X {
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
    public boolean replace(final K key, final Object oldValue, final E newValue) {
        V val = valueMap.get(key);

        return replace(val, oldValue, newValue);
    }

    private boolean replace(final V val, final Object oldValue, final E newValue) {
        if (val instanceof List) {
            final List<E> list = (List<E>) val;

            if (list instanceof ArrayList) {
                if (oldValue == null) {
                    for (int i = 0, len = list.size(); i < len; i++) {
                        if (list.get(i) == null) {
                            list.set(i, newValue);
                            return true;
                        }
                    }
                } else {
                    for (int i = 0, len = list.size(); i < len; i++) {
                        if (oldValue.equals(list.get(i))) {
                            list.set(i, newValue);
                            return true;
                        }
                    }
                }
            } else {
                final ListIterator<E> iter = list.listIterator();

                if (oldValue == null) {
                    while (iter.hasNext()) {
                        if (iter.next() == null) {
                            iter.set(newValue);
                            return true;
                        }
                    }
                } else {
                    while (iter.hasNext()) {
                        if (oldValue.equals(iter.next())) {
                            iter.set(newValue);
                            return true;
                        }
                    }
                }
            }

            return false;
        } else {
            if (val.remove(oldValue)) {
                val.add(newValue);
                return true;
            }

            return false;
        }
    }

    /**
     * Replaces all of the specified <code>oldValue</code> with the specified <code>newValue</code>.
     * <code>False</code> is returned if no <code>oldValue</code> is found.
     *
     * @param key
     * @param oldValues
     * @param newValue
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public boolean replaceAll(final K key, final Collection<?> oldValues, final E newValue) {
        V val = valueMap.get(key);

        if (val.removeAll(oldValues)) {
            val.add(newValue);
            return true;
        }

        return false;
    }

    /**
     * Replace the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * @param predicate
     * @param oldValue
     * @param newValue
     * 
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public <X extends Exception> boolean replaceIf(Try.Predicate<? super K, X> predicate, Object oldValue, E newValue) throws X {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey())) {
                modified = modified | replace(entry.getValue(), oldValue, newValue);
            }
        }

        return modified;
    }

    /**
     * Replace the specified value (one occurrence) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * @param predicate
     * @param oldValue
     * @param newValue
     * 
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public <X extends Exception> boolean replaceIf(Try.BiPredicate<? super K, ? super V, X> predicate, Object oldValue, E newValue) throws X {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue())) {
                modified = modified | replace(entry.getValue(), oldValue, newValue);
            }
        }

        return modified;
    }

    /**
     * Replace the specified value (all occurrences) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * @param predicate
     * @param oldValues
     * @param newValue
     * 
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public <X extends Exception> boolean replaceAllIf(Try.Predicate<? super K, X> predicate, Collection<?> oldValues, E newValue) throws X {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey())) {
                if (entry.getValue().removeAll(oldValues)) {
                    entry.getValue().add(newValue);
                    modified = true;
                }
            }
        }

        return modified;
    }

    /**
     * Replace the specified value (all occurrences) from the value set associated with keys which satisfy the specified <code>predicate</code>.
     * @param predicate
     * @param oldValues
     * @param newValue
     * 
     * @return <code>true</code> if this Multimap is modified by this operation, otherwise <code>false</code>.
     */
    public <X extends Exception> boolean replaceAllIf(Try.BiPredicate<? super K, ? super V, X> predicate, Collection<?> oldValues, E newValue) throws X {
        boolean modified = false;

        for (Map.Entry<K, V> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue())) {
                if (entry.getValue().removeAll(oldValues)) {
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
    public <X extends Exception> void replaceAll(Try.BiFunction<? super K, ? super V, ? extends V, X> function) throws X {
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

    public <X extends Exception> Multimap<K, E, V> filterByKey(Try.Predicate<? super K, X> filter) throws X {
        final Multimap<K, E, V> result = new Multimap<>(valueMap.getClass(), concreteValueType);

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey())) {
                result.valueMap.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public <X extends Exception> Multimap<K, E, V> filterByValue(Try.Predicate<? super V, X> filter) throws X {
        final Multimap<K, E, V> result = new Multimap<>(valueMap.getClass(), concreteValueType);

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            if (filter.test(entry.getValue())) {
                result.valueMap.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public <X extends Exception> Multimap<K, E, V> filter(Try.BiPredicate<? super K, ? super V, X> filter) throws X {
        final Multimap<K, E, V> result = new Multimap<>(valueMap.getClass(), concreteValueType);

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey(), entry.getValue())) {
                result.valueMap.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public <X extends Exception> void forEach(Try.BiConsumer<? super K, ? super V, X> action) throws X {
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
    public <R, X extends Exception, X2 extends Exception> R forEach(final R seed, Try.TriFunction<R, ? super K, ? super V, R, X> accumulator,
            final Try.TriPredicate<? super R, ? super K, ? super V, X2> conditionToBreak) throws X, X2 {
        N.requireNonNull(accumulator);
        N.requireNonNull(conditionToBreak);

        R result = seed;

        for (Map.Entry<K, V> entry : valueMap.entrySet()) {
            result = accumulator.apply(result, entry.getKey(), entry.getValue());

            if (conditionToBreak.test(result, entry.getKey(), entry.getValue())) {
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
    public <X extends Exception> V computeIfAbsent(K key, Try.Function<? super K, ? extends V, X> mappingFunction) throws X {
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
    public <X extends Exception> V computeIfPresent(K key, Try.BiFunction<? super K, ? super V, ? extends V, X> remappingFunction) throws X {
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
    public <X extends Exception> V compute(K key, Try.BiFunction<? super K, ? super V, ? extends V, X> remappingFunction) throws X {
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
    public <X extends Exception> V merge(K key, V value, Try.BiFunction<? super V, ? super V, ? extends V, X> remappingFunction) throws X {
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
    public <X extends Exception> V merge(K key, E e, Try.BiFunction<? super V, ? super E, ? extends V, X> remappingFunction) throws X {
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
        return valueMap instanceof IdentityHashMap ? new IdentityHashMap<>(valueMap) : new HashMap<>(valueMap);
    }

    public <M extends Map<K, V>> M toMap(final IntFunction<M> supplier) {
        final M map = supplier.apply(size());
        map.putAll(valueMap);
        return map;
    }

    /**
     * Returns a view of this multimap as a {@code Map} from each distinct key
     * to the nonempty collection of that key's associated values.
     *
     * <p>Changes to the returned map or the collections that serve as its values
     * will update the underlying multimap, and vice versa.
     */
    public Map<K, V> unwrap() {
        return valueMap;
    }

    public Stream<Map.Entry<K, V>> stream() {
        return Stream.of(valueMap.entrySet());
    }

    public EntryStream<K, V> entryStream() {
        return EntryStream.of(valueMap);
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

    public <R, X extends Exception> R apply(Try.Function<? super Multimap<K, E, V>, R, X> func) throws X {
        return func.apply(this);
    }

    public <X extends Exception> void accept(Try.Consumer<? super Multimap<K, E, V>, X> action) throws X {
        action.accept(this);
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
