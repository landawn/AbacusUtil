/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class Properties<K, V> implements Map<K, V> {
    protected final Map<K, V> values;

    public Properties() {
        this(new HashMap<K, V>());
    }

    /**
     * 
     * @param valueMap The valueMap and this Properties share the same data; any changes to one will appear in the other.
     */
    protected Properties(final Map<? extends K, ? extends V> valueMap) {
        this.values = (Map<K, V>) valueMap;
    }

    public static <K, V, k extends K, v extends V> Properties<K, V> of(final k k1, final v v1) {
        final Map<K, V> m = N.asMap(k1, v1);
        return new Properties<K, V>(m);
    }

    public static <K, V, k extends K, v extends V> Properties<K, V> of(final k k1, final v v1, final k k2, final v v2) {
        final Map<K, V> m = N.asMap(k1, v1, k2, v2);
        return new Properties<K, V>(m);
    }

    public static <K, V, k extends K, v extends V> Properties<K, V> of(final k k1, final v v1, final k k2, final v v2, final k k3, final v v3) {
        final Map<K, V> m = N.asMap(k1, v1, k2, v2, k3, v3);
        return new Properties<K, V>(m);
    }

    public static <K, V> Properties<K, V> from(final Map<? extends K, ? extends V> map) {
        return new Properties<K, V>(new HashMap<K, V>(map));
    }

    @Override
    public V get(Object propName) {
        return values.get(propName);
    }

    /**
     * To avoid <code>NullPointerException</code> for primitive type if the target property is null or not set.
     * 
     * @param targetClass
     * @param propName
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T> T get(Class<T> targetClass, Object propName) {
        return N.as(targetClass, values.get(propName));
    }

    /**
     * 
     * @param propName
     * @param defaultValue
     *            is returned if the specified {@code propName} is not contained in this Properties instance or it's
     *            null.
     * @return
     */
    @SuppressWarnings("unchecked")
    public <T extends V> T get(Object propName, T defaultValue) {
        T result = (T) values.get(propName);

        if (result == null) {
            return defaultValue;
        }

        return result;
    }

    /**
     * 
     * @param targetClass
     * @param propName
     * @param defaultValue
     *            is returned if the specified {@code propName} is not contained in this Properties instance or it's null.
     * @return
     */
    public <T> T get(Class<T> targetClass, Object propName, T defaultValue) {
        Object result = values.get(propName);

        if (result == null) {
            return defaultValue;
        }

        return N.as(targetClass, result);
    }

    /**
     * 
     * @param propName
     * @param propValue
     * @return the same property
     */
    public Properties<K, V> set(K propName, V propValue) {
        put(propName, propValue);

        return this;
    }

    @Override
    public V put(K key, V value) {
        return values.put(key, value);
    }

    @Override
    public void putAll(Map<? extends K, ? extends V> m) {
        values.putAll(m);
    }

    public V putIfAbsent(K key, V value) {
        V v = get(key);
        if (v == null) {
            v = put(key, value);
        }

        return v;
    }

    @Override
    public V remove(Object key) {
        return values.remove(key);
    }

    /**
     * Removes the entry for the specified key only if it is currently
     * mapped to the specified value.
     */
    public boolean remove(Object key, Object value) {
        Object curValue = get(key);
        if (!Objects.equals(curValue, value) || (curValue == null && !containsKey(key))) {
            return false;
        }
        remove(key);
        return true;
    }

    /**
     * Replaces the entry for the specified key only if it is
     * currently mapped to some value.
     */
    public V replace(K key, V value) {
        V curValue;
        if (((curValue = get(key)) != null) || containsKey(key)) {
            curValue = put(key, value);
        }
        return curValue;
    }

    /**
     * Replaces the entry for the specified key only if currently
     * mapped to the specified value.
     */
    public boolean replace(K key, V oldValue, V newValue) {
        Object curValue = get(key);
        if (!Objects.equals(curValue, oldValue) || (curValue == null && !containsKey(key))) {
            return false;
        }
        put(key, newValue);
        return true;
    }

    @Override
    public boolean containsKey(Object key) {
        return values.containsKey(key);
    }

    @Override
    public boolean containsValue(Object value) {
        return values.containsValue(value);
    }

    @Override
    public Set<K> keySet() {
        return values.keySet();
    }

    @Override
    public Collection<V> values() {
        return values.values();
    }

    @Override
    public Set<java.util.Map.Entry<K, V>> entrySet() {
        return values.entrySet();
    }

    @Override
    public boolean isEmpty() {
        return values.isEmpty();
    }

    @Override
    public int size() {
        return values.size();
    }

    @Override
    public void clear() {
        values.clear();
    }

    public Properties<K, V> copy() {
        final Properties<K, V> copy = new Properties<K, V>(N.newInstance(this.values.getClass()));

        copy.values.putAll(this.values);

        return copy;
    }

    @Override
    public int hashCode() {
        return 31 + ((values == null) ? 0 : values.hashCode());
    }

    @Override
    public boolean equals(Object obj) {
        return this == obj || (obj instanceof Properties && N.equals(((Properties<K, V>) obj).values, values));
    }

    @Override
    public String toString() {
        return values.toString();
    }
}
