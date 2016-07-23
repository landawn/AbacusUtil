
package com.landawn.abacus.util;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

public final class ImmutableMap<K, V> implements Map<K, V> {
    private final Map<K, V> map;

    ImmutableMap(Map<K, V> map) {
        this.map = map;
    }

    public static <K, V> ImmutableMap<K, V> of(Object... a) {
        return new ImmutableMap<K, V>((ImmutableMap<K, V>) N.asUnmodifiableMap(a));
    }

    /**
     * 
     * @param map the elements in this <code>map</code> are shared by the returned ImmutableMap.
     * @return
     */
    public static <K, V> ImmutableMap<K, V> of(Map<K, V> map) {
        return new ImmutableMap<K, V>(N.asUnmodifiableMap(map));
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final V put(K k, V v) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final V remove(Object o) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final void putAll(Map<? extends K, ? extends V> map) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final void clear() {
        throw new UnsupportedOperationException();
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
        return values().contains(value);
    }

    @Override
    public V get(Object key) {
        return map.get(key);
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
    public Set<java.util.Map.Entry<K, V>> entrySet() {
        return map.entrySet();
    }

    @Override
    public int size() {
        return map.size();
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ImmutableMap && ((ImmutableMap<K, V>) obj).map.equals(map);
    }

    @Override
    public int hashCode() {
        return map.hashCode();
    }

    @Override
    public String toString() {
        return map.toString();
    }
}
