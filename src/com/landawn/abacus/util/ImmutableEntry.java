package com.landawn.abacus.util;

import java.util.Map;

public interface ImmutableEntry<K, V> extends Map.Entry<K, V> {

    public static <K, V> ImmutableEntry<K, V> of(K key, V value) {
        return Tuple.of(key, value);
    }

    /**
     * Should always throw UnsupportedOperationException.
     * 
     * @deprecated
     * @throws UnsupportedOperationException
     */
    @Deprecated
    @Override
    V setValue(V v);
}
