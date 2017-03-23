/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
 * ORACLE PROPRIETARY/CONFIDENTIAL. Use is subject to license terms.
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 *
 */

package com.landawn.abacus.util;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * Import the useful default Map methods from JDK 1.8 for programming on JDK 7 and Android.
 *
 */
public final class Maps {
    private Maps() {
        // singleton.
    }

    public static <K, V> NullabLe<V> get(final Map<K, V> map, final Object key) {
        final V val = map.get(key);

        if (val != null || map.containsKey(key)) {
            return NullabLe.of(val);
        } else {
            return NullabLe.empty();
        }
    }

    /**
     * Returns a list of values of the keys which exist in the specified <code>Map</code>.
     * If the key dosn't exist in the <code>Map</code>, No value will be added into the returned list. 
     * 
     * @param map
     * @param keys
     * @return
     */
    public static <K, V> List<V> getIfPresent(final Map<K, V> map, final Collection<?> keys) {
        if (N.isNullOrEmpty(keys)) {
            return new ArrayList<>(0);
        }

        final List<V> result = new ArrayList<>(keys.size());
        V val = null;

        for (Object key : keys) {
            val = map.get(key);

            if (val != null || map.containsKey(key)) {
                result.add(val);
            }
        }

        return result;
    }

    /**
     * Returns the value to which the specified key is mapped, or
     * {@code defaultValue} if this map contains no mapping for the key.
     *
     * @implSpec
     * The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties.
     *
     * @param key the key whose associated value is to be returned
     * @param defaultValue the default mapping of the key
     * @return the value to which the specified key is mapped, or
     * {@code defaultValue} if this map contains no mapping for the key
     * @throws ClassCastException if the key is of an inappropriate type for
     * this map
     * (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws NullPointerException if the specified key is null and this map
     * does not permit null keys
     * (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @since 1.8
     */
    public static <K, V> V getOrDefault(final Map<K, V> map, final Object key, final V defaultValue) {
        final V val = map.get(key);

        if (val != null || map.containsKey(key)) {
            return val;
        } else {
            return defaultValue;
        }
    }

    public static <K, V> List<V> getOrDefault(final Map<K, V> map, final Collection<?> keys, final V defaultValue) {
        if (N.isNullOrEmpty(keys)) {
            return new ArrayList<>(0);
        }

        final List<V> result = new ArrayList<>(keys.size());
        V val = null;

        for (Object key : keys) {
            val = map.get(key);

            if (val != null || map.containsKey(key)) {
                result.add(val);
            } else {
                result.add(defaultValue);
            }
        }

        return result;
    }

    /**
     * If the specified key is not already associated with a value (or is mapped
     * to {@code null}) associates it with the given value and returns
     * {@code null}, else returns the current value.
     *
     * @implSpec
     * The default implementation is equivalent to, for this {@code
     * map}:
     *
     * <pre> {@code
     * V v = map.get(key);
     * if (v == null)
     *     v = map.put(key, value);
     *
     * return v;
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties.
     *
     * @param key key with which the specified value is to be associated
     * @param value value to be associated with the specified key
     * @return the previous value associated with the specified key, or
     *         {@code null} if there was no mapping for the key.
     *         (A {@code null} return can also indicate that the map
     *         previously associated {@code null} with the key,
     *         if the implementation supports null values.)
     * @throws UnsupportedOperationException if the {@code put} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the key or value is of an inappropriate
     *         type for this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws NullPointerException if the specified key or value is null,
     *         and this map does not permit null keys or values
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws IllegalArgumentException if some property of the specified key
     *         or value prevents it from being stored in this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @since 1.8
     */
    public static <K, V> V putIfAbsent(final Map<K, V> map, K key, final V value) {
        V v = map.get(key);

        if (v == null) {
            v = map.put(key, value);
        }

        return v;
    }

    /**
     * Removes the entry for the specified key only if it is currently
     * mapped to the specified value.
     *
     * @implSpec
     * The default implementation is equivalent to, for this {@code map}:
     *
     * <pre> {@code
     * if (map.containsKey(key) && N.equals(map.get(key), value)) {
     *     map.remove(key);
     *     return true;
     * } else
     *     return false;
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties.
     *
     * @param key key with which the specified value is associated
     * @param value value expected to be associated with the specified key
     * @return {@code true} if the value was removed
     * @throws UnsupportedOperationException if the {@code remove} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the key or value is of an inappropriate
     *         type for this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws NullPointerException if the specified key or value is null,
     *         and this map does not permit null keys or values
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @since 1.8
     */
    public static <K, V> boolean remove(final Map<K, V> map, final Object key, final Object value) {
        final Object curValue = map.get(key);

        if (!N.equals(curValue, value) || (curValue == null && !map.containsKey(key))) {
            return false;
        }

        map.remove(key);
        return true;
    }

    public static void removeAll(final Map<?, ?> map, final Collection<?> keys) {
        if (N.isNullOrEmpty(keys)) {
            return;
        }

        for (Object key : keys) {
            map.remove(key);
        }
    }

    /**
     * The the entries from the specified <code>Map</code>
     * 
     * @param map
     * @param entriesToRemove
     */
    public static void removeAll(final Map<?, ?> map, final Map<?, ?> entriesToRemove) {
        if (N.isNullOrEmpty(entriesToRemove)) {
            return;
        }

        for (Map.Entry<?, ?> entry : entriesToRemove.entrySet()) {
            if (N.equals(map.get(entry.getKey()), entry.getValue())) {
                map.remove(entry.getKey());
            }
        }
    }

    /**
     * Check if the specified <code>Map</code> contains the specified <code>Entry</code>
     * 
     * @param map
     * @param entry
     * @return
     */
    public static boolean contains(final Map<?, ?> map, Map.Entry<?, ?> entry) {
        final Object val = map.get(entry.getKey());

        return (val != null && N.equals(val, entry.getValue())) || (entry.getValue() == null && map.containsKey(entry.getKey()));
    }

    public static <K, V> Map<K, V> intersection(final Map<K, V> map, final Map<? extends K, ? extends V> map2) {
        final Map<K, V> result = new LinkedHashMap<>();

        Object val = null;
        for (Map.Entry<K, V> entry : map.entrySet()) {
            val = map2.get(entry.getKey());

            if ((val != null && N.equals(val, entry.getValue())) || (entry.getValue() == null && map.containsKey(entry.getKey()))) {
                result.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public static <K, V> Map<K, V> difference(final Map<K, V> map, final Map<? extends K, ? extends V> map2) {
        final Map<K, V> result = new LinkedHashMap<>();

        Object val = null;
        for (Map.Entry<K, V> entry : map.entrySet()) {
            val = map2.get(entry.getKey());

            if ((val == null && map2.containsKey(entry.getKey()) == false) || N.equals(val, entry.getValue()) == false) {
                result.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    public static <K, V> Map<K, Pair<NullabLe<V>, NullabLe<V>>> symmetricDifference(final Map<K, V> map, final Map<K, V> map2) {
        final Map<K, Pair<NullabLe<V>, NullabLe<V>>> result = new LinkedHashMap<>();

        V val = null;
        for (Map.Entry<K, V> entry : map.entrySet()) {
            val = map2.get(entry.getKey());

            if (val == null && map2.containsKey(entry.getKey()) == false) {
                result.put(entry.getKey(), Pair.of(NullabLe.of(entry.getValue()), NullabLe.<V> empty()));
            } else if (N.equals(val, entry.getValue()) == false) {
                result.put(entry.getKey(), Pair.of(NullabLe.of(entry.getValue()), NullabLe.of(val)));
            }
        }

        for (Map.Entry<K, V> entry : map2.entrySet()) {
            if (map.containsKey(entry.getKey()) == false) {
                result.put(entry.getKey(), Pair.of(NullabLe.<V> empty(), NullabLe.of(entry.getValue())));
            }
        }

        return result;
    }

    /**
     * Replaces the entry for the specified key only if currently
     * mapped to the specified value.
     *
     * @implSpec
     * The default implementation is equivalent to, for this {@code map}:
     *
     * <pre> {@code
     * if (map.containsKey(key) && N.equals(map.get(key), value)) {
     *     map.put(key, newValue);
     *     return true;
     * } else
     *     return false;
     * }</pre>
     *
     * The default implementation does not throw NullPointerException
     * for maps that do not support null values if oldValue is null unless
     * newValue is also null.
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties.
     *
     * @param key key with which the specified value is associated
     * @param oldValue value expected to be associated with the specified key
     * @param newValue value to be associated with the specified key
     * @return {@code true} if the value was replaced
     * @throws UnsupportedOperationException if the {@code put} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the class of a specified key or value
     *         prevents it from being stored in this map
     * @throws NullPointerException if a specified key or newValue is null,
     *         and this map does not permit null keys or values
     * @throws NullPointerException if oldValue is null and this map does not
     *         permit null values
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws IllegalArgumentException if some property of a specified key
     *         or value prevents it from being stored in this map
     * @since 1.8
     */
    public static <K, V> boolean replace(final Map<K, V> map, final K key, final V oldValue, final V newValue) {
        final Object curValue = map.get(key);

        if (!N.equals(curValue, oldValue) || (curValue == null && !map.containsKey(key))) {
            return false;
        }

        map.put(key, newValue);
        return true;
    }

    /**
     * Replaces the entry for the specified key only if it is
     * currently mapped to some value.
     *
     * @implSpec
     * The default implementation is equivalent to, for this {@code map}:
     *
     * <pre> {@code
     * if (map.containsKey(key)) {
     *     return map.put(key, value);
     * } else
     *     return null;
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties.
      *
     * @param key key with which the specified value is associated
     * @param value value to be associated with the specified key
     * @return the previous value associated with the specified key, or
     *         {@code null} if there was no mapping for the key.
     *         (A {@code null} return can also indicate that the map
     *         previously associated {@code null} with the key,
     *         if the implementation supports null values.)
     * @throws UnsupportedOperationException if the {@code put} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the class of the specified key or value
     *         prevents it from being stored in this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws NullPointerException if the specified key or value is null,
     *         and this map does not permit null keys or values
     * @throws IllegalArgumentException if some property of the specified key
     *         or value prevents it from being stored in this map
     * @since 1.8
     */
    public static <K, V> V replace(final Map<K, V> map, final K key, final V value) {
        V curValue = null;

        if (((curValue = map.get(key)) != null) || map.containsKey(key)) {
            curValue = map.put(key, value);
        }

        return curValue;
    }

    /**
     * Replaces each entry's value with the result of invoking the given
     * function on that entry until all entries have been processed or the
     * function throws an exception.  Exceptions thrown by the function are
     * relayed to the caller.
     *
     * @implSpec
     * <p>The default implementation is equivalent to, for this {@code map}:
     * <pre> {@code
     * for (Map.Entry<K, V> entry : map.entrySet())
     *     entry.setValue(function.apply(entry.getKey(), entry.getValue()));
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties.
     *
     * @param function the function to apply to each entry
     * @throws UnsupportedOperationException if the {@code set} operation
     * is not supported by this map's entry set iterator.
     * @throws ClassCastException if the class of a replacement value
     * prevents it from being stored in this map
     * @throws NullPointerException if the specified function is null, or the
     * specified replacement value is null, and this map does not permit null
     * values
     * @throws ClassCastException if a replacement value is of an inappropriate
     *         type for this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws NullPointerException if function or a replacement value is null,
     *         and this map does not permit null keys or values
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws IllegalArgumentException if some property of a replacement value
     *         prevents it from being stored in this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ConcurrentModificationException if an entry is found to be
     * removed during iteration
     * @since 1.8
     */
    public static <K, V> void replaceAll(final Map<K, V> map, final BiFunction<? super K, ? super V, ? extends V> function) {
        N.requireNonNull(function);

        K k = null;
        V v = null;

        for (Map.Entry<K, V> entry : map.entrySet()) {
            try {
                k = entry.getKey();
                v = entry.getValue();
            } catch (IllegalStateException ise) {
                // this usually means the entry is no longer in the map.
                throw new ConcurrentModificationException(ise);
            }

            // ise thrown from function is not a cme.
            v = function.apply(k, v);

            try {
                entry.setValue(v);
            } catch (IllegalStateException ise) {
                // this usually means the entry is no longer in the map.
                throw new ConcurrentModificationException(ise);
            }
        }
    }

    /**
     * Performs the given action for each entry in this map until all entries
     * have been processed or the action throws an exception.   Unless
     * otherwise specified by the implementing class, actions are performed in
     * the order of entry set iteration (if an iteration order is specified.)
     * Exceptions thrown by the action are relayed to the caller.
     *
     * @implSpec
     * The default implementation is equivalent to, for this {@code map}:
     * <pre> {@code
     * for (Map.Entry<K, V> entry : map.entrySet())
     *     action.accept(entry.getKey(), entry.getValue());
     * }</pre>
     *
     * The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties.
     *
     * @param action The action to be performed for each entry
     * @throws NullPointerException if the specified action is null
     * @throws ConcurrentModificationException if an entry is found to be
     * removed during iteration
     * @since 1.8
     */
    public static <K, V> void forEach(final Map<K, V> map, final BiConsumer<? super K, ? super V> action) {
        N.requireNonNull(action);

        K k = null;
        V v = null;

        for (Map.Entry<K, V> entry : map.entrySet()) {
            try {
                k = entry.getKey();
                v = entry.getValue();
            } catch (IllegalStateException ise) {
                // this usually means the entry is no longer in the map.
                throw new ConcurrentModificationException(ise);
            }

            action.accept(k, v);
        }
    }

    /**
     * If the specified key is not already associated with a value (or is mapped
     * to {@code null}), attempts to compute its value using the given mapping
     * function and enters it into this map unless {@code null}.
     *
     * <p>If the function returns {@code null} no mapping is recorded. If
     * the function itself throws an (unchecked) exception, the
     * exception is rethrown, and no mapping is recorded.  The most
     * common usage is to construct a new object serving as an initial
     * mapped value or memoized result, as in:
     *
     * <pre> {@code
     * map.computeIfAbsent(key, k -> new Value(f(k)));
     * }</pre>
     *
     * <p>Or to implement a multi-value map, {@code Map<K,Collection<V>>},
     * supporting multiple values per key:
     *
     * <pre> {@code
     * map.computeIfAbsent(key, k -> new HashSet<V>()).add(v);
     * }</pre>
     *
     *
     * @implSpec
     * The default implementation is equivalent to the following steps for this
     * {@code map}, then returning the current value or {@code null} if now
     * absent:
     *
     * <pre> {@code
     * if (map.get(key) == null) {
     *     V newValue = mappingFunction.apply(key);
     *     if (newValue != null)
     *         map.put(key, newValue);
     * }
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties. In particular, all implementations of
     * subinterface {@link java.util.concurrent.ConcurrentMap} must document
     * whether the function is applied once atomically only if the value is not
     * present.
     *
     * @param key key with which the specified value is to be associated
     * @param mappingFunction the function to compute a value
     * @return the current (existing or computed) value associated with
     *         the specified key, or null if the computed value is null
     * @throws NullPointerException if the specified key is null and
     *         this map does not support null keys, or the mappingFunction
     *         is null
     * @throws UnsupportedOperationException if the {@code put} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the class of the specified key or value
     *         prevents it from being stored in this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @since 1.8
     */
    public static <K, V> V computeIfAbsent(final Map<K, V> map, final K key, final Function<? super K, ? extends V> mappingFunction) {
        N.requireNonNull(mappingFunction);
        V v = null;

        if ((v = map.get(key)) == null) {
            V newValue = null;

            if ((newValue = mappingFunction.apply(key)) != null) {
                map.put(key, newValue);
                return newValue;
            }
        }

        return v;
    }

    /**
     * If the value for the specified key is present and non-null, attempts to
     * compute a new mapping given the key and its current mapped value.
     *
     * <p>If the function returns {@code null}, the mapping is removed.  If the
     * function itself throws an (unchecked) exception, the exception is
     * rethrown, and the current mapping is left unchanged.
    *
     * @implSpec
     * The default implementation is equivalent to performing the following
     * steps for this {@code map}, then returning the current value or
     * {@code null} if now absent:
     *
     * <pre> {@code
     * if (map.get(key) != null) {
     *     V oldValue = map.get(key);
     *     V newValue = remappingFunction.apply(key, oldValue);
     *     if (newValue != null)
     *         map.put(key, newValue);
     *     else
     *         map.remove(key);
     * }
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties. In particular, all implementations of
     * subinterface {@link java.util.concurrent.ConcurrentMap} must document
     * whether the function is applied once atomically only if the value is not
     * present.
     *
     * @param key key with which the specified value is to be associated
     * @param remappingFunction the function to compute a value
     * @return the new value associated with the specified key, or null if none
     * @throws NullPointerException if the specified key is null and
     *         this map does not support null keys, or the
     *         remappingFunction is null
     * @throws UnsupportedOperationException if the {@code put} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the class of the specified key or value
     *         prevents it from being stored in this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @since 1.8
     */
    public static <K, V> V computeIfPresent(final Map<K, V> map, K key, final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
        N.requireNonNull(remappingFunction);

        V oldValue = null;

        if ((oldValue = map.get(key)) != null) {
            V newValue = remappingFunction.apply(key, oldValue);

            if (newValue != null) {
                map.put(key, newValue);
                return newValue;
            } else {
                map.remove(key);
                return null;
            }
        } else {
            return null;
        }
    }

    /**
     * Attempts to compute a mapping for the specified key and its current
     * mapped value (or {@code null} if there is no current mapping). For
     * example, to either create or append a {@code String} msg to a value
     * mapping:
     *
     * <pre> {@code
     * map.compute(key, (k, v) -> (v == null) ? msg : v.concat(msg))}</pre>
     * (Method {@link #merge merge()} is often simpler to use for such purposes.)
     *
     * <p>If the function returns {@code null}, the mapping is removed (or
     * remains absent if initially absent).  If the function itself throws an
     * (unchecked) exception, the exception is rethrown, and the current mapping
     * is left unchanged.
     *
     * @implSpec
     * The default implementation is equivalent to performing the following
     * steps for this {@code map}, then returning the current value or
     * {@code null} if absent:
     *
     * <pre> {@code
     * V oldValue = map.get(key);
     * V newValue = remappingFunction.apply(key, oldValue);
     * if (oldValue != null ) {
     *    if (newValue != null)
     *       map.put(key, newValue);
     *    else
     *       map.remove(key);
     * } else {
     *    if (newValue != null)
     *       map.put(key, newValue);
     *    else
     *       return null;
     * }
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties. In particular, all implementations of
     * subinterface {@link java.util.concurrent.ConcurrentMap} must document
     * whether the function is applied once atomically only if the value is not
     * present.
     *
     * @param key key with which the specified value is to be associated
     * @param remappingFunction the function to compute a value
     * @return the new value associated with the specified key, or null if none
     * @throws NullPointerException if the specified key is null and
     *         this map does not support null keys, or the
     *         remappingFunction is null
     * @throws UnsupportedOperationException if the {@code put} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the class of the specified key or value
     *         prevents it from being stored in this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @since 1.8
     */
    public static <K, V> V compute(final Map<K, V> map, K key, final BiFunction<? super K, ? super V, ? extends V> remappingFunction) {
        N.requireNonNull(remappingFunction);

        V oldValue = map.get(key);
        V newValue = remappingFunction.apply(key, oldValue);

        if (newValue == null) {
            // delete mapping
            if (oldValue != null || map.containsKey(key)) {
                // something to remove
                map.remove(key);
                return null;
            } else {
                // nothing to do. Leave things as they were.
                return null;
            }
        } else {
            // add or replace old mapping
            map.put(key, newValue);
            return newValue;
        }
    }

    /**
     * If the specified key is not already associated with a value or is
     * associated with null, associates it with the given non-null value.
     * Otherwise, replaces the associated value with the results of the given
     * remapping function, or removes if the result is {@code null}. This
     * method may be of use when combining multiple mapped values for a key.
     * For example, to either create or append a {@code String msg} to a
     * value mapping:
     *
     * <pre> {@code
     * map.merge(key, msg, String::concat)
     * }</pre>
     *
     * <p>If the function returns {@code null} the mapping is removed.  If the
     * function itself throws an (unchecked) exception, the exception is
     * rethrown, and the current mapping is left unchanged.
     *
     * @implSpec
     * The default implementation is equivalent to performing the following
     * steps for this {@code map}, then returning the current value or
     * {@code null} if absent:
     *
     * <pre> {@code
     * V oldValue = map.get(key);
     * V newValue = (oldValue == null) ? value :
     *              remappingFunction.apply(oldValue, value);
     * if (newValue == null)
     *     map.remove(key);
     * else
     *     map.put(key, newValue);
     * }</pre>
     *
     * <p>The default implementation makes no guarantees about synchronization
     * or atomicity properties of this method. Any implementation providing
     * atomicity guarantees must override this method and document its
     * concurrency properties. In particular, all implementations of
     * subinterface {@link java.util.concurrent.ConcurrentMap} must document
     * whether the function is applied once atomically only if the value is not
     * present.
     *
     * @param key key with which the resulting value is to be associated
     * @param value the non-null value to be merged with the existing value
     *        associated with the key or, if no existing value or a null value
     *        is associated with the key, to be associated with the key
     * @param remappingFunction the function to recompute a value if present
     * @return the new value associated with the specified key, or null if no
     *         value is associated with the key
     * @throws UnsupportedOperationException if the {@code put} operation
     *         is not supported by this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws ClassCastException if the class of the specified key or value
     *         prevents it from being stored in this map
     *         (<a href="{@docRoot}/java/util/Collection.html#optional-restrictions">optional</a>)
     * @throws NullPointerException if the specified key is null and this map
     *         does not support null keys or the value or remappingFunction is
     *         null
     * @since 1.8
     */
    public static <K, V> V merge(final Map<K, V> map, final K key, final V value, final BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
        N.requireNonNull(remappingFunction);
        N.requireNonNull(value);

        V oldValue = map.get(key);
        V newValue = (oldValue == null) ? value : remappingFunction.apply(oldValue, value);

        if (newValue == null) {
            map.remove(key);
        } else {
            map.put(key, newValue);
        }

        return newValue;
    }

    public static <T> T map2Entity(final Class<T> targetClass, final Map<String, Object> m) {
        return map2Entity(targetClass, m, N.isDirtyMarker(targetClass) == false, true);
    }

    @SuppressWarnings("unchecked")
    public static <T> T map2Entity(final Class<T> targetClass, final Map<String, Object> m, final boolean ignoreNullProperty,
            final boolean ignoreUnknownProperty) {
        checkEntityClass(targetClass);

        final T entity = N.newInstance(targetClass);

        String propName = null;
        Object propValue = null;
        Method propSetMethod = null;
        Class<?> paramClass = null;

        for (Map.Entry<String, Object> entry : m.entrySet()) {
            propName = entry.getKey();
            propValue = entry.getValue();

            if (ignoreNullProperty && (propValue == null)) {
                continue;
            }

            propSetMethod = RefUtil.getPropSetMethod(targetClass, propName);

            if (propSetMethod == null) {
                RefUtil.setPropValue(entity, propName, propValue, ignoreUnknownProperty);
            } else {
                paramClass = propSetMethod.getParameterTypes()[0];

                if (propValue != null && N.typeOf(propValue.getClass()).isMap() && N.isEntity(paramClass)) {
                    RefUtil.setPropValue(entity, propSetMethod,
                            map2Entity(paramClass, (Map<String, Object>) propValue, ignoreNullProperty, ignoreUnknownProperty));
                } else {
                    RefUtil.setPropValue(entity, propSetMethod, propValue);
                }
            }
        }

        return entity;
    }

    public static <T> List<T> map2Entity(final Class<T> targetClass, final Collection<Map<String, Object>> mList) {
        return map2Entity(targetClass, mList, N.isDirtyMarker(targetClass) == false, true);
    }

    public static <T> List<T> map2Entity(final Class<T> targetClass, final Collection<Map<String, Object>> mList, final boolean igoreNullProperty,
            final boolean ignoreUnknownProperty) {
        checkEntityClass(targetClass);

        final List<T> entityList = new ArrayList<>(mList.size());

        for (Map<String, Object> m : mList) {
            entityList.add(map2Entity(targetClass, m, igoreNullProperty, ignoreUnknownProperty));
        }

        return entityList;
    }

    private static <T> void checkEntityClass(final Class<T> cls) {
        if (!N.isEntity(cls)) {
            throw new IllegalArgumentException("No property getter/setter method is found in the specified class: " + RefUtil.getCanonicalClassName(cls));
        }
    }

    public static Map<String, Object> entity2Map(final Object entity) {
        return entity2Map(entity, (entity instanceof DirtyMarker == false));
    }

    public static Map<String, Object> entity2Map(final Object entity, final boolean ignoreNullProperty) {
        return entity2Map(entity, ignoreNullProperty, null);
    }

    public static Map<String, Object> entity2Map(final Object entity, final Collection<String> ignoredPropNames) {
        return entity2Map(entity, (entity instanceof DirtyMarker == false), ignoredPropNames);
    }

    public static Map<String, Object> entity2Map(final Object entity, final boolean ignoreNullProperty, final Collection<String> ignoredPropNames) {
        return entity2Map(entity, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    @SuppressWarnings("deprecation")
    public static Map<String, Object> entity2Map(final Object entity, final boolean ignoreNullProperty, final Collection<String> ignoredPropNames,
            final NamingPolicy keyNamingPolicy) {
        final int initCapacity = (entity instanceof DirtyMarker ? ((DirtyMarker) entity).signedPropNames().size()
                : N.initHashCapacity(RefUtil.getPropGetMethodList(entity.getClass()).size()));
        final Map<String, Object> resultMap = new LinkedHashMap<>(initCapacity);

        entity2Map(resultMap, entity, ignoreNullProperty, ignoredPropNames, keyNamingPolicy);

        return resultMap;
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2Map(final T resultMap, final Object entity) {
        return entity2Map(resultMap, entity, (entity instanceof DirtyMarker == false));
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2Map(final T resultMap, final Object entity, final boolean ignoreNullProperty) {
        return entity2Map(resultMap, entity, ignoreNullProperty, null);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoredPropNames
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2Map(final T resultMap, final Object entity, final Collection<String> ignoredPropNames) {
        return entity2Map(resultMap, entity, (entity instanceof DirtyMarker == false), ignoredPropNames);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @param ignoredPropNames
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2Map(final T resultMap, final Object entity, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames) {
        return entity2Map(resultMap, entity, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @param ignoredPropNames
     * @param keyNamingPolicy
     * @return the input <code>resultMap</code>
     */
    @SuppressWarnings("deprecation")
    public static <T extends Map<String, Object>> T entity2Map(final T resultMap, final Object entity, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames, NamingPolicy keyNamingPolicy) {
        keyNamingPolicy = keyNamingPolicy == null ? NamingPolicy.CAMEL_CASE : keyNamingPolicy;

        final boolean hasIgnoredPropNames = N.notNullOrEmpty(ignoredPropNames);
        Set<String> signedPropNames = null;

        if (entity instanceof DirtyMarker) {
            final Class<?> entityClass = entity.getClass();
            signedPropNames = ((DirtyMarker) entity).signedPropNames();

            if (signedPropNames.size() == 0) {
                // logger.warn("no property is signed in the specified source entity: "
                // + toString(entity));

                return resultMap;
            } else {
                final Set<String> tmp = new HashSet<>(N.initHashCapacity(signedPropNames.size()));

                for (String propName : signedPropNames) {
                    tmp.add(RefUtil.getPropNameByMethod(RefUtil.getPropGetMethod(entityClass, propName)));
                }

                signedPropNames = tmp;
            }
        }

        final Map<String, Method> getterMethodList = RefUtil.checkPropGetMethodList(entity.getClass());
        String propName = null;
        Object propValue = null;

        try {
            switch (keyNamingPolicy) {
                case CAMEL_CASE: {
                    for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                        propName = entry.getKey();

                        if (signedPropNames != null && signedPropNames.contains(propName) == false) {
                            continue;
                        }

                        if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                            continue;
                        }

                        propValue = entry.getValue().invoke(entity);

                        if (ignoreNullProperty && (propValue == null)) {
                            continue;
                        }

                        resultMap.put(propName, propValue);
                    }

                    break;
                }

                case LOWER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                        propName = entry.getKey();

                        if (signedPropNames != null && signedPropNames.contains(propName) == false) {
                            continue;
                        }

                        if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                            continue;
                        }

                        propValue = entry.getValue().invoke(entity);

                        if (ignoreNullProperty && (propValue == null)) {
                            continue;
                        }

                        resultMap.put(RefUtil.toLowerCaseWithUnderscore(propName), propValue);
                    }

                    break;
                }

                case UPPER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                        propName = entry.getKey();

                        if (signedPropNames != null && signedPropNames.contains(propName) == false) {
                            continue;
                        }

                        if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                            continue;
                        }

                        propValue = entry.getValue().invoke(entity);

                        if (ignoreNullProperty && (propValue == null)) {
                            continue;
                        }

                        resultMap.put(RefUtil.toUpperCaseWithUnderscore(propName), propValue);
                    }

                    break;
                }

                default:
                    throw new IllegalArgumentException("Unsupported NamingPolicy: " + keyNamingPolicy);
            }
        } catch (Exception e) {
            throw new AbacusException(e);
        }

        return resultMap;
    }

    public static List<Map<String, Object>> entity2Map(final Collection<?> entityList) {
        return entity2Map(entityList, null);
    }

    public static List<Map<String, Object>> entity2Map(final Collection<?> entityList, final boolean ignoreNullProperty) {
        return entity2Map(entityList, ignoreNullProperty, null);
    }

    public static List<Map<String, Object>> entity2Map(final Collection<?> entityList, final Collection<String> ignoredPropNames) {
        final boolean ignoreNullProperty = N.isNullOrEmpty(entityList) ? true
                : (entityList instanceof ArrayList ? ((ArrayList<?>) entityList).get(0) : entityList.iterator().next()) instanceof DirtyMarker == false;

        return entity2Map(entityList, ignoreNullProperty, ignoredPropNames);
    }

    public static List<Map<String, Object>> entity2Map(final Collection<?> entityList, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames) {
        return entity2Map(entityList, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    public static List<Map<String, Object>> entity2Map(final Collection<?> entityList, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames, final NamingPolicy keyNamingPolicy) {
        final List<Map<String, Object>> resultList = new ArrayList<>(entityList.size());

        for (Object entity : entityList) {
            resultList.add(entity2Map(entity, ignoreNullProperty, ignoredPropNames, keyNamingPolicy));
        }

        return resultList;
    }

    public static Map<String, Object> deepEntity2Map(final Object entity) {
        return deepEntity2Map(entity, (entity instanceof DirtyMarker == false));
    }

    public static Map<String, Object> deepEntity2Map(final Object entity, final boolean ignoreNullProperty) {
        return deepEntity2Map(entity, ignoreNullProperty, null);
    }

    public static Map<String, Object> deepEntity2Map(final Object entity, final Collection<String> ignoredPropNames) {
        return deepEntity2Map(entity, (entity instanceof DirtyMarker == false), ignoredPropNames);
    }

    public static Map<String, Object> deepEntity2Map(final Object entity, final boolean ignoreNullProperty, final Collection<String> ignoredPropNames) {
        return deepEntity2Map(entity, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    @SuppressWarnings("deprecation")
    public static Map<String, Object> deepEntity2Map(final Object entity, final boolean ignoreNullProperty, final Collection<String> ignoredPropNames,
            final NamingPolicy keyNamingPolicy) {
        final int initCapacity = entity instanceof DirtyMarker ? ((DirtyMarker) entity).signedPropNames().size()
                : N.initHashCapacity(RefUtil.getPropGetMethodList(entity.getClass()).size());
        final Map<String, Object> resultMap = new LinkedHashMap<>(initCapacity);

        deepEntity2Map(resultMap, entity, ignoreNullProperty, ignoredPropNames, keyNamingPolicy);

        return resultMap;
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T deepEntity2Map(final T resultMap, final Object entity) {
        return deepEntity2Map(resultMap, entity, (entity instanceof DirtyMarker == false));
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T deepEntity2Map(final T resultMap, final Object entity, final boolean ignoreNullProperty) {
        return deepEntity2Map(resultMap, entity, ignoreNullProperty, null);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoredPropNames
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T deepEntity2Map(final T resultMap, final Object entity, final Collection<String> ignoredPropNames) {
        return deepEntity2Map(resultMap, entity, (entity instanceof DirtyMarker == false), ignoredPropNames);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @param ignoredPropNames
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T deepEntity2Map(final T resultMap, final Object entity, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames) {
        return deepEntity2Map(resultMap, entity, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @param ignoredPropNames
     * @param keyNamingPolicy
     * @return the input <code>resultMap</code>
     */
    @SuppressWarnings("deprecation")
    public static <T extends Map<String, Object>> T deepEntity2Map(final T resultMap, final Object entity, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames, NamingPolicy keyNamingPolicy) {
        keyNamingPolicy = keyNamingPolicy == null ? NamingPolicy.CAMEL_CASE : keyNamingPolicy;
        final boolean hasIgnoredPropNames = N.notNullOrEmpty(ignoredPropNames);

        Set<String> signedPropNames = null;

        if (entity instanceof DirtyMarker) {
            final Class<?> entityClass = entity.getClass();
            signedPropNames = ((DirtyMarker) entity).signedPropNames();

            if (signedPropNames.size() == 0) {
                // logger.warn("no property is signed in the specified source entity: "
                // + toString(entity));

                return resultMap;
            } else {
                final Set<String> tmp = new HashSet<>(N.initHashCapacity(signedPropNames.size()));

                for (String propName : signedPropNames) {
                    tmp.add(RefUtil.getPropNameByMethod(RefUtil.getPropGetMethod(entityClass, propName)));
                }

                signedPropNames = tmp;
            }
        }

        final Map<String, Method> getterMethodList = RefUtil.checkPropGetMethodList(entity.getClass());
        String propName = null;
        Object propValue = null;

        try {
            switch (keyNamingPolicy) {
                case CAMEL_CASE: {
                    for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                        propName = entry.getKey();

                        if (signedPropNames != null && signedPropNames.contains(propName) == false) {
                            continue;
                        }

                        if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                            continue;
                        }

                        propValue = entry.getValue().invoke(entity);

                        if (ignoreNullProperty && (propValue == null)) {
                            continue;
                        }

                        if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                            resultMap.put(propName, propValue);
                        } else {
                            resultMap.put(propName, deepEntity2Map(propValue, ignoreNullProperty, null, keyNamingPolicy));
                        }
                    }

                    break;
                }

                case LOWER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                        propName = entry.getKey();

                        if (signedPropNames != null && signedPropNames.contains(propName) == false) {
                            continue;
                        }

                        if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                            continue;
                        }

                        propValue = entry.getValue().invoke(entity);

                        if (ignoreNullProperty && (propValue == null)) {
                            continue;
                        }

                        if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                            resultMap.put(RefUtil.toLowerCaseWithUnderscore(propName), propValue);
                        } else {
                            resultMap.put(RefUtil.toLowerCaseWithUnderscore(propName), deepEntity2Map(propValue, ignoreNullProperty, null, keyNamingPolicy));
                        }
                    }

                    break;
                }

                case UPPER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                        propName = entry.getKey();

                        if (signedPropNames != null && signedPropNames.contains(propName) == false) {
                            continue;
                        }

                        if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                            continue;
                        }

                        propValue = entry.getValue().invoke(entity);

                        if (ignoreNullProperty && (propValue == null)) {
                            continue;
                        }

                        if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                            resultMap.put(RefUtil.toUpperCaseWithUnderscore(propName), propValue);
                        } else {
                            resultMap.put(RefUtil.toUpperCaseWithUnderscore(propName), deepEntity2Map(propValue, ignoreNullProperty, null, keyNamingPolicy));
                        }
                    }

                    break;
                }

                default:
                    throw new IllegalArgumentException("Unsupported NamingPolicy: " + keyNamingPolicy);
            }
        } catch (Exception e) {
            throw new AbacusException(e);
        }

        return resultMap;
    }

    public static List<Map<String, Object>> deepEntity2Map(final Collection<?> entityList) {
        return deepEntity2Map(entityList, null);
    }

    public static List<Map<String, Object>> deepEntity2Map(final Collection<?> entityList, final boolean ignoreNullProperty) {
        return deepEntity2Map(entityList, ignoreNullProperty, null);
    }

    public static List<Map<String, Object>> deepEntity2Map(final Collection<?> entityList, final Collection<String> ignoredPropNames) {
        final boolean ignoreNullProperty = N.isNullOrEmpty(entityList) ? true
                : (entityList instanceof ArrayList ? ((ArrayList<?>) entityList).get(0) : entityList.iterator().next()) instanceof DirtyMarker == false;

        return deepEntity2Map(entityList, ignoreNullProperty, ignoredPropNames);
    }

    public static List<Map<String, Object>> deepEntity2Map(final Collection<?> entityList, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames) {
        return deepEntity2Map(entityList, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    public static List<Map<String, Object>> deepEntity2Map(final Collection<?> entityList, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames, final NamingPolicy keyNamingPolicy) {
        final List<Map<String, Object>> resultList = new ArrayList<>(entityList.size());

        for (Object entity : entityList) {
            resultList.add(deepEntity2Map(entity, ignoreNullProperty, ignoredPropNames, keyNamingPolicy));
        }

        return resultList;
    }

    public static Map<String, Object> entity2FlatMap(final Object entity) {
        return entity2FlatMap(entity, (entity instanceof DirtyMarker == false));
    }

    public static Map<String, Object> entity2FlatMap(final Object entity, final boolean ignoreNullProperty) {
        return entity2FlatMap(entity, ignoreNullProperty, null);
    }

    public static Map<String, Object> entity2FlatMap(final Object entity, final Collection<String> ignoredPropNames) {
        return entity2FlatMap(entity, (entity instanceof DirtyMarker == false), ignoredPropNames);
    }

    public static Map<String, Object> entity2FlatMap(final Object entity, final boolean ignoreNullProperty, final Collection<String> ignoredPropNames) {
        return entity2FlatMap(entity, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    @SuppressWarnings("deprecation")
    public static Map<String, Object> entity2FlatMap(final Object entity, final boolean ignoreNullProperty, final Collection<String> ignoredPropNames,
            final NamingPolicy keyNamingPolicy) {
        final int initCapacity = entity instanceof DirtyMarker ? ((DirtyMarker) entity).signedPropNames().size()
                : N.initHashCapacity(RefUtil.getPropGetMethodList(entity.getClass()).size());
        final Map<String, Object> resultMap = new LinkedHashMap<>(initCapacity);

        entity2FlatMap(resultMap, entity, ignoreNullProperty, ignoredPropNames, keyNamingPolicy);

        return resultMap;
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2FlatMap(final T resultMap, final Object entity) {
        return entity2FlatMap(resultMap, entity, (entity instanceof DirtyMarker == false));
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2FlatMap(final T resultMap, final Object entity, final boolean ignoreNullProperty) {
        return entity2FlatMap(resultMap, entity, ignoreNullProperty, null);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoredPropNames
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2FlatMap(final T resultMap, final Object entity, final Collection<String> ignoredPropNames) {
        return entity2FlatMap(resultMap, entity, (entity instanceof DirtyMarker == false), ignoredPropNames);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @param ignoredPropNames
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2FlatMap(final T resultMap, final Object entity, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames) {
        return entity2FlatMap(resultMap, entity, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @param ignoredPropNames
     * @param keyNamingPolicy
     * @return the input <code>resultMap</code>
     */
    public static <T extends Map<String, Object>> T entity2FlatMap(final T resultMap, final Object entity, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames, final NamingPolicy keyNamingPolicy) {
        return entity2FlatMap(resultMap, entity, ignoreNullProperty, ignoredPropNames, keyNamingPolicy, null);
    }

    /**
     *
     * @param resultMap
     * @param entity
     * @param ignoreNullProperty
     * @param ignoredPropNames
     * @param keyNamingPolicy
     * @param parentPropName
     * @return the input <code>resultMap</code>
     */
    @SuppressWarnings("deprecation")
    static <T extends Map<String, Object>> T entity2FlatMap(final T resultMap, final Object entity, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames, final NamingPolicy keyNamingPolicy, final String parentPropName) {
        final boolean hasIgnoredPropNames = N.notNullOrEmpty(ignoredPropNames);
        final boolean isNullParentPropName = (parentPropName == null);

        if (entity instanceof DirtyMarker) {
            final Class<?> entityClass = entity.getClass();
            final Set<String> signedPropNames = ((DirtyMarker) entity).signedPropNames();

            if (signedPropNames.size() == 0) {
                // logger.warn("no property is signed in the specified source entity: "
                // + toString(entity));
            } else {
                Method propGetMethod = null;
                Object propValue = null;

                try {
                    switch (keyNamingPolicy) {
                        case CAMEL_CASE: {
                            for (String propName : signedPropNames) {
                                propGetMethod = RefUtil.getPropGetMethod(entityClass, propName);
                                propName = RefUtil.getPropNameByMethod(propGetMethod);

                                if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                                    continue;
                                }

                                propValue = propGetMethod.invoke(entity);

                                if (ignoreNullProperty && (propValue == null)) {
                                    continue;
                                }

                                if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                                    if (isNullParentPropName) {
                                        resultMap.put(propName, propValue);
                                    } else {
                                        resultMap.put(parentPropName + D.PERIOD + propName, propValue);
                                    }
                                } else {
                                    if (isNullParentPropName) {
                                        entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, propName);
                                    } else {
                                        entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, parentPropName + D.PERIOD + propName);
                                    }
                                }
                            }

                            break;
                        }

                        case LOWER_CASE_WITH_UNDERSCORE: {
                            for (String propName : signedPropNames) {
                                propGetMethod = RefUtil.getPropGetMethod(entityClass, propName);
                                propName = RefUtil.getPropNameByMethod(propGetMethod);

                                if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                                    continue;
                                }

                                propName = RefUtil.toLowerCaseWithUnderscore(propName);
                                propValue = propGetMethod.invoke(entity);

                                if (ignoreNullProperty && (propValue == null)) {
                                    continue;
                                }

                                if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                                    if (isNullParentPropName) {
                                        resultMap.put(propName, propValue);
                                    } else {
                                        resultMap.put(parentPropName + D.PERIOD + propName, propValue);
                                    }
                                } else {
                                    if (isNullParentPropName) {
                                        entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, propName);
                                    } else {
                                        entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, parentPropName + D.PERIOD + propName);
                                    }
                                }
                            }

                            break;
                        }

                        case UPPER_CASE_WITH_UNDERSCORE: {
                            for (String propName : signedPropNames) {
                                propGetMethod = RefUtil.getPropGetMethod(entityClass, propName);
                                propName = RefUtil.getPropNameByMethod(propGetMethod);

                                if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                                    continue;
                                }

                                propName = RefUtil.toUpperCaseWithUnderscore(propName);
                                propValue = propGetMethod.invoke(entity);

                                if (ignoreNullProperty && (propValue == null)) {
                                    continue;
                                }

                                if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                                    if (isNullParentPropName) {
                                        resultMap.put(propName, propValue);
                                    } else {
                                        resultMap.put(parentPropName + D.PERIOD + propName, propValue);
                                    }
                                } else {
                                    if (isNullParentPropName) {
                                        entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, propName);
                                    } else {
                                        entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, parentPropName + D.PERIOD + propName);
                                    }
                                }
                            }

                            break;
                        }

                        default:
                            throw new IllegalArgumentException("Unsupported NamingPolicy: " + keyNamingPolicy);
                    }

                } catch (Exception e) {
                    throw new AbacusException(e);
                }
            }
        } else {
            final Map<String, Method> getterMethodList = RefUtil.checkPropGetMethodList(entity.getClass());
            String propName = null;
            Object propValue = null;

            try {
                switch (keyNamingPolicy) {
                    case CAMEL_CASE: {
                        for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                            propName = entry.getKey();

                            if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                                continue;
                            }

                            propValue = entry.getValue().invoke(entity);

                            if (ignoreNullProperty && (propValue == null)) {
                                continue;
                            }

                            if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                                if (isNullParentPropName) {
                                    resultMap.put(propName, propValue);
                                } else {
                                    resultMap.put(parentPropName + D.PERIOD + propName, propValue);
                                }
                            } else {
                                if (isNullParentPropName) {
                                    entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, propName);
                                } else {
                                    entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, parentPropName + D.PERIOD + propName);
                                }
                            }
                        }

                        break;
                    }

                    case LOWER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                            propName = entry.getKey();

                            if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                                continue;
                            }

                            propName = RefUtil.toLowerCaseWithUnderscore(propName);
                            propValue = entry.getValue().invoke(entity);

                            if (ignoreNullProperty && (propValue == null)) {
                                continue;
                            }

                            if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                                if (isNullParentPropName) {
                                    resultMap.put(propName, propValue);
                                } else {
                                    resultMap.put(parentPropName + D.PERIOD + propName, propValue);
                                }
                            } else {
                                if (isNullParentPropName) {
                                    entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, propName);
                                } else {
                                    entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, parentPropName + D.PERIOD + propName);
                                }
                            }
                        }

                        break;
                    }

                    case UPPER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                            propName = entry.getKey();

                            if (hasIgnoredPropNames && ignoredPropNames.contains(propName)) {
                                continue;
                            }

                            propName = RefUtil.toUpperCaseWithUnderscore(propName);
                            propValue = entry.getValue().invoke(entity);

                            if (ignoreNullProperty && (propValue == null)) {
                                continue;
                            }

                            if ((propValue == null) || !N.isEntity(propValue.getClass())) {
                                if (isNullParentPropName) {
                                    resultMap.put(propName, propValue);
                                } else {
                                    resultMap.put(parentPropName + D.PERIOD + propName, propValue);
                                }
                            } else {
                                if (isNullParentPropName) {
                                    entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, propName);
                                } else {
                                    entity2FlatMap(resultMap, propValue, ignoreNullProperty, null, keyNamingPolicy, parentPropName + D.PERIOD + propName);
                                }
                            }
                        }

                        break;
                    }

                    default:
                        throw new IllegalArgumentException("Unsupported NamingPolicy: " + keyNamingPolicy);
                }
            } catch (Exception e) {
                throw new AbacusException(e);
            }
        }

        return resultMap;
    }

    public static List<Map<String, Object>> entity2FlatMap(final Collection<?> entityList) {
        return entity2FlatMap(entityList, null);
    }

    public static List<Map<String, Object>> entity2FlatMap(final Collection<?> entityList, final boolean ignoreNullProperty) {
        return entity2FlatMap(entityList, ignoreNullProperty, null);
    }

    public static List<Map<String, Object>> entity2FlatMap(final Collection<?> entityList, final Collection<String> ignoredPropNames) {
        final boolean ignoreNullProperty = N.isNullOrEmpty(entityList) ? true
                : (entityList instanceof ArrayList ? ((ArrayList<?>) entityList).get(0) : entityList.iterator().next()) instanceof DirtyMarker == false;

        return entity2FlatMap(entityList, ignoreNullProperty, ignoredPropNames);
    }

    public static List<Map<String, Object>> entity2FlatMap(final Collection<?> entityList, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames) {
        return entity2FlatMap(entityList, ignoreNullProperty, ignoredPropNames, NamingPolicy.CAMEL_CASE);
    }

    public static List<Map<String, Object>> entity2FlatMap(final Collection<?> entityList, final boolean ignoreNullProperty,
            final Collection<String> ignoredPropNames, final NamingPolicy keyNamingPolicy) {
        final List<Map<String, Object>> resultList = new ArrayList<>(entityList.size());

        for (Object entity : entityList) {
            resultList.add(entity2FlatMap(entity, ignoreNullProperty, ignoredPropNames, keyNamingPolicy));
        }

        return resultList;
    }

    public static String join(final Map<?, ?> m) {
        return join(m, N.ELEMENT_SEPARATOR);
    }

    public static String join(final Map<?, ?> m, final char entryDelimiter) {
        if (N.isNullOrEmpty(m)) {
            return N.EMPTY_STRING;
        }

        return join(m, 0, m.size(), entryDelimiter);
    }

    public static String join(final Map<?, ?> m, final String entryDelimiter) {
        if (N.isNullOrEmpty(m)) {
            return N.EMPTY_STRING;
        }

        return join(m, 0, m.size(), entryDelimiter);
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final char entryDelimiter) {
        return Maps.join(m, fromIndex, toIndex, entryDelimiter, false);
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final char entryDelimiter, final boolean trim) {
        return Maps.join(m, fromIndex, toIndex, entryDelimiter, D._EQUAL, trim);
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final String entryDelimiter) {
        return Maps.join(m, fromIndex, toIndex, entryDelimiter, false);
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final String entryDelimiter, final boolean trim) {
        return Maps.join(m, fromIndex, toIndex, entryDelimiter, D.EQUAL, trim);
    }

    public static String join(final Map<?, ?> m, final char entryDelimiter, final char keyValueDelimiter) {
        if (N.isNullOrEmpty(m)) {
            return N.EMPTY_STRING;
        }

        return join(m, 0, m.size(), entryDelimiter, keyValueDelimiter);
    }

    public static String join(final Map<?, ?> m, final String entryDelimiter, final String keyValueDelimiter) {
        if (N.isNullOrEmpty(m)) {
            return N.EMPTY_STRING;
        }

        return join(m, 0, m.size(), entryDelimiter, keyValueDelimiter);
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final char entryDelimiter, final char keyValueDelimiter) {
        return Maps.join(m, fromIndex, toIndex, entryDelimiter, keyValueDelimiter, false);
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final char entryDelimiter, final char keyValueDelimiter,
            final boolean trim) {
        N.checkFromToIndex(fromIndex, toIndex, m == null ? 0 : m.size());

        if ((N.isNullOrEmpty(m) && fromIndex == 0 && toIndex == 0) || (fromIndex == toIndex && fromIndex < m.size())) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            int i = 0;
            for (Map.Entry<?, ?> entry : m.entrySet()) {
                if (i++ > fromIndex) {
                    sb.append(entryDelimiter);
                }

                if (i > fromIndex) {
                    sb.append(trim ? N.toString(entry.getKey()).trim() : N.toString(entry.getKey()));
                    sb.append(keyValueDelimiter);
                    sb.append(trim ? N.toString(entry.getValue()).trim() : N.toString(entry.getValue()));
                }

                if (i >= toIndex) {
                    break;
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final String entryDelimiter, final String keyValueDelimiter) {
        return Maps.join(m, fromIndex, toIndex, entryDelimiter, keyValueDelimiter, false);
    }

    public static String join(final Map<?, ?> m, final int fromIndex, final int toIndex, final String entryDelimiter, final String keyValueDelimiter,
            final boolean trim) {
        N.checkFromToIndex(fromIndex, toIndex, m == null ? 0 : m.size());

        if ((N.isNullOrEmpty(m) && fromIndex == 0 && toIndex == 0) || (fromIndex == toIndex && fromIndex < m.size())) {
            return N.EMPTY_STRING;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            int i = 0;
            for (Map.Entry<?, ?> entry : m.entrySet()) {
                if (i++ > fromIndex) {
                    sb.append(entryDelimiter);
                }

                if (i > fromIndex) {
                    sb.append(trim ? N.toString(entry.getKey()).trim() : N.toString(entry.getKey()));
                    sb.append(keyValueDelimiter);
                    sb.append(trim ? N.toString(entry.getValue()).trim() : N.toString(entry.getValue()));
                }

                if (i >= toIndex) {
                    break;
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }
}
