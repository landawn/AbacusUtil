/*
 * Copyright (C) 2017 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package com.landawn.abacus.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * 
 * @see N#newSetMultimap()
 * @see N#newSetMultimap(Class, Class)
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 */
public final class SetMultimap<K, E> extends Multimap<K, E, Set<E>> {
    SetMultimap() {
        this(HashMap.class, HashSet.class);
    }

    SetMultimap(int initialCapacity) {
        this(new HashMap<K, Set<E>>(initialCapacity), HashSet.class);
    }

    @SuppressWarnings("rawtypes")
    SetMultimap(final Class<? extends Map> mapType, final Class<? extends Set> valueType) {
        super(mapType, valueType);
    }

    @SuppressWarnings("rawtypes")
    SetMultimap(final Map<K, Set<E>> valueMap, final Class<? extends Set> valueType) {
        super(valueMap, valueType);
    }

    public static <K, E> SetMultimap<K, E> of(final K k1, final E v1) {
        final SetMultimap<K, E> map = new SetMultimap<>();

        map.put(k1, v1);

        return map;
    }

    public static <K, E> SetMultimap<K, E> of(final K k1, final E v1, final K k2, final E v2) {
        final SetMultimap<K, E> map = new SetMultimap<>();

        map.put(k1, v1);
        map.put(k2, v2);

        return map;
    }

    public static <K, E> SetMultimap<K, E> of(final K k1, final E v1, final K k2, final E v2, final K k3, final E v3) {
        final SetMultimap<K, E> map = new SetMultimap<>();

        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);

        return map;
    }

    public static <K, E> SetMultimap<K, E> of(final K k1, final E v1, final K k2, final E v2, final K k3, final E v3, final K k4, final E v4) {
        final SetMultimap<K, E> map = new SetMultimap<>();

        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);

        return map;
    }

    public static <K, E> SetMultimap<K, E> of(final K k1, final E v1, final K k2, final E v2, final K k3, final E v3, final K k4, final E v4, final K k5,
            final E v5) {
        final SetMultimap<K, E> map = new SetMultimap<>();

        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);

        return map;
    }

    public static <K, E> SetMultimap<K, E> of(final K k1, final E v1, final K k2, final E v2, final K k3, final E v3, final K k4, final E v4, final K k5,
            final E v5, final K k6, final E v6) {
        final SetMultimap<K, E> map = new SetMultimap<>();

        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);

        return map;
    }

    public static <K, E> SetMultimap<K, E> of(final K k1, final E v1, final K k2, final E v2, final K k3, final E v3, final K k4, final E v4, final K k5,
            final E v5, final K k6, final E v6, final K k7, final E v7) {
        final SetMultimap<K, E> map = new SetMultimap<>();

        map.put(k1, v1);
        map.put(k2, v2);
        map.put(k3, v3);
        map.put(k4, v4);
        map.put(k5, v5);
        map.put(k6, v6);
        map.put(k7, v7);

        return map;
    }

    public static <K, E> SetMultimap<K, E> from(final Map<? extends K, ? extends E> map) {
        final SetMultimap<K, E> multimap = new SetMultimap<>(N.initHashCapacity(map == null ? 0 : map.size()));

        if (N.notNullOrEmpty(map)) {
            multimap.putAll(map);
        }

        return multimap;
    }

    public static <K, E> SetMultimap<K, E> from2(final Map<? extends K, ? extends Collection<? extends E>> map) {
        final SetMultimap<K, E> multimap = new SetMultimap<>(N.initHashCapacity(map == null ? 0 : map.size()));

        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<? extends K, ? extends Collection<? extends E>> entry : map.entrySet()) {
                multimap.putAll(entry.getKey(), entry.getValue());
            }
        }

        return multimap;
    }

    public static <T, K, X extends Exception> SetMultimap<K, T> from(final Collection<? extends T> c,
            final Try.Function<? super T, ? extends K, X> keyExtractor) throws X {
        final SetMultimap<K, T> multimap = N.newSetMultimap(N.initHashCapacity(N.min(9, c == null ? 0 : c.size())));

        if (N.notNullOrEmpty(c)) {
            for (T e : c) {
                multimap.put(keyExtractor.apply(e), e);
            }
        }

        return multimap;
    }

    public static <T, K, E, X extends Exception, X2 extends Exception> SetMultimap<K, E> from(final Collection<? extends T> c,
            final Try.Function<? super T, ? extends K, X> keyExtractor, final Try.Function<? super T, ? extends E, X2> valueExtractor) throws X, X2 {
        final SetMultimap<K, E> multimap = N.newSetMultimap(N.initHashCapacity(N.min(9, c == null ? 0 : c.size())));

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
     * @return
     * @see Multimap#invertFrom(Map, com.landawn.abacus.util.function.Supplier)
     */
    public static <K, E> SetMultimap<E, K> invertFrom(final Map<K, E> map) {
        final SetMultimap<E, K> multimap = new SetMultimap<>();

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
     * @return
     * @see Multimap#flatInvertFrom(Map, com.landawn.abacus.util.function.Supplier)
     */
    public static <K, E> SetMultimap<E, K> flatInvertFrom(final Map<K, ? extends Collection<? extends E>> map) {
        final SetMultimap<E, K> multimap = new SetMultimap<>();

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
     * @param map
     * @return
     */
    public static <K, E, V extends Collection<E>> SetMultimap<E, K> invertFrom(final Multimap<K, E, V> map) {
        final SetMultimap<E, K> multimap = new SetMultimap<>();

        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<K, V> entry : map.entrySet()) {
                final V c = entry.getValue();

                if (N.notNullOrEmpty(c)) {
                    for (E e : c) {
                        multimap.put(e, entry.getKey());
                    }
                }
            }
        }

        return multimap;
    }

    @SuppressWarnings("rawtypes")
    public static <K, E, V extends Set<E>> SetMultimap<K, E> wrap(final Map<K, V> map) {
        Class<? extends Set> valueType = HashSet.class;

        for (V v : map.values()) {
            if (v != null) {
                valueType = v.getClass();
                break;
            }
        }

        return new SetMultimap<K, E>((Map<K, Set<E>>) map, valueType);
    }

    @Override
    public <X extends Exception> SetMultimap<K, E> filterByKey(Try.Predicate<? super K, X> filter) throws X {
        final SetMultimap<K, E> result = new SetMultimap<>(valueMap.getClass(), concreteValueType);

        for (Map.Entry<K, Set<E>> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey())) {
                result.valueMap.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    @Override
    public <X extends Exception> SetMultimap<K, E> filterByValue(Try.Predicate<? super Set<E>, X> filter) throws X {
        final SetMultimap<K, E> result = new SetMultimap<>(valueMap.getClass(), concreteValueType);

        for (Map.Entry<K, Set<E>> entry : valueMap.entrySet()) {
            if (filter.test(entry.getValue())) {
                result.valueMap.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    @Override
    public <X extends Exception> SetMultimap<K, E> filter(Try.BiPredicate<? super K, ? super Set<E>, X> filter) throws X {
        final SetMultimap<K, E> result = new SetMultimap<>(valueMap.getClass(), concreteValueType);

        for (Map.Entry<K, Set<E>> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey(), entry.getValue())) {
                result.valueMap.put(entry.getKey(), entry.getValue());
            }
        }

        return result;
    }

    //    public SetMultimap<E, K> inversed() {
    //        final SetMultimap<E, K> multimap = new SetMultimap<E, K>(valueMap.getClass(), concreteValueType);
    //
    //        if (N.notNullOrEmpty(valueMap)) {
    //            for (Map.Entry<K, ? extends Set<? extends E>> entry : valueMap.entrySet()) {
    //                final Set<? extends E> c = entry.getValue();
    //
    //                if (N.notNullOrEmpty(c)) {
    //                    for (E e : c) {
    //                        multimap.put(e, entry.getKey());
    //                    }
    //                }
    //            }
    //        }
    //
    //        return multimap;
    //    }
}
