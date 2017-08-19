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

import com.landawn.abacus.util.function.Function;

/**
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 * @see N#newSetMultimap()
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

    public static <K, E> SetMultimap<K, E> from(final Collection<? extends E> c, final Function<? super E, ? extends K> keyExtractor) {
        final SetMultimap<K, E> multimap = N.newSetMultimap(N.initHashCapacity(N.min(9, c == null ? 0 : c.size())));

        if (N.notNullOrEmpty(c)) {
            for (E e : c) {
                multimap.put(keyExtractor.apply(e), e);
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
}
