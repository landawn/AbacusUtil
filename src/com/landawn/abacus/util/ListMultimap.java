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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.landawn.abacus.util.function.Function;

/**
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 * @see N#newListMultimap()
 */
public final class ListMultimap<K, E> extends Multimap<K, E, List<E>> {
    ListMultimap() {
        this(HashMap.class, ArrayList.class);
    }

    ListMultimap(int initialCapacity) {
        this(new HashMap<K, List<E>>(initialCapacity), ArrayList.class);
    }

    @SuppressWarnings("rawtypes")
    ListMultimap(final Class<? extends Map> mapType, final Class<? extends List> valueType) {
        super(mapType, valueType);
    }

    @SuppressWarnings("rawtypes")
    ListMultimap(final Map<K, List<E>> valueMap, final Class<? extends List> valueType) {
        super(valueMap, valueType);
    }

    public static <K, E> ListMultimap<K, E> from(final Map<? extends K, ? extends E> map) {
        final ListMultimap<K, E> multimap = new ListMultimap<>(N.initHashCapacity(map == null ? 0 : map.size()));

        if (N.notNullOrEmpty(map)) {
            multimap.putAll(map);
        }

        return multimap;
    }

    public static <K, E> ListMultimap<K, E> from2(final Map<? extends K, ? extends Collection<? extends E>> map) {
        final ListMultimap<K, E> multimap = new ListMultimap<>(N.initHashCapacity(map == null ? 0 : map.size()));

        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<? extends K, ? extends Collection<? extends E>> entry : map.entrySet()) {
                multimap.putAll(entry.getKey(), entry.getValue());
            }
        }

        return multimap;
    }

    public static <K, E> ListMultimap<K, E> from(final Collection<? extends E> c, final Function<? super E, ? extends K> keyExtractor) {
        final ListMultimap<K, E> multimap = N.newListMultimap(N.initHashCapacity(N.min(9, c == null ? 0 : c.size())));

        if (N.notNullOrEmpty(c)) {
            for (E e : c) {
                multimap.put(keyExtractor.apply(e), e);
            }
        }

        return multimap;
    }

    @SuppressWarnings("rawtypes")
    public static <K, E, V extends List<E>> ListMultimap<K, E> wrap(final Map<K, V> map) {
        Class<? extends List> valueType = ArrayList.class;

        for (V v : map.values()) {
            if (v != null) {
                valueType = v.getClass();
                break;
            }
        }

        return new ListMultimap<K, E>((Map<K, List<E>>) map, valueType);
    }
}
