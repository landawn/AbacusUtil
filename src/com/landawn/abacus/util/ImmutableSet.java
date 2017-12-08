/*
 * Copyright (C) 2016 HaiYang Li
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
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class ImmutableSet<E> extends ImmutableCollection<E> implements Set<E> {

    @SuppressWarnings("rawtypes")
    private static final ImmutableSet EMPTY = new ImmutableSet(Collections.EMPTY_SET);

    ImmutableSet(Set<? extends E> set) {
        super(Collections.unmodifiableSet(set));
    }

    public static <E> ImmutableSet<E> empty() {
        return EMPTY;
    }

    @SafeVarargs
    public static <E> ImmutableSet<E> from(E... a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        return new ImmutableSet<>(N.asLinkedHashSet(a));
    }

    /**
     * 
     * @param set the elements in this <code>Set</code> are shared by the returned ImmutableSet.
     * @return
     */
    public static <E> ImmutableSet<E> of(final Set<? extends E> set) {
        if (set == null) {
            return empty();
        } else if (set instanceof ImmutableSet) {
            return (ImmutableSet<E>) set;
        }

        return new ImmutableSet<>(set);
    }

    /**
     * 
     * @param set
     * @return
     */
    public static <E> ImmutableSet<E> copyOf(final Collection<? extends E> set) {
        if (N.isNullOrEmpty(set)) {
            return empty();
        }

        return new ImmutableSet<>(new LinkedHashSet<>(set));
    }
}
