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

import java.util.Iterator;
import java.util.Set;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ImmutableSet<E> extends ImmutableCollection<E> implements Set<E> {
    private final Set<E> set;

    ImmutableSet(Set<? extends E> list) {
        this.set = (Set<E>) list;
    }

    public static <E> ImmutableSet<E> of(E... a) {
        return new ImmutableSet<E>(N.asLinkedHashSet(a));
    }

    /**
     * 
     * @param set the elements in this <code>Set</code> are shared by the returned ImmutableSet.
     * @return
     */
    public static <E> ImmutableSet<E> of(Set<? extends E> set) {
        return new ImmutableSet<E>(set);
    }

    @Override
    public boolean contains(Object o) {
        return set.contains(o);
    }

    @Override
    public Iterator<E> iterator() {
        return set.iterator();
    }

    @Override
    public int size() {
        return set.size();
    }

    @Override
    public Object[] toArray() {
        return set.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return set.toArray(a);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ImmutableSet && ((ImmutableSet<E>) obj).set.equals(set);
    }

    @Override
    public int hashCode() {
        return set.hashCode();
    }

    @Override
    public String toString() {
        return set.toString();
    }
}
