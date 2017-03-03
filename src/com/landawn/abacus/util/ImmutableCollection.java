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

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.Iterator;
import java.util.function.Predicate;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
abstract class ImmutableCollection<E> extends AbstractCollection<E> {
    final Collection<E> c;

    protected ImmutableCollection(Collection<? extends E> c) {
        this.c = (Collection<E>) c;
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final boolean add(E e) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final boolean addAll(Collection<? extends E> newElements) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final boolean remove(Object object) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public boolean removeIf(Predicate<? super E> filter) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final boolean removeAll(Collection<?> oldElements) {
        throw new UnsupportedOperationException();
    }

    /**
     * @deprecated Unsupported operation.
     */
    @Deprecated
    @Override
    public final boolean retainAll(Collection<?> elementsToKeep) {
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
    public boolean contains(Object o) {
        return c.contains(o);
    }

    @Override
    public Iterator<E> iterator() {
        return c.iterator();
    }

    @Override
    public int size() {
        return c.size();
    }

    @Override
    public Object[] toArray() {
        return c.toArray();
    }

    @Override
    public <T> T[] toArray(T[] a) {
        return c.toArray(a);
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof ImmutableCollection && ((ImmutableCollection<E>) obj).c.equals(c);
    }

    @Override
    public int hashCode() {
        return c.hashCode();
    }

    @Override
    public String toString() {
        return c.toString();
    }
}
