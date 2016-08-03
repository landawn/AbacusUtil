/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public abstract class AbastractArrayList<C, P, E, A, L extends ArrayList<C, P, E, A, L>> implements ArrayList<C, P, E, A, L> {
    /**
     * Default initial capacity.
     */
    static final int DEFAULT_CAPACITY = 10;

    /**
     * The maximum size of array to allocate. Some VMs reserve some header words in an array. Attempts to allocate
     * larger arrays may result in OutOfMemoryError: Requested array size exceeds VM limit
     */
    static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

    static int hugeCapacity(int minCapacity) {
        if (minCapacity < 0) {
            throw new OutOfMemoryError();
        }

        return (minCapacity > MAX_ARRAY_SIZE) ? Integer.MAX_VALUE : MAX_ARRAY_SIZE;
    }

    @Override
    public void forEach(C action) {
        forEach(0, size(), action);
    }

    @Override
    public boolean allMatch(P filter) {
        return allMatch(0, size(), filter);
    }

    @Override
    public boolean anyMatch(P filter) {
        return anyMatch(0, size(), filter);
    }

    @Override
    public boolean noneMatch(P filter) {
        return noneMatch(0, size(), filter);
    }

    @Override
    public int count(P filter) {
        return count(0, size(), filter);
    }

    @Override
    public L filter(P filter) {
        return filter(0, size(), filter);
    }

    @Override
    public List<L> split(int size) {
        return split(0, size(), size);
    }

    @Override
    public L distinct() {
        return distinct(0, size());
    }

    @Override
    public L copy() {
        return copy(0, size());
    }

    @Override
    public List<E> toList() {
        return toList(0, size());
    }

    @Override
    public List<E> toList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        final List<E> result = N.newArrayList(toIndex - fromIndex);

        toList(result, fromIndex, toIndex);

        return result;
    }

    @Override
    public void toList(List<E> list) {
        toList(list, 0, size());
    }

    @Override
    public Set<E> toSet() {
        return toSet(0, size());
    }

    @Override
    public Set<E> toSet(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Set<E> result = new HashSet<E>(N.initHashCapacity(toIndex - fromIndex));

        toSet(result, fromIndex, toIndex);

        return result;
    }

    @Override
    public void toSet(Set<E> set) {
        toSet(set, 0, size());
    }

    @Override
    public Multiset<E> toMultiset() {
        return toMultiset(0, size());
    }

    @Override
    public Multiset<E> toMultiset(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Multiset<E> result = new Multiset<E>(N.initHashCapacity(N.min(64, toIndex - fromIndex)));

        toMultiset(result, fromIndex, toIndex);

        return result;
    }

    @Override
    public void toMultiset(Multiset<E> multiset) {
        toMultiset(multiset, 0, size());
    }

    protected void checkIndex(final int fromIndex, final int toIndex) {
        if (fromIndex < 0) {
            throw new IndexOutOfBoundsException("fromIndex = " + fromIndex);
        }
        if (toIndex > size()) {
            throw new IndexOutOfBoundsException("toIndex = " + toIndex);
        }
        if (fromIndex > toIndex) {
            throw new IllegalArgumentException("fromIndex(" + fromIndex + ") > toIndex(" + toIndex + ")");
        }
    }
}
