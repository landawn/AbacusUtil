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

import java.security.SecureRandom;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

import com.landawn.abacus.util.function.IntFunction;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public abstract class AbstractList<C, P, E, A, L extends AbstractList<C, P, E, A, L>> {
    /**
     * Default initial capacity.
     */
    static final int DEFAULT_CAPACITY = 10;

    /**
     * The maximum size of array to allocate. Some VMs reserve some header words in an array. Attempts to allocate
     * larger arrays may result in OutOfMemoryError: Requested array size exceeds VM limit
     */
    static final int MAX_ARRAY_SIZE = Integer.MAX_VALUE - 8;

    static final Random RAND = new SecureRandom();

    static int hugeCapacity(int minCapacity) {
        if (minCapacity < 0) {
            throw new OutOfMemoryError();
        }

        return (minCapacity > MAX_ARRAY_SIZE) ? Integer.MAX_VALUE : MAX_ARRAY_SIZE;
    }

    /**
     *
     * @return the element array shared with this List.
     */
    public abstract A array();

    /**
     * 
     * @param l
     */
    public abstract void addAll(L l);

    /**
     * 
     * @param index
     * @param l
     */
    public abstract void addAll(int index, L l);

    /**
     * 
     * @param a
     */
    public abstract void addAll(A a);

    /**
     * 
     * @param index
     * @param a
     */
    public abstract void addAll(int index, A a);

    /**
     * 
     * @param a
     */
    public abstract boolean removeAll(A a);

    public abstract void deleteAll(int... indices);

    // public abstract boolean containsAll(L l);

    public abstract boolean containsAll(A a);

    // public abstract boolean joint(L l);

    public abstract boolean joint(A a);

    // public abstract boolean disjoint(L l);

    public abstract boolean disjoint(A a);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public abstract L subList(final int fromIndex, final int toIndex);

    /**
     * Performs the given action for each element of the Iterable until all elements have been processed or the action throws an exception.
     * 
     * @param action
     */
    public void forEach(C action) {
        forEach(0, size(), action);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param action
     */
    public abstract void forEach(final int fromIndex, final int toIndex, C action);

    /**
     * Returns whether all elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    public boolean allMatch(P filter) {
        return allMatch(0, size(), filter);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public abstract boolean allMatch(final int fromIndex, final int toIndex, P filter);

    /**
     * Returns whether any elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    public boolean anyMatch(P filter) {
        return anyMatch(0, size(), filter);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public abstract boolean anyMatch(final int fromIndex, final int toIndex, P filter);

    /**
     * Returns whether no elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    public boolean noneMatch(P filter) {
        return noneMatch(0, size(), filter);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public abstract boolean noneMatch(final int fromIndex, final int toIndex, P filter);

    /**
     * 
     * @param filter
     * @return
     */
    public int count(P filter) {
        return count(0, size(), filter);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public abstract int count(final int fromIndex, final int toIndex, P filter);

    /**
     * 
     * @param filter
     * @return a new List with the elements match the provided predicate.
     */
    public L filter(P filter) {
        return filter(0, size(), filter);
    }

    /**
     * 
     * @param filter
     * @return a new List with the elements match the provided predicate.
     */
    public L filter(P filter, int max) {
        return filter(0, size(), filter, max);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public abstract L filter(final int fromIndex, final int toIndex, P filter);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    public abstract L filter(final int fromIndex, final int toIndex, P filter, int max);

    /**
     *
     * @return a new List with distinct elements
     */
    public L distinct() {
        return distinct(0, size());
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public abstract L distinct(final int fromIndex, final int toIndex);

    /**
     *
     */
    public abstract void sort();

    /**
     * 
     */
    public abstract void reverse();

    /**
     * 
     */
    public abstract void reverse(final int fromIndex, final int toIndex);

    /**
     * 
     * @param distance
     */
    public abstract void rotate(int distance);

    public abstract void shuffle();

    public abstract void swap(int i, int j);

    /**
     *
     * @return a copy of this List
     */
    public L copy() {
        return copy(0, size());
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public abstract L copy(final int fromIndex, final int toIndex);

    /**
     * Returns consecutive sub lists of this list, each of the same size (the final list may be smaller),
     * or an empty List if the specified list is null or empty.
     *
     * @return
     */
    public List<L> split(int size) {
        return split(0, size(), size);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public abstract List<L> split(final int fromIndex, final int toIndex, int size);

    public List<L> split(P predicate) {
        return split(0, size(), predicate);
    }

    /**
     * Split the List by the specified predicate.
     * 
     * <pre>
     * <code>
     * // split the number sequence by window 5.
     * final MutableInt border = MutableInt.of(5);
     * IntList.of(1, 2, 3, 5, 7, 9, 10, 11, 19).split(e -> {
     *     if (e <= border.intValue()) {
     *         return true;
     *     } else {
     *         border.addAndGet(5);
     *         return false;
     *     }
     * }).forEach(N::println);
     * </code>
     * </pre>
     * 
     * @param fromIndex
     * @param toIndex
     * @param predicate
     * @return
     */
    public abstract List<L> split(final int fromIndex, final int toIndex, P predicate);

    public String join() {
        return join(N.ELEMENT_SEPARATOR);
    }

    public String join(final char delimiter) {
        return join(0, size(), delimiter);
    }

    public String join(final String delimiter) {
        return join(0, size(), delimiter);
    }

    public String join(final int fromIndex, final int toIndex) {
        return join(fromIndex, toIndex, N.ELEMENT_SEPARATOR);
    }

    public abstract String join(final int fromIndex, final int toIndex, final char delimiter);

    public abstract String join(final int fromIndex, final int toIndex, final String delimiter);

    /**
     * 
     *
     * @return this List with trailing unused space removed.
     */
    public abstract L trimToSize();

    public abstract void clear();

    public abstract boolean isEmpty();

    public abstract int size();

    public List<E> toList() {
        return toList(0, size());
    }

    public List<E> toList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        final IntFunction<List<E>> supplier = createListSupplier();

        return toList(fromIndex, toIndex, supplier);
    }

    public List<E> toList(final IntFunction<List<E>> supplier) {
        return toList(0, size(), supplier);
    }

    public abstract List<E> toList(final int fromIndex, final int toIndex, final IntFunction<List<E>> supplier);

    public Set<E> toSet() {
        return toSet(0, size());
    }

    public Set<E> toSet(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        final IntFunction<Set<E>> supplier = createSetSupplier();

        return toSet(fromIndex, toIndex, supplier);
    }

    public Set<E> toSet(final IntFunction<Set<E>> supplier) {
        return toSet(0, size(), supplier);
    }

    public abstract Set<E> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<E>> supplier);

    public Multiset<E> toMultiset() {
        return toMultiset(0, size());
    }

    public Multiset<E> toMultiset(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        final IntFunction<Multiset<E>> supplier = createMultisetSupplier();

        return toMultiset(fromIndex, toIndex, supplier);
    }

    public Multiset<E> toMultiset(final IntFunction<Multiset<E>> supplier) {
        return toMultiset(0, size(), supplier);
    }

    public abstract Multiset<E> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<E>> supplier);

    protected void checkIndex(final int fromIndex, final int toIndex) {
        N.checkIndex(fromIndex, toIndex, size());
    }

    protected <T> IntFunction<List<T>> createListSupplier() {
        return new IntFunction<List<T>>() {
            @Override
            public List<T> apply(int len) {
                return new java.util.ArrayList<T>(len);
            }
        };
    }

    protected <T> IntFunction<Set<T>> createSetSupplier() {
        return new IntFunction<Set<T>>() {
            @Override
            public Set<T> apply(int len) {
                return new HashSet<T>(N.initHashCapacity(len));
            }
        };
    }

    protected <T> IntFunction<Multiset<T>> createMultisetSupplier() {
        return new IntFunction<Multiset<T>>() {
            @Override
            public Multiset<T> apply(int len) {
                return new Multiset<T>(N.initHashCapacity(len));
            }
        };
    }

    protected <K, V> IntFunction<Map<K, V>> createMapSupplier() {
        return new IntFunction<Map<K, V>>() {
            @Override
            public Map<K, V> apply(int len) {
                return new HashMap<K, V>(N.initHashCapacity(len));
            }
        };
    }

    protected <K, U, V extends Collection<U>> IntFunction<Multimap<K, U, V>> createMultimapSupplier() {
        return new IntFunction<Multimap<K, U, V>>() {
            @Override
            public Multimap<K, U, V> apply(int len) {
                return new Multimap<K, U, V>(N.initHashCapacity(len));
            }
        };
    }
}
