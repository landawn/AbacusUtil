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
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.RandomAccess;
import java.util.Set;

import com.landawn.abacus.util.function.IntFunction;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public abstract class AbstractList<C, P, E, A, L extends AbstractList<C, P, E, A, L>> implements RandomAccess, java.io.Serializable {
    private static final long serialVersionUID = 1504784980113045443L;

    static final IntFunction<Multiset<Object>> MULTISET_FACTORY = new IntFunction<Multiset<Object>>() {
        @Override
        public Multiset<Object> apply(int len) {
            return new Multiset<>(N.initHashCapacity(len));
        }
    };

    static final IntFunction<Multimap<Object, Object, List<Object>>> MULTIMAP_FACTORY = new IntFunction<Multimap<Object, Object, List<Object>>>() {
        @Override
        public Multimap<Object, Object, List<Object>> apply(int len) {
            return new Multimap<Object, Object, List<Object>>(N.initHashCapacity(len));
        }
    };

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
     * @param a
     */
    public abstract boolean addAll(A a);

    /**
     * 
     * @param index
     * @param a
     */
    public abstract boolean addAll(int index, A a);

    /**
     * 
     * @param a
     */
    public abstract boolean removeAll(A a);

    public abstract boolean removeIf(P p);

    public abstract void deleteAll(int... indices);

    // public abstract boolean containsAll(L l);

    public abstract boolean containsAll(A a);

    // public abstract boolean containsAny(L l);

    public abstract boolean containsAny(A a);

    // public abstract boolean disjoint(L l);

    public abstract boolean disjoint(A a);

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

    public abstract boolean hasDuplicates();

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

    public abstract void shuffle(final Random rnd);

    public abstract void swap(int i, int j);

    /**
     * 
     * @return a copy of this List
     */
    public abstract L copy();

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public abstract L copy(final int fromIndex, final int toIndex);

    /**
     * 
     * @param from
     * @param to
     * @param step
     * @return
     */
    public abstract L copy(final int from, final int to, final int step);

    /**
     * Returns consecutive sub lists of this list, each of the same size (the final list may be smaller),
     * or an empty List if the specified list is null or empty.
     *
     * @return
     */
    public ExList<L> split(int size) {
        return split(0, size(), size);
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    public abstract ExList<L> split(final int fromIndex, final int toIndex, int size);

    //    public List<L> split(P predicate) {
    //        return split(0, size(), predicate);
    //    }
    //
    //    /**
    //     * Split the List by the specified predicate.
    //     * 
    //     * <pre>
    //     * <code>
    //     * // split the number sequence by window 5.
    //     * final MutableInt border = MutableInt.of(5);
    //     * IntList.of(1, 2, 3, 5, 7, 9, 10, 11, 19).split(e -> {
    //     *     if (e <= border.intValue()) {
    //     *         return true;
    //     *     } else {
    //     *         border.addAndGet(5);
    //     *         return false;
    //     *     }
    //     * }).forEach(N::println);
    //     * </code>
    //     * </pre>
    //     * 
    //     * @param fromIndex
    //     * @param toIndex
    //     * @param predicate
    //     * @return
    //     */
    //    public abstract List<L> split(final int fromIndex, final int toIndex, P predicate);

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
        checkFromToIndex(fromIndex, toIndex);

        final IntFunction<List<E>> supplier = createListSupplier();

        return toList(fromIndex, toIndex, supplier);
    }

    public <R extends List<E>> R toList(final IntFunction<R> supplier) {
        return toList(0, size(), supplier);
    }

    public abstract <R extends List<E>> R toList(final int fromIndex, final int toIndex, final IntFunction<R> supplier);

    public Set<E> toSet() {
        return toSet(0, size());
    }

    public Set<E> toSet(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        final IntFunction<Set<E>> supplier = createSetSupplier();

        return toSet(fromIndex, toIndex, supplier);
    }

    public <R extends Set<E>> R toSet(final IntFunction<R> supplier) {
        return toSet(0, size(), supplier);
    }

    public abstract <R extends Set<E>> R toSet(final int fromIndex, final int toIndex, final IntFunction<R> supplier);

    public Multiset<E> toMultiset() {
        return toMultiset(0, size());
    }

    public Multiset<E> toMultiset(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        final IntFunction<Multiset<E>> supplier = createMultisetSupplier();

        return toMultiset(fromIndex, toIndex, supplier);
    }

    public Multiset<E> toMultiset(final IntFunction<Multiset<E>> supplier) {
        return toMultiset(0, size(), supplier);
    }

    public abstract Multiset<E> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<E>> supplier);

    public void println() {
        N.println(toString());
    }

    protected void checkFromToIndex(final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, size());
    }

    protected <T> IntFunction<List<T>> createListSupplier() {
        return Fn.Factory.ofList();
    }

    protected <T> IntFunction<Set<T>> createSetSupplier() {
        return Fn.Factory.ofSet();
    }

    protected <K, V> IntFunction<Map<K, V>> createMapSupplier() {
        return Fn.Factory.ofMap();
    }

    @SuppressWarnings("rawtypes")
    protected <T> IntFunction<Multiset<T>> createMultisetSupplier() {
        return (IntFunction) MULTISET_FACTORY;
    }

    @SuppressWarnings("rawtypes")
    protected <K, U, V extends Collection<U>> IntFunction<Multimap<K, U, V>> createMultimapFactory() {
        return (IntFunction) MULTIMAP_FACTORY;
    }
}
