/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed toIndex in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.Comparator;
import java.util.List;
import java.util.Set;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public interface ArrayList<C, P, E, A, L extends ArrayList<C, P, E, A, L>> {
    /**
     *
     * @return the element array shared with this List.
     */
    A array();

    /**
     * 
     * @param l
     */
    void addAll(L l);

    /**
     * 
     * @param index
     * @param l
     */
    void addAll(int index, L l);

    /**
     * 
     * @param l
     * @return <tt>true</tt> if this list changed as a result of the call
     */
    boolean removeAll(L l);

    /**
     * 
     * @param l
     * @return <tt>true</tt> if this list changed as a result of the call
     */
    boolean retainAll(L l);

    /**
     * 
     * @param l
     * @return
     */
    boolean containsAll(L l);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    L subList(final int fromIndex, final int toIndex);

    /**
     * Performs the given action for each element of the Iterable until all elements have been processed or the action throws an exception.
     * 
     * @param action
     */
    void forEach(C action);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param action
     */
    void forEach(final int fromIndex, final int toIndex, C action);

    /**
     * Returns whether all elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    boolean allMatch(P filter);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    boolean allMatch(final int fromIndex, final int toIndex, P filter);

    /**
     * Returns whether any elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    boolean anyMatch(P filter);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    boolean anyMatch(final int fromIndex, final int toIndex, P filter);

    /**
     * Returns whether no elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    boolean noneMatch(P filter);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    boolean noneMatch(final int fromIndex, final int toIndex, P filter);

    /**
     * 
     * @param filter
     * @return
     */
    int count(P filter);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    int count(final int fromIndex, final int toIndex, P filter);

    /**
     * 
     * @param filter
     * @return a new List with the elements match the provided predicate.
     */
    L filter(P filter);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param filter
     * @return
     */
    L filter(final int fromIndex, final int toIndex, P filter);

    /**
     * Returns consecutive sub lists of this list, each of the same size (the final list may be smaller),
     * or an empty List if the specified list is null or empty.
     *
     * @return
     */
    List<L> split(int size);

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @param size
     * @return
     */
    List<L> split(final int fromIndex, final int toIndex, int size);

    /**
     *
     * @return a new List with distinct elements
     */
    L distinct();

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    L distinct(final int fromIndex, final int toIndex);

    L top(final int top);

    L top(final int fromIndex, final int toIndex, final int top);

    L top(final int top, Comparator<E> cmp);

    L top(final int fromIndex, final int toIndex, final int top, Comparator<E> cmp);

    /**
     *
     * @return this List
     */
    void sort();

    /**
     *
     * @return a copy of this List
     */
    L copy();

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    L copy(final int fromIndex, final int toIndex);

    /**
     * 
     *
     * @return this List with trailing unused space removed.
     */
    L trimToSize();

    void clear();

    boolean isEmpty();

    int size();

    List<E> toList();

    List<E> toList(final int fromIndex, final int toIndex);

    void toList(List<E> list);

    void toList(List<E> list, final int fromIndex, final int toIndex);

    Set<E> toSet();

    Set<E> toSet(final int fromIndex, final int toIndex);

    void toSet(Set<E> set);

    void toSet(Set<E> set, final int fromIndex, final int toIndex);

    Multiset<E> toMultiset();

    void toMultiset(Multiset<E> multiset);

    Multiset<E> toMultiset(final int fromIndex, final int toIndex);

    void toMultiset(Multiset<E> multiset, final int fromIndex, final int toIndex);
}
