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

import java.util.List;
import java.util.Set;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public interface PrimitiveList<C, P, E, A, L extends PrimitiveList<C, P, E, A, L>> {
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
    L subList(int fromIndex, int toIndex);

    /**
     * Performs the given action for each element of the Iterable until all elements have been processed or the action throws an exception.
     * 
     * @param action
     */
    void forEach(C action);

    /**
     * Returns whether all elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    boolean allMatch(P filter);

    /**
     * Returns whether any elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    boolean anyMatch(P filter);

    /**
     * Returns whether no elements of this List match the provided predicate.
     * 
     * @param filter
     * @return
     */
    boolean noneMatch(P filter);

    /**
     * 
     * @param filter
     * @return
     */
    int count(P filter);

    /**
     * 
     * @param filter
     * @return a new List with the elements match the provided predicate.
     */
    L filter(P filter);

    /**
     *
     * @return a new List with distinct elements
     */
    L distinct();

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
     * @return this List
     */
    L trimToSize();

    void clear();

    boolean isEmpty();

    int size();

    List<E> toList();

    void toList(List<E> list);

    Set<E> toSet();

    void toSet(Set<E> set);

    Multiset<E> toMultiset();

    void toMultiset(Multiset<E> multiset);
}
