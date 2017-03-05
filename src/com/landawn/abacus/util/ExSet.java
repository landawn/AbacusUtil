/*
 * Copyright (c) 2017, Haiyang Li.
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

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * 
 * @since 0.9.55
 * 
 * @author Haiyang Li
 */
public interface ExSet<T> extends Set<T>, ExCollection<T> {

    static <T> ExSet<T> newHashSet() {
        return of(new HashSet<T>());
    }

    static <T> ExSet<T> newHashSet(int initialCapacity) {
        return of(new HashSet<T>(initialCapacity));
    }

    static <T> ExSet<T> newLinkedHashSet() {
        return of(new LinkedHashSet<T>());
    }

    static <T> ExSet<T> newLinkedHashSet(int initialCapacity) {
        return of(new LinkedHashSet<T>(initialCapacity));
    }

    static <T> ExSet<T> of(final T... a) {
        final Set<T> set = N.asSet(a);
        return ExSet.of(set);
    }

    static <T> ExSet<T> of(final Set<? extends T> c) {
        return new ExSet<T>() {
            private final Set<T> set = (Set<T>) c;

            @Override
            public int size() {
                return set.size();
            }

            @Override
            public boolean isEmpty() {
                return set.isEmpty();
            }

            @Override
            public boolean contains(Object o) {
                return set.contains(o);
            }

            @Override
            public Iterator<T> iterator() {
                return set.iterator();
            }

            @Override
            public Object[] toArray() {
                return set.toArray();
            }

            @Override
            public <A> A[] toArray(A[] a) {
                return set.toArray(a);
            }

            @Override
            public boolean add(T e) {
                return set.add(e);
            }

            @Override
            public boolean remove(Object o) {
                return set.remove(o);
            }

            @Override
            public boolean containsAll(Collection<?> c) {
                return set.containsAll(c);
            }

            @Override
            public boolean addAll(Collection<? extends T> c) {
                return set.addAll(c);
            }

            @Override
            public boolean retainAll(Collection<?> c) {
                return set.retainAll(c);
            }

            @Override
            public boolean removeAll(Collection<?> c) {
                return set.removeAll(c);
            }

            @Override
            public void clear() {
                set.clear();
            }

            @Override
            public int hashCode() {
                return set.hashCode();
            }

            @Override
            public boolean equals(Object obj) {
                return set.equals(obj);
            }

            @Override
            public String toString() {
                return set.toString();
            }
        };
    }

    static <T> ExSet<T> from(final Collection<? extends T> c) {
        return of(new HashSet<T>(c));
    }
}
