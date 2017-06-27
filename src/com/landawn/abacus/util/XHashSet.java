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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * It's designed to supported primitive/object array.
 * The elements in the array must not be modified after the array is added into the set.
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public class XHashSet<E> implements Set<E> {
    private final ToIntFunction<? super E> hashFunction;
    private final BiFunction<? super E, ? super E, Boolean> equalsFunction;
    private final Set<Wrapper<E>> set;

    public XHashSet(final ToIntFunction<? super E> hashFunction, final BiFunction<? super E, ? super E, Boolean> equalsFunction) {
        this.hashFunction = hashFunction;
        this.equalsFunction = equalsFunction;
        this.set = new HashSet<>();
    }

    public XHashSet(final ToIntFunction<? super E> hashFunction, final BiFunction<? super E, ? super E, Boolean> equalsFunction, final int initialCapacity) {
        this.hashFunction = hashFunction;
        this.equalsFunction = equalsFunction;
        this.set = new HashSet<>(initialCapacity);
    }

    @SuppressWarnings("rawtypes")
    public XHashSet(final ToIntFunction<? super E> hashFunction, final BiFunction<? super E, ? super E, Boolean> equalsFunction,
            final Class<? extends Set> setType) {
        this.hashFunction = hashFunction;
        this.equalsFunction = equalsFunction;
        this.set = N.newInstance(setType);
    }

    XHashSet(final ToIntFunction<? super E> hashFunction, final BiFunction<? super E, ? super E, Boolean> equalsFunction, Set<Wrapper<E>> set) {
        this.hashFunction = hashFunction;
        this.equalsFunction = equalsFunction;
        this.set = set;
    }

    @Override
    public boolean add(E e) {
        return set.add(Wrapper.of(e, hashFunction, equalsFunction));
    }

    @Override
    public boolean addAll(Collection<? extends E> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        boolean result = false;

        for (E e : c) {
            result |= add(e);
        }

        return result;
    }

    @Override
    public boolean remove(Object o) {
        return set.remove(Wrapper.of((E) o, hashFunction, equalsFunction));
    }

    @Override
    public boolean removeAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        boolean result = false;

        for (Object e : c) {
            result |= remove(e);
        }

        return result;
    }

    @Override
    public boolean retainAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            if (set.isEmpty()) {
                return false;
            } else {
                set.clear();

                return true;
            }
        }

        List<Wrapper<?>> list = new ArrayList<>(c.size());

        for (Object e : c) {
            list.add(Wrapper.of((E) e, hashFunction, equalsFunction));
        }

        return set.retainAll(list);
    }

    @Override
    public boolean contains(Object o) {
        return set.contains(Wrapper.of((E) o, hashFunction, equalsFunction));
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        for (Object e : c) {
            if (contains(e) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public Iterator<E> iterator() {
        return new Itr<E>(set.iterator());
    }

    @Override
    public Object[] toArray() {
        final int size = size();

        if (size == 0) {
            return N.EMPTY_OBJECT_ARRAY;
        }

        final Object[] result = new Object[size];
        int i = 0;

        for (Wrapper<E> e : set) {
            result[i++] = e.value();
        }

        return result;
    }

    @Override
    public <T> T[] toArray(T[] a) {
        final int size = size();

        if (a.length < size) {
            a = N.newArray(a.getClass().getComponentType(), size);
        }

        final Object[] result = a;
        int i = 0;

        for (Wrapper<E> e : set) {
            result[i++] = e.value();
        }

        return a;
    }

    @Override
    public int size() {
        return set.size();
    }

    @Override
    public boolean isEmpty() {
        return set.isEmpty();
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
        return obj == this || (obj instanceof XHashSet && ((XHashSet<E>) obj).set.equals(set));
    }

    @Override
    public String toString() {
        return set.toString();
    }

    static class Itr<T> implements Iterator<T> {
        private final Iterator<Wrapper<T>> it;

        Itr(Iterator<Wrapper<T>> it) {
            this.it = it;
        }

        @Override
        public boolean hasNext() {
            return it.hasNext();
        }

        @Override
        public T next() {
            return it.next().value();
        }

        @Override
        public void remove() {
            it.remove();
        }
    }

}
