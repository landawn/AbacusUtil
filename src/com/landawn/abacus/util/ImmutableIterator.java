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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Supplier;

/**
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 */
public abstract class ImmutableIterator<E> implements java.util.Iterator<E> {
    @SuppressWarnings("rawtypes")
    public static final ImmutableIterator EMPTY = new ImmutableIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public Object next() {
            throw new NoSuchElementException();
        }
    };

    public static <T> ImmutableIterator<T> empty() {
        return EMPTY;
    }

    public static <T> ImmutableIterator<T> of(final T[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static <T> ImmutableIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ImmutableIterator<T>() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }
        };
    }

    public static <T> ImmutableIterator<T> of(final Iterator<T> iterA) {
        if (iterA == null) {
            return empty();
        } else if (iterA instanceof ImmutableIterator) {
            return (ImmutableIterator<T>) iterA;
        }

        return new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return iterA.hasNext();
            }

            @Override
            public T next() {
                return iterA.next();
            }
        };
    }

    /**
     * @deprecated - UnsupportedOperationException
     */
    @Deprecated
    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    public int size() {
        int cnt = 0;

        while (hasNext()) {
            next();

            cnt++;
        }

        return cnt;
    }

    public List<E> toList() {
        final List<E> list = new ArrayList<>();

        while (hasNext()) {
            list.add(next());
        }

        return list;
    }

    public Set<E> toSet() {
        final Set<E> list = new HashSet<>();

        while (hasNext()) {
            list.add(next());
        }

        return list;
    }

    public <C extends Collection<E>> C toCollection(Supplier<C> collectionFactory) {
        final C c = collectionFactory.get();

        while (hasNext()) {
            c.add(next());
        }

        return c;
    }

    public <K> Map<K, E> toMap(final Function<? super E, K> keyExactor) {
        final Map<K, E> result = new HashMap<>();
        E next = null;

        while (hasNext()) {
            next = next();
            result.put(keyExactor.apply(next), next);
        }

        return result;
    }
}
