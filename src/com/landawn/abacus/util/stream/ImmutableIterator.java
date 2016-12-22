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

package com.landawn.abacus.util.stream;

import static com.landawn.abacus.util.stream.StreamBase.checkIndex;

import java.util.Collection;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjectList;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class ImmutableIterator<T> implements java.util.Iterator<T> {

    public static <T> ImmutableIterator<T> of(final Iterator<? extends T> iter) {
        return new ImmutableIterator<T>() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public T next() {
                return iter.next();
            }
        };
    }

    public static <T> ImmutableIterator<T> of(final Collection<? extends T> c) {
        final Iterator<? extends T> iter = c.iterator();

        return new QueuedIterator<T>(Integer.MAX_VALUE) {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public T next() {
                return iter.next();
            }
        };
    }

    public static <T> ImmutableIterator<T> of(final T[] a) {
        return of(a, 0, a.length);
    }

    public static <T> ImmutableIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex, a.length);

        return new QueuedIterator<T>(Integer.MAX_VALUE) {
            int cursor = fromIndex;

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

            @Override
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public <A> A[] toArray(A[] b) {
                b = b.length >= toIndex - cursor ? b : (A[]) N.newArray(b.getClass().getComponentType(), toIndex - cursor);

                N.copy(a, cursor, b, 0, toIndex - cursor);

                return b;
            }
        };
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

    public long count() {
        long result = 0;

        while (hasNext()) {
            next();
            result++;
        }

        return result;
    }

    public void skip(long n) {
        while (n > 0 && hasNext()) {
            next();
            n--;
        }
    }

    public <A> A[] toArray(A[] a) {
        final ObjectList<A> list = new ObjectList<>(a);

        while (hasNext()) {
            list.add((A) next());
        }

        return list.array() == a ? a : (A[]) list.trimToSize().array();
    }

    static abstract class QueuedIterator<T> extends ImmutableIterator<T> {
        private final int max;

        QueuedIterator(int max) {
            this.max = max;
        }

        public int max() {
            return max;
        }
    }
}
