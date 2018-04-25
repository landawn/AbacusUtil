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
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 */
public abstract class ObjIterator<E> extends ImmutableIterator<E> {
    @SuppressWarnings("rawtypes")
    private static final ObjIterator EMPTY = new ObjIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public Object next() {
            throw new NoSuchElementException();
        }
    };

    public static <T> ObjIterator<T> empty() {
        return EMPTY;
    }

    public static <T> ObjIterator<T> just(final T val) {
        return new ObjIterator<T>() {
            private boolean done = false;

            @Override
            public boolean hasNext() {
                return done == false;
            }

            @Override
            public T next() {
                if (done) {
                    throw new NoSuchElementException();
                }

                done = true;

                return val;
            }
        };
    }

    @SafeVarargs
    public static <T> ObjIterator<T> of(final T... a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static <T> ObjIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ObjIterator<T>() {
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

            @Override
            public Object[] toArray() {
                final Object[] res = new Object[toIndex - cursor];

                N.copy(a, cursor, res, 0, res.length);

                return res;
            }

            @Override
            public <A> A[] toArray(A[] output) {
                if (output.length < toIndex - cursor) {
                    output = N.copyOf(output, toIndex - cursor);
                }

                N.copy(a, cursor, output, 0, toIndex - cursor);

                return output;
            }

            @Override
            public List<T> toList() {
                return N.asList((T[]) toArray());
            }
        };
    }

    public static <T> ObjIterator<T> of(final Iterator<T> iter) {
        if (iter == null) {
            return empty();
        } else if (iter instanceof ObjIterator) {
            return (ObjIterator<T>) iter;
        }

        return new ObjIterator<T>() {
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

    public Object[] toArray() {
        return toArray(N.EMPTY_OBJECT_ARRAY);
    }

    public <A> A[] toArray(A[] a) {
        return toList().toArray(a);
    }

    public List<E> toList() {
        final List<E> list = new ArrayList<>();

        while (hasNext()) {
            list.add(next());
        }

        return list;
    }

    public Stream<E> stream() {
        return Stream.of(this);
    }
}
