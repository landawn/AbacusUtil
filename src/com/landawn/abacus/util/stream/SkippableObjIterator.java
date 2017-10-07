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

import static com.landawn.abacus.util.stream.StreamBase.checkFromToIndex;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ObjIterator;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class SkippableObjIterator<T> extends ObjIterator<T> implements SkippableIterator<T> {
    @SuppressWarnings("rawtypes")
    public static final SkippableObjIterator EMPTY = new QueuedIterator(0) {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public Object next() {
            throw new NoSuchElementException();
        }

        @Override
        public void skip(long n) {
            // Do nothing.
        }

        @Override
        public long count() {
            return 0;
        }
    };

    public static <T> SkippableObjIterator<T> empty() {
        return EMPTY;
    }

    public static <T> SkippableObjIterator<T> of(final T[] a) {
        return a == null ? EMPTY : of(a, 0, a.length);
    }

    public static <T> SkippableObjIterator<T> of(final T[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new QueuedIterator<T>(toIndex - fromIndex) {
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
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
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

    public static <T> SkippableObjIterator<T> of(final Collection<? extends T> c) {
        final Iterator<? extends T> iter = c.iterator();

        return new QueuedIterator<T>(c.size()) {
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

    public static <T> SkippableObjIterator<T> from(final Iterator<? extends T> iter) {
        if (iter instanceof SkippableObjIterator) {
            return ((SkippableObjIterator<T>) iter);
        }

        return new SkippableObjIterator<T>() {
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

    @Override
    public void skip(long n) {
        while (n > 0 && hasNext()) {
            next();
            n--;
        }
    }

    @Override
    public long count() {
        long result = 0;

        while (hasNext()) {
            next();
            result++;
        }

        return result;
    }

    static abstract class QueuedIterator<T> extends SkippableObjIterator<T> {
        private final int max;

        QueuedIterator(int max) {
            this.max = max;
        }

        public int max() {
            return max;
        }
    }
}
