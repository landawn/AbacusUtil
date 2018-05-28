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

package com.landawn.abacus.util;

import java.util.NoSuchElementException;

import com.landawn.abacus.util.function.BooleanSupplier;
import com.landawn.abacus.util.function.LongSupplier;
import com.landawn.abacus.util.stream.LongStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class LongIterator extends ImmutableIterator<Long> {
    public static final LongIterator EMPTY = new LongIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public long nextLong() {
            throw new NoSuchElementException();
        }
    };

    public static LongIterator empty() {
        return EMPTY;
    }

    @SafeVarargs
    public static LongIterator of(final long... a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static LongIterator of(final long[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new LongIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long nextLong() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }

            @Override
            public LongList toList() {
                return LongList.of(N.copyOfRange(a, cursor, toIndex));
            }
        };
    }

    /**
     * Returns an infinite {@code LongIterator}.
     * 
     * @param supplier
     * @return
     */
    public static LongIterator generate(final LongSupplier supplier) {
        N.checkArgNotNull(supplier);

        return new LongIterator() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public long nextLong() {
                return supplier.getAsLong();
            }
        };
    }

    /**
     * 
     * @param hasNext
     * @param supplier
     * @return
     */
    public static LongIterator generate(final BooleanSupplier hasNext, final LongSupplier supplier) {
        N.checkArgNotNull(hasNext);
        N.checkArgNotNull(supplier);

        return new LongIterator() {
            @Override
            public boolean hasNext() {
                return hasNext.getAsBoolean();
            }

            @Override
            public long nextLong() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return supplier.getAsLong();
            }
        };
    }

    /**
     * 
     * @Deprecated use <code>nextLong()</code> instead.
     */
    @Deprecated
    @Override
    public Long next() {
        return nextLong();
    }

    public abstract long nextLong();

    public long[] toArray() {
        return toList().trimToSize().array();
    }

    public LongList toList() {
        final LongList list = new LongList();

        while (hasNext()) {
            list.add(nextLong());
        }

        return list;
    }

    public LongStream stream() {
        return LongStream.of(this);
    }

    public <E extends Exception> void forEachRemaining(Try.LongConsumer<E> action) throws E {
        N.checkArgNotNull(action);

        while (hasNext()) {
            action.accept(nextLong());
        }
    }
}
