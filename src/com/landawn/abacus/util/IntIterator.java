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
import com.landawn.abacus.util.function.IntSupplier;
import com.landawn.abacus.util.stream.IntStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class IntIterator extends ImmutableIterator<Integer> {
    public static final IntIterator EMPTY = new IntIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public int nextInt() {
            throw new NoSuchElementException();
        }
    };

    public static IntIterator empty() {
        return EMPTY;
    }

    @SafeVarargs
    public static IntIterator of(final int... a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static IntIterator of(final int[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new IntIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public int nextInt() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public int[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }

            @Override
            public IntList toList() {
                return IntList.of(N.copyOfRange(a, cursor, toIndex));
            }
        };
    }

    /**
     * Returns an infinite {@code IntIterator}.
     * 
     * @param supplier
     * @return
     */
    public static IntIterator generate(final IntSupplier supplier) {
        N.checkArgNotNull(supplier);

        return new IntIterator() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public int nextInt() {
                return supplier.getAsInt();
            }
        };
    }

    /**
     * 
     * @param hasNext
     * @param supplier
     * @return
     */
    public static IntIterator generate(final BooleanSupplier hasNext, final IntSupplier supplier) {
        N.checkArgNotNull(hasNext);
        N.checkArgNotNull(supplier);

        return new IntIterator() {
            @Override
            public boolean hasNext() {
                return hasNext.getAsBoolean();
            }

            @Override
            public int nextInt() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return supplier.getAsInt();
            }
        };
    }

    /**
     * 
     * @Deprecated use <code>nextInt()</code> instead.
     */
    @Deprecated
    @Override
    public Integer next() {
        return nextInt();
    }

    public abstract int nextInt();

    public int[] toArray() {
        return toList().trimToSize().array();
    }

    public IntList toList() {
        final IntList list = new IntList();

        while (hasNext()) {
            list.add(nextInt());
        }

        return list;
    }

    public IntStream stream() {
        return IntStream.of(this);
    }

    @Override
    @Deprecated
    public void forEachRemaining(java.util.function.Consumer<? super Integer> action) {
        super.forEachRemaining(action);
    }

    public <E extends Exception> void forEachRemaining(Try.IntConsumer<E> action) throws E {
        N.checkArgNotNull(action);

        while (hasNext()) {
            action.accept(nextInt());
        }
    }
}
