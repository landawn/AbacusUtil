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
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class BooleanIterator extends ImmutableIterator<Boolean> {
    public static final BooleanIterator EMPTY = new BooleanIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public boolean nextBoolean() {
            throw new NoSuchElementException();
        }
    };

    public static BooleanIterator empty() {
        return EMPTY;
    }

    public static BooleanIterator of(final boolean[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    /**
     * Returns an infinite {@code BooleanIterator}.
     * 
     * @param supplier
     * @return
     */
    public static BooleanIterator generate(final BooleanSupplier supplier) {
        N.requireNonNull(supplier);

        return new BooleanIterator() {
            @Override
            public boolean hasNext() {
                return true;
            }

            @Override
            public boolean nextBoolean() {
                return supplier.getAsBoolean();
            }
        };
    }

    public static BooleanIterator generate(final BooleanSupplier hasNext, final BooleanSupplier supplier) {
        N.requireNonNull(hasNext);
        N.requireNonNull(supplier);

        return new BooleanIterator() {
            @Override
            public boolean hasNext() {
                return hasNext.getAsBoolean();
            }

            @Override
            public boolean nextBoolean() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return supplier.getAsBoolean();
            }
        };
    }

    public static BooleanIterator of(final boolean[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new BooleanIterator() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public boolean nextBoolean() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public boolean[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }

            @Override
            public BooleanList toList() {
                return BooleanList.of(N.copyOfRange(a, cursor, toIndex));
            }
        };
    }

    /**
     * 
     * @Deprecated use <code>nextBoolean()</code> instead.
     */
    @Deprecated
    @Override
    public Boolean next() {
        return nextBoolean();
    }

    public abstract boolean nextBoolean();

    public boolean[] toArray() {
        return toList().trimToSize().array();
    }

    public BooleanList toList() {
        final BooleanList list = new BooleanList();

        while (hasNext()) {
            list.add(nextBoolean());
        }

        return list;
    }

    public Stream<Boolean> stream() {
        return Stream.of(this);
    }

    public <E extends Exception> void forEachRemaining(Try.BooleanConsumer<E> action) throws E {
        N.requireNonNull(action);

        while (hasNext()) {
            action.accept(nextBoolean());
        }
    }
}
