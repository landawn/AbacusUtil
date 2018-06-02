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

import java.util.Iterator;
import java.util.NoSuchElementException;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class ShortIteratorEx extends ShortIterator implements IteratorEx<Short> {
    public static final ShortIteratorEx EMPTY = new ShortIteratorEx() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public short nextShort() {
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

        @Override
        public short[] toArray() {
            return N.EMPTY_SHORT_ARRAY;
        }

        @Override
        public void close() {
            // Do nothing.
        }
    };

    public static ShortIteratorEx empty() {
        return EMPTY;
    }

    public static ShortIteratorEx of(final short[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static ShortIteratorEx of(final short[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, N.len(a));

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ShortIteratorEx() {
            private int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short nextShort() {
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
            public short[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }

            @Override
            public ShortList toList() {
                return ShortList.of(N.copyOfRange(a, cursor, toIndex));
            }

            @Override
            public void close() {
                // Do nothing.
            }
        };
    }

    public static ShortIteratorEx of(final ShortIterator iter) {
        if (iter instanceof ShortIteratorEx) {
            return ((ShortIteratorEx) iter);
        }

        return new ShortIteratorEx() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public short nextShort() {
                return iter.nextShort();
            }

            @Override
            public void close() {
                // Do nothing.
            }
        };
    }

    public static ShortIteratorEx from(final Iterator<Short> iter) {
        if (iter instanceof ObjIteratorEx) {
            final ObjIteratorEx<Short> iteratorEx = ((ObjIteratorEx<Short>) iter);

            return new ShortIteratorEx() {
                @Override
                public boolean hasNext() {
                    return iteratorEx.hasNext();
                }

                @Override
                public short nextShort() {
                    return iteratorEx.next();
                }

                @Override
                public void skip(long n) {
                    iteratorEx.skip(n);
                }

                @Override
                public long count() {
                    return iteratorEx.count();
                }

                @Override
                public void close() {
                    iteratorEx.close();
                }
            };
        } else {
            return new ShortIteratorEx() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public short nextShort() {
                    return iter.next();
                }

                @Override
                public void close() {
                    // Do nothing.
                }
            };
        }
    }

    @Override
    public void skip(long n) {
        while (n > 0 && hasNext()) {
            nextShort();
            n--;
        }
    }

    @Override
    public long count() {
        long result = 0;

        while (hasNext()) {
            nextShort();
            result++;
        }

        return result;
    }

    @Override
    public void close() {
        // Do nothing.
    }
}
