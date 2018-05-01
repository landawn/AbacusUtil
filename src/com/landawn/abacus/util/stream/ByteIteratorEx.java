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

import com.landawn.abacus.util.ByteIterator;
import com.landawn.abacus.util.ByteList;
import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class ByteIteratorEx extends ByteIterator implements IteratorEx<Byte> {
    public static final ByteIteratorEx EMPTY = new ByteIteratorEx() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public byte nextByte() {
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
        public byte[] toArray() {
            return N.EMPTY_BYTE_ARRAY;
        }

        @Override
        public void close() {
            // Do nothing.
        }
    };

    public static ByteIteratorEx empty() {
        return EMPTY;
    }

    public static ByteIteratorEx of(final byte[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static ByteIteratorEx of(final byte[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, N.len(a));

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ByteIteratorEx() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public byte nextByte() {
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
            public byte[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }

            @Override
            public ByteList toList() {
                return ByteList.of(N.copyOfRange(a, cursor, toIndex));
            }

            @Override
            public void close() {
                // Do nothing.
            }
        };
    }

    public static ByteIteratorEx of(final ByteIterator iter) {
        if (iter instanceof ByteIteratorEx) {
            return ((ByteIteratorEx) iter);
        }

        return new ByteIteratorEx() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public byte nextByte() {
                return iter.nextByte();
            }

            @Override
            public void close() {
                // Do nothing.
            }
        };
    }

    public static ByteIteratorEx from(final Iterator<Byte> iter) {
        if (iter instanceof ObjIteratorEx) {
            final ObjIteratorEx<Byte> iteratorEx = ((ObjIteratorEx<Byte>) iter);

            return new ByteIteratorEx() {
                @Override
                public boolean hasNext() {
                    return iteratorEx.hasNext();
                }

                @Override
                public byte nextByte() {
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
            return new ByteIteratorEx() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public byte nextByte() {
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
            nextByte();
            n--;
        }
    }

    @Override
    public long count() {
        long result = 0;

        while (hasNext()) {
            nextByte();
            result++;
        }

        return result;
    }

    @Override
    public void close() {
        // Do nothing.
    }
}
