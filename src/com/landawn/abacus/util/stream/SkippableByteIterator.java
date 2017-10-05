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
public abstract class SkippableByteIterator extends ByteIterator implements Skippable {
    public static final SkippableByteIterator EMPTY = new SkippableByteIterator() {
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
    };

    public static SkippableByteIterator empty() {
        return EMPTY;
    }

    public static SkippableByteIterator of(final byte[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static SkippableByteIterator of(final byte[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new SkippableByteIterator() {
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
        };
    }

    public static SkippableByteIterator of(final ByteIterator iter) {
        if (iter instanceof SkippableByteIterator) {
            return ((SkippableByteIterator) iter);
        }

        return new SkippableByteIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public byte nextByte() {
                return iter.nextByte();
            }
        };
    }

    public static SkippableByteIterator from(final Iterator<Byte> iter) {
        if (iter instanceof SkippableObjIterator) {
            final SkippableObjIterator<Byte> skippableIterator = ((SkippableObjIterator<Byte>) iter);

            return new SkippableByteIterator() {
                @Override
                public boolean hasNext() {
                    return skippableIterator.hasNext();
                }

                @Override
                public byte nextByte() {
                    return skippableIterator.next();
                }

                @Override
                public void skip(long n) {
                    skippableIterator.skip(n);
                }

                @Override
                public long count() {
                    return skippableIterator.count();
                }
            };
        } else {
            return new SkippableByteIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public byte nextByte() {
                    return iter.next();
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
}
