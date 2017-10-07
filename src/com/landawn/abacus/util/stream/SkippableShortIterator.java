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

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ShortIterator;
import com.landawn.abacus.util.ShortList;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class SkippableShortIterator extends ShortIterator implements SkippableIterator<Short> {
    public static final SkippableShortIterator EMPTY = new SkippableShortIterator() {
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
    };

    public static SkippableShortIterator empty() {
        return EMPTY;
    }

    public static SkippableShortIterator of(final short[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static SkippableShortIterator of(final short[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new SkippableShortIterator() {
            int cursor = fromIndex;

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
        };
    }

    public static SkippableShortIterator of(final ShortIterator iter) {
        if (iter instanceof SkippableShortIterator) {
            return ((SkippableShortIterator) iter);
        }

        return new SkippableShortIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public short nextShort() {
                return iter.nextShort();
            }
        };
    }

    public static SkippableShortIterator from(final Iterator<Short> iter) {
        if (iter instanceof SkippableObjIterator) {
            final SkippableObjIterator<Short> skippableIterator = ((SkippableObjIterator<Short>) iter);

            return new SkippableShortIterator() {
                @Override
                public boolean hasNext() {
                    return skippableIterator.hasNext();
                }

                @Override
                public short nextShort() {
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
            return new SkippableShortIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public short nextShort() {
                    return iter.next();
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
}
