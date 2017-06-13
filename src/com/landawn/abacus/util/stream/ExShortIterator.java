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
public abstract class ExShortIterator extends ShortIterator implements SkippableIterator {
    public static final ExShortIterator EMPTY = new ExShortIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public short nextShort() {
            throw new NoSuchElementException();
        }

        @Override
        public long count() {
            return 0;
        }

        @Override
        public void skip(long n) {
            // Do nothing.
        }

        @Override
        public short[] toArray() {
            return N.EMPTY_SHORT_ARRAY;
        }
    };

    public static ExShortIterator empty() {
        return EMPTY;
    }

    public static ExShortIterator of(final short[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static ExShortIterator of(final short[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ExShortIterator() {
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
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public short[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }
        };
    }

    public static ExShortIterator of(final ShortIterator iter) {
        if (iter instanceof ExShortIterator) {
            return ((ExShortIterator) iter);
        }

        return new ExShortIterator() {
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

    public static ExShortIterator of(final Iterator<Short> iter) {
        if (iter instanceof ExIterator) {
            final ExIterator<Short> exIterator = ((ExIterator<Short>) iter);

            return new ExShortIterator() {
                @Override
                public boolean hasNext() {
                    return exIterator.hasNext();
                }

                @Override
                public short nextShort() {
                    return exIterator.next();
                }

                @Override
                public long count() {
                    return exIterator.count();
                }

                @Override
                public void skip(long n) {
                    exIterator.skip(n);
                }
            };
        } else {
            return new ExShortIterator() {
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
    public long count() {
        long result = 0;

        while (hasNext()) {
            nextShort();
            result++;
        }

        return result;
    }

    @Override
    public void skip(long n) {
        while (n > 0 && hasNext()) {
            nextShort();
            n--;
        }
    }

    public short[] toArray() {
        final ShortList list = new ShortList();

        while (hasNext()) {
            list.add(nextShort());
        }

        return list.trimToSize().array();
    }
}
