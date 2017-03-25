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

import com.landawn.abacus.util.LongIterator;
import com.landawn.abacus.util.LongList;
import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class ExLongIterator extends LongIterator {
    public static final ExLongIterator EMPTY = of(N.EMPTY_LONG_ARRAY);

    public static ExLongIterator of(final long[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static ExLongIterator of(final long[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ExLongIterator() {
            int cursor = fromIndex;

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
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public long[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }
        };
    }

    public static ExLongIterator of(final LongIterator iter) {
        if (iter instanceof ExLongIterator) {
            return ((ExLongIterator) iter);
        }

        return new ExLongIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public long nextLong() {
                return iter.nextLong();
            }
        };
    }

    public static ExLongIterator of(final Iterator<Long> iter) {
        if (iter instanceof ExIterator) {
            final ExIterator<Long> exIterator = ((ExIterator<Long>) iter);

            return new ExLongIterator() {
                @Override
                public boolean hasNext() {
                    return exIterator.hasNext();
                }

                @Override
                public long nextLong() {
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
            return new ExLongIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public long nextLong() {
                    return iter.next();
                }
            };
        }
    }

    public long count() {
        long result = 0;

        while (hasNext()) {
            nextLong();
            result++;
        }

        return result;
    }

    public void skip(long n) {
        while (n > 0 && hasNext()) {
            nextLong();
            n--;
        }
    }

    public long[] toArray() {
        final LongList list = new LongList();

        while (hasNext()) {
            list.add(nextLong());
        }

        return list.trimToSize().array();
    }
}
