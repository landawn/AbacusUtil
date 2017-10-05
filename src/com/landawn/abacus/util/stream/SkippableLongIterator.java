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
public abstract class SkippableLongIterator extends LongIterator implements Skippable {
    public static final SkippableLongIterator EMPTY = new SkippableLongIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public long nextLong() {
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
        public long[] toArray() {
            return N.EMPTY_LONG_ARRAY;
        }
    };

    public static SkippableLongIterator empty() {
        return EMPTY;
    }

    public static SkippableLongIterator of(final long[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static SkippableLongIterator of(final long[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new SkippableLongIterator() {
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
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
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

    public static SkippableLongIterator of(final LongIterator iter) {
        if (iter instanceof SkippableLongIterator) {
            return ((SkippableLongIterator) iter);
        }

        return new SkippableLongIterator() {
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

    public static SkippableLongIterator from(final Iterator<Long> iter) {
        if (iter instanceof SkippableObjIterator) {
            final SkippableObjIterator<Long> skippableIterator = ((SkippableObjIterator<Long>) iter);

            return new SkippableLongIterator() {
                @Override
                public boolean hasNext() {
                    return skippableIterator.hasNext();
                }

                @Override
                public long nextLong() {
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
            return new SkippableLongIterator() {
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

    @Override
    public void skip(long n) {
        while (n > 0 && hasNext()) {
            nextLong();
            n--;
        }
    }

    @Override
    public long count() {
        long result = 0;

        while (hasNext()) {
            nextLong();
            result++;
        }

        return result;
    }
}
