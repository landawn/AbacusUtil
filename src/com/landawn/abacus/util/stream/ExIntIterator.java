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

import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.IntList;
import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class ExIntIterator extends IntIterator {
    public static final ExIntIterator EMPTY = new ExIntIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public int nextInt() {
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
        public int[] toArray() {
            return N.EMPTY_INT_ARRAY;
        }
    };

    public static ExIntIterator empty() {
        return EMPTY;
    }

    public static ExIntIterator of(final int[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static ExIntIterator of(final int[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ExIntIterator() {
            int cursor = fromIndex;

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
            public long count() {
                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public int[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }
        };
    }

    public static ExIntIterator of(final IntIterator iter) {
        if (iter instanceof ExIntIterator) {
            return ((ExIntIterator) iter);
        }

        return new ExIntIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public int nextInt() {
                return iter.nextInt();
            }
        };
    }

    public static ExIntIterator of(final Iterator<Integer> iter) {
        if (iter instanceof ExIterator) {
            final ExIterator<Integer> exIterator = ((ExIterator<Integer>) iter);

            return new ExIntIterator() {
                @Override
                public boolean hasNext() {
                    return exIterator.hasNext();
                }

                @Override
                public int nextInt() {
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
            return new ExIntIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public int nextInt() {
                    return iter.next();
                }
            };
        }
    }

    public long count() {
        long result = 0;

        while (hasNext()) {
            nextInt();
            result++;
        }

        return result;
    }

    public void skip(long n) {
        while (n > 0 && hasNext()) {
            nextInt();
            n--;
        }
    }

    public int[] toArray() {
        final IntList list = new IntList();

        while (hasNext()) {
            list.add(nextInt());
        }

        return list.trimToSize().array();
    }
}
