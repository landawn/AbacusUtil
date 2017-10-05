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

import com.landawn.abacus.util.FloatIterator;
import com.landawn.abacus.util.FloatList;
import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class SkippableFloatIterator extends FloatIterator implements Skippable {
    public static final SkippableFloatIterator EMPTY = new SkippableFloatIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public float nextFloat() {
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
        public float[] toArray() {
            return N.EMPTY_FLOAT_ARRAY;
        }
    };

    public static SkippableFloatIterator empty() {
        return EMPTY;
    }

    public static SkippableFloatIterator of(final float[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static SkippableFloatIterator of(final float[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new SkippableFloatIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float nextFloat() {
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
            public float[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }

            @Override
            public FloatList toList() {
                return FloatList.of(N.copyOfRange(a, cursor, toIndex));
            }
        };
    }

    public static SkippableFloatIterator of(final FloatIterator iter) {
        if (iter instanceof SkippableFloatIterator) {
            return ((SkippableFloatIterator) iter);
        }

        return new SkippableFloatIterator() {
            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public float nextFloat() {
                return iter.nextFloat();
            }
        };
    }

    public static SkippableFloatIterator from(final Iterator<Float> iter) {
        if (iter instanceof SkippableObjIterator) {
            final SkippableObjIterator<Float> skippableIterator = ((SkippableObjIterator<Float>) iter);

            return new SkippableFloatIterator() {
                @Override
                public boolean hasNext() {
                    return skippableIterator.hasNext();
                }

                @Override
                public float nextFloat() {
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
            return new SkippableFloatIterator() {
                @Override
                public boolean hasNext() {
                    return iter.hasNext();
                }

                @Override
                public float nextFloat() {
                    return iter.next();
                }
            };
        }
    }

    @Override
    public void skip(long n) {
        while (n > 0 && hasNext()) {
            nextFloat();
            n--;
        }
    }

    @Override
    public long count() {
        long result = 0;

        while (hasNext()) {
            nextFloat();
            result++;
        }

        return result;
    }
}
