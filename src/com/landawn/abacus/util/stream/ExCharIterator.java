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

import java.util.NoSuchElementException;

import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class ExCharIterator extends CharIterator {
    public static final ExCharIterator EMPTY = of(N.EMPTY_CHAR_ARRAY);

    public static ExCharIterator of(final char[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static ExCharIterator of(final char[] a, final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex, a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new ExCharIterator() {
            int cursor = fromIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char nextChar() {
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
            public char[] toArray() {
                return N.copyOfRange(a, cursor, toIndex);
            }
        };
    }

    public long count() {
        long result = 0;

        while (hasNext()) {
            nextChar();
            result++;
        }

        return result;
    }

    public void skip(long n) {
        while (n > 0 && hasNext()) {
            nextChar();
            n--;
        }
    }

    public char[] toArray() {
        final CharList list = new CharList();

        while (hasNext()) {
            list.add(nextChar());
        }

        return list.trimToSize().array();
    }
}
