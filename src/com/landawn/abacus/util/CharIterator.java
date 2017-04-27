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

package com.landawn.abacus.util;

import java.util.NoSuchElementException;

import com.landawn.abacus.util.function.CharConsumer;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class CharIterator extends ImmutableIterator<Character> {
    public static final CharIterator EMPTY = new CharIterator() {
        @Override
        public boolean hasNext() {
            return false;
        }

        @Override
        public char nextChar() {
            throw new NoSuchElementException();
        }
    };

    public static CharIterator empty() {
        return EMPTY;
    }

    public static CharIterator of(final char[] a) {
        return N.isNullOrEmpty(a) ? EMPTY : of(a, 0, a.length);
    }

    public static CharIterator of(final char[] a, final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, a == null ? 0 : a.length);

        if (fromIndex == toIndex) {
            return EMPTY;
        }

        return new CharIterator() {
            private int cursor = fromIndex;

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
        };
    }

    /**
     * 
     * @Deprecated use <code>nextChar()</code> instead.
     */
    @Deprecated
    @Override
    public Character next() {
        return nextChar();
    }

    public abstract char nextChar();

    public void forEachRemaining(CharConsumer action) {
        N.requireNonNull(action);

        while (hasNext()) {
            action.accept(nextChar());
        }
    }
}
