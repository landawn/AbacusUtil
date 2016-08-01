/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class BigIntegerList extends ObjectList<BigInteger> {
    private static final BigInteger[] EMPTY_BIG_INTEGER = new BigInteger[0];

    public BigIntegerList() {
        this(EMPTY_BIG_INTEGER);
    }

    public BigIntegerList(int initialCapacity) {
        this(new BigInteger[initialCapacity]);
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public BigIntegerList(BigInteger[] a) {
        super(a);
    }

    public BigIntegerList(BigInteger[] a, int size) {
        super(a, size);
    }

    public static BigIntegerList of(BigInteger[] a) {
        return new BigIntegerList(a);
    }

    public static BigIntegerList of(BigInteger[] a, int size) {
        return new BigIntegerList(a, size);
    }

    public static BigIntegerList of(String[] a) {
        return of(a, 0, a.length);
    }

    public static BigIntegerList of(String[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final BigInteger[] elementData = new BigInteger[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = a[i] == null ? null : new BigInteger(a[i]);
        }

        return of(elementData);
    }

    public static BigIntegerList of(List<String> c) {
        return of(c, null);
    }

    public static BigIntegerList of(List<String> c, BigInteger defaultValueForNull) {
        final BigInteger[] a = new BigInteger[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : new BigInteger(e);
        }

        return of(a);
    }

    public static BigIntegerList of(Collection<BigInteger> c) {
        return of(c, null);
    }

    public static BigIntegerList of(Collection<BigInteger> c, BigInteger defaultValueForNull) {
        final BigInteger[] a = new BigInteger[c.size()];
        int idx = 0;

        for (BigInteger e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }
}
