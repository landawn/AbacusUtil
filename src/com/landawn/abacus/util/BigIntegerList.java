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
public abstract class BigIntegerList extends ObjectList<BigInteger> {
    private static final BigInteger[] EMPTY_BIG_INTEGER_ARRAY = new BigInteger[0];

    private BigIntegerList() {
        super(new BigInteger[0]);
        // utility class
    }

    public static ObjectList<BigInteger> empty() {
        return new ObjectList<BigInteger>(EMPTY_BIG_INTEGER_ARRAY);
    }

    public static ObjectList<BigInteger> of(BigInteger... a) {
        return new ObjectList<BigInteger>(a);
    }

    public static ObjectList<BigInteger> of(BigInteger[] a, int size) {
        return new ObjectList<BigInteger>(a, size);
    }

    public static ObjectList<BigInteger> from(String... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<BigInteger> from(String[] a, int startIndex, int endIndex) {
        if (startIndex < 0 || endIndex < 0 || endIndex < startIndex) {
            throw new IllegalArgumentException("Invalid startIndex or endIndex: " + startIndex + ", " + endIndex);
        }

        final BigInteger[] elementData = new BigInteger[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i] == null ? null : new BigInteger(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<BigInteger> from(List<String> c) {
        return from(c, null);
    }

    public static ObjectList<BigInteger> from(List<String> c, BigInteger defaultValueForNull) {
        final BigInteger[] a = new BigInteger[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : new BigInteger(e);
        }

        return of(a);
    }

    public static ObjectList<BigInteger> from(Collection<BigInteger> c) {
        return from(c, null);
    }

    public static ObjectList<BigInteger> from(Collection<BigInteger> c, BigInteger defaultValueForNull) {
        final BigInteger[] a = new BigInteger[c.size()];
        int idx = 0;

        for (BigInteger e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }
}
