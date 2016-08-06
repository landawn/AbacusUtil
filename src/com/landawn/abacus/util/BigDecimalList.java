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

import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class BigDecimalList extends ObjectList<BigDecimal> {
    private static final BigDecimal[] EMPTY_BIG_DECIMAL_ARRAY = new BigDecimal[0];

    private BigDecimalList() {
        super(new BigDecimal[0]);
        // utility class
    }

    public static ObjectList<BigDecimal> empty() {
        return new ObjectList<BigDecimal>(EMPTY_BIG_DECIMAL_ARRAY);
    }

    public static ObjectList<BigDecimal> of(BigDecimal... a) {
        return new ObjectList<BigDecimal>(a);
    }

    public static ObjectList<BigDecimal> of(BigDecimal[] a, int size) {
        return new ObjectList<BigDecimal>(a, size);
    }

    public static ObjectList<BigDecimal> from(String... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<BigDecimal> from(String[] a, int startIndex, int endIndex) {
        if (startIndex < 0 || endIndex < 0 || endIndex < startIndex) {
            throw new IllegalArgumentException("Invalid startIndex or endIndex: " + startIndex + ", " + endIndex);
        }

        final BigDecimal[] elementData = new BigDecimal[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i] == null ? null : new BigDecimal(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<BigDecimal> from(List<String> c) {
        return from(c, null);
    }

    public static ObjectList<BigDecimal> from(List<String> c, BigDecimal defaultValueForNull) {
        final BigDecimal[] a = new BigDecimal[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : new BigDecimal(e);
        }

        return of(a);
    }

    public static ObjectList<BigDecimal> from(Collection<BigDecimal> c) {
        return from(c, null);
    }

    public static ObjectList<BigDecimal> from(Collection<BigDecimal> c, BigDecimal defaultValueForNull) {
        final BigDecimal[] a = new BigDecimal[c.size()];
        int idx = 0;

        for (BigDecimal e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }
}
