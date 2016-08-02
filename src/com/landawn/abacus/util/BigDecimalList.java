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
    private BigDecimalList() {
        super(new BigDecimal[0]);
        // utility class
    }

    public static ObjectList<BigDecimal> of(BigDecimal[] a) {
        return new ObjectList<BigDecimal>(a);
    }

    public static ObjectList<BigDecimal> of(BigDecimal[] a, int size) {
        return new ObjectList<BigDecimal>(a, size);
    }

    public static ObjectList<BigDecimal> of(String[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<BigDecimal> of(String[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final BigDecimal[] elementData = new BigDecimal[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = a[i] == null ? null : new BigDecimal(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<BigDecimal> of(List<String> c) {
        return of(c, null);
    }

    public static ObjectList<BigDecimal> of(List<String> c, BigDecimal defaultValueForNull) {
        final BigDecimal[] a = new BigDecimal[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : new BigDecimal(e);
        }

        return of(a);
    }

    public static ObjectList<BigDecimal> of(Collection<BigDecimal> c) {
        return of(c, null);
    }

    public static ObjectList<BigDecimal> of(Collection<BigDecimal> c, BigDecimal defaultValueForNull) {
        final BigDecimal[] a = new BigDecimal[c.size()];
        int idx = 0;

        for (BigDecimal e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }
}
