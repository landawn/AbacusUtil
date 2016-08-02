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
import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class StringList {
    private StringList() {
        // utility class
    }

    public static ObjectList<String> of(String[] a) {
        return new ObjectList<String>(a);
    }

    public static ObjectList<String> of(String[] a, int size) {
        return new ObjectList<String>(a, size);
    }

    public static ObjectList<String> of(boolean[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(boolean[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> of(char[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(char[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> of(byte[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(byte[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> of(short[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(short[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> of(long[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(long[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> of(float[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(float[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> of(double[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(double[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> of(BigInteger[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(BigInteger[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = a[i] == null ? null : a[i].toString();
        }

        return of(elementData);
    }

    public static ObjectList<String> of(BigDecimal[] a) {
        return of(a, 0, a.length);
    }

    public static ObjectList<String> of(BigDecimal[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final String[] elementData = new String[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = a[i] == null ? null : a[i].toString();
        }

        return of(elementData);
    }

    public static ObjectList<String> of(List<Number> c) {
        return of(c, null);
    }

    public static ObjectList<String> of(List<Number> c, String defaultValueForNull) {
        final String[] a = new String[c.size()];
        int idx = 0;

        for (Number e : c) {
            a[idx++] = e == null ? defaultValueForNull : e.toString();
        }

        return of(a);
    }

    public static ObjectList<String> of(Collection<String> c) {
        return of(c, null);
    }

    public static ObjectList<String> of(Collection<String> c, String defaultValueForNull) {
        final String[] a = new String[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }
}
