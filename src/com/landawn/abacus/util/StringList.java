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
public abstract class StringList extends ObjectList<String> {
    private StringList() {
        super(new String[0]);
        // utility class
    }

    public static ObjectList<String> empty() {
        return new ObjectList<String>(N.EMPTY_STRING_ARRAY);
    }

    public static ObjectList<String> of(String... a) {
        return new ObjectList<String>(a);
    }

    public static ObjectList<String> of(String[] a, int size) {
        return new ObjectList<String>(a, size);
    }

    public static ObjectList<String> from(boolean... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(boolean[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(char... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(char[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(byte... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(byte[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(short... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(short[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(int... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(int[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(long... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(long[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(float... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(float[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(double... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(double[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = String.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static ObjectList<String> from(BigInteger... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(BigInteger[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i] == null ? null : a[i].toString();
        }

        return of(elementData);
    }

    public static ObjectList<String> from(BigDecimal... a) {
        return from(a, 0, a.length);
    }

    public static ObjectList<String> from(BigDecimal[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final String[] elementData = new String[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i] == null ? null : a[i].toString();
        }

        return of(elementData);
    }

    public static ObjectList<String> from(List<? extends Number> c) {
        return from(c, null);
    }

    public static ObjectList<String> from(List<? extends Number> c, String defaultValueForNull) {
        final String[] a = new String[c.size()];
        int idx = 0;

        for (Number e : c) {
            a[idx++] = e == null ? defaultValueForNull : e.toString();
        }

        return of(a);
    }

    public static ObjectList<String> from(Collection<String> c) {
        return from(c, null);
    }

    public static ObjectList<String> from(Collection<String> c, String defaultValueForNull) {
        final String[] a = new String[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }
}
