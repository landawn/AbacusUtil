/*
 * Copyright (c) 2018, Haiyang Li.
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

import java.util.List;

public final class Index {
    public static final OptionalInt NOT_FOUND = OptionalInt.of(N.INDEX_NOT_FOUND);

    private Index() {
        // singleton.
    }

    public static OptionalInt of(final boolean[] a, final boolean e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final boolean[] a, final int fromIndex, final boolean e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final char[] a, final char e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final char[] a, final int fromIndex, final char e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final byte[] a, final byte e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final byte[] a, final int fromIndex, final byte e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final short[] a, final short e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final short[] a, final int fromIndex, final short e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final int[] a, final int e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final int[] a, final int fromIndex, final int e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final long[] a, final long e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final long[] a, final int fromIndex, final long e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final float[] a, final float e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final float[] a, final int fromIndex, final float e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final double[] a, final double e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final double[] a, final int fromIndex, final double e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final Object[] a, final Object e) {
        return toOptionalInt(N.indexOf(a, e));
    }

    public static OptionalInt of(final Object[] a, final int fromIndex, final Object e) {
        return toOptionalInt(N.indexOf(a, fromIndex, e));
    }

    public static OptionalInt of(final List<?> list, final Object e) {
        return toOptionalInt(N.indexOf(list, e));
    }

    public static OptionalInt of(final List<?> list, final int fromIndex, final Object e) {
        return toOptionalInt(N.indexOf(list, fromIndex, e));
    }

    public static OptionalInt of(final String str, final int targetChar) {
        return toOptionalInt(StringUtil.indexOf(str, targetChar));
    }

    public static OptionalInt of(final String str, final int fromIndex, final int targetChar) {
        return toOptionalInt(StringUtil.indexOf(str, fromIndex, targetChar));
    }

    public static OptionalInt of(final String str, final String substr) {
        return toOptionalInt(StringUtil.indexOf(str, substr));
    }

    public static OptionalInt of(final String str, final int fromIndex, final String substr) {
        return toOptionalInt(StringUtil.indexOf(str, fromIndex, substr));
    }

    public static OptionalInt last(final boolean[] a, final boolean e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final boolean[] a, final int fromIndex, final boolean e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final char[] a, final char e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final char[] a, final int fromIndex, final char e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final byte[] a, final byte e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final byte[] a, final int fromIndex, final byte e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final short[] a, final short e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final short[] a, final int fromIndex, final short e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final int[] a, final int e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final int[] a, final int fromIndex, final int e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final long[] a, final long e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final long[] a, final int fromIndex, final long e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final float[] a, final float e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final float[] a, final int fromIndex, final float e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final double[] a, final double e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final double[] a, final int fromIndex, final double e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final Object[] a, final Object e) {
        return toOptionalInt(N.lastIndexOf(a, e));
    }

    public static OptionalInt last(final Object[] a, final int fromIndex, final Object e) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, e));
    }

    public static OptionalInt last(final List<?> list, final Object e) {
        return toOptionalInt(N.lastIndexOf(list, e));
    }

    public static OptionalInt last(final List<?> list, final int fromIndex, final Object e) {
        return toOptionalInt(N.lastIndexOf(list, fromIndex, e));
    }

    public static OptionalInt last(final String str, final int targetChar) {
        return toOptionalInt(StringUtil.lastIndexOf(str, targetChar));
    }

    public static OptionalInt last(final String str, final int fromIndex, final int targetChar) {
        return toOptionalInt(StringUtil.lastIndexOf(str, fromIndex, targetChar));
    }

    public static OptionalInt last(final String str, final String substr) {
        return toOptionalInt(StringUtil.lastIndexOf(str, substr));
    }

    public static OptionalInt last(final String str, final int fromIndex, final String substr) {
        return toOptionalInt(StringUtil.lastIndexOf(str, fromIndex, substr));
    }

    private static OptionalInt toOptionalInt(int index) {
        return index == N.INDEX_NOT_FOUND ? NOT_FOUND : OptionalInt.of(index);
    }

}
