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
    public static final OptionalInt NOT_FOUND = OptionalInt.empty();

    private Index() {
        // singleton.
    }

    public static OptionalInt of(final boolean[] a, final boolean objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final boolean[] a, final int fromIndex, final boolean objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final char[] a, final char objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final char[] a, final int fromIndex, final char objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final byte[] a, final byte objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final byte[] a, final int fromIndex, final byte objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final short[] a, final short objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final short[] a, final int fromIndex, final short objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final int[] a, final int objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final int[] a, final int fromIndex, final int objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final long[] a, final long objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final long[] a, final int fromIndex, final long objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final float[] a, final float objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final float[] a, final int fromIndex, final float objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final double[] a, final double objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final double[] a, final int fromIndex, final double objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final Object[] a, final Object objToFind) {
        return toOptionalInt(N.indexOf(a, objToFind));
    }

    public static OptionalInt of(final Object[] a, final int fromIndex, final Object objToFind) {
        return toOptionalInt(N.indexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt of(final List<?> list, final Object objToFind) {
        return toOptionalInt(N.indexOf(list, objToFind));
    }

    public static OptionalInt of(final List<?> list, final int fromIndex, final Object objToFind) {
        return toOptionalInt(N.indexOf(list, fromIndex, objToFind));
    }

    public static OptionalInt of(final String str, final int objToFind) {
        return toOptionalInt(StringUtil.indexOf(str, objToFind));
    }

    public static OptionalInt of(final String str, final int fromIndex, final int objToFind) {
        return toOptionalInt(StringUtil.indexOf(str, fromIndex, objToFind));
    }

    public static OptionalInt of(final String str, final String objToFind) {
        return toOptionalInt(StringUtil.indexOf(str, objToFind));
    }

    public static OptionalInt of(final String str, final int fromIndex, final String objToFind) {
        return toOptionalInt(StringUtil.indexOf(str, fromIndex, objToFind));
    }

    public static OptionalInt last(final boolean[] a, final boolean objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final boolean[] a, final int fromIndex, final boolean objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final char[] a, final char objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final char[] a, final int fromIndex, final char objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final byte[] a, final byte objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final byte[] a, final int fromIndex, final byte objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final short[] a, final short objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final short[] a, final int fromIndex, final short objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final int[] a, final int objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final int[] a, final int fromIndex, final int objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final long[] a, final long objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final long[] a, final int fromIndex, final long objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final float[] a, final float objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final float[] a, final int fromIndex, final float objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final double[] a, final double objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final double[] a, final int fromIndex, final double objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final Object[] a, final Object objToFind) {
        return toOptionalInt(N.lastIndexOf(a, objToFind));
    }

    public static OptionalInt last(final Object[] a, final int fromIndex, final Object objToFind) {
        return toOptionalInt(N.lastIndexOf(a, fromIndex, objToFind));
    }

    public static OptionalInt last(final List<?> list, final Object objToFind) {
        return toOptionalInt(N.lastIndexOf(list, objToFind));
    }

    public static OptionalInt last(final List<?> list, final int fromIndex, final Object objToFind) {
        return toOptionalInt(N.lastIndexOf(list, fromIndex, objToFind));
    }

    public static OptionalInt last(final String str, final int objToFind) {
        return toOptionalInt(StringUtil.lastIndexOf(str, objToFind));
    }

    public static OptionalInt last(final String str, final int fromIndex, final int objToFind) {
        return toOptionalInt(StringUtil.lastIndexOf(str, fromIndex, objToFind));
    }

    public static OptionalInt last(final String str, final String objToFind) {
        return toOptionalInt(StringUtil.lastIndexOf(str, objToFind));
    }

    public static OptionalInt last(final String str, final int fromIndex, final String objToFind) {
        return toOptionalInt(StringUtil.lastIndexOf(str, fromIndex, objToFind));
    }

    private static OptionalInt toOptionalInt(int index) {
        return index < 0 ? NOT_FOUND : OptionalInt.of(index);
    }

}
