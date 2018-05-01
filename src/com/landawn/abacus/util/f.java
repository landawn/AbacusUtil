/*
 * Copyright (C) 2017 HaiYang Li
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

import java.math.RoundingMode;

import com.landawn.abacus.annotation.Beta;

@Beta
public final class f {

    private static final String ARRAY_PRINT_SEPERATOR = N.repeatt('-', 80);

    static final char CHAR_0 = (char) 0;
    static final byte BYTE_0 = (byte) 0;
    static final byte BYTE_1 = (byte) 1;
    static final short SHORT_0 = (short) 0;

    private f() {
        // utility class.
    }

    public static <T, E extends Exception> void replaceAlll(final T[] a, final Try.UnaryOperator<T, E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.apply(a[i]);
        }
    }

    public static <T, E extends Exception> void replaceAlll(final T[][] a, final Try.UnaryOperator<T, E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAlll(a[i], operator);
        }
    }

    public static <T, E extends Exception> void replaceAlll(final T[][][] a, final Try.UnaryOperator<T, E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAlll(a[i], operator);
        }
    }

    public static <T, E extends Exception> void replaceIff(final T[] a, final Try.Predicate<? super T, E> predicate, final T newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <T, E extends Exception> void replaceIff(final T[][] a, final Try.Predicate<? super T, E> predicate, final T newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIff(a[i], predicate, newValue);
        }
    }

    public static <T, E extends Exception> void replaceIff(final T[][][] a, final Try.Predicate<? super T, E> predicate, final T newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIff(a[i], predicate, newValue);
        }
    }

    public static <T> T[] flattenn(final T[][] a) {
        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final T[] c = N.newArray(a.getClass().getComponentType().getComponentType(), count);
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static <T> T[] flattenn(final T[][][] a) {
        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final T[] c = N.newArray(a.getClass().getComponentType().getComponentType().getComponentType(), count);
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static <T> T[][] reshappe(final T[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        //        if (N.isNullOrEmpty(a)) {
        //            return new T[0][0];
        //        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final T[][] c = N.newArray(a.getClass(), n);

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static <T> T[][][] reshappe(final T[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        //        if (N.isNullOrEmpty(a)) {
        //            return new T[0][0][0];
        //        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final T[][][] c = N.newArray(N.newArray(a.getClass(), 0).getClass(), n);

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = N.newArray(a.getClass(), N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING)));

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static <T, E extends Exception> T[] mapp(final T[] a, final Try.UnaryOperator<T, E> func) throws E {
        if (a == null) {
            return null;
        }

        return map((Class<T>) a.getClass().getComponentType(), a, func);
    }

    public static <T, R, E extends Exception> R[] map(final Class<R> cls, final T[] a, final Try.Function<? super T, R, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final R[] c = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            c[i] = func.apply(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> T[][] mapp(final T[][] a, final Try.UnaryOperator<T, E> func) throws E {
        if (a == null) {
            return null;
        }

        return map((Class<T>) a.getClass().getComponentType().getComponentType(), a, func);
    }

    public static <T, R, E extends Exception> R[][] map(final Class<R> cls, final T[][] a, final Try.Function<? super T, R, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final R[][] c = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            c[i] = map(cls, a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> T[][][] mapp(final T[][][] a, final Try.UnaryOperator<T, E> func) throws E {
        if (a == null) {
            return null;
        }

        return map((Class<T>) a.getClass().getComponentType().getComponentType().getComponentType(), a, func);
    }

    public static <T, R, E extends Exception> R[][][] map(final Class<R> cls, final T[][][] a, final Try.Function<? super T, R, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final R[][][] c = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            c[i] = map(cls, a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> boolean[] mapToBoolean(final T[] a, final Try.ToBooleanFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[] c = new boolean[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsBoolean(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> boolean[][] mapToBoolean(final T[][] a, final Try.ToBooleanFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][] c = new boolean[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToBoolean(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> boolean[][][] mapToBoolean(final T[][][] a, final Try.ToBooleanFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][][] c = new boolean[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToBoolean(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> char[] mapToChar(final T[] a, final Try.ToCharFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final char[] c = new char[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsChar(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> char[][] mapToChar(final T[][] a, final Try.ToCharFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final char[][] c = new char[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToChar(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> char[][][] mapToChar(final T[][][] a, final Try.ToCharFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final char[][][] c = new char[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToChar(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> byte[] mapToByte(final T[] a, final Try.ToByteFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final byte[] c = new byte[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsByte(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> byte[][] mapToByte(final T[][] a, final Try.ToByteFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final byte[][] c = new byte[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToByte(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> byte[][][] mapToByte(final T[][][] a, final Try.ToByteFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final byte[][][] c = new byte[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToByte(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> short[] mapToShort(final T[] a, final Try.ToShortFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final short[] c = new short[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsShort(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> short[][] mapToShort(final T[][] a, final Try.ToShortFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final short[][] c = new short[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToShort(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> short[][][] mapToShort(final T[][][] a, final Try.ToShortFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final short[][][] c = new short[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToShort(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> int[] mapToInt(final T[] a, final Try.ToIntFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[] c = new int[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsInt(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> int[][] mapToInt(final T[][] a, final Try.ToIntFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][] c = new int[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToInt(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> int[][][] mapToInt(final T[][][] a, final Try.ToIntFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][][] c = new int[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToInt(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> long[] mapToLong(final T[] a, final Try.ToLongFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[] c = new long[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsLong(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> long[][] mapToLong(final T[][] a, final Try.ToLongFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][] c = new long[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToLong(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> long[][][] mapToLong(final T[][][] a, final Try.ToLongFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][][] c = new long[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToLong(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> float[] mapToFloat(final T[] a, final Try.ToFloatFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[] c = new float[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsFloat(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> float[][] mapToFloat(final T[][] a, final Try.ToFloatFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][] c = new float[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToFloat(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> float[][][] mapToFloat(final T[][][] a, final Try.ToFloatFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][][] c = new float[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToFloat(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> double[] mapToDouble(final T[] a, final Try.ToDoubleFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[] c = new double[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsDouble(a[i]);
        }

        return c;
    }

    public static <T, E extends Exception> double[][] mapToDouble(final T[][] a, final Try.ToDoubleFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][] c = new double[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToDouble(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> double[][][] mapToDouble(final T[][][] a, final Try.ToDoubleFunction<? super T, E> func) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][][] c = new double[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToDouble(a[i], func);
        }

        return c;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final boolean[] a, final Try.BooleanFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final boolean[][] a, final Try.BooleanFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final boolean[][][] a, final Try.BooleanFunction<? extends T, E> mapper)
            throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final char[] a, final Try.CharFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final char[][] a, final Try.CharFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final char[][][] a, final Try.CharFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final byte[] a, final Try.ByteFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final byte[][] a, final Try.ByteFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final byte[][][] a, final Try.ByteFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final short[] a, final Try.ShortFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final short[][] a, final Try.ShortFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final short[][][] a, final Try.ShortFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final int[] a, final Try.IntFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final int[][] a, final Try.IntFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final int[][][] a, final Try.IntFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final long[] a, final Try.LongFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final long[][] a, final Try.LongFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final long[][][] a, final Try.LongFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final float[] a, final Try.FloatFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final float[][] a, final Try.FloatFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final float[][][] a, final Try.FloatFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[] mapToObj(final Class<T> cls, final double[] a, final Try.DoubleFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T, E extends Exception> T[][] mapToObj(final Class<T> cls, final double[][] a, final Try.DoubleFunction<? extends T, E> mapper) throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T, E extends Exception> T[][][] mapToObj(final Class<T> cls, final double[][][] a, final Try.DoubleFunction<? extends T, E> mapper)
            throws E {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <A, B, E extends Exception> A[] zipp(final A[] a, final B[] b, final Try.BiFunction<? super A, ? super B, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, zipFunction);
    }

    public static <A, B, R, E extends Exception> R[] zip(final Class<R> cls, final A[] a, final B[] b,
            final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final R[] result = N.newArray(cls, N.min(lenA, lenB));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <A, B, E extends Exception> A[] zipp(final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final Try.BiFunction<? super A, ? super B, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R, E extends Exception> R[] zip(final Class<R> cls, final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), cls, a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <A, B, R, E extends Exception> R[] zip(final int len, final Class<R> cls, final A[] a, final B[] b, final A valueForNoneA,
            final B valueForNoneB, final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final R[] result = N.newArray(cls, len);

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <A, B, C, E extends Exception> A[] zipp(final A[] a, final B[] b, final C[] c,
            final Try.TriFunction<? super A, ? super B, ? super C, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, c, zipFunction);
    }

    public static <A, B, C, R, E extends Exception> R[] zip(final Class<R> cls, final A[] a, final B[] b, final C[] c,
            final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final R[] result = N.newArray(cls, N.min(lenA, lenB, lenC));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <A, B, C, E extends Exception> A[] zipp(final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public static <A, B, C, R, E extends Exception> R[] zip(final Class<R> cls, final A[] a, final B[] b, final C[] c, final A valueForNoneA,
            final B valueForNoneB, final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), cls, a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <A, B, C, R, E extends Exception> R[] zip(final int len, final Class<R> cls, final A[] a, final B[] b, final C[] c, final A valueForNoneA,
            final B valueForNoneB, final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final R[] result = N.newArray(cls, len);

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <A, B, E extends Exception> A[][] zipp(final A[][] a, final B[][] b, final Try.BiFunction<? super A, ? super B, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, zipFunction);
    }

    public static <A, B, R, E extends Exception> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b,
            final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), N.min(lenA, lenB));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <A, B, E extends Exception> A[][] zipp(final A[][] a, final B[][] b, final A valueForNoneA, final B valueForNoneB,
            final Try.BiFunction<? super A, ? super B, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R, E extends Exception> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b, final A valueForNoneA, final B valueForNoneB,
            final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), cls, a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <A, B, R, E extends Exception> R[][] zip(final int len, final int rowLen, final Class<R> cls, final A[][] a, final B[][] b,
            final A valueForNoneA, final B valueForNoneB, final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, cls, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, cls, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, cls, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, cls, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <A, B, C, E extends Exception> A[][] zipp(final A[][] a, final B[][] b, final C[][] c,
            final Try.TriFunction<? super A, ? super B, ? super C, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, c, zipFunction);
    }

    public static <A, B, C, R, E extends Exception> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b, final C[][] c,
            final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), N.min(lenA, lenB, lenC));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <A, B, C, E extends Exception> A[][] zipp(final A[][] a, final B[][] b, final C[][] c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public static <A, B, C, R, E extends Exception> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b, final C[][] c, final A valueForNoneA,
            final B valueForNoneB, final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), cls, a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <A, B, C, R, E extends Exception> R[][] zip(final int len, final int rowLen, final Class<R> cls, final A[][] a, final B[][] b, final C[][] c,
            final A valueForNoneA, final B valueForNoneB, final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, cls, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, cls, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC, zipFunction);
            }
        }

        return result;
    }

    public static <A, B, E extends Exception> A[][][] zipp(final A[][][] a, final B[][][] b, final Try.BiFunction<? super A, ? super B, A, E> zipFunction)
            throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, zipFunction);
    }

    public static <A, B, R, E extends Exception> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b,
            final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final R[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), N.min(lenA, lenB));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <A, B, E extends Exception> A[][][] zipp(final A[][][] a, final B[][][] b, final A valueForNoneA, final B valueForNoneB,
            final Try.BiFunction<? super A, ? super B, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R, E extends Exception> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b, final A valueForNoneA, final B valueForNoneB,
            final Try.BiFunction<? super A, ? super B, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final R[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), N.max(lenA, lenB));

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(cls, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(cls, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(cls, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <A, B, C, E extends Exception> A[][][] zipp(final A[][][] a, final B[][][] b, final C[][][] c,
            final Try.TriFunction<? super A, ? super B, ? super C, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, c, zipFunction);
    }

    public static <A, B, C, R, E extends Exception> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b, final C[][][] c,
            final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final R[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), N.min(lenA, lenB, lenC));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <A, B, C, E extends Exception> A[][][] zipp(final A[][][] a, final B[][][] b, final C[][][] c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, A, E> zipFunction) throws E {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    public static <A, B, C, R, E extends Exception> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b, final C[][][] c, final A valueForNoneA,
            final B valueForNoneB, final C valueForNoneC, final Try.TriFunction<? super A, ? super B, ? super C, R, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final R[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), N.max(lenA, lenB, lenC));

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(cls, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(cls, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                    zipFunction);
        }

        return result;
    }

    public static <T> T[] copy(Class<T[]> newType, Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.newArray(newType.getComponentType(), 0);
        }

        return N.copyOf(a, a.length, newType);
    }

    public static <T> T[][] copy(Class<T[][]> newType, Object[][] a) {
        final Class<T[]> componentType = (Class<T[]>) newType.getComponentType();

        if (N.isNullOrEmpty(a)) {
            return N.newArray(componentType, 0);
        }

        final int len = N.len(a);
        final T[][] result = N.newArray(componentType, len);

        for (int i = 0; i < len; i++) {
            result[i] = copy(componentType, a[i]);
        }

        return result;
    }

    public static <T> T[][][] copy(Class<T[][][]> newType, Object[][][] a) {
        final Class<T[][]> componentType = (Class<T[][]>) newType.getComponentType();

        if (N.isNullOrEmpty(a)) {
            return N.newArray(componentType, 0);
        }

        final int len = N.len(a);
        final T[][][] result = N.newArray(componentType, len);

        for (int i = 0; i < len; i++) {
            result[i] = copy(componentType, a[i]);
        }

        return result;
    }

    public static <T> void println(final T[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static <T> void println(final T[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = N.len(a);
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static <T> void println(final T[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = N.len(a);
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static <T> int maxLen(T[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = N.len(a);
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static <E extends Exception> void replaceAll(final boolean[] a, final Try.BooleanUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsBoolean(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final boolean[][] a, final Try.BooleanUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final boolean[][][] a, final Try.BooleanUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final boolean[] a, final Try.BooleanPredicate<E> predicate, final boolean newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final boolean[][] a, final Try.BooleanPredicate<E> predicate, final boolean newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final boolean[][][] a, final Try.BooleanPredicate<E> predicate, final boolean newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static boolean[] flatten(final boolean[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final boolean[] c = new boolean[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static boolean[] flatten(final boolean[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final boolean[] c = new boolean[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static boolean[][] reshape(final boolean[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new boolean[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final boolean[][] c = new boolean[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static boolean[][][] reshape(final boolean[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new boolean[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final boolean[][][] c = new boolean[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new boolean[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static <E extends Exception> boolean[] zip(final boolean[] a, final boolean[] b, final Try.BooleanBiFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final boolean[] result = new boolean[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> boolean[] zip(final boolean[] a, final boolean[] b, final boolean valueForNoneA, final boolean valueForNoneB,
            final Try.BooleanBiFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> boolean[] zip(final int len, final boolean[] a, final boolean[] b, final boolean valueForNoneA,
            final boolean valueForNoneB, final Try.BooleanBiFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final boolean[] result = new boolean[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> boolean[] zip(final boolean[] a, final boolean[] b, final boolean[] c,
            final Try.BooleanTriFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final boolean[] result = new boolean[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> boolean[] zip(final boolean[] a, final boolean[] b, final boolean[] c, final boolean valueForNoneA,
            final boolean valueForNoneB, final boolean valueForNoneC, final Try.BooleanTriFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> boolean[] zip(final int len, final boolean[] a, final boolean[] b, final boolean[] c, final boolean valueForNoneA,
            final boolean valueForNoneB, final boolean valueForNoneC, final Try.BooleanTriFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final boolean[] result = new boolean[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> boolean[][] zip(final boolean[][] a, final boolean[][] b, final Try.BooleanBiFunction<Boolean, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final boolean[][] result = new boolean[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> boolean[][] zip(final boolean[][] a, final boolean[][] b, final boolean valueForNoneA, final boolean valueForNoneB,
            final Try.BooleanBiFunction<Boolean, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> boolean[][] zip(final int len, final int rowLen, final boolean[][] a, final boolean[][] b, final boolean valueForNoneA,
            final boolean valueForNoneB, final Try.BooleanBiFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final boolean[][] result = new boolean[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> boolean[][] zip(final boolean[][] a, final boolean[][] b, final boolean[][] c,
            final Try.BooleanTriFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final boolean[][] result = new boolean[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> boolean[][] zip(final boolean[][] a, final boolean[][] b, final boolean[][] c, final boolean valueForNoneA,
            final boolean valueForNoneB, final boolean valueForNoneC, final Try.BooleanTriFunction<Boolean, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> boolean[][] zip(final int len, final int rowLen, final boolean[][] a, final boolean[][] b, final boolean[][] c,
            final boolean valueForNoneA, final boolean valueForNoneB, final boolean valueForNoneC, final Try.BooleanTriFunction<Boolean, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final boolean[][] result = new boolean[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final Try.BooleanBiFunction<Boolean, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final boolean[][][] result = new boolean[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final boolean valueForNoneA,
            final boolean valueForNoneB, final Try.BooleanBiFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final boolean[][][] result = new boolean[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final boolean[][][] c,
            final Try.BooleanTriFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final boolean[][][] result = new boolean[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final boolean[][][] c, final boolean valueForNoneA,
            final boolean valueForNoneB, final boolean valueForNoneC, final Try.BooleanTriFunction<Boolean, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final boolean[][][] result = new boolean[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Boolean[] box(final boolean[] a) {
        return Array.box(a);
    }

    public static Boolean[][] box(final boolean[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final Boolean[][] result = new Boolean[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Boolean[][][] box(final boolean[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final Boolean[][][] result = new Boolean[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static boolean[] unbox(final Boolean[] a) {
        return Array.unbox(a);
    }

    public static boolean[] unbox(final Boolean[] a, final boolean valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static boolean[][] unbox(final Boolean[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][] result = new boolean[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static boolean[][] unbox(final Boolean[][] a, final boolean valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][] result = new boolean[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static boolean[][][] unbox(final Boolean[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][][] result = new boolean[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static boolean[][][] unbox(final Boolean[][][] a, final boolean valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][][] result = new boolean[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final boolean[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final boolean[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final boolean[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(boolean[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void plus(final char[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] += param;
        }
    }

    public static void plus(final char[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void plus(final char[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void minus(final char[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] -= param;
        }
    }

    public static void minus(final char[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void minus(final char[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void multipliedBy(final char[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] *= param;
        }
    }

    public static void multipliedBy(final char[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void multipliedBy(final char[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void dividedBy(final char[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] /= param;
        }
    }

    public static void dividedBy(final char[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static void dividedBy(final char[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static <E extends Exception> void replaceAll(final char[] a, final Try.CharUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsChar(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final char[][] a, final Try.CharUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final char[][][] a, final Try.CharUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final char[] a, final Try.CharPredicate<E> predicate, final char newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final char[][] a, final Try.CharPredicate<E> predicate, final char newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final char[][][] a, final Try.CharPredicate<E> predicate, final char newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static char[] flatten(final char[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final char[] c = new char[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static char[] flatten(final char[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_CHAR_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final char[] c = new char[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static char[][] reshape(final char[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new char[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final char[][] c = new char[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static char[][][] reshape(final char[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new char[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final char[][][] c = new char[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new char[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static <E extends Exception> char[] zip(final char[] a, final char[] b, final Try.CharBiFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final char[] result = new char[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> char[] zip(final char[] a, final char[] b, final char valueForNoneA, final char valueForNoneB,
            final Try.CharBiFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> char[] zip(final int len, final char[] a, final char[] b, final char valueForNoneA, final char valueForNoneB,
            final Try.CharBiFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final char[] result = new char[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> char[] zip(final char[] a, final char[] b, final char[] c, final Try.CharTriFunction<Character, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final char[] result = new char[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> char[] zip(final char[] a, final char[] b, final char[] c, final char valueForNoneA, final char valueForNoneB,
            final char valueForNoneC, final Try.CharTriFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> char[] zip(final int len, final char[] a, final char[] b, final char[] c, final char valueForNoneA,
            final char valueForNoneB, final char valueForNoneC, final Try.CharTriFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final char[] result = new char[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> char[][] zip(final char[][] a, final char[][] b, final Try.CharBiFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final char[][] result = new char[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> char[][] zip(final char[][] a, final char[][] b, final char valueForNoneA, final char valueForNoneB,
            final Try.CharBiFunction<Character, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> char[][] zip(final int len, final int rowLen, final char[][] a, final char[][] b, final char valueForNoneA,
            final char valueForNoneB, final Try.CharBiFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final char[][] result = new char[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> char[][] zip(final char[][] a, final char[][] b, final char[][] c, final Try.CharTriFunction<Character, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final char[][] result = new char[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> char[][] zip(final char[][] a, final char[][] b, final char[][] c, final char valueForNoneA, final char valueForNoneB,
            final char valueForNoneC, final Try.CharTriFunction<Character, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> char[][] zip(final int len, final int rowLen, final char[][] a, final char[][] b, final char[][] c,
            final char valueForNoneA, final char valueForNoneB, final char valueForNoneC, final Try.CharTriFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final char[][] result = new char[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> char[][][] zip(final char[][][] a, final char[][][] b, final Try.CharBiFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final char[][][] result = new char[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> char[][][] zip(final char[][][] a, final char[][][] b, final char valueForNoneA, final char valueForNoneB,
            final Try.CharBiFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final char[][][] result = new char[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> char[][][] zip(final char[][][] a, final char[][][] b, final char[][][] c,
            final Try.CharTriFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final char[][][] result = new char[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> char[][][] zip(final char[][][] a, final char[][][] b, final char[][][] c, final char valueForNoneA,
            final char valueForNoneB, final char valueForNoneC, final Try.CharTriFunction<Character, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final char[][][] result = new char[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Character[] box(final char[] a) {
        return Array.box(a);
    }

    public static Character[][] box(final char[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Character[][] result = new Character[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Character[][][] box(final char[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Character[][][] result = new Character[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static char[] unbox(final Character[] a) {
        return Array.unbox(a);
    }

    public static char[] unbox(final Character[] a, final char valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static char[][] unbox(final Character[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final char[][] result = new char[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static char[][] unbox(final Character[][] a, final char valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final char[][] result = new char[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static char[][][] unbox(final Character[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final char[][][] result = new char[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static char[][][] unbox(final Character[][][] a, final char valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final char[][][] result = new char[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final char[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final char[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final char[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(char[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void plus(final byte[] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] += param;
        }
    }

    public static void plus(final byte[][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void plus(final byte[][][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void minus(final byte[] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] -= param;
        }
    }

    public static void minus(final byte[][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void minus(final byte[][][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void multipliedBy(final byte[] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] *= param;
        }
    }

    public static void multipliedBy(final byte[][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void multipliedBy(final byte[][][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void dividedBy(final byte[] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] /= param;
        }
    }

    public static void dividedBy(final byte[][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static void dividedBy(final byte[][][] a, final byte param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static <E extends Exception> void replaceAll(final byte[] a, final Try.ByteUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsByte(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final byte[][] a, final Try.ByteUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final byte[][][] a, final Try.ByteUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final byte[] a, final Try.BytePredicate<E> predicate, final byte newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final byte[][] a, final Try.BytePredicate<E> predicate, final byte newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final byte[][][] a, final Try.BytePredicate<E> predicate, final byte newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static byte[] flatten(final byte[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final byte[] c = new byte[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static byte[] flatten(final byte[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_BYTE_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final byte[] c = new byte[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static byte[][] reshape(final byte[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new byte[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final byte[][] c = new byte[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static byte[][][] reshape(final byte[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new byte[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final byte[][][] c = new byte[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new byte[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static byte[] add(final byte[] a, final byte[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] + b[i]);
        }

        return result;
    }

    public static byte[] add(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return add(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[] add(final int len, final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (byte) (a[i] + b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (byte) (valueForNoneA + b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (byte) (a[i] + valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (byte) (valueForNoneA + valueForNoneB);
            }
        }

        return result;
    }

    public static byte[] add(final byte[] a, final byte[] b, final byte[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] + b[i] + c[i]);
        }

        return result;
    }

    public static byte[] add(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB, final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return add(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[] add(final int len, final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (byte) (a[i] + b[i] + c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (byte) ((i < lenA ? a[i] : valueForNoneA) + (i < lenB ? b[i] : valueForNoneB) + (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static byte[][] add(final byte[][] a, final byte[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static byte[][] add(final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB) {
        return add(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[][] add(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = add(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = add(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = add(rowLen, (byte[]) null, (byte[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][] add(final byte[][] a, final byte[][] b, final byte[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][] add(final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        return add(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[][] add(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = add(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
            }
        }

        return result;
    }

    public static byte[][][] add(final byte[][][] a, final byte[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static byte[][][] add(final byte[][][] a, final byte[][][] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = add(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = add(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = add(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][][] add(final byte[][][] a, final byte[][][] b, final byte[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][][] add(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = add(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = add(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static byte[] subtract(final byte[] a, final byte[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] - b[i]);
        }

        return result;
    }

    public static byte[] subtract(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return subtract(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[] subtract(final int len, final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (byte) (a[i] - b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (byte) (valueForNoneA - b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (byte) (a[i] - valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (byte) (valueForNoneA - valueForNoneB);
            }
        }

        return result;
    }

    public static byte[] subtract(final byte[] a, final byte[] b, final byte[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] - b[i] - c[i]);
        }

        return result;
    }

    public static byte[] subtract(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return subtract(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[] subtract(final int len, final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (byte) (a[i] - b[i] - c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (byte) ((i < lenA ? a[i] : valueForNoneA) - (i < lenB ? b[i] : valueForNoneB) - (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static byte[][] subtract(final byte[][] a, final byte[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static byte[][] subtract(final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB) {
        return subtract(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[][] subtract(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = subtract(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = subtract(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = subtract(rowLen, (byte[]) null, (byte[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][] subtract(final byte[][] a, final byte[][] b, final byte[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][] subtract(final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        return subtract(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[][] subtract(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = subtract(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static byte[][][] subtract(final byte[][][] a, final byte[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static byte[][][] subtract(final byte[][][] a, final byte[][][] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = subtract(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = subtract(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = subtract(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][][] subtract(final byte[][][] a, final byte[][][] b, final byte[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][][] subtract(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = subtract(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = subtract(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static byte[] multipliedBy(final byte[] a, final byte[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] * b[i]);
        }

        return result;
    }

    public static byte[] multipliedBy(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return multipliedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[] multipliedBy(final int len, final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (byte) (a[i] * b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (byte) (valueForNoneA * b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (byte) (a[i] * valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (byte) (valueForNoneA * valueForNoneB);
            }
        }

        return result;
    }

    public static byte[] multipliedBy(final byte[] a, final byte[] b, final byte[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] * b[i] * c[i]);
        }

        return result;
    }

    public static byte[] multipliedBy(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return multipliedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[] multipliedBy(final int len, final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (byte) (a[i] * b[i] * c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (byte) ((i < lenA ? a[i] : valueForNoneA) * (i < lenB ? b[i] : valueForNoneB) * (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static byte[][] multipliedBy(final byte[][] a, final byte[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static byte[][] multipliedBy(final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB) {
        return multipliedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[][] multipliedBy(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte valueForNoneA,
            final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = multipliedBy(rowLen, (byte[]) null, (byte[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][] multipliedBy(final byte[][] a, final byte[][] b, final byte[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][] multipliedBy(final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        return multipliedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[][] multipliedBy(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = multipliedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static byte[][][] multipliedBy(final byte[][][] a, final byte[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static byte[][][] multipliedBy(final byte[][][] a, final byte[][][] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = multipliedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = multipliedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][][] multipliedBy(final byte[][][] a, final byte[][][] b, final byte[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][][] multipliedBy(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = multipliedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] / b[i]);
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[] dividedBy(final int len, final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (byte) (a[i] / b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (byte) (valueForNoneA / b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (byte) (a[i] / valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (byte) (valueForNoneA / valueForNoneB);
            }
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b, final byte[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] / b[i] / c[i]);
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[] dividedBy(final int len, final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (byte) (a[i] / b[i] / c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (byte) ((i < lenA ? a[i] : valueForNoneA) / (i < lenB ? b[i] : valueForNoneB) / (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static byte[][] dividedBy(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (byte[]) null, (byte[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b, final byte[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static byte[][] dividedBy(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b, final byte valueForNoneA, final byte valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b, final byte[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]));
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static byte[] dividedBy(final int len, final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB,
            final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (byte) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]));
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (byte) (valueForNoneA / (b[i] == 0 ? defaultValueForZero : b[i]));
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (byte) (a[i] / valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (byte) (valueForNoneA / valueForNoneB);
            }
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b, final byte[] c, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (byte) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]));
        }

        return result;
    }

    public static byte[] dividedBy(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB, final byte valueForNoneC,
            final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
    }

    private static byte[] dividedBy(final int len, final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (byte) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]));
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (byte) ((i < lenA ? a[i] : valueForNoneA) / (i < lenB ? (b[i] == 0 ? defaultValueForZero : b[i]) : valueForNoneB)
                        / (i < lenC ? (c[i] == 0 ? defaultValueForZero : c[i]) : valueForNoneC));
            }
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB, final byte defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static byte[][] dividedBy(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB,
            final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (byte[]) null, (byte[]) null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b, final byte[][] c, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static byte[][] dividedBy(final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final byte defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                defaultValueForZero);
    }

    private static byte[][] dividedBy(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC, defaultValueForZero);
            }
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b, final byte valueForNoneA, final byte valueForNoneB,
            final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static byte[][][] dividedBy(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final byte defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                    defaultValueForZero);
        }

        return result;
    }

    public static <E extends Exception> byte[] zip(final byte[] a, final byte[] b, final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> byte[] zip(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB,
            final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> byte[] zip(final int len, final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB,
            final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> byte[] zip(final byte[] a, final byte[] b, final byte[] c, final Try.ByteTriFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> byte[] zip(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final Try.ByteTriFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> byte[] zip(final int len, final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC, final Try.ByteTriFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[] result = new byte[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> byte[][] zip(final byte[][] a, final byte[][] b, final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> byte[][] zip(final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB,
            final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> byte[][] zip(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte valueForNoneA,
            final byte valueForNoneB, final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> byte[][] zip(final byte[][] a, final byte[][] b, final byte[][] c, final Try.ByteTriFunction<Byte, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> byte[][] zip(final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final Try.ByteTriFunction<Byte, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> byte[][] zip(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte[][] c,
            final byte valueForNoneA, final byte valueForNoneB, final byte valueForNoneC, final Try.ByteTriFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][] result = new byte[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> byte[][][] zip(final byte[][][] a, final byte[][][] b, final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> byte[][][] zip(final byte[][][] a, final byte[][][] b, final byte valueForNoneA, final byte valueForNoneB,
            final Try.ByteBiFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final byte[][][] result = new byte[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> byte[][][] zip(final byte[][][] a, final byte[][][] b, final byte[][][] c,
            final Try.ByteTriFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> byte[][][] zip(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC, final Try.ByteTriFunction<Byte, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final byte[][][] result = new byte[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Byte[] box(final byte[] a) {
        return Array.box(a);
    }

    public static Byte[][] box(final byte[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Byte[][] result = new Byte[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Byte[][][] box(final byte[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Byte[][][] result = new Byte[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static byte[] unbox(final Byte[] a) {
        return Array.unbox(a);
    }

    public static byte[] unbox(final Byte[] a, final byte valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static byte[][] unbox(final Byte[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final byte[][] result = new byte[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static byte[][] unbox(final Byte[][] a, final byte valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final byte[][] result = new byte[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static byte[][][] unbox(final Byte[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final byte[][][] result = new byte[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static byte[][][] unbox(final Byte[][][] a, final byte valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final byte[][][] result = new byte[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final byte[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final byte[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final byte[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(byte[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void plus(final short[] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] += param;
        }
    }

    public static void plus(final short[][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void plus(final short[][][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void minus(final short[] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] -= param;
        }
    }

    public static void minus(final short[][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void minus(final short[][][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void multipliedBy(final short[] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] *= param;
        }
    }

    public static void multipliedBy(final short[][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void multipliedBy(final short[][][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void dividedBy(final short[] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] /= param;
        }
    }

    public static void dividedBy(final short[][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static void dividedBy(final short[][][] a, final short param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static <E extends Exception> void replaceAll(final short[] a, final Try.ShortUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsShort(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final short[][] a, final Try.ShortUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final short[][][] a, final Try.ShortUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final short[] a, final Try.ShortPredicate<E> predicate, final short newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final short[][] a, final Try.ShortPredicate<E> predicate, final short newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final short[][][] a, final Try.ShortPredicate<E> predicate, final short newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static short[] flatten(final short[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final short[] c = new short[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static short[] flatten(final short[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_SHORT_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final short[] c = new short[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static short[][] reshape(final short[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new short[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final short[][] c = new short[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static short[][][] reshape(final short[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new short[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final short[][][] c = new short[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new short[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static short[] add(final short[] a, final short[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] + b[i]);
        }

        return result;
    }

    public static short[] add(final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return add(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[] add(final int len, final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (short) (a[i] + b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (short) (valueForNoneA + b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (short) (a[i] + valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (short) (valueForNoneA + valueForNoneB);
            }
        }

        return result;
    }

    public static short[] add(final short[] a, final short[] b, final short[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] + b[i] + c[i]);
        }

        return result;
    }

    public static short[] add(final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return add(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[] add(final int len, final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (short) (a[i] + b[i] + c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (short) ((i < lenA ? a[i] : valueForNoneA) + (i < lenB ? b[i] : valueForNoneB) + (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static short[][] add(final short[][] a, final short[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static short[][] add(final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB) {
        return add(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[][] add(final int len, final int rowLen, final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = add(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = add(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = add(rowLen, (short[]) null, (short[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][] add(final short[][] a, final short[][] b, final short[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][] add(final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        return add(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[][] add(final int len, final int rowLen, final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = add(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
            }
        }

        return result;
    }

    public static short[][][] add(final short[][][] a, final short[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static short[][][] add(final short[][][] a, final short[][][] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = add(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = add(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = add(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][][] add(final short[][][] a, final short[][][] b, final short[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][][] add(final short[][][] a, final short[][][] b, final short[][][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = add(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = add(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static short[] subtract(final short[] a, final short[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] - b[i]);
        }

        return result;
    }

    public static short[] subtract(final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return subtract(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[] subtract(final int len, final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (short) (a[i] - b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (short) (valueForNoneA - b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (short) (a[i] - valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (short) (valueForNoneA - valueForNoneB);
            }
        }

        return result;
    }

    public static short[] subtract(final short[] a, final short[] b, final short[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] - b[i] - c[i]);
        }

        return result;
    }

    public static short[] subtract(final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return subtract(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[] subtract(final int len, final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (short) (a[i] - b[i] - c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (short) ((i < lenA ? a[i] : valueForNoneA) - (i < lenB ? b[i] : valueForNoneB) - (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static short[][] subtract(final short[][] a, final short[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static short[][] subtract(final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB) {
        return subtract(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[][] subtract(final int len, final int rowLen, final short[][] a, final short[][] b, final short valueForNoneA,
            final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = subtract(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = subtract(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = subtract(rowLen, (short[]) null, (short[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][] subtract(final short[][] a, final short[][] b, final short[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][] subtract(final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        return subtract(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[][] subtract(final int len, final int rowLen, final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = subtract(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static short[][][] subtract(final short[][][] a, final short[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static short[][][] subtract(final short[][][] a, final short[][][] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = subtract(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = subtract(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = subtract(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][][] subtract(final short[][][] a, final short[][][] b, final short[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][][] subtract(final short[][][] a, final short[][][] b, final short[][][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = subtract(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = subtract(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static short[] multipliedBy(final short[] a, final short[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] * b[i]);
        }

        return result;
    }

    public static short[] multipliedBy(final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return multipliedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[] multipliedBy(final int len, final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (short) (a[i] * b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (short) (valueForNoneA * b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (short) (a[i] * valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (short) (valueForNoneA * valueForNoneB);
            }
        }

        return result;
    }

    public static short[] multipliedBy(final short[] a, final short[] b, final short[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] * b[i] * c[i]);
        }

        return result;
    }

    public static short[] multipliedBy(final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return multipliedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[] multipliedBy(final int len, final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (short) (a[i] * b[i] * c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (short) ((i < lenA ? a[i] : valueForNoneA) * (i < lenB ? b[i] : valueForNoneB) * (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static short[][] multipliedBy(final short[][] a, final short[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static short[][] multipliedBy(final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB) {
        return multipliedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[][] multipliedBy(final int len, final int rowLen, final short[][] a, final short[][] b, final short valueForNoneA,
            final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = multipliedBy(rowLen, (short[]) null, (short[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][] multipliedBy(final short[][] a, final short[][] b, final short[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][] multipliedBy(final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        return multipliedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[][] multipliedBy(final int len, final int rowLen, final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = multipliedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static short[][][] multipliedBy(final short[][][] a, final short[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static short[][][] multipliedBy(final short[][][] a, final short[][][] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = multipliedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = multipliedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][][] multipliedBy(final short[][][] a, final short[][][] b, final short[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][][] multipliedBy(final short[][][] a, final short[][][] b, final short[][][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = multipliedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] / b[i]);
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[] dividedBy(final int len, final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (short) (a[i] / b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (short) (valueForNoneA / b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (short) (a[i] / valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (short) (valueForNoneA / valueForNoneB);
            }
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b, final short[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] / b[i] / c[i]);
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[] dividedBy(final int len, final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (short) (a[i] / b[i] / c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (short) ((i < lenA ? a[i] : valueForNoneA) / (i < lenB ? b[i] : valueForNoneB) / (i < lenC ? c[i] : valueForNoneC));
            }
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static short[][] dividedBy(final int len, final int rowLen, final short[][] a, final short[][] b, final short valueForNoneA,
            final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (short[]) null, (short[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b, final short[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static short[][] dividedBy(final int len, final int rowLen, final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b, final short valueForNoneA, final short valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b, final short[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b, final short[][][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]));
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static short[] dividedBy(final int len, final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB,
            final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = (short) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]));
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = (short) (valueForNoneA / (b[i] == 0 ? defaultValueForZero : b[i]));
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = (short) (a[i] / valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = (short) (valueForNoneA / valueForNoneB);
            }
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b, final short[] c, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = (short) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]));
        }

        return result;
    }

    public static short[] dividedBy(final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
    }

    private static short[] dividedBy(final int len, final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = (short) (a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]));
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (short) ((i < lenA ? a[i] : valueForNoneA) / (i < lenB ? (b[i] == 0 ? defaultValueForZero : b[i]) : valueForNoneB)
                        / (i < lenC ? (c[i] == 0 ? defaultValueForZero : c[i]) : valueForNoneC));
            }
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB,
            final short defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static short[][] dividedBy(final int len, final int rowLen, final short[][] a, final short[][] b, final short valueForNoneA,
            final short valueForNoneB, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (short[]) null, (short[]) null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b, final short[][] c, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static short[][] dividedBy(final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final short defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                defaultValueForZero);
    }

    private static short[][] dividedBy(final int len, final int rowLen, final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC, defaultValueForZero);
            }
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b, final short valueForNoneA, final short valueForNoneB,
            final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b, final short[][][] c, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static short[][][] dividedBy(final short[][][] a, final short[][][] b, final short[][][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final short defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                    defaultValueForZero);
        }

        return result;
    }

    public static <E extends Exception> short[] zip(final short[] a, final short[] b, final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> short[] zip(final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB,
            final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> short[] zip(final int len, final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB,
            final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> short[] zip(final short[] a, final short[] b, final short[] c, final Try.ShortTriFunction<Short, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> short[] zip(final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final Try.ShortTriFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> short[] zip(final int len, final short[] a, final short[] b, final short[] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC, final Try.ShortTriFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[] result = new short[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> short[][] zip(final short[][] a, final short[][] b, final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> short[][] zip(final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB,
            final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> short[][] zip(final int len, final int rowLen, final short[][] a, final short[][] b, final short valueForNoneA,
            final short valueForNoneB, final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> short[][] zip(final short[][] a, final short[][] b, final short[][] c, final Try.ShortTriFunction<Short, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> short[][] zip(final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC, final Try.ShortTriFunction<Short, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> short[][] zip(final int len, final int rowLen, final short[][] a, final short[][] b, final short[][] c,
            final short valueForNoneA, final short valueForNoneB, final short valueForNoneC, final Try.ShortTriFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][] result = new short[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> short[][][] zip(final short[][][] a, final short[][][] b, final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> short[][][] zip(final short[][][] a, final short[][][] b, final short valueForNoneA, final short valueForNoneB,
            final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final short[][][] result = new short[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> short[][][] zip(final short[][][] a, final short[][][] b, final short[][][] c,
            final Try.ShortTriFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> short[][][] zip(final short[][][] a, final short[][][] b, final short[][][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC, final Try.ShortTriFunction<Short, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final short[][][] result = new short[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Short[] box(final short[] a) {
        return Array.box(a);
    }

    public static Short[][] box(final short[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Short[][] result = new Short[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Short[][][] box(final short[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Short[][][] result = new Short[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static short[] unbox(final Short[] a) {
        return Array.unbox(a);
    }

    public static short[] unbox(final Short[] a, final short valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static short[][] unbox(final Short[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final short[][] result = new short[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static short[][] unbox(final Short[][] a, final short valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final short[][] result = new short[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static short[][][] unbox(final Short[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final short[][][] result = new short[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static short[][][] unbox(final Short[][][] a, final short valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final short[][][] result = new short[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final short[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final short[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final short[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(short[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void plus(final int[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] += param;
        }
    }

    public static void plus(final int[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void plus(final int[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void minus(final int[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] -= param;
        }
    }

    public static void minus(final int[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void minus(final int[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void multipliedBy(final int[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] *= param;
        }
    }

    public static void multipliedBy(final int[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void multipliedBy(final int[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void dividedBy(final int[] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] /= param;
        }
    }

    public static void dividedBy(final int[][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static void dividedBy(final int[][][] a, final int param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static <E extends Exception> void replaceAll(final int[] a, final Try.IntUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsInt(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final int[][] a, final Try.IntUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final int[][][] a, final Try.IntUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final int[] a, final Try.IntPredicate<E> predicate, final int newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final int[][] a, final Try.IntPredicate<E> predicate, final int newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final int[][][] a, final Try.IntPredicate<E> predicate, final int newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static int[] flatten(final int[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final int[] c = new int[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static int[] flatten(final int[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_INT_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final int[] c = new int[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static int[][] reshape(final int[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new int[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final int[][] c = new int[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static int[][][] reshape(final int[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new int[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final int[][][] c = new int[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new int[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static int[] add(final int[] a, final int[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i];
        }

        return result;
    }

    public static int[] add(final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return add(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[] add(final int len, final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] + b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA + b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] + valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA + valueForNoneB;
            }
        }

        return result;
    }

    public static int[] add(final int[] a, final int[] b, final int[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        return result;
    }

    public static int[] add(final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return add(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[] add(final int len, final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) + (i < lenB ? b[i] : valueForNoneB) + (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static int[][] add(final int[][] a, final int[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static int[][] add(final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        return add(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[][] add(final int len, final int rowLen, final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = add(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = add(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = add(rowLen, (int[]) null, (int[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][] add(final int[][] a, final int[][] b, final int[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][] add(final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC) {
        return add(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[][] add(final int len, final int rowLen, final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA,
            final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = add(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
            }
        }

        return result;
    }

    public static int[][][] add(final int[][][] a, final int[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static int[][][] add(final int[][][] a, final int[][][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = add(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = add(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = add(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][][] add(final int[][][] a, final int[][][] b, final int[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][][] add(final int[][][] a, final int[][][] b, final int[][][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = add(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = add(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static int[] subtract(final int[] a, final int[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i];
        }

        return result;
    }

    public static int[] subtract(final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return subtract(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[] subtract(final int len, final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] - b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA - b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] - valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA - valueForNoneB;
            }
        }

        return result;
    }

    public static int[] subtract(final int[] a, final int[] b, final int[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        return result;
    }

    public static int[] subtract(final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return subtract(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[] subtract(final int len, final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) - (i < lenB ? b[i] : valueForNoneB) - (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static int[][] subtract(final int[][] a, final int[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static int[][] subtract(final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        return subtract(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[][] subtract(final int len, final int rowLen, final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = subtract(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = subtract(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = subtract(rowLen, (int[]) null, (int[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][] subtract(final int[][] a, final int[][] b, final int[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][] subtract(final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        return subtract(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[][] subtract(final int len, final int rowLen, final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA,
            final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = subtract(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static int[][][] subtract(final int[][][] a, final int[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static int[][][] subtract(final int[][][] a, final int[][][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = subtract(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = subtract(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = subtract(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][][] subtract(final int[][][] a, final int[][][] b, final int[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][][] subtract(final int[][][] a, final int[][][] b, final int[][][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = subtract(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = subtract(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static int[] multipliedBy(final int[] a, final int[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i];
        }

        return result;
    }

    public static int[] multipliedBy(final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return multipliedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[] multipliedBy(final int len, final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] * b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA * b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] * valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA * valueForNoneB;
            }
        }

        return result;
    }

    public static int[] multipliedBy(final int[] a, final int[] b, final int[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        return result;
    }

    public static int[] multipliedBy(final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return multipliedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[] multipliedBy(final int len, final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) * (i < lenB ? b[i] : valueForNoneB) * (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static int[][] multipliedBy(final int[][] a, final int[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static int[][] multipliedBy(final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        return multipliedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[][] multipliedBy(final int len, final int rowLen, final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = multipliedBy(rowLen, (int[]) null, (int[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][] multipliedBy(final int[][] a, final int[][] b, final int[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][] multipliedBy(final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        return multipliedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[][] multipliedBy(final int len, final int rowLen, final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA,
            final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = multipliedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static int[][][] multipliedBy(final int[][][] a, final int[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static int[][][] multipliedBy(final int[][][] a, final int[][][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = multipliedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = multipliedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][][] multipliedBy(final int[][][] a, final int[][][] b, final int[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][][] multipliedBy(final int[][][] a, final int[][][] b, final int[][][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = multipliedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i];
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[] dividedBy(final int len, final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b, final int[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[] dividedBy(final int len, final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? b[i] : valueForNoneB) / (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static int[][] dividedBy(final int len, final int rowLen, final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (int[]) null, (int[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b, final int[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static int[][] dividedBy(final int len, final int rowLen, final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA,
            final int valueForNoneB, final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b, final int valueForNoneA, final int valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b, final int[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b, final int[][][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static int[] dividedBy(final int len, final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB,
            final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / (b[i] == 0 ? defaultValueForZero : b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b, final int[] c, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        return result;
    }

    public static int[] dividedBy(final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC,
            final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
    }

    private static int[] dividedBy(final int len, final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? (b[i] == 0 ? defaultValueForZero : b[i]) : valueForNoneB)
                        / (i < lenC ? (c[i] == 0 ? defaultValueForZero : c[i]) : valueForNoneC);
            }
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB, final int defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static int[][] dividedBy(final int len, final int rowLen, final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB,
            final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (int[]) null, (int[]) null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b, final int[][] c, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static int[][] dividedBy(final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final int defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                defaultValueForZero);
    }

    private static int[][] dividedBy(final int len, final int rowLen, final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA,
            final int valueForNoneB, final int valueForNoneC, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC, defaultValueForZero);
            }
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b, final int valueForNoneA, final int valueForNoneB, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b, final int[][][] c, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static int[][][] dividedBy(final int[][][] a, final int[][][] b, final int[][][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final int defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                    defaultValueForZero);
        }

        return result;
    }

    public static <E extends Exception> int[] zip(final int[] a, final int[] b, final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> int[] zip(final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB,
            final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> int[] zip(final int len, final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB,
            final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> int[] zip(final int[] a, final int[] b, final int[] c, final Try.IntTriFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> int[] zip(final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final Try.IntTriFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> int[] zip(final int len, final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final Try.IntTriFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[] result = new int[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> int[][] zip(final int[][] a, final int[][] b, final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> int[][] zip(final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB,
            final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> int[][] zip(final int len, final int rowLen, final int[][] a, final int[][] b, final int valueForNoneA,
            final int valueForNoneB, final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> int[][] zip(final int[][] a, final int[][] b, final int[][] c, final Try.IntTriFunction<Integer, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> int[][] zip(final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final Try.IntTriFunction<Integer, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> int[][] zip(final int len, final int rowLen, final int[][] a, final int[][] b, final int[][] c,
            final int valueForNoneA, final int valueForNoneB, final int valueForNoneC, final Try.IntTriFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][] result = new int[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> int[][][] zip(final int[][][] a, final int[][][] b, final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> int[][][] zip(final int[][][] a, final int[][][] b, final int valueForNoneA, final int valueForNoneB,
            final Try.IntBiFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final int[][][] result = new int[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> int[][][] zip(final int[][][] a, final int[][][] b, final int[][][] c, final Try.IntTriFunction<Integer, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> int[][][] zip(final int[][][] a, final int[][][] b, final int[][][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final Try.IntTriFunction<Integer, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final int[][][] result = new int[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Integer[] box(final int[] a) {
        return Array.box(a);
    }

    public static Integer[][] box(final int[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Integer[][] result = new Integer[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Integer[][][] box(final int[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Integer[][][] result = new Integer[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static int[] unbox(final Integer[] a) {
        return Array.unbox(a);
    }

    public static int[] unbox(final Integer[] a, final int valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static int[][] unbox(final Integer[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final int[][] result = new int[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static int[][] unbox(final Integer[][] a, final int valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final int[][] result = new int[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static int[][][] unbox(final Integer[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final int[][][] result = new int[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static int[][][] unbox(final Integer[][][] a, final int valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final int[][][] result = new int[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final int[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final int[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final int[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(int[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void plus(final long[] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] += param;
        }
    }

    public static void plus(final long[][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void plus(final long[][][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void minus(final long[] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] -= param;
        }
    }

    public static void minus(final long[][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void minus(final long[][][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void multipliedBy(final long[] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] *= param;
        }
    }

    public static void multipliedBy(final long[][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void multipliedBy(final long[][][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void dividedBy(final long[] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] /= param;
        }
    }

    public static void dividedBy(final long[][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static void dividedBy(final long[][][] a, final long param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static <E extends Exception> void replaceAll(final long[] a, final Try.LongUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsLong(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final long[][] a, final Try.LongUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final long[][][] a, final Try.LongUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final long[] a, final Try.LongPredicate<E> predicate, final long newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final long[][] a, final Try.LongPredicate<E> predicate, final long newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final long[][][] a, final Try.LongPredicate<E> predicate, final long newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static long[] flatten(final long[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final long[] c = new long[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static long[] flatten(final long[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_LONG_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final long[] c = new long[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static long[][] reshape(final long[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new long[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final long[][] c = new long[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static long[][][] reshape(final long[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new long[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final long[][][] c = new long[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new long[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static long[] add(final long[] a, final long[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i];
        }

        return result;
    }

    public static long[] add(final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return add(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[] add(final int len, final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] + b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA + b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] + valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA + valueForNoneB;
            }
        }

        return result;
    }

    public static long[] add(final long[] a, final long[] b, final long[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        return result;
    }

    public static long[] add(final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB, final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return add(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[] add(final int len, final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) + (i < lenB ? b[i] : valueForNoneB) + (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static long[][] add(final long[][] a, final long[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static long[][] add(final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB) {
        return add(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[][] add(final int len, final int rowLen, final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = add(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = add(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = add(rowLen, (long[]) null, (long[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][] add(final long[][] a, final long[][] b, final long[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][] add(final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        return add(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[][] add(final int len, final int rowLen, final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = add(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
            }
        }

        return result;
    }

    public static long[][][] add(final long[][][] a, final long[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static long[][][] add(final long[][][] a, final long[][][] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = add(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = add(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = add(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][][] add(final long[][][] a, final long[][][] b, final long[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][][] add(final long[][][] a, final long[][][] b, final long[][][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = add(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = add(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static long[] subtract(final long[] a, final long[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i];
        }

        return result;
    }

    public static long[] subtract(final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return subtract(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[] subtract(final int len, final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] - b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA - b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] - valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA - valueForNoneB;
            }
        }

        return result;
    }

    public static long[] subtract(final long[] a, final long[] b, final long[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        return result;
    }

    public static long[] subtract(final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return subtract(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[] subtract(final int len, final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) - (i < lenB ? b[i] : valueForNoneB) - (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static long[][] subtract(final long[][] a, final long[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static long[][] subtract(final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB) {
        return subtract(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[][] subtract(final int len, final int rowLen, final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = subtract(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = subtract(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = subtract(rowLen, (long[]) null, (long[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][] subtract(final long[][] a, final long[][] b, final long[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][] subtract(final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        return subtract(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[][] subtract(final int len, final int rowLen, final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = subtract(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static long[][][] subtract(final long[][][] a, final long[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static long[][][] subtract(final long[][][] a, final long[][][] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = subtract(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = subtract(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = subtract(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][][] subtract(final long[][][] a, final long[][][] b, final long[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][][] subtract(final long[][][] a, final long[][][] b, final long[][][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = subtract(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = subtract(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static long[] multipliedBy(final long[] a, final long[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i];
        }

        return result;
    }

    public static long[] multipliedBy(final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return multipliedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[] multipliedBy(final int len, final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] * b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA * b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] * valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA * valueForNoneB;
            }
        }

        return result;
    }

    public static long[] multipliedBy(final long[] a, final long[] b, final long[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        return result;
    }

    public static long[] multipliedBy(final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return multipliedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[] multipliedBy(final int len, final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) * (i < lenB ? b[i] : valueForNoneB) * (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static long[][] multipliedBy(final long[][] a, final long[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static long[][] multipliedBy(final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB) {
        return multipliedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[][] multipliedBy(final int len, final int rowLen, final long[][] a, final long[][] b, final long valueForNoneA,
            final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = multipliedBy(rowLen, (long[]) null, (long[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][] multipliedBy(final long[][] a, final long[][] b, final long[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][] multipliedBy(final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        return multipliedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[][] multipliedBy(final int len, final int rowLen, final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = multipliedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static long[][][] multipliedBy(final long[][][] a, final long[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static long[][][] multipliedBy(final long[][][] a, final long[][][] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = multipliedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = multipliedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][][] multipliedBy(final long[][][] a, final long[][][] b, final long[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][][] multipliedBy(final long[][][] a, final long[][][] b, final long[][][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = multipliedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i];
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[] dividedBy(final int len, final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b, final long[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[] dividedBy(final int len, final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? b[i] : valueForNoneB) / (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static long[][] dividedBy(final int len, final int rowLen, final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (long[]) null, (long[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b, final long[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static long[][] dividedBy(final int len, final int rowLen, final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b, final long valueForNoneA, final long valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b, final long[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b, final long[][][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static long[] dividedBy(final int len, final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB,
            final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / (b[i] == 0 ? defaultValueForZero : b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b, final long[] c, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        return result;
    }

    public static long[] dividedBy(final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB, final long valueForNoneC,
            final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
    }

    private static long[] dividedBy(final int len, final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? (b[i] == 0 ? defaultValueForZero : b[i]) : valueForNoneB)
                        / (i < lenC ? (c[i] == 0 ? defaultValueForZero : c[i]) : valueForNoneC);
            }
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB, final long defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static long[][] dividedBy(final int len, final int rowLen, final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB,
            final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (long[]) null, (long[]) null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b, final long[][] c, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static long[][] dividedBy(final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final long defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                defaultValueForZero);
    }

    private static long[][] dividedBy(final int len, final int rowLen, final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC, defaultValueForZero);
            }
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b, final long valueForNoneA, final long valueForNoneB,
            final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b, final long[][][] c, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static long[][][] dividedBy(final long[][][] a, final long[][][] b, final long[][][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final long defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                    defaultValueForZero);
        }

        return result;
    }

    public static <E extends Exception> long[] zip(final long[] a, final long[] b, final Try.LongBiFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> long[] zip(final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB,
            final Try.LongBiFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> long[] zip(final int len, final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB,
            final Try.LongBiFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> long[] zip(final long[] a, final long[] b, final long[] c, final Try.LongTriFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> long[] zip(final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final Try.LongTriFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> long[] zip(final int len, final long[] a, final long[] b, final long[] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC, final Try.LongTriFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[] result = new long[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> long[][] zip(final long[][] a, final long[][] b, final Try.LongBiFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> long[][] zip(final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB,
            final Try.LongBiFunction<Long, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> long[][] zip(final int len, final int rowLen, final long[][] a, final long[][] b, final long valueForNoneA,
            final long valueForNoneB, final Try.LongBiFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> long[][] zip(final long[][] a, final long[][] b, final long[][] c, final Try.LongTriFunction<Long, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> long[][] zip(final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final Try.LongTriFunction<Long, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> long[][] zip(final int len, final int rowLen, final long[][] a, final long[][] b, final long[][] c,
            final long valueForNoneA, final long valueForNoneB, final long valueForNoneC, final Try.LongTriFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][] result = new long[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> long[][][] zip(final long[][][] a, final long[][][] b, final Try.LongBiFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> long[][][] zip(final long[][][] a, final long[][][] b, final long valueForNoneA, final long valueForNoneB,
            final Try.LongBiFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final long[][][] result = new long[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> long[][][] zip(final long[][][] a, final long[][][] b, final long[][][] c,
            final Try.LongTriFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> long[][][] zip(final long[][][] a, final long[][][] b, final long[][][] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC, final Try.LongTriFunction<Long, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final long[][][] result = new long[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Long[] box(final long[] a) {
        return Array.box(a);
    }

    public static Long[][] box(final long[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Long[][] result = new Long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Long[][][] box(final long[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Long[][][] result = new Long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static long[] unbox(final Long[] a) {
        return Array.unbox(a);
    }

    public static long[] unbox(final Long[] a, final long valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static long[][] unbox(final Long[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final long[][] result = new long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static long[][] unbox(final Long[][] a, final long valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final long[][] result = new long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static long[][][] unbox(final Long[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final long[][][] result = new long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static long[][][] unbox(final Long[][][] a, final long valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final long[][][] result = new long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final long[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final long[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final long[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(long[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void plus(final float[] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] += param;
        }
    }

    public static void plus(final float[][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void plus(final float[][][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void minus(final float[] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] -= param;
        }
    }

    public static void minus(final float[][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void minus(final float[][][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void multipliedBy(final float[] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] *= param;
        }
    }

    public static void multipliedBy(final float[][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void multipliedBy(final float[][][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void dividedBy(final float[] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] /= param;
        }
    }

    public static void dividedBy(final float[][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static void dividedBy(final float[][][] a, final float param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static <E extends Exception> void replaceAll(final float[] a, final Try.FloatUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsFloat(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final float[][] a, final Try.FloatUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final float[][][] a, final Try.FloatUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final float[] a, final Try.FloatPredicate<E> predicate, final float newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final float[][] a, final Try.FloatPredicate<E> predicate, final float newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final float[][][] a, final Try.FloatPredicate<E> predicate, final float newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static float[] flatten(final float[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final float[] c = new float[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static float[] flatten(final float[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final float[] c = new float[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static float[][] reshape(final float[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new float[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final float[][] c = new float[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static float[][][] reshape(final float[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new float[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final float[][][] c = new float[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new float[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static float[] add(final float[] a, final float[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i];
        }

        return result;
    }

    public static float[] add(final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return add(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[] add(final int len, final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] + b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA + b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] + valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA + valueForNoneB;
            }
        }

        return result;
    }

    public static float[] add(final float[] a, final float[] b, final float[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        return result;
    }

    public static float[] add(final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return add(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[] add(final int len, final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) + (i < lenB ? b[i] : valueForNoneB) + (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static float[][] add(final float[][] a, final float[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static float[][] add(final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB) {
        return add(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[][] add(final int len, final int rowLen, final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = add(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = add(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = add(rowLen, (float[]) null, (float[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][] add(final float[][] a, final float[][] b, final float[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][] add(final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        return add(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[][] add(final int len, final int rowLen, final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = add(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
            }
        }

        return result;
    }

    public static float[][][] add(final float[][][] a, final float[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static float[][][] add(final float[][][] a, final float[][][] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = add(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = add(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = add(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][][] add(final float[][][] a, final float[][][] b, final float[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][][] add(final float[][][] a, final float[][][] b, final float[][][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = add(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = add(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static float[] subtract(final float[] a, final float[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i];
        }

        return result;
    }

    public static float[] subtract(final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return subtract(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[] subtract(final int len, final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] - b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA - b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] - valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA - valueForNoneB;
            }
        }

        return result;
    }

    public static float[] subtract(final float[] a, final float[] b, final float[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        return result;
    }

    public static float[] subtract(final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return subtract(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[] subtract(final int len, final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) - (i < lenB ? b[i] : valueForNoneB) - (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static float[][] subtract(final float[][] a, final float[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static float[][] subtract(final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB) {
        return subtract(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[][] subtract(final int len, final int rowLen, final float[][] a, final float[][] b, final float valueForNoneA,
            final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = subtract(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = subtract(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = subtract(rowLen, (float[]) null, (float[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][] subtract(final float[][] a, final float[][] b, final float[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][] subtract(final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        return subtract(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[][] subtract(final int len, final int rowLen, final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = subtract(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static float[][][] subtract(final float[][][] a, final float[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static float[][][] subtract(final float[][][] a, final float[][][] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = subtract(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = subtract(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = subtract(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][][] subtract(final float[][][] a, final float[][][] b, final float[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][][] subtract(final float[][][] a, final float[][][] b, final float[][][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = subtract(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = subtract(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static float[] multipliedBy(final float[] a, final float[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i];
        }

        return result;
    }

    public static float[] multipliedBy(final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return multipliedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[] multipliedBy(final int len, final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] * b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA * b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] * valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA * valueForNoneB;
            }
        }

        return result;
    }

    public static float[] multipliedBy(final float[] a, final float[] b, final float[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        return result;
    }

    public static float[] multipliedBy(final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return multipliedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[] multipliedBy(final int len, final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) * (i < lenB ? b[i] : valueForNoneB) * (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static float[][] multipliedBy(final float[][] a, final float[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static float[][] multipliedBy(final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB) {
        return multipliedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[][] multipliedBy(final int len, final int rowLen, final float[][] a, final float[][] b, final float valueForNoneA,
            final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = multipliedBy(rowLen, (float[]) null, (float[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][] multipliedBy(final float[][] a, final float[][] b, final float[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][] multipliedBy(final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        return multipliedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[][] multipliedBy(final int len, final int rowLen, final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = multipliedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static float[][][] multipliedBy(final float[][][] a, final float[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static float[][][] multipliedBy(final float[][][] a, final float[][][] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = multipliedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = multipliedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][][] multipliedBy(final float[][][] a, final float[][][] b, final float[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][][] multipliedBy(final float[][][] a, final float[][][] b, final float[][][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = multipliedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i];
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[] dividedBy(final int len, final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b, final float[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[] dividedBy(final int len, final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? b[i] : valueForNoneB) / (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static float[][] dividedBy(final int len, final int rowLen, final float[][] a, final float[][] b, final float valueForNoneA,
            final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (float[]) null, (float[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b, final float[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static float[][] dividedBy(final int len, final int rowLen, final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b, final float valueForNoneA, final float valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b, final float[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b, final float[][][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static float[] dividedBy(final int len, final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB,
            final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / (b[i] == 0 ? defaultValueForZero : b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b, final float[] c, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        return result;
    }

    public static float[] dividedBy(final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
    }

    private static float[] dividedBy(final int len, final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? (b[i] == 0 ? defaultValueForZero : b[i]) : valueForNoneB)
                        / (i < lenC ? (c[i] == 0 ? defaultValueForZero : c[i]) : valueForNoneC);
            }
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB,
            final float defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static float[][] dividedBy(final int len, final int rowLen, final float[][] a, final float[][] b, final float valueForNoneA,
            final float valueForNoneB, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (float[]) null, (float[]) null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b, final float[][] c, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static float[][] dividedBy(final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final float defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                defaultValueForZero);
    }

    private static float[][] dividedBy(final int len, final int rowLen, final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC, defaultValueForZero);
            }
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b, final float valueForNoneA, final float valueForNoneB,
            final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b, final float[][][] c, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static float[][][] dividedBy(final float[][][] a, final float[][][] b, final float[][][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final float defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                    defaultValueForZero);
        }

        return result;
    }

    public static <E extends Exception> float[] zip(final float[] a, final float[] b, final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> float[] zip(final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB,
            final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> float[] zip(final int len, final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB,
            final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> float[] zip(final float[] a, final float[] b, final float[] c, final Try.FloatTriFunction<Float, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> float[] zip(final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final Try.FloatTriFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> float[] zip(final int len, final float[] a, final float[] b, final float[] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC, final Try.FloatTriFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[] result = new float[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> float[][] zip(final float[][] a, final float[][] b, final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> float[][] zip(final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB,
            final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> float[][] zip(final int len, final int rowLen, final float[][] a, final float[][] b, final float valueForNoneA,
            final float valueForNoneB, final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> float[][] zip(final float[][] a, final float[][] b, final float[][] c, final Try.FloatTriFunction<Float, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> float[][] zip(final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC, final Try.FloatTriFunction<Float, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> float[][] zip(final int len, final int rowLen, final float[][] a, final float[][] b, final float[][] c,
            final float valueForNoneA, final float valueForNoneB, final float valueForNoneC, final Try.FloatTriFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][] result = new float[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> float[][][] zip(final float[][][] a, final float[][][] b, final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> float[][][] zip(final float[][][] a, final float[][][] b, final float valueForNoneA, final float valueForNoneB,
            final Try.FloatBiFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final float[][][] result = new float[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> float[][][] zip(final float[][][] a, final float[][][] b, final float[][][] c,
            final Try.FloatTriFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> float[][][] zip(final float[][][] a, final float[][][] b, final float[][][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC, final Try.FloatTriFunction<Float, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final float[][][] result = new float[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Float[] box(final float[] a) {
        return Array.box(a);
    }

    public static Float[][] box(final float[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Float[][] result = new Float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Float[][][] box(final float[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Float[][][] result = new Float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static float[] unbox(final Float[] a) {
        return Array.unbox(a);
    }

    public static float[] unbox(final Float[] a, final float valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static float[][] unbox(final Float[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final float[][] result = new float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static float[][] unbox(final Float[][] a, final float valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final float[][] result = new float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static float[][][] unbox(final Float[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final float[][][] result = new float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static float[][][] unbox(final Float[][][] a, final float valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final float[][][] result = new float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final float[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final float[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final float[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(float[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void plus(final double[] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] += param;
        }
    }

    public static void plus(final double[][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void plus(final double[][][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            plus(a[i], param);
        }
    }

    public static void minus(final double[] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] -= param;
        }
    }

    public static void minus(final double[][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void minus(final double[][][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            minus(a[i], param);
        }
    }

    public static void multipliedBy(final double[] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] *= param;
        }
    }

    public static void multipliedBy(final double[][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void multipliedBy(final double[][][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            multipliedBy(a[i], param);
        }
    }

    public static void dividedBy(final double[] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] /= param;
        }
    }

    public static void dividedBy(final double[][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static void dividedBy(final double[][][] a, final double param) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            dividedBy(a[i], param);
        }
    }

    public static <E extends Exception> void replaceAll(final double[] a, final Try.DoubleUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsDouble(a[i]);
        }
    }

    public static <E extends Exception> void replaceAll(final double[][] a, final Try.DoubleUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceAll(final double[][][] a, final Try.DoubleUnaryOperator<E> operator) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <E extends Exception> void replaceIf(final double[] a, final Try.DoublePredicate<E> predicate, final double newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            if (predicate.test(a[i])) {
                a[i] = newValue;
            }
        }
    }

    public static <E extends Exception> void replaceIf(final double[][] a, final Try.DoublePredicate<E> predicate, final double newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static <E extends Exception> void replaceIf(final double[][][] a, final Try.DoublePredicate<E> predicate, final double newValue) throws E {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceIf(a[i], predicate, newValue);
        }
    }

    public static double[] flatten(final double[][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            count += (a[i] == null ? 0 : a[i].length);
        }

        final double[] c = new double[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            N.copy(a[i], 0, c, from, a[i].length);

            from += a[i].length;
        }

        return c;
    }

    public static double[] flatten(final double[][][] a) {
        if (N.isNullOrEmpty(a)) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        int count = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                count += (a[i][j] == null ? 0 : a[i][j].length);
            }
        }

        final double[] c = new double[count];
        int from = 0;

        for (int i = 0, n = a.length; i < n; i++) {
            if (N.isNullOrEmpty(a[i])) {
                continue;
            }

            for (int j = 0, m = a[i].length; j < m; j++) {
                if (N.isNullOrEmpty(a[i][j])) {
                    continue;
                }

                N.copy(a[i][j], 0, c, from, a[i][j].length);

                from += a[i][j].length;
            }
        }

        return c;
    }

    public static double[][] reshape(final double[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        if (N.isNullOrEmpty(a)) {
            return new double[0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m, RoundingMode.CEILING);
        final double[][] c = new double[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static double[][][] reshape(final double[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, "'m'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new double[0][0][0];
        }

        final int len = a.length;
        final int n = Maths.divide(len, m * l, RoundingMode.CEILING);
        final double[][][] c = new double[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new double[N.min(m, Maths.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static double[] add(final double[] a, final double[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i];
        }

        return result;
    }

    public static double[] add(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return add(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[] add(final int len, final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] + b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA + b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] + valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA + valueForNoneB;
            }
        }

        return result;
    }

    public static double[] add(final double[] a, final double[] b, final double[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        return result;
    }

    public static double[] add(final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return add(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[] add(final int len, final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] + b[i] + c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) + (i < lenB ? b[i] : valueForNoneB) + (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static double[][] add(final double[][] a, final double[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static double[][] add(final double[][] a, final double[][] b, final double valueForNoneA, final double valueForNoneB) {
        return add(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[][] add(final int len, final int rowLen, final double[][] a, final double[][] b, final double valueForNoneA,
            final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = add(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = add(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = add(rowLen, (double[]) null, (double[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][] add(final double[][] a, final double[][] b, final double[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][] add(final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        return add(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[][] add(final int len, final int rowLen, final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = add(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = add(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
            }
        }

        return result;
    }

    public static double[][][] add(final double[][][] a, final double[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i]);
        }

        return result;
    }

    public static double[][][] add(final double[][][] a, final double[][][] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = add(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = add(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = add(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][][] add(final double[][][] a, final double[][][] b, final double[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = add(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][][] add(final double[][][] a, final double[][][] b, final double[][][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = add(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = add(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static double[] subtract(final double[] a, final double[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i];
        }

        return result;
    }

    public static double[] subtract(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return subtract(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[] subtract(final int len, final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] - b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA - b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] - valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA - valueForNoneB;
            }
        }

        return result;
    }

    public static double[] subtract(final double[] a, final double[] b, final double[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        return result;
    }

    public static double[] subtract(final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return subtract(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[] subtract(final int len, final double[] a, final double[] b, final double[] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] - b[i] - c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) - (i < lenB ? b[i] : valueForNoneB) - (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static double[][] subtract(final double[][] a, final double[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static double[][] subtract(final double[][] a, final double[][] b, final double valueForNoneA, final double valueForNoneB) {
        return subtract(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[][] subtract(final int len, final int rowLen, final double[][] a, final double[][] b, final double valueForNoneA,
            final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = subtract(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = subtract(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = subtract(rowLen, (double[]) null, (double[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][] subtract(final double[][] a, final double[][] b, final double[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][] subtract(final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        return subtract(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[][] subtract(final int len, final int rowLen, final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = subtract(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = subtract(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static double[][][] subtract(final double[][][] a, final double[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i]);
        }

        return result;
    }

    public static double[][][] subtract(final double[][][] a, final double[][][] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = subtract(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = subtract(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = subtract(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][][] subtract(final double[][][] a, final double[][][] b, final double[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = subtract(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][][] subtract(final double[][][] a, final double[][][] b, final double[][][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = subtract(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = subtract(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static double[] multipliedBy(final double[] a, final double[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i];
        }

        return result;
    }

    public static double[] multipliedBy(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return multipliedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[] multipliedBy(final int len, final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] * b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA * b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] * valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA * valueForNoneB;
            }
        }

        return result;
    }

    public static double[] multipliedBy(final double[] a, final double[] b, final double[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        return result;
    }

    public static double[] multipliedBy(final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return multipliedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[] multipliedBy(final int len, final double[] a, final double[] b, final double[] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] * b[i] * c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) * (i < lenB ? b[i] : valueForNoneB) * (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static double[][] multipliedBy(final double[][] a, final double[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static double[][] multipliedBy(final double[][] a, final double[][] b, final double valueForNoneA, final double valueForNoneB) {
        return multipliedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[][] multipliedBy(final int len, final int rowLen, final double[][] a, final double[][] b, final double valueForNoneA,
            final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = multipliedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = multipliedBy(rowLen, (double[]) null, (double[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][] multipliedBy(final double[][] a, final double[][] b, final double[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][] multipliedBy(final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        return multipliedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[][] multipliedBy(final int len, final int rowLen, final double[][] a, final double[][] b, final double[][] c,
            final double valueForNoneA, final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = multipliedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = multipliedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static double[][][] multipliedBy(final double[][][] a, final double[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i]);
        }

        return result;
    }

    public static double[][][] multipliedBy(final double[][][] a, final double[][][] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = multipliedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = multipliedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][][] multipliedBy(final double[][][] a, final double[][][] b, final double[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][][] multipliedBy(final double[][][] a, final double[][][] b, final double[][][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = multipliedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = multipliedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i];
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[] dividedBy(final int len, final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / b[i];
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / b[i];
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b, final double[] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[] dividedBy(final int len, final double[] a, final double[] b, final double[] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / b[i] / c[i];
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? b[i] : valueForNoneB) / (i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b, final double valueForNoneA, final double valueForNoneB) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB);
    }

    private static double[][] dividedBy(final int len, final int rowLen, final double[][] a, final double[][] b, final double valueForNoneA,
            final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (double[]) null, (double[]) null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b, final double[][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC);
    }

    private static double[][] dividedBy(final int len, final int rowLen, final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC);
            }
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i]);
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b, final double valueForNoneA, final double valueForNoneB) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b, final double[][][] c) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b, final double[][][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC);
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB,
            final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return dividedBy(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static double[] dividedBy(final int len, final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB,
            final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = valueForNoneA / (b[i] == 0 ? defaultValueForZero : b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = a[i] / valueForNoneB;
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = valueForNoneA / valueForNoneB;
            }
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b, final double[] c, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        return result;
    }

    public static double[] dividedBy(final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return dividedBy(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
    }

    private static double[] dividedBy(final int len, final double[] a, final double[] b, final double[] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = a[i] / (b[i] == 0 ? defaultValueForZero : b[i]) / (c[i] == 0 ? defaultValueForZero : c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = (i < lenA ? a[i] : valueForNoneA) / (i < lenB ? (b[i] == 0 ? defaultValueForZero : b[i]) : valueForNoneB)
                        / (i < lenC ? (c[i] == 0 ? defaultValueForZero : c[i]) : valueForNoneC);
            }
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b, final double valueForNoneA, final double valueForNoneB,
            final double defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, defaultValueForZero);
    }

    private static double[][] dividedBy(final int len, final int rowLen, final double[][] a, final double[][] b, final double valueForNoneA,
            final double valueForNoneB, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = dividedBy(rowLen, null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = dividedBy(rowLen, a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = dividedBy(rowLen, (double[]) null, (double[]) null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b, final double[][] c, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static double[][] dividedBy(final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final double defaultValueForZero) {
        return dividedBy(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                defaultValueForZero);
    }

    private static double[][] dividedBy(final int len, final int rowLen, final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = dividedBy(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = dividedBy(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB,
                        valueForNoneC, defaultValueForZero);
            }
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], defaultValueForZero);
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b, final double valueForNoneA, final double valueForNoneB,
            final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = dividedBy(null, b[i], valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = dividedBy(a[i], null, valueForNoneA, valueForNoneB, defaultValueForZero);
            }
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b, final double[][][] c, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], defaultValueForZero);
        }

        return result;
    }

    public static double[][][] dividedBy(final double[][][] a, final double[][][] b, final double[][][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final double defaultValueForZero) {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = dividedBy(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, defaultValueForZero);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = dividedBy(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                    defaultValueForZero);
        }

        return result;
    }

    public static <E extends Exception> double[] zip(final double[] a, final double[] b, final Try.DoubleBiFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <E extends Exception> double[] zip(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB,
            final Try.DoubleBiFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> double[] zip(final int len, final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB,
            final Try.DoubleBiFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zipFunction.apply(valueForNoneA, b[i]);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zipFunction.apply(a[i], valueForNoneB);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zipFunction.apply(valueForNoneA, valueForNoneB);
            }
        }

        return result;
    }

    public static <E extends Exception> double[] zip(final double[] a, final double[] b, final double[] c, final Try.DoubleTriFunction<Double, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <E extends Exception> double[] zip(final double[] a, final double[] b, final double[] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final Try.DoubleTriFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <E extends Exception> double[] zip(final int len, final double[] a, final double[] b, final double[] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final Try.DoubleTriFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[] result = new double[len];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC);
            }
        }

        return result;
    }

    public static <E extends Exception> double[][] zip(final double[][] a, final double[][] b, final Try.DoubleBiFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> double[][] zip(final double[][] a, final double[][] b, final double valueForNoneA, final double valueForNoneB,
            final Try.DoubleBiFunction<Double, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <E extends Exception> double[][] zip(final int len, final int rowLen, final double[][] a, final double[][] b, final double valueForNoneA,
            final double valueForNoneB, final Try.DoubleBiFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB && lenA < len) {
            for (int i = lenA, min = N.min(lenB, len); i < min; i++) {
                result[i] = zip(rowLen, null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA && lenB < len) {
            for (int i = lenB, min = N.min(lenA, len); i < min; i++) {
                result[i] = zip(rowLen, a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        if (N.max(lenA, lenB) < len) {
            for (int i = N.max(lenA, lenB); i < len; i++) {
                result[i] = zip(rowLen, null, null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> double[][] zip(final double[][] a, final double[][] b, final double[][] c,
            final Try.DoubleTriFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> double[][] zip(final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final Try.DoubleTriFunction<Double, E> zipFunction) throws E {
        return zip(N.max(N.len(a), N.len(b), N.len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <E extends Exception> double[][] zip(final int len, final int rowLen, final double[][] a, final double[][] b, final double[][] c,
            final double valueForNoneA, final double valueForNoneB, final double valueForNoneC, final Try.DoubleTriFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][] result = new double[len][];

        for (int i = 0, min = N.min(lenA, lenB, lenC, len); i < min; i++) {
            result[i] = zip(rowLen, a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        if (N.min(lenA, lenB, lenC) < len) {
            for (int i = N.min(lenA, lenB, lenC); i < len; i++) {
                result[i] = zip(rowLen, i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC,
                        zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> double[][][] zip(final double[][][] a, final double[][][] b, final Try.DoubleBiFunction<Double, E> zipFunction)
            throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> double[][][] zip(final double[][][] a, final double[][][] b, final double valueForNoneA, final double valueForNoneB,
            final Try.DoubleBiFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);

        final double[][][] result = new double[N.max(lenA, lenB)][][];

        for (int i = 0, min = N.min(lenA, lenB); i < min; i++) {
            result[i] = zip(a[i], b[i], valueForNoneA, valueForNoneB, zipFunction);
        }

        if (lenA < lenB) {
            for (int i = lenA; i < lenB; i++) {
                result[i] = zip(null, b[i], valueForNoneA, valueForNoneB, zipFunction);
            }
        } else if (lenB < lenA) {
            for (int i = lenB; i < lenA; i++) {
                result[i] = zip(a[i], null, valueForNoneA, valueForNoneB, zipFunction);
            }
        }

        return result;
    }

    public static <E extends Exception> double[][][] zip(final double[][][] a, final double[][][] b, final double[][][] c,
            final Try.DoubleTriFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <E extends Exception> double[][][] zip(final double[][][] a, final double[][][] b, final double[][][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final Try.DoubleTriFunction<Double, E> zipFunction) throws E {
        final int lenA = N.len(a);
        final int lenB = N.len(b);
        final int lenC = N.len(c);

        final double[][][] result = new double[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static Double[] box(final double[] a) {
        return Array.box(a);
    }

    public static Double[][] box(final double[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Double[][] result = new Double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static Double[][][] box(final double[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final Double[][][] result = new Double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = box(a[i]);
        }

        return result;
    }

    public static double[] unbox(final Double[] a) {
        return Array.unbox(a);
    }

    public static double[] unbox(final Double[] a, final double valueForNul) {
        return Array.unbox(a, valueForNul);
    }

    public static double[][] unbox(final Double[][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static double[][] unbox(final Double[][] a, final double valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static double[][][] unbox(final Double[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i]);
        }

        return result;
    }

    public static double[][][] unbox(final Double[][][] a, final double valueForNul) {
        if (a == null) {
            return null;
        }

        final int len = a.length;
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = unbox(a[i], valueForNul);
        }

        return result;
    }

    public static void println(final double[] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            N.println(a);
        }
    }

    public static void println(final double[][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                println(a[i]);
            }
        }
    }

    public static void println(final double[][][] a) {
        if (a == null) {
            N.println("null");
        } else if (a.length == 0) {
            N.println("[]");
        } else {
            final int len = a.length;
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static int maxLen(double[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = a.length;
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static boolean[] toBoolean(final byte[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[] result = new boolean[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i] > 0;
        }

        return result;
    }

    public static boolean[][] toBoolean(final byte[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][] result = new boolean[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toBoolean(a[i]);
        }

        return result;
    }

    public static boolean[][][] toBoolean(final byte[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][][] result = new boolean[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toBoolean(a[i]);
        }

        return result;
    }

    public static boolean[] toBoolean(final int[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[] result = new boolean[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i] > 0;
        }

        return result;
    }

    public static boolean[][] toBoolean(final int[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][] result = new boolean[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toBoolean(a[i]);
        }

        return result;
    }

    public static boolean[][][] toBoolean(final int[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final boolean[][][] result = new boolean[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toBoolean(a[i]);
        }

        return result;
    }

    public static byte[] toByte(final boolean[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final byte[] result = new byte[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i] ? BYTE_1 : BYTE_0;
        }

        return result;
    }

    public static byte[][] toByte(final boolean[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final byte[][] result = new byte[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toByte(a[i]);
        }

        return result;
    }

    public static byte[][][] toByte(final boolean[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final byte[][][] result = new byte[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toByte(a[i]);
        }

        return result;
    }

    public static int[] toInt(final boolean[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[] result = new int[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i] ? 1 : 0;
        }

        return result;
    }

    public static int[][] toInt(final boolean[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][] result = new int[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static int[][][] toInt(final boolean[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][][] result = new int[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static int[] toInt(final char[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[] result = new int[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static int[][] toInt(final char[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][] result = new int[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static int[][][] toInt(final char[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][][] result = new int[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static int[] toInt(final byte[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[] result = new int[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static int[][] toInt(final byte[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][] result = new int[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static int[][][] toInt(final byte[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][][] result = new int[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static int[] toInt(final short[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[] result = new int[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static int[][] toInt(final short[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][] result = new int[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static int[][][] toInt(final short[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final int[][][] result = new int[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static long[] toLong(final boolean[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[] result = new long[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i] ? 1 : 0;
        }

        return result;
    }

    public static long[][] toLong(final boolean[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][] result = new long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[][][] toLong(final boolean[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][][] result = new long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[] toLong(final char[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[] result = new long[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static long[][] toLong(final char[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][] result = new long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[][][] toLong(final char[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][][] result = new long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[] toLong(final byte[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[] result = new long[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static long[][] toLong(final byte[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][] result = new long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[][][] toLong(final byte[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][][] result = new long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[] toLong(final short[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[] result = new long[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static long[][] toLong(final short[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][] result = new long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[][][] toLong(final short[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][][] result = new long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[] toLong(final int[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[] result = new long[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static long[][] toLong(final int[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][] result = new long[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static long[][][] toLong(final int[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final long[][][] result = new long[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toLong(a[i]);
        }

        return result;
    }

    public static float[] toFloat(final char[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[] result = new float[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static float[][] toFloat(final char[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][] result = new float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[][][] toFloat(final char[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][][] result = new float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[] toFloat(final byte[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[] result = new float[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static float[][] toFloat(final byte[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][] result = new float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[][][] toFloat(final byte[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][][] result = new float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[] toFloat(final short[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[] result = new float[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static float[][] toFloat(final short[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][] result = new float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[][][] toFloat(final short[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][][] result = new float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[] toFloat(final int[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[] result = new float[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static float[][] toFloat(final int[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][] result = new float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[][][] toFloat(final int[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][][] result = new float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[] toFloat(final long[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[] result = new float[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static float[][] toFloat(final long[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][] result = new float[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static float[][][] toFloat(final long[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final float[][][] result = new float[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toFloat(a[i]);
        }

        return result;
    }

    public static double[] toDouble(final char[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[] result = new double[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static double[][] toDouble(final char[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[][][] toDouble(final char[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[] toDouble(final byte[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[] result = new double[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static double[][] toDouble(final byte[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[][][] toDouble(final byte[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[] toDouble(final short[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[] result = new double[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static double[][] toDouble(final short[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[][][] toDouble(final short[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[] toDouble(final int[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[] result = new double[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static double[][] toDouble(final int[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[][][] toDouble(final int[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[] toDouble(final long[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[] result = new double[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static double[][] toDouble(final long[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[][][] toDouble(final long[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[] toDouble(final float[] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[] result = new double[len];

        for (int i = 0; i < len; i++) {
            result[i] = a[i];
        }

        return result;
    }

    public static double[][] toDouble(final float[][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][] result = new double[len][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }

    public static double[][][] toDouble(final float[][][] a) {
        if (a == null) {
            return null;
        }

        final int len = N.len(a);
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }
}
