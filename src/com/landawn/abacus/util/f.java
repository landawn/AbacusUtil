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
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BooleanBiFunction;
import com.landawn.abacus.util.function.BooleanFunction;
import com.landawn.abacus.util.function.BooleanTriFunction;
import com.landawn.abacus.util.function.BooleanUnaryOperator;
import com.landawn.abacus.util.function.ByteBiFunction;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.ByteTriFunction;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.CharBiFunction;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharTriFunction;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.FloatBiFunction;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatTriFunction;
import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntBiFunction;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntTriFunction;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.LongBiFunction;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongTriFunction;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortTriFunction;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.UnaryOperator;

@Beta
public final class f {

    private static final String ARRAY_PRINT_SEPERATOR = N.repeat('-', 80);

    private f() {
        // singleton.
    }

    public static <T> void replaceAll(final T[] a, final UnaryOperator<T> operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.apply(a[i]);
        }
    }

    public static <T> void replaceAll(final T[][] a, final UnaryOperator<T> operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <T> void replaceAll(final T[][][] a, final UnaryOperator<T> operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static <T> T[] flatten(final T[][] a) {
        //        if (N.isNullOrEmpty(a)) {
        //            return N.EMPTY_BOOLEAN_ARRAY;
        //        }

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

    public static <T> T[] flatten(final T[][][] a) {
        //        if (N.isNullOrEmpty(a)) {
        //            return N.EMPTY_BOOLEAN_ARRAY;
        //        }

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

    public static <T> T[][] reshape(final T[] a, final int m) {
        N.checkArgument(m > 0, "'m' must be positive number: m = %s", m);

        //        if (N.isNullOrEmpty(a)) {
        //            return new T[0][0];
        //        }

        final int len = a.length;
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final T[][] c = N.newArray(a.getClass(), n);

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static <T> T[][][] reshape(final T[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        //        if (N.isNullOrEmpty(a)) {
        //            return new T[0][0][0];
        //        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final T[][][] c = N.newArray(N.newArray(a.getClass(), 0).getClass(), n);

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = N.newArray(a.getClass(), N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING)));

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static <T> T[] map(final T[] a, final Function<? super T, T> func) {
        if (a == null) {
            return null;
        }

        return map((Class<T>) a.getClass().getComponentType(), a, func);
    }

    public static <T, R> R[] map(final Class<R> cls, final T[] a, final Function<? super T, R> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final R[] c = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            c[i] = func.apply(a[i]);
        }

        return c;
    }

    public static <T> T[][] map(final T[][] a, final Function<? super T, T> func) {
        if (a == null) {
            return null;
        }

        return map((Class<T>) a.getClass().getComponentType().getComponentType(), a, func);
    }

    public static <T, R> R[][] map(final Class<R> cls, final T[][] a, final Function<? super T, R> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final R[][] c = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            c[i] = map(cls, a[i], func);
        }

        return c;
    }

    public static <T> T[][][] map(final T[][][] a, final Function<? super T, T> func) {
        if (a == null) {
            return null;
        }

        return map((Class<T>) a.getClass().getComponentType().getComponentType().getComponentType(), a, func);
    }

    public static <T, R> R[][][] map(final Class<R> cls, final T[][][] a, final Function<? super T, R> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final R[][][] c = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            c[i] = map(cls, a[i], func);
        }

        return c;
    }

    public static <T> boolean[] mapToBoolean(final T[] a, final ToBooleanFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final boolean[] c = new boolean[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsBoolean(a[i]);
        }

        return c;
    }

    public static <T> boolean[][] mapToBoolean(final T[][] a, final ToBooleanFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final boolean[][] c = new boolean[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToBoolean(a[i], func);
        }

        return c;
    }

    public static <T> boolean[][][] mapToBoolean(final T[][][] a, final ToBooleanFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final boolean[][][] c = new boolean[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToBoolean(a[i], func);
        }

        return c;
    }

    public static <T> char[] mapToChar(final T[] a, final ToCharFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final char[] c = new char[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsChar(a[i]);
        }

        return c;
    }

    public static <T> char[][] mapToChar(final T[][] a, final ToCharFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final char[][] c = new char[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToChar(a[i], func);
        }

        return c;
    }

    public static <T> char[][][] mapToChar(final T[][][] a, final ToCharFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final char[][][] c = new char[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToChar(a[i], func);
        }

        return c;
    }

    public static <T> byte[] mapToByte(final T[] a, final ToByteFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final byte[] c = new byte[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsByte(a[i]);
        }

        return c;
    }

    public static <T> byte[][] mapToByte(final T[][] a, final ToByteFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final byte[][] c = new byte[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToByte(a[i], func);
        }

        return c;
    }

    public static <T> byte[][][] mapToByte(final T[][][] a, final ToByteFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final byte[][][] c = new byte[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToByte(a[i], func);
        }

        return c;
    }

    public static <T> short[] mapToShort(final T[] a, final ToShortFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final short[] c = new short[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsShort(a[i]);
        }

        return c;
    }

    public static <T> short[][] mapToShort(final T[][] a, final ToShortFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final short[][] c = new short[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToShort(a[i], func);
        }

        return c;
    }

    public static <T> short[][][] mapToShort(final T[][][] a, final ToShortFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final short[][][] c = new short[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToShort(a[i], func);
        }

        return c;
    }

    public static <T> int[] mapToInt(final T[] a, final ToIntFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final int[] c = new int[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsInt(a[i]);
        }

        return c;
    }

    public static <T> int[][] mapToInt(final T[][] a, final ToIntFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final int[][] c = new int[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToInt(a[i], func);
        }

        return c;
    }

    public static <T> int[][][] mapToInt(final T[][][] a, final ToIntFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final int[][][] c = new int[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToInt(a[i], func);
        }

        return c;
    }

    public static <T> long[] mapToLong(final T[] a, final ToLongFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final long[] c = new long[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsLong(a[i]);
        }

        return c;
    }

    public static <T> long[][] mapToLong(final T[][] a, final ToLongFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final long[][] c = new long[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToLong(a[i], func);
        }

        return c;
    }

    public static <T> long[][][] mapToLong(final T[][][] a, final ToLongFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final long[][][] c = new long[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToLong(a[i], func);
        }

        return c;
    }

    public static <T> float[] mapToFloat(final T[] a, final ToFloatFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final float[] c = new float[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsFloat(a[i]);
        }

        return c;
    }

    public static <T> float[][] mapToFloat(final T[][] a, final ToFloatFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final float[][] c = new float[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToFloat(a[i], func);
        }

        return c;
    }

    public static <T> float[][][] mapToFloat(final T[][][] a, final ToFloatFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final float[][][] c = new float[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToFloat(a[i], func);
        }

        return c;
    }

    public static <T> double[] mapToDouble(final T[] a, final ToDoubleFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final double[] c = new double[len];

        for (int i = 0; i < len; i++) {
            c[i] = func.applyAsDouble(a[i]);
        }

        return c;
    }

    public static <T> double[][] mapToDouble(final T[][] a, final ToDoubleFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final double[][] c = new double[len][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToDouble(a[i], func);
        }

        return c;
    }

    public static <T> double[][][] mapToDouble(final T[][][] a, final ToDoubleFunction<? super T> func) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final double[][][] c = new double[len][][];

        for (int i = 0; i < len; i++) {
            c[i] = mapToDouble(a[i], func);
        }

        return c;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final boolean[] a, final BooleanFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final boolean[][] a, final BooleanFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final boolean[][][] a, final BooleanFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final char[] a, final CharFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final char[][] a, final CharFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final char[][][] a, final CharFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final byte[] a, final ByteFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final byte[][] a, final ByteFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final byte[][][] a, final ByteFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final short[] a, final ShortFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final short[][] a, final ShortFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final short[][][] a, final ShortFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final int[] a, final IntFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final int[][] a, final IntFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final int[][][] a, final IntFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final long[] a, final LongFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final long[][] a, final LongFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final long[][][] a, final LongFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final float[] a, final FloatFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final float[][] a, final FloatFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final float[][][] a, final FloatFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[] mapToObj(final Class<T> cls, final double[] a, final DoubleFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[] result = N.newArray(cls, len);

        for (int i = 0; i < len; i++) {
            result[i] = mapper.apply(a[i]);
        }

        return result;
    }

    public static <T> T[][] mapToObj(final Class<T> cls, final double[][] a, final DoubleFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][] result = N.newArray(N.newArray(cls, 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <T> T[][][] mapToObj(final Class<T> cls, final double[][][] a, final DoubleFunction<? extends T> mapper) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
        final T[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), len);

        for (int i = 0; i < len; i++) {
            result[i] = mapToObj(cls, a[i], mapper);
        }

        return result;
    }

    public static <A, B> A[] zip(final A[] a, final B[] b, final BiFunction<? super A, ? super B, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, zipFunction);
    }

    public static <A, B, R> R[] zip(final Class<R> cls, final A[] a, final B[] b, final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final R[] result = N.newArray(cls, N.min(lenA, lenB));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static <A, B> A[] zip(final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R> R[] zip(final Class<R> cls, final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), cls, a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <A, B, R> R[] zip(final int len, final Class<R> cls, final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static <A, B, C> A[] zip(final A[] a, final B[] b, final C[] c, final TriFunction<? super A, ? super B, ? super C, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, c, zipFunction);
    }

    public static <A, B, C, R> R[] zip(final Class<R> cls, final A[] a, final B[] b, final C[] c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final R[] result = N.newArray(cls, N.min(lenA, lenB, lenC));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static <A, B, C> A[] zip(final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super A, ? super B, ? super C, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType(), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public static <A, B, C, R> R[] zip(final Class<R> cls, final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), cls, a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static <A, B, C, R> R[] zip(final int len, final Class<R> cls, final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static <A, B> A[][] zip(final A[][] a, final B[][] b, final BiFunction<? super A, ? super B, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, zipFunction);
    }

    public static <A, B, R> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b, final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), N.min(lenA, lenB));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <A, B> A[][] zip(final A[][] a, final B[][] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), cls, a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static <A, B, R> R[][] zip(final int len, final int rowLen, final Class<R> cls, final A[][] a, final B[][] b, final A valueForNoneA,
            final B valueForNoneB, final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static <A, B, C> A[][] zip(final A[][] a, final B[][] b, final C[][] c, final TriFunction<? super A, ? super B, ? super C, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, c, zipFunction);
    }

    public static <A, B, C, R> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b, final C[][] c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), N.min(lenA, lenB, lenC));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <A, B, C> A[][] zip(final A[][] a, final B[][] b, final C[][] c, final A valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super A, ? super B, ? super C, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType(), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public static <A, B, C, R> R[][] zip(final Class<R> cls, final A[][] a, final B[][] b, final C[][] c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), cls, a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    private static <A, B, C, R> R[][] zip(final int len, final int rowLen, final Class<R> cls, final A[][] a, final B[][] b, final C[][] c,
            final A valueForNoneA, final B valueForNoneB, final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static <A, B> A[][][] zip(final A[][][] a, final B[][][] b, final BiFunction<? super A, ? super B, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, zipFunction);
    }

    public static <A, B, R> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b, final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final R[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), N.min(lenA, lenB));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], zipFunction);
        }

        return result;
    }

    public static <A, B> A[][][] zip(final A[][][] a, final B[][][] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static <A, B, C> A[][][] zip(final A[][][] a, final B[][][] b, final C[][][] c, final TriFunction<? super A, ? super B, ? super C, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, c, zipFunction);
    }

    public static <A, B, C, R> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b, final C[][][] c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final R[][][] result = N.newArray(N.newArray(N.newArray(cls, 0).getClass(), 0).getClass(), N.min(lenA, lenB, lenC));

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(cls, a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static <A, B, C> A[][][] zip(final A[][][] a, final B[][][] b, final C[][][] c, final A valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super A, ? super B, ? super C, A> zipFunction) {
        return zip((Class<A>) a.getClass().getComponentType().getComponentType().getComponentType(), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC,
                zipFunction);
    }

    public static <A, B, C, R> R[][][] zip(final Class<R> cls, final A[][][] a, final B[][][] b, final C[][][] c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static <T> Matrix<T> matrix(final T[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static <T> Matrix<T> matrix(final T[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static <T> Matrix<T> matrix(final T[][] a, final int n, final int m) {
        return matrix(a, n, m, null);
    }

    public static <T> Matrix<T> matrix(final T[][] a, final int n, final int m, final T valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final Class<T> componentType = (Class<T>) a.getClass().getComponentType().getComponentType();
        final T[][] c = N.newArray(a.getClass().getComponentType(), n);

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = N.newArray(componentType, m);

                if (m > 0 && valueForDefault != null) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != null) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return Matrix.of(c);
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

        final int len = len(a);
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

        final int len = len(a);
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
            final int len = len(a);
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
            final int len = len(a);
            for (int i = 0; i < len; i++) {
                if (i > 0) {
                    N.println(ARRAY_PRINT_SEPERATOR);
                }

                println(a[i]);
            }
        }
    }

    private static <T> int len(T[] a) {
        return a == null ? 0 : a.length;
    }

    private static <T> int len(T[][] a) {
        return a == null ? 0 : a.length;
    }

    private static <T> int len(T[][][] a) {
        return a == null ? 0 : a.length;
    }

    private static <T> int maxLen(T[][] a) {
        if (a == null) {
            return 0;
        }

        final int len = len(a);
        int maxLen = 0;

        for (int i = 0; i < len; i++) {
            maxLen = N.max(maxLen, a[i] == null ? 0 : a[i].length);
        }

        return maxLen;
    }

    public static void replaceAll(final boolean[] a, final BooleanUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsBoolean(a[i]);
        }
    }

    public static void replaceAll(final boolean[][] a, final BooleanUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final boolean[][][] a, final BooleanUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final boolean[][] c = new boolean[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static boolean[][][] reshape(final boolean[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new boolean[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final boolean[][][] c = new boolean[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new boolean[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static boolean[] zip(final boolean[] a, final boolean[] b, final BooleanBiFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final boolean[] result = new boolean[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static boolean[] zip(final boolean[] a, final boolean[] b, final boolean valueForNoneA, final boolean valueForNoneB,
            final BooleanBiFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static boolean[] zip(final int len, final boolean[] a, final boolean[] b, final boolean valueForNoneA, final boolean valueForNoneB,
            final BooleanBiFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static boolean[] zip(final boolean[] a, final boolean[] b, final boolean[] c, final BooleanTriFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final boolean[] result = new boolean[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static boolean[] zip(final boolean[] a, final boolean[] b, final boolean[] c, final boolean valueForNoneA, final boolean valueForNoneB,
            final boolean valueForNoneC, final BooleanTriFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static boolean[] zip(final int len, final boolean[] a, final boolean[] b, final boolean[] c, final boolean valueForNoneA,
            final boolean valueForNoneB, final boolean valueForNoneC, final BooleanTriFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static boolean[][] zip(final boolean[][] a, final boolean[][] b, final BooleanBiFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final boolean[][] result = new boolean[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static boolean[][] zip(final boolean[][] a, final boolean[][] b, final boolean valueForNoneA, final boolean valueForNoneB,
            final BooleanBiFunction<Boolean> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static boolean[][] zip(final int len, final int rowLen, final boolean[][] a, final boolean[][] b, final boolean valueForNoneA,
            final boolean valueForNoneB, final BooleanBiFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static boolean[][] zip(final boolean[][] a, final boolean[][] b, final boolean[][] c, final BooleanTriFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final boolean[][] result = new boolean[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static boolean[][] zip(final boolean[][] a, final boolean[][] b, final boolean[][] c, final boolean valueForNoneA, final boolean valueForNoneB,
            final boolean valueForNoneC, final BooleanTriFunction<Boolean> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static boolean[][] zip(final int len, final int rowLen, final boolean[][] a, final boolean[][] b, final boolean[][] c, final boolean valueForNoneA,
            final boolean valueForNoneB, final boolean valueForNoneC, final BooleanTriFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final BooleanBiFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final boolean[][][] result = new boolean[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final boolean valueForNoneA, final boolean valueForNoneB,
            final BooleanBiFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final boolean[][][] c, final BooleanTriFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final boolean[][][] result = new boolean[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static boolean[][][] zip(final boolean[][][] a, final boolean[][][] b, final boolean[][][] c, final boolean valueForNoneA,
            final boolean valueForNoneB, final boolean valueForNoneC, final BooleanTriFunction<Boolean> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final boolean[][][] result = new boolean[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static BooleanMatrix matrix(final boolean[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static BooleanMatrix matrix(final boolean[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static BooleanMatrix matrix(final boolean[][] a, final int n, final int m) {
        return matrix(a, n, m, false);
    }

    public static BooleanMatrix matrix(final boolean[][] a, final int n, final int m, final boolean valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final boolean[][] c = new boolean[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new boolean[m];

                if (m > 0 && valueForDefault != false) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != false) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return BooleanMatrix.of(c);
    }

    public static Boolean[] box(final boolean[] a) {
        return Array.box(a);
    }

    public static Boolean[][] box(final boolean[][] a) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

    private static int len(boolean[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(boolean[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(boolean[][][] a) {
        return a == null ? 0 : a.length;
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

    public static void replaceAll(final char[] a, final CharUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsChar(a[i]);
        }
    }

    public static void replaceAll(final char[][] a, final CharUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final char[][][] a, final CharUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final char[][] c = new char[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static char[][][] reshape(final char[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new char[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final char[][][] c = new char[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new char[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static char[] zip(final char[] a, final char[] b, final CharBiFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final char[] result = new char[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static char[] zip(final char[] a, final char[] b, final char valueForNoneA, final char valueForNoneB, final CharBiFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static char[] zip(final int len, final char[] a, final char[] b, final char valueForNoneA, final char valueForNoneB,
            final CharBiFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static char[] zip(final char[] a, final char[] b, final char[] c, final CharTriFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final char[] result = new char[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static char[] zip(final char[] a, final char[] b, final char[] c, final char valueForNoneA, final char valueForNoneB, final char valueForNoneC,
            final CharTriFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static char[] zip(final int len, final char[] a, final char[] b, final char[] c, final char valueForNoneA, final char valueForNoneB,
            final char valueForNoneC, final CharTriFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static char[][] zip(final char[][] a, final char[][] b, final CharBiFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final char[][] result = new char[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static char[][] zip(final char[][] a, final char[][] b, final char valueForNoneA, final char valueForNoneB,
            final CharBiFunction<Character> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static char[][] zip(final int len, final int rowLen, final char[][] a, final char[][] b, final char valueForNoneA, final char valueForNoneB,
            final CharBiFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static char[][] zip(final char[][] a, final char[][] b, final char[][] c, final CharTriFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final char[][] result = new char[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static char[][] zip(final char[][] a, final char[][] b, final char[][] c, final char valueForNoneA, final char valueForNoneB,
            final char valueForNoneC, final CharTriFunction<Character> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static char[][] zip(final int len, final int rowLen, final char[][] a, final char[][] b, final char[][] c, final char valueForNoneA,
            final char valueForNoneB, final char valueForNoneC, final CharTriFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static char[][][] zip(final char[][][] a, final char[][][] b, final CharBiFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final char[][][] result = new char[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static char[][][] zip(final char[][][] a, final char[][][] b, final char valueForNoneA, final char valueForNoneB,
            final CharBiFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static char[][][] zip(final char[][][] a, final char[][][] b, final char[][][] c, final CharTriFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final char[][][] result = new char[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static char[][][] zip(final char[][][] a, final char[][][] b, final char[][][] c, final char valueForNoneA, final char valueForNoneB,
            final char valueForNoneC, final CharTriFunction<Character> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final char[][][] result = new char[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static CharMatrix matrix(final char[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static CharMatrix matrix(final char[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static CharMatrix matrix(final char[][] a, final int n, final int m) {
        return matrix(a, n, m, (char) 0);
    }

    public static CharMatrix matrix(final char[][] a, final int n, final int m, final char valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final char[][] c = new char[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new char[m];

                if (m > 0 && valueForDefault != (char) 0) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != (char) 0) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return CharMatrix.of(c);
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

    private static int len(char[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(char[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(char[][][] a) {
        return a == null ? 0 : a.length;
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

    public static void replaceAll(final byte[] a, final ByteUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsByte(a[i]);
        }
    }

    public static void replaceAll(final byte[][] a, final ByteUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final byte[][][] a, final ByteUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final byte[][] c = new byte[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static byte[][][] reshape(final byte[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new byte[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final byte[][][] c = new byte[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new byte[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static byte[] zip(final byte[] a, final byte[] b, final ByteBiFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final byte[] result = new byte[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static byte[] zip(final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB, final ByteBiFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static byte[] zip(final int len, final byte[] a, final byte[] b, final byte valueForNoneA, final byte valueForNoneB,
            final ByteBiFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static byte[] zip(final byte[] a, final byte[] b, final byte[] c, final ByteTriFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final byte[] result = new byte[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static byte[] zip(final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB, final byte valueForNoneC,
            final ByteTriFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static byte[] zip(final int len, final byte[] a, final byte[] b, final byte[] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final ByteTriFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static byte[][] zip(final byte[][] a, final byte[][] b, final ByteBiFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final byte[][] result = new byte[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static byte[][] zip(final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB, final ByteBiFunction<Byte> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static byte[][] zip(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte valueForNoneA, final byte valueForNoneB,
            final ByteBiFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static byte[][] zip(final byte[][] a, final byte[][] b, final byte[][] c, final ByteTriFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final byte[][] result = new byte[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static byte[][] zip(final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final ByteTriFunction<Byte> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static byte[][] zip(final int len, final int rowLen, final byte[][] a, final byte[][] b, final byte[][] c, final byte valueForNoneA,
            final byte valueForNoneB, final byte valueForNoneC, final ByteTriFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static byte[][][] zip(final byte[][][] a, final byte[][][] b, final ByteBiFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final byte[][][] result = new byte[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static byte[][][] zip(final byte[][][] a, final byte[][][] b, final byte valueForNoneA, final byte valueForNoneB,
            final ByteBiFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static byte[][][] zip(final byte[][][] a, final byte[][][] b, final byte[][][] c, final ByteTriFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final byte[][][] result = new byte[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static byte[][][] zip(final byte[][][] a, final byte[][][] b, final byte[][][] c, final byte valueForNoneA, final byte valueForNoneB,
            final byte valueForNoneC, final ByteTriFunction<Byte> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final byte[][][] result = new byte[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static ByteMatrix matrix(final byte[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static ByteMatrix matrix(final byte[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static ByteMatrix matrix(final byte[][] a, final int n, final int m) {
        return matrix(a, n, m, (byte) 0);
    }

    public static ByteMatrix matrix(final byte[][] a, final int n, final int m, final byte valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final byte[][] c = new byte[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new byte[m];

                if (m > 0 && valueForDefault != (byte) 0) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != (byte) 0) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return ByteMatrix.of(c);
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

    private static int len(byte[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(byte[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(byte[][][] a) {
        return a == null ? 0 : a.length;
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

    public static void replaceAll(final short[] a, final ShortUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsShort(a[i]);
        }
    }

    public static void replaceAll(final short[][] a, final ShortUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final short[][][] a, final ShortUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final short[][] c = new short[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static short[][][] reshape(final short[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new short[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final short[][][] c = new short[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new short[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static short[] zip(final short[] a, final short[] b, final ShortBiFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final short[] result = new short[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static short[] zip(final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static short[] zip(final int len, final short[] a, final short[] b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static short[] zip(final short[] a, final short[] b, final short[] c, final ShortTriFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final short[] result = new short[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static short[] zip(final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static short[] zip(final int len, final short[] a, final short[] b, final short[] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static short[][] zip(final short[][] a, final short[][] b, final ShortBiFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final short[][] result = new short[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static short[][] zip(final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<Short> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static short[][] zip(final int len, final int rowLen, final short[][] a, final short[][] b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static short[][] zip(final short[][] a, final short[][] b, final short[][] c, final ShortTriFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final short[][] result = new short[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static short[][] zip(final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<Short> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static short[][] zip(final int len, final int rowLen, final short[][] a, final short[][] b, final short[][] c, final short valueForNoneA,
            final short valueForNoneB, final short valueForNoneC, final ShortTriFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static short[][][] zip(final short[][][] a, final short[][][] b, final ShortBiFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final short[][][] result = new short[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static short[][][] zip(final short[][][] a, final short[][][] b, final short valueForNoneA, final short valueForNoneB,
            final ShortBiFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static short[][][] zip(final short[][][] a, final short[][][] b, final short[][][] c, final ShortTriFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final short[][][] result = new short[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static short[][][] zip(final short[][][] a, final short[][][] b, final short[][][] c, final short valueForNoneA, final short valueForNoneB,
            final short valueForNoneC, final ShortTriFunction<Short> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final short[][][] result = new short[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static ShortMatrix matrix(final short[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static ShortMatrix matrix(final short[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static ShortMatrix matrix(final short[][] a, final int n, final int m) {
        return matrix(a, n, m, (short) 0);
    }

    public static ShortMatrix matrix(final short[][] a, final int n, final int m, final short valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final short[][] c = new short[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new short[m];

                if (m > 0 && valueForDefault != (short) 0) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != (short) 0) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return ShortMatrix.of(c);
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

    private static int len(short[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(short[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(short[][][] a) {
        return a == null ? 0 : a.length;
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

    public static void replaceAll(final int[] a, final IntUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsInt(a[i]);
        }
    }

    public static void replaceAll(final int[][] a, final IntUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final int[][][] a, final IntUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final int[][] c = new int[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static int[][][] reshape(final int[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new int[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final int[][][] c = new int[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new int[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static int[] zip(final int[] a, final int[] b, final IntBiFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final int[] result = new int[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static int[] zip(final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB, final IntBiFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static int[] zip(final int len, final int[] a, final int[] b, final int valueForNoneA, final int valueForNoneB,
            final IntBiFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static int[] zip(final int[] a, final int[] b, final int[] c, final IntTriFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final int[] result = new int[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static int[] zip(final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC,
            final IntTriFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static int[] zip(final int len, final int[] a, final int[] b, final int[] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final IntTriFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static int[][] zip(final int[][] a, final int[][] b, final IntBiFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final int[][] result = new int[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static int[][] zip(final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB, final IntBiFunction<Integer> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static int[][] zip(final int len, final int rowLen, final int[][] a, final int[][] b, final int valueForNoneA, final int valueForNoneB,
            final IntBiFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static int[][] zip(final int[][] a, final int[][] b, final int[][] c, final IntTriFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final int[][] result = new int[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static int[][] zip(final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA, final int valueForNoneB, final int valueForNoneC,
            final IntTriFunction<Integer> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static int[][] zip(final int len, final int rowLen, final int[][] a, final int[][] b, final int[][] c, final int valueForNoneA,
            final int valueForNoneB, final int valueForNoneC, final IntTriFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static int[][][] zip(final int[][][] a, final int[][][] b, final IntBiFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final int[][][] result = new int[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static int[][][] zip(final int[][][] a, final int[][][] b, final int valueForNoneA, final int valueForNoneB,
            final IntBiFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static int[][][] zip(final int[][][] a, final int[][][] b, final int[][][] c, final IntTriFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final int[][][] result = new int[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static int[][][] zip(final int[][][] a, final int[][][] b, final int[][][] c, final int valueForNoneA, final int valueForNoneB,
            final int valueForNoneC, final IntTriFunction<Integer> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final int[][][] result = new int[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static IntMatrix matrix(final int[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static IntMatrix matrix(final int[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static IntMatrix matrix(final int[][] a, final int n, final int m) {
        return matrix(a, n, m, 0);
    }

    public static IntMatrix matrix(final int[][] a, final int n, final int m, final int valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final int[][] c = new int[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new int[m];

                if (m > 0 && valueForDefault != 0) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != 0) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return IntMatrix.of(c);
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

    private static int len(int[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(int[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(int[][][] a) {
        return a == null ? 0 : a.length;
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

    public static void replaceAll(final long[] a, final LongUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsLong(a[i]);
        }
    }

    public static void replaceAll(final long[][] a, final LongUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final long[][][] a, final LongUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final long[][] c = new long[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static long[][][] reshape(final long[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new long[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final long[][][] c = new long[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new long[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static long[] zip(final long[] a, final long[] b, final LongBiFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final long[] result = new long[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static long[] zip(final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB, final LongBiFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static long[] zip(final int len, final long[] a, final long[] b, final long valueForNoneA, final long valueForNoneB,
            final LongBiFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static long[] zip(final long[] a, final long[] b, final long[] c, final LongTriFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final long[] result = new long[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static long[] zip(final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB, final long valueForNoneC,
            final LongTriFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static long[] zip(final int len, final long[] a, final long[] b, final long[] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final LongTriFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static long[][] zip(final long[][] a, final long[][] b, final LongBiFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final long[][] result = new long[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static long[][] zip(final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB, final LongBiFunction<Long> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static long[][] zip(final int len, final int rowLen, final long[][] a, final long[][] b, final long valueForNoneA, final long valueForNoneB,
            final LongBiFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static long[][] zip(final long[][] a, final long[][] b, final long[][] c, final LongTriFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final long[][] result = new long[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static long[][] zip(final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final LongTriFunction<Long> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static long[][] zip(final int len, final int rowLen, final long[][] a, final long[][] b, final long[][] c, final long valueForNoneA,
            final long valueForNoneB, final long valueForNoneC, final LongTriFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static long[][][] zip(final long[][][] a, final long[][][] b, final LongBiFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final long[][][] result = new long[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static long[][][] zip(final long[][][] a, final long[][][] b, final long valueForNoneA, final long valueForNoneB,
            final LongBiFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static long[][][] zip(final long[][][] a, final long[][][] b, final long[][][] c, final LongTriFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final long[][][] result = new long[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static long[][][] zip(final long[][][] a, final long[][][] b, final long[][][] c, final long valueForNoneA, final long valueForNoneB,
            final long valueForNoneC, final LongTriFunction<Long> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final long[][][] result = new long[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static LongMatrix matrix(final long[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static LongMatrix matrix(final long[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static LongMatrix matrix(final long[][] a, final int n, final int m) {
        return matrix(a, n, m, 0);
    }

    public static LongMatrix matrix(final long[][] a, final int n, final int m, final long valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final long[][] c = new long[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new long[m];

                if (m > 0 && valueForDefault != 0) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != 0) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return LongMatrix.of(c);
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

    private static int len(long[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(long[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(long[][][] a) {
        return a == null ? 0 : a.length;
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

    public static void replaceAll(final float[] a, final FloatUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsFloat(a[i]);
        }
    }

    public static void replaceAll(final float[][] a, final FloatUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final float[][][] a, final FloatUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final float[][] c = new float[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static float[][][] reshape(final float[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new float[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final float[][][] c = new float[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new float[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static float[] zip(final float[] a, final float[] b, final FloatBiFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final float[] result = new float[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static float[] zip(final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB,
            final FloatBiFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static float[] zip(final int len, final float[] a, final float[] b, final float valueForNoneA, final float valueForNoneB,
            final FloatBiFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static float[] zip(final float[] a, final float[] b, final float[] c, final FloatTriFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final float[] result = new float[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static float[] zip(final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final FloatTriFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static float[] zip(final int len, final float[] a, final float[] b, final float[] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final FloatTriFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static float[][] zip(final float[][] a, final float[][] b, final FloatBiFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final float[][] result = new float[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static float[][] zip(final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB,
            final FloatBiFunction<Float> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static float[][] zip(final int len, final int rowLen, final float[][] a, final float[][] b, final float valueForNoneA, final float valueForNoneB,
            final FloatBiFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static float[][] zip(final float[][] a, final float[][] b, final float[][] c, final FloatTriFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final float[][] result = new float[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static float[][] zip(final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final FloatTriFunction<Float> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static float[][] zip(final int len, final int rowLen, final float[][] a, final float[][] b, final float[][] c, final float valueForNoneA,
            final float valueForNoneB, final float valueForNoneC, final FloatTriFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static float[][][] zip(final float[][][] a, final float[][][] b, final FloatBiFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final float[][][] result = new float[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static float[][][] zip(final float[][][] a, final float[][][] b, final float valueForNoneA, final float valueForNoneB,
            final FloatBiFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static float[][][] zip(final float[][][] a, final float[][][] b, final float[][][] c, final FloatTriFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final float[][][] result = new float[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static float[][][] zip(final float[][][] a, final float[][][] b, final float[][][] c, final float valueForNoneA, final float valueForNoneB,
            final float valueForNoneC, final FloatTriFunction<Float> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final float[][][] result = new float[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static FloatMatrix matrix(final float[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static FloatMatrix matrix(final float[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static FloatMatrix matrix(final float[][] a, final int n, final int m) {
        return matrix(a, n, m, 0);
    }

    public static FloatMatrix matrix(final float[][] a, final int n, final int m, final float valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final float[][] c = new float[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new float[m];

                if (m > 0 && valueForDefault != 0) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != 0) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return FloatMatrix.of(c);
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

    private static int len(float[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(float[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(float[][][] a) {
        return a == null ? 0 : a.length;
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

    public static void replaceAll(final double[] a, final DoubleUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            a[i] = operator.applyAsDouble(a[i]);
        }
    }

    public static void replaceAll(final double[][] a, final DoubleUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
        }
    }

    public static void replaceAll(final double[][][] a, final DoubleUnaryOperator operator) {
        if (N.isNullOrEmpty(a)) {
            return;
        }

        for (int i = 0, n = a.length; i < n; i++) {
            replaceAll(a[i], operator);
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
        final int n = Math2.divide(len, m, RoundingMode.CEILING);
        final double[][] c = new double[n][];

        for (int i = 0, from = 0; i < n; i++, from += m) {
            c[i] = N.copyOfRange(a, from, from + N.min(len - from, m));
        }

        return c;
    }

    public static double[][][] reshape(final double[] a, final int m, final int l) {
        N.checkArgument(m > 0 && l > 0, " 'm'  and 'l' must be positive number: m = %s, l = %s", m, l);

        if (N.isNullOrEmpty(a)) {
            return new double[0][0][0];
        }

        final int len = a.length;
        final int n = Math2.divide(len, m * l, RoundingMode.CEILING);
        final double[][][] c = new double[n][][];

        for (int i = 0, from = 0; i < n; i++) {
            c[i] = new double[N.min(m, Math2.divide(len - from, l, RoundingMode.CEILING))][];

            for (int j = 0, y = c[i].length; j < y; j++, from += l) {
                c[i][j] = N.copyOfRange(a, from, from + N.min(len - from, l));
            }
        }

        return c;
    }

    public static double[] zip(final double[] a, final double[] b, final DoubleBiFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final double[] result = new double[N.min(lenA, lenB)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i]);
        }

        return result;
    }

    public static double[] zip(final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        return zip(N.max(lenA, lenB), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static double[] zip(final int len, final double[] a, final double[] b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static double[] zip(final double[] a, final double[] b, final double[] c, final DoubleTriFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final double[] result = new double[N.min(lenA, lenB, lenC)];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zipFunction.apply(a[i], b[i], c[i]);
        }

        return result;
    }

    public static double[] zip(final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        return zip(N.max(lenA, lenB, lenC), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static double[] zip(final int len, final double[] a, final double[] b, final double[] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static double[][] zip(final double[][] a, final double[][] b, final DoubleBiFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final double[][] result = new double[N.min(lenA, lenB)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static double[][] zip(final double[][] a, final double[][] b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> zipFunction) {
        return zip(N.max(len(a), len(b)), N.max(maxLen(a), maxLen(b)), a, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    private static double[][] zip(final int len, final int rowLen, final double[][] a, final double[][] b, final double valueForNoneA,
            final double valueForNoneB, final DoubleBiFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static double[][] zip(final double[][] a, final double[][] b, final double[][] c, final DoubleTriFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final double[][] result = new double[N.min(lenA, lenB, lenC)][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static double[][] zip(final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        return zip(N.max(len(a), len(b), len(c)), N.max(maxLen(a), maxLen(b), maxLen(c)), a, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    private static double[][] zip(final int len, final int rowLen, final double[][] a, final double[][] b, final double[][] c, final double valueForNoneA,
            final double valueForNoneB, final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

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

    public static double[][][] zip(final double[][][] a, final double[][][] b, final DoubleBiFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

        final double[][][] result = new double[N.min(lenA, lenB)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], zipFunction);
        }

        return result;
    }

    public static double[][][] zip(final double[][][] a, final double[][][] b, final double valueForNoneA, final double valueForNoneB,
            final DoubleBiFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);

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

    public static double[][][] zip(final double[][][] a, final double[][][] b, final double[][][] c, final DoubleTriFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final double[][][] result = new double[N.min(lenA, lenB, lenC)][][];

        for (int i = 0, len = result.length; i < len; i++) {
            result[i] = zip(a[i], b[i], c[i], zipFunction);
        }

        return result;
    }

    public static double[][][] zip(final double[][][] a, final double[][][] b, final double[][][] c, final double valueForNoneA, final double valueForNoneB,
            final double valueForNoneC, final DoubleTriFunction<Double> zipFunction) {
        final int lenA = len(a);
        final int lenB = len(b);
        final int lenC = len(c);

        final double[][][] result = new double[N.max(lenA, lenB, lenC)][][];

        for (int i = 0, min = N.min(lenA, lenB, lenC); i < min; i++) {
            result[i] = zip(a[i], b[i], c[i], valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        for (int i = N.min(lenA, lenB, lenC), len = result.length; i < len; i++) {
            result[i] = zip(i < lenA ? a[i] : null, i < lenB ? b[i] : null, i < lenC ? c[i] : null, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
        }

        return result;
    }

    public static DoubleMatrix matrix(final double[][] a) {
        return matrix(a, len(a), maxLen(a));
    }

    public static DoubleMatrix matrix(final double[][] a, final int m) {
        return matrix(a, len(a), m);
    }

    public static DoubleMatrix matrix(final double[][] a, final int n, final int m) {
        return matrix(a, n, m, 0);
    }

    public static DoubleMatrix matrix(final double[][] a, final int n, final int m, final double valueForDefault) {
        N.checkArgument(n >= 0 && m >= 0, "'n' and 'm' can't be negative nubmer: n = %s, m = %s", n, m);

        final int lenA = len(a);
        final double[][] c = new double[n][];

        for (int i = 0, len = c.length; i < len; i++) {
            if (i >= lenA || N.isNullOrEmpty(a[i])) {
                c[i] = new double[m];

                if (m > 0 && valueForDefault != 0) {
                    N.fill(c[i], valueForDefault);
                }
            } else {
                c[i] = N.copyOf(a[i], m);

                if (a[i].length < m && valueForDefault != 0) {
                    N.fill(c[i], a[i].length, m, valueForDefault);
                }
            }
        }

        return DoubleMatrix.of(c);
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

    private static int len(double[] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(double[][] a) {
        return a == null ? 0 : a.length;
    }

    private static int len(double[][][] a) {
        return a == null ? 0 : a.length;
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

    public static int[] toInt(final char[] a) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
        final int[][][] result = new int[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toInt(a[i]);
        }

        return result;
    }

    public static long[] toLong(final char[] a) {
        if (a == null) {
            return null;
        }

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
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

        final int len = len(a);
        final double[][][] result = new double[len][][];

        for (int i = 0; i < len; i++) {
            result[i] = toDouble(a[i]);
        }

        return result;
    }
}
