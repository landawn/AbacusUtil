package com.landawn.abacus.util;

import java.math.RoundingMode;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.function.BooleanUnaryOperator;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.function.UnaryOperator;

@Beta
public final class f {
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
}
