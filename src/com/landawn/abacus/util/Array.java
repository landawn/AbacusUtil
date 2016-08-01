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
import java.util.Calendar;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.Callable;

import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.EntityId;
import com.landawn.abacus.condition.Condition;
import com.landawn.abacus.type.Type;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.lang.reflect.Array
 */
public final class Array {
    private Array() {
    }

    public static <T> T newInstance(final Class<?> componentType, final int length) throws NegativeArraySizeException {
        return (T) java.lang.reflect.Array.newInstance(componentType, length);
    }

    public static <T> T newInstance(final Class<?> componentType, final int... dimensions) throws IllegalArgumentException, NegativeArraySizeException {
        return (T) java.lang.reflect.Array.newInstance(componentType, dimensions);
    }

    public static int getLength(final Object array) throws IllegalArgumentException {
        return java.lang.reflect.Array.getLength(array);
    }

    public static <T> T get(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return (T) java.lang.reflect.Array.get(array, index);
    }

    public static boolean getBoolean(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getBoolean(array, index);
    }

    public static byte getByte(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getByte(array, index);
    }

    public static char getChar(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getChar(array, index);
    }

    public static short getShort(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getShort(array, index);
    }

    public static int getInt(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getInt(array, index);
    }

    public static long getLong(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getLong(array, index);
    }

    public static float getFloat(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getFloat(array, index);
    }

    public static double getDouble(final Object array, final int index) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        return java.lang.reflect.Array.getDouble(array, index);
    }

    public static void set(final Object array, final int index, final Object value) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.set(array, index, value);
    }

    public static void setBoolean(final Object array, final int index, final boolean z) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setBoolean(array, index, z);
    }

    public static void setByte(final Object array, final int index, final byte b) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setByte(array, index, b);
    }

    public static void setChar(final Object array, final int index, final char c) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setChar(array, index, c);
    }

    public static void setShort(final Object array, final int index, final short s) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setShort(array, index, s);
    }

    public static void setInt(final Object array, final int index, final int i) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setInt(array, index, i);
    }

    public static void setLong(final Object array, final int index, final long l) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setLong(array, index, l);
    }

    public static void setFloat(final Object array, final int index, final float f) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setFloat(array, index, f);
    }

    public static void setDouble(final Object array, final int index, final double d) throws IllegalArgumentException, ArrayIndexOutOfBoundsException {
        java.lang.reflect.Array.setDouble(array, index, d);
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static boolean[] of(final boolean... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static char[] of(final char... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static byte[] of(final byte... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static short[] of(final short... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static int[] of(final int... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static long[] of(final long... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static float[] of(final float... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static double[] of(final double... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static String[] of(final String... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static BigInteger[] of(final BigInteger... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static BigDecimal[] of(final BigDecimal... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Enum<T>> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends java.util.Date> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Calendar> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Runnable> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Callable<?>> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static Class[] of(final Class... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends EntityId> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends DirtyMarker> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Condition> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Type<?>> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends List<?>> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Set<?>> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Queue<?>> T[] of(final T... a) {
        return a;
    }

    /**
     * Returns the input array
     *
     * @param a
     * @return
     */
    public static <T extends Map<?, ?>> T[] of(final T... a) {
        return a;
    }

    //    // Only for Java 8. it's ambiguous in the Java version before 8.
    //    /**
    //     * Returns the input array
    //     *
    //     * @param a
    //     * @return
    //     */
    //    public static <T> T[] of(final T... a) {
    //        return a;
    //    }

    public static char[] range(char startInclusive, final char endExclusive) {
        if (endExclusive == startInclusive) {
            return N.EMPTY_CHAR_ARRAY;
        }

        final char[] a = new char[endExclusive - startInclusive];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static byte[] range(byte startInclusive, final byte endExclusive) {
        if (endExclusive == startInclusive) {
            return N.EMPTY_BYTE_ARRAY;
        }

        final byte[] a = new byte[endExclusive - startInclusive];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static short[] range(short startInclusive, final short endExclusive) {
        if (endExclusive == startInclusive) {
            return N.EMPTY_SHORT_ARRAY;
        }

        final short[] a = new short[endExclusive - startInclusive];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static int[] range(int startInclusive, final int endExclusive) {
        if (endExclusive == startInclusive) {
            return N.EMPTY_INT_ARRAY;
        }

        final int[] a = new int[endExclusive - startInclusive];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static long[] range(long startInclusive, final long endExclusive) {
        if (endExclusive == startInclusive) {
            return N.EMPTY_LONG_ARRAY;
        }

        final long[] a = new long[(int) (endExclusive - startInclusive)];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static float[] range(float startInclusive, final float endExclusive) {
        if (endExclusive == startInclusive) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        int tmp = (int) (endExclusive - startInclusive);
        final float[] a = new float[(startInclusive + tmp == endExclusive) ? tmp : tmp + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static double[] range(double startInclusive, final double endExclusive) {
        if (endExclusive == startInclusive) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        int tmp = (int) (endExclusive - startInclusive);
        final double[] a = new double[(startInclusive + tmp == endExclusive) ? tmp : tmp + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static char[] range(char startInclusive, final char endExclusive, final int by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return N.EMPTY_CHAR_ARRAY;
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int tmp = (endExclusive - startInclusive) / by;
        final int len = startInclusive + (tmp * by) == endExclusive ? tmp : tmp + 1;
        final char[] a = new char[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static byte[] range(byte startInclusive, final byte endExclusive, final byte by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return N.EMPTY_BYTE_ARRAY;
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int tmp = (endExclusive - startInclusive) / by;
        final int len = startInclusive + (tmp * by) == endExclusive ? tmp : tmp + 1;
        final byte[] a = new byte[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static short[] range(short startInclusive, final short endExclusive, final short by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return N.EMPTY_SHORT_ARRAY;
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int tmp = (endExclusive - startInclusive) / by;
        final int len = startInclusive + (tmp * by) == endExclusive ? tmp : tmp + 1;
        final short[] a = new short[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static int[] range(int startInclusive, final int endExclusive, final int by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return N.EMPTY_INT_ARRAY;
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int tmp = (endExclusive - startInclusive) / by;
        final int len = startInclusive + (tmp * by) == endExclusive ? tmp : tmp + 1;
        final int[] a = new int[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static long[] range(long startInclusive, final long endExclusive, final long by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return N.EMPTY_LONG_ARRAY;
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int tmp = (int) ((endExclusive - startInclusive) / by);
        final int len = startInclusive + (tmp * by) == endExclusive ? tmp : tmp + 1;
        final long[] a = new long[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static float[] range(float startInclusive, final float endExclusive, final float by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return N.EMPTY_FLOAT_ARRAY;
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int tmp = (int) ((endExclusive - startInclusive) / by);
        final int len = startInclusive + (tmp * by) == endExclusive ? tmp : tmp + 1;
        final float[] a = new float[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static double[] range(double startInclusive, final double endExclusive, final double by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return N.EMPTY_DOUBLE_ARRAY;
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int tmp = (int) ((endExclusive - startInclusive) / by);
        final int len = startInclusive + (tmp * by) == endExclusive ? tmp : tmp + 1;
        final double[] a = new double[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static char[] rangeClosed(char startInclusive, final char endInclusive) {
        final char[] a = new char[endInclusive - startInclusive + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static byte[] rangeClosed(byte startInclusive, final byte endInclusive) {
        final byte[] a = new byte[endInclusive - startInclusive + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static short[] rangeClosed(short startInclusive, final short endInclusive) {
        final short[] a = new short[endInclusive - startInclusive + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static int[] rangeClosed(int startInclusive, final int endInclusive) {
        final int[] a = new int[endInclusive - startInclusive + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static long[] rangeClosed(long startInclusive, final long endInclusive) {
        final long[] a = new long[(int) (endInclusive - startInclusive) + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static float[] rangeClosed(float startInclusive, final float endInclusive) {
        final float[] a = new float[(int) (endInclusive - startInclusive) + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static double[] rangeClosed(double startInclusive, final double endInclusive) {
        final double[] a = new double[(int) (endInclusive - startInclusive) + 1];

        for (int i = 0, len = a.length; i < len; i++) {
            a[i] = startInclusive++;
        }

        return a;
    }

    public static char[] rangeClosed(char startInclusive, final char endExclusive, final int by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return new char[] { startInclusive };
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int len = (endExclusive - startInclusive) / by + 1;
        final char[] a = new char[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static byte[] rangeClosed(byte startInclusive, final byte endExclusive, final byte by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return new byte[] { startInclusive };
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int len = (endExclusive - startInclusive) / by + 1;
        final byte[] a = new byte[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static short[] rangeClosed(short startInclusive, final short endExclusive, final short by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return new short[] { startInclusive };
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int len = (endExclusive - startInclusive) / by + 1;
        final short[] a = new short[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static int[] rangeClosed(int startInclusive, final int endExclusive, final int by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return new int[] { startInclusive };
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int len = (endExclusive - startInclusive) / by + 1;
        final int[] a = new int[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static long[] rangeClosed(long startInclusive, final long endExclusive, final long by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return new long[] { startInclusive };
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int len = (int) ((endExclusive - startInclusive) / by) + 1;
        final long[] a = new long[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static float[] rangeClosed(float startInclusive, final float endExclusive, final float by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return new float[] { startInclusive };
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int len = (int) (((double) endExclusive - (double) startInclusive) / by) + 1;
        final float[] a = new float[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    // Doesn't work as expected due to precision issue. "3.3d - 1.1d != 2.2d". Refer to: https://en.wikipedia.org/wiki/IEEE_floating_point
    // http://stackoverflow.com/questions/15625556/java-adding-and-subtracting-doubles-are-giving-strange-results
    static double[] rangeClosed(double startInclusive, final double endExclusive, final double by) {
        if (by == 0) {
            throw new IllegalArgumentException("The input parameter 'by' can't be zero");
        }

        if (endExclusive == startInclusive) {
            return new double[] { startInclusive };
        }

        if (endExclusive > startInclusive != by > 0) {
            throw new IllegalArgumentException(
                    "The input 'startInclusive' (" + startInclusive + ") and 'endExclusive' (" + endExclusive + ") are not consistent with by (" + by + ").");
        }

        final int len = (int) ((endExclusive - startInclusive) / by) + 1;
        final double[] a = new double[len];

        for (int i = 0; i < len; i++, startInclusive += by) {
            a[i] = startInclusive;
        }

        return a;
    }

    public static Class<?> wrap(final Class<?> cls) {
        Class<?> result = N.PRIMITIVE_2_WRAPPER.get(cls);
    
        if (result == null) {
            throw new IllegalArgumentException(N.getCanonicalClassName(cls) + " is not a primitive (array) type");
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive booleans to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code boolean} array
     * @return a {@code Boolean} array, {@code null} if null array input
     */
    public static Boolean[] wrap(final boolean[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Boolean[] wrap(final boolean[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_BOOLEAN_OBJECT_ARRAY;
        }
    
        final Boolean[] result = new Boolean[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Boolean.valueOf(a[j]);
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive chars to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code char} array
     * @return a {@code Character} array, {@code null} if null array input
     */
    public static Character[] wrap(final char[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Character[] wrap(final char[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_CHARACTER_OBJECT_ARRAY;
        }
    
        final Character[] result = new Character[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Character.valueOf(a[j]);
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive bytes to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code byte} array
     * @return a {@code Byte} array, {@code null} if null array input
     */
    public static Byte[] wrap(final byte[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Byte[] wrap(final byte[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_BYTE_OBJECT_ARRAY;
        }
    
        final Byte[] result = new Byte[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Byte.valueOf(a[j]);
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive shorts to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code short} array
     * @return a {@code Short} array, {@code null} if null array input
     */
    public static Short[] wrap(final short[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Short[] wrap(final short[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_SHORT_OBJECT_ARRAY;
        }
    
        final Short[] result = new Short[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Short.valueOf(a[j]);
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive ints to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            an {@code int} array
     * @return an {@code Integer} array, {@code null} if null array input
     */
    public static Integer[] wrap(final int[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Integer[] wrap(final int[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_INTEGER_OBJECT_ARRAY;
        }
    
        final Integer[] result = new Integer[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Integer.valueOf(a[j]);
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive longs to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code long} array
     * @return a {@code Long} array, {@code null} if null array input
     */
    public static Long[] wrap(final long[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Long[] wrap(final long[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_LONG_OBJECT_ARRAY;
        }
    
        final Long[] result = new Long[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Long.valueOf(a[j]);
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive floats to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code float} array
     * @return a {@code Float} array, {@code null} if null array input
     */
    public static Float[] wrap(final float[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Float[] wrap(final float[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_FLOAT_OBJECT_ARRAY;
        }
    
        final Float[] result = new Float[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Float.valueOf(a[j]);
        }
    
        return result;
    }

    /**
     * <p>
     * Converts an array of primitive doubles to objects.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code double} array
     * @return a {@code Double} array, {@code null} if null array input
     */
    public static Double[] wrap(final double[] a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, a.length);
    }

    public static Double[] wrap(final double[] a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_DOUBLE_OBJECT_ARRAY;
        }
    
        final Double[] result = new Double[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = Double.valueOf(a[j]);
        }
    
        return result;
    }

    public static <T> T wrap(final Object a) {
        if (a == null) {
            return null;
        }
    
        return wrap(a, 0, getLength(a));
    }

    public static <T> T wrap(final Object a, final int fromIndex, final int toIndex) {
        if (a == null) {
            return null;
        }
    
        final Class<?> cls = a.getClass();
        final Integer enumInt = N.CLASS_TYPE_ENUM.get(cls);
    
        if (enumInt == null) {
            throw new IllegalArgumentException(N.getCanonicalClassName(cls) + " is not a primitive array");
        }
    
        switch (enumInt) {
            case 11:
                return (T) wrap((boolean[]) a, fromIndex, toIndex);
    
            case 12:
                return (T) wrap((char[]) a, fromIndex, toIndex);
    
            case 13:
                return (T) wrap((byte[]) a, fromIndex, toIndex);
    
            case 14:
                return (T) wrap((short[]) a, fromIndex, toIndex);
    
            case 15:
                return (T) wrap((int[]) a, fromIndex, toIndex);
    
            case 16:
                return (T) wrap((long[]) a, fromIndex, toIndex);
    
            case 17:
                return (T) wrap((float[]) a, fromIndex, toIndex);
    
            case 18:
                return (T) wrap((double[]) a, fromIndex, toIndex);
    
            default:
                throw new IllegalArgumentException(N.getCanonicalClassName(cls) + " is not a primitive array");
        }
    }

    public static Class<?> unwrap(final Class<?> cls) {
        Class<?> result = N.PRIMITIVE_2_WRAPPER.getByValue(cls);
    
        if (result == null) {
            throw new IllegalArgumentException(N.getCanonicalClassName(cls) + " is not a wrapper of primitive (array) type");
        }
    
        return result;
    }

    // Boolean array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Booleans to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Boolean} array, may be {@code null}
     * @return a {@code boolean} array, {@code null} if null array input
     */
    public static boolean[] unwrap(final Boolean[] a) {
        return unwrap(a, false);
    }

    /**
     * <p>
     * Converts an array of object Booleans to primitives handling {@code null}.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Boolean} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return a {@code boolean} array, {@code null} if null array input
     */
    public static boolean[] unwrap(final Boolean[] a, final boolean valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static boolean[] unwrap(final Boolean[] a, final int fromIndex, final int toIndex, final boolean valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_BOOLEAN_ARRAY;
        }
    
        final boolean[] result = new boolean[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].booleanValue());
        }
    
        return result;
    }

    // Character array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Characters to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Character} array, may be {@code null}
     * @return a {@code char} array, {@code null} if null array input
     */
    public static char[] unwrap(final Character[] a) {
        return unwrap(a, (char) 0);
    }

    /**
     * <p>
     * Converts an array of object Character to primitives handling {@code null}
     * .
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Character} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return a {@code char} array, {@code null} if null array input
     */
    public static char[] unwrap(final Character[] a, final char valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static char[] unwrap(final Character[] a, final int fromIndex, final int toIndex, final char valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_CHAR_ARRAY;
        }
    
        final char[] result = new char[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].charValue());
        }
    
        return result;
    }

    // Byte array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Bytes to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Byte} array, may be {@code null}
     * @return a {@code byte} array, {@code null} if null array input
     */
    public static byte[] unwrap(final Byte[] a) {
        return unwrap(a, (byte) 0);
    }

    /**
     * <p>
     * Converts an array of object Bytes to primitives handling {@code null}.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Byte} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return a {@code byte} array, {@code null} if null array input
     */
    public static byte[] unwrap(final Byte[] a, final byte valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static byte[] unwrap(final Byte[] a, final int fromIndex, final int toIndex, final byte valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_BYTE_ARRAY;
        }
    
        final byte[] result = new byte[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].byteValue());
        }
    
        return result;
    }

    // Short array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Shorts to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Short} array, may be {@code null}
     * @return a {@code byte} array, {@code null} if null array input
     */
    public static short[] unwrap(final Short[] a) {
        return unwrap(a, (short) 0);
    }

    /**
     * <p>
     * Converts an array of object Short to primitives handling {@code null}.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Short} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return a {@code byte} array, {@code null} if null array input
     */
    public static short[] unwrap(final Short[] a, final short valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static short[] unwrap(final Short[] a, final int fromIndex, final int toIndex, final short valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_SHORT_ARRAY;
        }
    
        final short[] result = new short[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].shortValue());
        }
    
        return result;
    }

    // Int array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Integers to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Integer} array, may be {@code null}
     * @return an {@code int} array, {@code null} if null array input
     */
    public static int[] unwrap(final Integer[] a) {
        return unwrap(a, 0);
    }

    /**
     * <p>
     * Converts an array of object Integer to primitives handling {@code null}.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Integer} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return an {@code int} array, {@code null} if null array input
     */
    public static int[] unwrap(final Integer[] a, final int valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static int[] unwrap(final Integer[] a, final int fromIndex, final int toIndex, final int valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_INT_ARRAY;
        }
    
        final int[] result = new int[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].intValue());
        }
    
        return result;
    }

    // Long array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Longs to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Long} array, may be {@code null}
     * @return a {@code long} array, {@code null} if null array input
     */
    public static long[] unwrap(final Long[] a) {
        return unwrap(a, 0L);
    }

    /**
     * <p>
     * Converts an array of object Long to primitives handling {@code null}.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Long} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return a {@code long} array, {@code null} if null array input
     */
    public static long[] unwrap(final Long[] a, final long valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static long[] unwrap(final Long[] a, final int fromIndex, final int toIndex, final long valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_LONG_ARRAY;
        }
    
        final long[] result = new long[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].longValue());
        }
    
        return result;
    }

    // Float array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Floats to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Float} array, may be {@code null}
     * @return a {@code float} array, {@code null} if null array input
     */
    public static float[] unwrap(final Float[] a) {
        return unwrap(a, 0f);
    }

    /**
     * <p>
     * Converts an array of object Floats to primitives handling {@code null}.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Float} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return a {@code float} array, {@code null} if null array input
     */
    public static float[] unwrap(final Float[] a, final float valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static float[] unwrap(final Float[] a, final int fromIndex, final int toIndex, final float valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_FLOAT_ARRAY;
        }
    
        final float[] result = new float[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].floatValue());
        }
    
        return result;
    }

    // Double array converters
    // ----------------------------------------------------------------------
    /**
     * <p>
     * Converts an array of object Doubles to primitives.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Double} array, may be {@code null}
     * @return a {@code double} array, {@code null} if null array input
     */
    public static double[] unwrap(final Double[] a) {
        return unwrap(a, 0d);
    }

    /**
     * <p>
     * Converts an array of object Doubles to primitives handling {@code null}.
     * </p>
     *
     * <p>
     * This method returns {@code null} for a {@code null} input array.
     * </p>
     *
     * @param a
     *            a {@code Double} array, may be {@code null}
     * @param valueForNull
     *            the value to insert if {@code null} found
     * @return a {@code double} array, {@code null} if null array input
     */
    public static double[] unwrap(final Double[] a, final double valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, a.length, valueForNull);
    }

    public static double[] unwrap(final Double[] a, final int fromIndex, final int toIndex, final double valueForNull) {
        if (a == null) {
            return null;
        } else if (toIndex - fromIndex == 0) {
            return N.EMPTY_DOUBLE_ARRAY;
        }
    
        final double[] result = new double[toIndex - fromIndex];
    
        for (int i = 0, j = fromIndex; j < toIndex; i++, j++) {
            result[i] = (a[j] == null ? valueForNull : a[j].doubleValue());
        }
    
        return result;
    }

    public static <T> T unwrap(final Object a) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, null);
    }

    public static <T> T unwrap(final Object a, final Object valueForNull) {
        if (a == null) {
            return null;
        }
    
        return unwrap(a, 0, getLength(a), valueForNull);
    }

    public static <T> T unwrap(final Object a, final int fromIndex, final int toIndex, final Object valueForNull) {
        if (a == null) {
            return null;
        }
    
        final Class<?> cls = unwrap(a.getClass());
        final Object defaultValue = valueForNull == null ? N.defaultValueOf(cls.getComponentType()) : valueForNull;
        final Integer enumInt = N.CLASS_TYPE_ENUM.get(cls);
    
        if (enumInt == null) {
            throw new IllegalArgumentException(N.getCanonicalClassName(a.getClass()) + " is not a wrapper of primitive array");
        }
    
        switch (enumInt) {
            case 11:
                return (T) unwrap((Boolean[]) a, fromIndex, toIndex, ((Boolean) defaultValue).booleanValue());
    
            case 12:
                return (T) unwrap((Character[]) a, fromIndex, toIndex, ((Character) defaultValue).charValue());
    
            case 13:
                return (T) unwrap((Byte[]) a, fromIndex, toIndex, ((Number) defaultValue).byteValue());
    
            case 14:
                return (T) unwrap((Short[]) a, fromIndex, toIndex, ((Number) defaultValue).shortValue());
    
            case 15:
                return (T) unwrap((Integer[]) a, fromIndex, toIndex, ((Number) defaultValue).intValue());
    
            case 16:
                return (T) unwrap((Long[]) a, fromIndex, toIndex, ((Number) defaultValue).longValue());
    
            case 17:
                return (T) unwrap((Float[]) a, fromIndex, toIndex, ((Number) defaultValue).floatValue());
    
            case 18:
                return (T) unwrap((Double[]) a, fromIndex, toIndex, ((Number) defaultValue).doubleValue());
    
            default:
                throw new IllegalArgumentException(N.getCanonicalClassName(a.getClass()) + " is not a wrapper of primitive array");
        }
    }
}
