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

import java.util.Calendar;
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
    public static Class[] of(final Class<?>... a) {
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
        final float[] a = new float[startInclusive + tmp == endExclusive ? tmp : tmp + 1];

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
        final double[] a = new double[startInclusive + tmp == endExclusive ? tmp : tmp + 1];

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
}
