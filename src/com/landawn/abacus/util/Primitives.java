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

import com.landawn.abacus.util.function.BooleanSupplier;
import com.landawn.abacus.util.function.ByteSupplier;
import com.landawn.abacus.util.function.CharSupplier;
import com.landawn.abacus.util.function.DoubleSupplier;
import com.landawn.abacus.util.function.FloatSupplier;
import com.landawn.abacus.util.function.IntSupplier;
import com.landawn.abacus.util.function.LongSupplier;
import com.landawn.abacus.util.function.ShortSupplier;

public final class Primitives {

    // ...
    static final BiMap<Class<?>, Class<?>> PRIMITIVE_2_WRAPPER = new BiMap<>();

    static {
        PRIMITIVE_2_WRAPPER.put(boolean.class, Boolean.class);
        PRIMITIVE_2_WRAPPER.put(char.class, Character.class);
        PRIMITIVE_2_WRAPPER.put(byte.class, Byte.class);
        PRIMITIVE_2_WRAPPER.put(short.class, Short.class);
        PRIMITIVE_2_WRAPPER.put(int.class, Integer.class);
        PRIMITIVE_2_WRAPPER.put(long.class, Long.class);
        PRIMITIVE_2_WRAPPER.put(float.class, Float.class);
        PRIMITIVE_2_WRAPPER.put(double.class, Double.class);

        PRIMITIVE_2_WRAPPER.put(boolean[].class, Boolean[].class);
        PRIMITIVE_2_WRAPPER.put(char[].class, Character[].class);
        PRIMITIVE_2_WRAPPER.put(byte[].class, Byte[].class);
        PRIMITIVE_2_WRAPPER.put(short[].class, Short[].class);
        PRIMITIVE_2_WRAPPER.put(int[].class, Integer[].class);
        PRIMITIVE_2_WRAPPER.put(long[].class, Long[].class);
        PRIMITIVE_2_WRAPPER.put(float[].class, Float[].class);
        PRIMITIVE_2_WRAPPER.put(double[].class, Double[].class);
    }

    private Primitives() {
        // utility class.
    }

    public static boolean isPrimitiveType(final Class<?> cls) {
        return N.typeOf(cls).isPrimitiveType();
    }

    public static boolean isWrapperType(final Class<?> cls) {
        return N.typeOf(cls).isPrimitiveWrapper();
    }

    public static boolean isPrimitiveArrayType(final Class<?> cls) {
        return N.typeOf(cls).isPrimitiveArray();
    }

    /**
     * Returns the corresponding wrapper type of {@code type} if it is a primitive type; otherwise
     * returns {@code type} itself. Idempotent.
     *
     * <pre>
     *     wrap(int.class) == Integer.class
     *     wrap(Integer.class) == Integer.class
     *     wrap(String.class) == String.class
     * </pre>
     */
    public static Class<?> wrap(final Class<?> cls) {
        final Class<?> wrapped = PRIMITIVE_2_WRAPPER.get(cls);

        return wrapped == null ? cls : wrapped;
    }

    /**
     * Returns the corresponding primitive type of {@code type} if it is a wrapper type; otherwise
     * returns {@code type} itself. Idempotent.
     *
     * <pre>
     *     unwrap(Integer.class) == int.class
     *     unwrap(int.class) == int.class
     *     unwrap(String.class) == String.class
     * </pre>
     */
    public static Class<?> unwrap(final Class<?> cls) {
        Class<?> unwrapped = PRIMITIVE_2_WRAPPER.getByValue(cls);

        return unwrapped == null ? cls : unwrapped;
    }

    public static boolean unboxOrDefault(Boolean b) {
        if (b == null) {
            return false;
        }

        return b.booleanValue();
    }

    public static boolean unboxOrDefault(Boolean b, boolean defaultForNull) {
        if (b == null) {
            return defaultForNull;
        }

        return b.booleanValue();
    }

    public static char unboxOrDefault(Character c) {
        if (c == null) {
            return N.CHAR_0;
        }

        return c.charValue();
    }

    public static char unboxOrDefault(Character c, char defaultForNull) {
        if (c == null) {
            return defaultForNull;
        }

        return c.charValue();
    }

    public static byte unboxOrDefault(Byte b) {
        if (b == null) {
            return (byte) 0;
        }

        return b.byteValue();
    }

    public static byte unboxOrDefault(Byte b, byte defaultForNull) {
        if (b == null) {
            return defaultForNull;
        }

        return b.byteValue();
    }

    public static short unboxOrDefault(Short b) {
        if (b == null) {
            return (short) 0;
        }

        return b.shortValue();
    }

    public static short unboxOrDefault(Short b, short defaultForNull) {
        if (b == null) {
            return defaultForNull;
        }

        return b.shortValue();
    }

    public static int unboxOrDefault(Integer b) {
        if (b == null) {
            return 0;
        }

        return b.intValue();
    }

    public static int unboxOrDefault(Integer b, int defaultForNull) {
        if (b == null) {
            return defaultForNull;
        }

        return b.intValue();
    }

    public static long unboxOrDefault(Long b) {
        if (b == null) {
            return 0;
        }

        return b.longValue();
    }

    public static long unboxOrDefault(Long b, long defaultForNull) {
        if (b == null) {
            return defaultForNull;
        }

        return b.longValue();
    }

    public static float unboxOrDefault(Float b) {
        if (b == null) {
            return 0;
        }

        return b.floatValue();
    }

    public static float unboxOrDefault(Float b, float defaultForNull) {
        if (b == null) {
            return defaultForNull;
        }

        return b.floatValue();
    }

    public static double unboxOrDefault(Double b) {
        if (b == null) {
            return 0;
        }

        return b.doubleValue();
    }

    public static double unboxOrDefault(Double b, double defaultForNull) {
        if (b == null) {
            return defaultForNull;
        }

        return b.doubleValue();
    }

    public static boolean unboxOrGet(Boolean b, BooleanSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsBoolean();
        }

        return b.booleanValue();
    }

    public static char unboxOrGet(Character b, CharSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsChar();
        }

        return b.charValue();
    }

    public static byte unboxOrGet(Byte b, ByteSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsByte();
        }

        return b.byteValue();
    }

    public static short unboxOrGet(Short b, ShortSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsShort();
        }

        return b.shortValue();
    }

    public static int unboxOrGet(Integer b, IntSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsInt();
        }

        return b.intValue();
    }

    public static long unboxOrGet(Long b, LongSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsLong();
        }

        return b.longValue();
    }

    public static float unboxOrGet(Float b, FloatSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsFloat();
        }

        return b.floatValue();
    }

    public static double unboxOrGet(Double b, DoubleSupplier supplierForNull) {
        if (b == null) {
            return supplierForNull.getAsDouble();
        }

        return b.doubleValue();
    }

    // TODO
    public static final class Ints {

        private Ints() {
            // utility class.
        }
    }

    // TODO
    public static final class Longs {

        private Longs() {
            // utility class.
        }
    }

    // TODO
    public static final class Doubles {

        private Doubles() {
            // utility class.
        }
    }
}
