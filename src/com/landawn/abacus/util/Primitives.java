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
        N.checkArgNotNull(cls, "cls");

        return N.typeOf(cls).isPrimitiveType();
    }

    public static boolean isWrapperType(final Class<?> cls) {
        N.checkArgNotNull(cls, "cls");

        return N.typeOf(cls).isPrimitiveWrapper();
    }

    public static boolean isPrimitiveArrayType(final Class<?> cls) {
        N.checkArgNotNull(cls, "cls");

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
        N.checkArgNotNull(cls, "cls");

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
        N.checkArgNotNull(cls, "cls");

        Class<?> unwrapped = PRIMITIVE_2_WRAPPER.getByValue(cls);

        return unwrapped == null ? cls : unwrapped;
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
