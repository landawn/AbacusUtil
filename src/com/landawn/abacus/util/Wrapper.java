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

import java.lang.reflect.Method;

import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.ToIntFunction;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Wrapper<T> {
    static final ToIntFunction<Object> arrayHashFunction = new ToIntFunction<Object>() {
        @Override
        public int applyAsInt(Object value) {
            return N.deepHashCode(value);
        }
    };

    static final BiFunction<Object, Object, Boolean> arrayEqualsFunction = new BiFunction<Object, Object, Boolean>() {
        @Override
        public Boolean apply(Object t, Object u) {
            return N.deepEquals(t, u);
        }
    };

    private final T value;
    private final ToIntFunction<? super T> hashFunction;
    private final BiFunction<? super T, ? super T, Boolean> equalsFunction;
    private final Class<?> paramTypeOfHashFunction;
    private final Class<?> paramTypeOfEqualsFunction;
    private int hashCode;

    private Wrapper(T value, ToIntFunction<? super T> hashFunction, BiFunction<? super T, ? super T, Boolean> equalsFunction) {
        this.value = value;
        this.hashFunction = hashFunction;
        this.equalsFunction = equalsFunction;
        Method[] methods = hashFunction.getClass().getDeclaredMethods();

        Class<?> paramType = null;

        for (Method m : methods) {
            if (m.getName().equals("applyAsInt")) {
                paramType = m.getParameterTypes()[0];
                break;
            }
        }

        paramTypeOfHashFunction = paramType;

        for (Method m : methods) {
            if (m.getName().equals("apply")) {
                paramType = m.getParameterTypes()[0];
                break;
            }
        }

        paramTypeOfEqualsFunction = paramType;
    }

    public static <T> Wrapper<T> of(T array) {
        // return new Wrapper<T>(checkArray(array), arrayHashFunction, arrayEqualsFunction);
        return new Wrapper<T>(array, arrayHashFunction, arrayEqualsFunction);
    }

    public static <T> Wrapper<T> of(T value, ToIntFunction<? super T> hashFunction, BiFunction<? super T, ? super T, Boolean> equalsFunction) {
        return new Wrapper<T>(value, hashFunction, equalsFunction);
    }

    public T value() {
        return value;
    }

    //    static <T> T checkArray(T a) {
    //        if (a != null && a.getClass().isArray() == false) {
    //            throw new IllegalArgumentException(a.getClass().getCanonicalName() + " is not array type");
    //        }
    //
    //        return a;
    //    }

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = value == null ? 0 : paramTypeOfHashFunction.isAssignableFrom(value.getClass()) ? hashFunction.applyAsInt(value) : value.hashCode();
        }

        return hashCode;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (obj instanceof Wrapper) {
            final Wrapper<T> other = (Wrapper<T>) obj;

            if ((value == null || paramTypeOfEqualsFunction.isAssignableFrom(value.getClass()))
                    && (other.value == null || paramTypeOfEqualsFunction.isAssignableFrom(other.value.getClass()))) {

                return equalsFunction.apply(value, other.value);
            }
        }

        return false;
    }

    @Override
    public String toString() {
        return "Wrapper of Object: " + N.toString(value);
    }
}
