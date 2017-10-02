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
    private int hashCode;

    private Wrapper(T value, ToIntFunction<? super T> hashFunction, BiFunction<? super T, ? super T, Boolean> equalsFunction) {
        this.value = value;
        this.hashFunction = hashFunction;
        this.equalsFunction = equalsFunction;
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
            hashCode = value == null ? 0 : hashFunction.applyAsInt(value);
        }

        return hashCode;
    }

    @Override
    public boolean equals(Object obj) {
        return (obj == this) || (obj instanceof Wrapper && equalsFunction.apply(((Wrapper<T>) obj).value, value));
    }

    @Override
    public String toString() {
        return "Wrapper of Object: " + N.toString(value);
    }
}
