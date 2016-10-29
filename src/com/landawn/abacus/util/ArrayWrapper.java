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

import com.landawn.abacus.type.Type;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ArrayWrapper<T> {
    private T value;
    private Type<Object> type;
    private int hashCode;

    private ArrayWrapper(T value) {
        this.value = value;
        this.type = value == null ? null : N.typeOf(value.getClass());
    }

    public static <T> ArrayWrapper<T> of(T value) {
        return new ArrayWrapper<T>(value);
    }

    public T getValue() {
        return value;
    }

    public void setValue(T value) {
        this.value = value;
        this.type = value == null ? null : N.typeOf(value.getClass());
        this.hashCode = 0;
    }

    public void clear() {
        this.value = null;
        this.type = null;
        this.hashCode = 0;
    }

    @Override
    public int hashCode() {
        if (hashCode == 0) {
            hashCode = value == null ? 0 : type.deepHashCode(value);
        }

        return hashCode;
    }

    @Override
    public boolean equals(Object obj) {
        return (obj == this) || (obj instanceof ArrayWrapper && N.deepEquals(((ArrayWrapper<T>) obj).value, value));

    }

    @Override
    public String toString() {
        return value == null ? N.NULL_STRING : type.deepToString(value);
    }
}
