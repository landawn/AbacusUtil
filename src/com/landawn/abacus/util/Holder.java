/*
 * Copyright (c) 2017, Haiyang Li.
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

public final class Holder<T> extends Reference<T, Holder<T>> {
    public Holder() {
        this(null);
    }

    Holder(T value) {
        super(value);
    }

    public static <T> Holder<T> of(T value) {
        return new Holder<>(value);
    }

    public static final class R<T> extends Reference<T, R<T>> {
        public R() {
            this(null);
        }

        R(T value) {
            super(value);
        }

        public static <T> R<T> of(T value) {
            return new R<>(value);
        }
    }
}