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

/**
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 */
public class Timed<T> {
    private final T value;
    private final long timeInMillis;

    private Timed(T value, long timeInMillis) {
        this.value = value;
        this.timeInMillis = timeInMillis;
    }

    public static <T> Timed<T> of(T value, long timeInMillis) {
        return new Timed<>(value, timeInMillis);
    }

    public T value() {
        return value;
    }

    public long time() {
        return timeInMillis;
    }
}
