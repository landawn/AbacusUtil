/*
 * Copyright (C) 2015 HaiYang Li
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

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Output<T> {
    private volatile T value;

    public Output() {
    }

    public Output(T value) {
        this.value = value;
    }

    public static <T> Output<T> of(T value) {
        return new Output<>(value);
    }

    public T value() {
        return value;
    }

    public T getValue() {
        return value;
    }

    public Output<T> setValue(final T value) {
        this.value = value;

        return this;
    }

    public T getAndSet(final T value) {
        final T result = this.value;
        this.value = value;
        return result;
    }

    public T setAndGet(final T value) {
        this.value = value;
        return this.value;
    }

    public boolean isNotNull() {
        return value != null;
    }

    public void accept(final Consumer<? super T> action) {
        action.accept(value);
    }

    public void acceptIfNotNull(final Consumer<? super T> action) {
        if (isNotNull()) {
            action.accept(value);
        }
    }

    public <U> U apply(final Function<? super T, U> action) {
        return action.apply(value);
    }

    /**
     * Execute the specified action if value is not null, otherwise return null directly.
     * 
     * @param action
     * @return
     */
    public <U> U applyIfNotNull(final Function<? super T, U> action) {
        if (isNotNull()) {
            return action.apply(value);
        } else {
            return null;
        }
    }

    public Stream<T> stream() {
        return Stream.of(value);
    }

    /**
     * 
     * @return an empty Stream if the value is null.
     */
    public Stream<T> streamIfNotNull() {
        return isNotNull() ? Stream.of(value) : Stream.<T> empty();
    }

    public Output0<T> __() {
        return Output0.of(value);
    }

    @Override
    public int hashCode() {
        return (value == null) ? 0 : value.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        return this == obj || (obj instanceof Output && N.equals(((Output<T>) obj).value, value));
    }

    @Override
    public String toString() {
        return N.toString(value);
    }

    public static final class Output0<T> {
        private final T value;

        public Output0(T value) {
            this.value = value;
        }

        public static <T> Output0<T> of(T value) {
            return new Output0<>(value);
        }

        public T value() {
            return value;
        }

        public boolean isNotNull() {
            return value != null;
        }

        public void accept(final Consumer<? super T> action) {
            action.accept(value);
        }

        public void acceptIfNotNull(final Consumer<? super T> action) {
            if (isNotNull()) {
                action.accept(value);
            }
        }

        public <U> U apply(final Function<? super T, U> action) {
            return action.apply(value);
        }

        /**
         * Execute the specified action if value is not null, otherwise return null directly.
         * 
         * @param action
         * @return
         */
        public <U> U applyIfNotNull(final Function<? super T, U> action) {
            if (isNotNull()) {
                return action.apply(value);
            } else {
                return null;
            }
        }

        public Stream<T> stream() {
            return Stream.of(value);
        }

        /**
         * 
         * @return an empty Stream if the value is null.
         */
        public Stream<T> streamIfNotNull() {
            return isNotNull() ? Stream.of(value) : Stream.<T> empty();
        }

        public Output<T> __() {
            return Output.of(value);
        }

        @Override
        public int hashCode() {
            return (value == null) ? 0 : value.hashCode();
        }

        @Override
        public boolean equals(final Object obj) {
            return this == obj || (obj instanceof Output0 && N.equals(((Output0<T>) obj).value, value));
        }

        @Override
        public String toString() {
            return N.toString(value);
        }
    }
}