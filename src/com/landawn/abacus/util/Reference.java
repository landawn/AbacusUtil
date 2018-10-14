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

import java.util.Objects;

import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Stream;

abstract class Reference<T, R extends Reference<T, R>> {
    private T value;

    protected Reference() {
        this(null);
    }

    protected Reference(T value) {
        this.value = value;
    }

    public T value() {
        return value;
    }

    /**
     * 
     * @return
     * @deprecated replace by {@link #value()}.
     */
    @Deprecated
    public T getValue() {
        return value;
    }

    public R setValue(final T value) {
        this.value = value;

        return (R) this;
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

    public final <E extends Exception> T getAndUpdate(Try.UnaryOperator<T, E> updateFunction) throws E {
        final T res = value;
        this.value = updateFunction.apply(value);
        return res;
    }

    public final <E extends Exception> T updateAndGet(Try.UnaryOperator<T, E> updateFunction) throws E {
        this.value = updateFunction.apply(value);
        return value;
    }

    /**
     * Set with the specified new value and returns <code>true</code> if <code>predicate</code> returns true.
     * Otherwise just return <code>false</code> without setting the value to new value.
     * 
     * @param newValue
     * @param predicate - test the current value.
     * @return
     */
    public <E extends Exception> boolean setIf(final T newValue, final Try.Predicate<? super T, E> predicate) throws E {
        if (predicate.test(value)) {
            this.value = newValue;
            return true;
        }

        return false;
    }

    /**
     * Set with the specified new value and returns <code>true</code> if <code>predicate</code> returns true.
     * Otherwise just return <code>false</code> without setting the value to new value.
     * 
     * @param newValue
     * @param predicate the first parameter is the current value, the second parameter is the new value.
     * @return
     */
    public <E extends Exception> boolean setIf(final T newValue, final Try.BiPredicate<? super T, ? super T, E> predicate) throws E {
        if (predicate.test(value, newValue)) {
            this.value = newValue;
            return true;
        }

        return false;
    }

    public boolean isNull() {
        return value == null;
    }

    public boolean isNotNull() {
        return value != null;
    }

    public <E extends Exception> void ifNotNull(Try.Consumer<? super T, E> action) throws E {
        Objects.requireNonNull(action);

        if (isNotNull()) {
            action.accept(value);
        }
    }

    public <E extends Exception, E2 extends Exception> void ifNotNullOrElse(Try.Consumer<? super T, E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isNotNull()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    public <E extends Exception> void accept(final Try.Consumer<? super T, E> action) throws E {
        action.accept(value);
    }

    @Deprecated
    public <E extends Exception> void acceptIfNotNull(final Try.Consumer<? super T, E> action) throws E {
        Objects.requireNonNull(action);

        if (isNotNull()) {
            action.accept(value);
        }
    }

    public <U, E extends Exception> U map(final Try.Function<? super T, ? extends U, E> mapper) throws E {
        return mapper.apply(value);
    }

    public <U, E extends Exception> Nullable<U> mapIfNotNull(final Try.Function<? super T, ? extends U, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return Nullable.of((U) mapper.apply(value));
        } else {
            return Nullable.<U> empty();
        }
    }

    public <E extends Exception> Nullable<T> filter(final Try.Predicate<? super T, E> predicate) throws E {
        if (predicate.test(value)) {
            return Nullable.of(value);
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> Optional<T> filterIfNotNull(final Try.Predicate<? super T, E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isNotNull() && predicate.test(value)) {
            return Optional.of(value);
        } else {
            return Optional.<T> empty();
        }
    }

    public Stream<T> stream() {
        return Stream.of(value);
    }

    public Stream<T> streamIfNotNull() {
        if (isNotNull()) {
            return Stream.of(value);
        } else {
            return Stream.<T> empty();
        }
    }

    public T orElseIfNull(T other) {
        return isNotNull() ? value : other;
    }

    public <E extends Exception> T orElseGetIfNull(Try.Supplier<? extends T, E> other) throws E {
        Objects.requireNonNull(other);

        if (isNotNull()) {
            return value;
        } else {
            return other.get();
        }
    }

    public <X extends Throwable> T orElseThrowIfNull(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isNotNull()) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    /**
     * Returns a non-empty {@code Nullable} with the {@code value}.
     * 
     * @return
     */
    public Nullable<T> toNullable() {
        return Nullable.of(value);
    }

    /**
     * Returns an {@code Optional} with the {@code value} if {@code value} is not null, otherwise an empty {@code Optional} is returned.
     * 
     * @return
     */
    public Optional<T> toOptional() {
        return Optional.ofNullable(value);
    }

    @Override
    public int hashCode() {
        return (value == null) ? 0 : value.hashCode();
    }

    @SuppressWarnings("rawtypes")
    @Override
    public boolean equals(final Object obj) {
        return this == obj || (obj instanceof Reference && N.equals(((Reference) obj).value, value));
    }

    @Override
    public String toString() {
        return N.toString(value);
    }
}