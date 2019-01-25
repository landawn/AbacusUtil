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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Set;

import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Stream;

public final class Optional<T> {
    private static final Optional<?> EMPTY = new Optional<>();

    private final T value;

    private Optional() {
        this.value = null;
    }

    private Optional(T value) {
        this.value = Objects.requireNonNull(value);
    }

    public static <T> Optional<T> empty() {
        return (Optional<T>) EMPTY;
    }

    public static <T> Optional<T> of(T value) {
        return new Optional<>(value);
    }

    public static <T> Optional<T> ofNullable(T value) {
        if (value == null) {
            return empty();
        }

        return new Optional<>(value);
    }

    public static <T> Optional<T> from(java.util.Optional<T> optional) {
        return optional.isPresent() ? of(optional.get()) : Optional.<T> empty();
    }

    public T get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return value != null;
    }

    public boolean isEmpty() {
        return value == null;
    }

    /**
     * 
     * @param action
     * @return itself
     * @throws E
     */
    public <E extends Exception> Optional<T> ifPresent(Try.Consumer<? super T, E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent()) {
            action.accept(value);
        }

        return this;
    }

    /**
     * 
     * @param action
     * @param emptyAction
     * @return itself
     * @throws E
     * @throws E2
     */
    public <E extends Exception, E2 extends Exception> Optional<T> ifPresentOrElse(Try.Consumer<? super T, E> action, Try.Runnable<E2> emptyAction)
            throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }

        return this;
    }

    public <E extends Exception> Optional<T> filter(Try.Predicate<? super T, E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent() && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <U, E extends Exception> Nullable<U> map(final Try.Function<? super T, ? extends U, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return Nullable.<U> of(mapper.apply(value));
        } else {
            return Nullable.<U> empty();
        }
    }

    public <E extends Exception> OptionalBoolean mapToBoolean(final Try.ToBooleanFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalBoolean.of(mapper.applyAsBoolean(value));
        } else {
            return OptionalBoolean.empty();
        }
    }

    public <E extends Exception> OptionalChar mapToChar(final Try.ToCharFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalChar.of(mapper.applyAsChar(value));
        } else {
            return OptionalChar.empty();
        }
    }

    public <E extends Exception> OptionalByte mapToByte(final Try.ToByteFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalByte.of(mapper.applyAsByte(value));
        } else {
            return OptionalByte.empty();
        }
    }

    public <E extends Exception> OptionalShort mapToShort(final Try.ToShortFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalShort.of(mapper.applyAsShort(value));
        } else {
            return OptionalShort.empty();
        }
    }

    public <E extends Exception> OptionalInt mapToInt(final Try.ToIntFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalInt.of(mapper.applyAsInt(value));
        } else {
            return OptionalInt.empty();
        }
    }

    public <E extends Exception> OptionalLong mapToLong(final Try.ToLongFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalLong.of(mapper.applyAsLong(value));
        } else {
            return OptionalLong.empty();
        }
    }

    public <E extends Exception> OptionalFloat mapToFloat(final Try.ToFloatFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalFloat.of(mapper.applyAsFloat(value));
        } else {
            return OptionalFloat.empty();
        }
    }

    public <E extends Exception> OptionalDouble mapToDouble(final Try.ToDoubleFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalDouble.of(mapper.applyAsDouble(value));
        } else {
            return OptionalDouble.empty();
        }
    }

    public <U, E extends Exception> Optional<U> flatMap(Try.Function<? super T, Optional<U>, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> Optional<T> or(Try.Supplier<Optional<? extends T>, E> supplier) throws E {
        Objects.requireNonNull(supplier);

        if (isPresent()) {
            return this;
        } else {
            return Objects.requireNonNull((Optional<T>) supplier.get());
        }
    }

    public T orNull() {
        return isPresent() ? value : null;
    }

    public T orElse(T other) {
        return isPresent() ? value : other;
    }

    public <E extends Exception> T orElseGet(Try.Supplier<? extends T, E> other) throws E {
        if (isPresent()) {
            return value;
        } else {
            return other.get();
        }
    }

    //    public T orElseNull() {
    //        return isPresent() ? value : null;
    //    }

    public T orElseThrow() throws NoSuchElementException {
        if (isPresent()) {
            return value;
        } else {
            throw new NoSuchElementException("No value is present");
        }
    }

    public <X extends Throwable> T orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        if (isPresent()) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    public Stream<T> stream() {
        if (isPresent()) {
            return Stream.of(value);
        } else {
            return Stream.<T> empty();
        }
    }

    public java.util.Optional<T> __() {
        return isPresent() ? java.util.Optional.of(value) : java.util.Optional.<T> empty();
    }

    public List<T> toList() {
        if (isPresent()) {
            return N.asList(value);
        } else {
            return new ArrayList<>();
        }
    }

    public Set<T> toSet() {
        if (isPresent()) {
            return N.asSet(value);
        } else {
            return new HashSet<>();
        }
    }

    public ImmutableList<T> toImmutableList() {
        if (isPresent()) {
            return ImmutableList.of(value);
        } else {
            return ImmutableList.<T> empty();
        }
    }

    public ImmutableSet<T> toImmutableSet() {
        if (isPresent()) {
            return ImmutableSet.of(value);
        } else {
            return ImmutableSet.<T> empty();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof Optional) {
            final Optional<?> other = (Optional<?>) obj;

            return N.equals(value, other.value);
        }

        return false;
    }

    @Override
    public int hashCode() {
        return N.hashCode(isPresent()) * 31 + N.hashCode(value);
    }

    @Override
    public String toString() {
        if (isPresent()) {
            return String.format("Optional[%s]", value);
        }

        return "Optional.empty";
    }
}
