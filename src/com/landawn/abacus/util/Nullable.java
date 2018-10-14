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

public final class Nullable<T> {
    private static final Nullable<?> EMPTY = new Nullable<>();

    private final T value;
    private final boolean isPresent;

    private Nullable() {
        this.value = null;
        this.isPresent = false;
    }

    private Nullable(T value) {
        this.value = value;
        this.isPresent = true;
    }

    public static <T> Nullable<T> empty() {
        return (Nullable<T>) EMPTY;
    }

    public static <T> Nullable<T> of(T value) {
        return new Nullable<>(value);
    }

    public static <T> Nullable<T> from(Optional<T> optional) {
        if (optional.isPresent()) {
            return new Nullable<>(optional.get());
        } else {
            return Nullable.<T> empty();
        }
    }

    public T get() throws NoSuchElementException {
        return orElseThrow();
    }

    /**
     * Returns {@code true} if the value is present, otherwise returns {@code false}.
     * 
     * @return
     */
    public boolean isPresent() {
        return isPresent;
    }

    /**
     * Returns {@code true} if the value is not present, otherwise returns {@code false}.
     * 
     * @return
     */
    public boolean isNotPresent() {
        return isPresent == false;
    }

    /**
     * Returns {@code true} if the value is not present, otherwise returns {@code false}.
     * 
     * @return
     * @deprecated replaced by {@link #isNotPresent()}
     */
    @Deprecated
    public boolean isEmpty() {
        return isPresent == false;
    }

    /**
     * Returns {@code true} if the value is not present, or it is present but it's {@code null}, otherwise returns {@code false}.
     * 
     * @return
     */
    public boolean isNull() {
        return value == null;
    }

    /**
     * Returns {@code true} if the value is present and it's not {@code null}, otherwise returns {@code false}.
     * 
     * @return
     */
    public boolean isNotNull() {
        return value != null;
    }

    public <E extends Exception> void ifPresent(Try.Consumer<? super T, E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent()) {
            action.accept(value);
        }
    }

    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.Consumer<? super T, E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
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

    public <E extends Exception> Nullable<T> filter(Try.Predicate<? super T, E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent() && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> Optional<T> filterIfNotNull(Try.Predicate<? super T, E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isNotNull() && predicate.test(value)) {
            return Optional.of(value);
        } else {
            return Optional.empty();
        }
    }

    public <U, E extends Exception> Nullable<U> map(Try.Function<? super T, ? extends U, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return Nullable.of((U) mapper.apply(value));
        } else {
            return empty();
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

    public <U, E extends Exception> Nullable<U> mapIfNotNull(Try.Function<? super T, ? extends U, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return Nullable.of((U) mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalBoolean mapToBooleanIfNotNull(final Try.ToBooleanFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalBoolean.of(mapper.applyAsBoolean(value));
        } else {
            return OptionalBoolean.empty();
        }
    }

    public <E extends Exception> OptionalChar mapToCharIfNotNull(final Try.ToCharFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalChar.of(mapper.applyAsChar(value));
        } else {
            return OptionalChar.empty();
        }
    }

    public <E extends Exception> OptionalByte mapToByteIfNotNull(final Try.ToByteFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalByte.of(mapper.applyAsByte(value));
        } else {
            return OptionalByte.empty();
        }
    }

    public <E extends Exception> OptionalShort mapToShortIfNotNull(final Try.ToShortFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalShort.of(mapper.applyAsShort(value));
        } else {
            return OptionalShort.empty();
        }
    }

    public <E extends Exception> OptionalInt mapToIntIfNotNull(final Try.ToIntFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalInt.of(mapper.applyAsInt(value));
        } else {
            return OptionalInt.empty();
        }
    }

    public <E extends Exception> OptionalLong mapToLongIfNotNull(final Try.ToLongFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalLong.of(mapper.applyAsLong(value));
        } else {
            return OptionalLong.empty();
        }
    }

    public <E extends Exception> OptionalFloat mapToFloatIfNotNull(final Try.ToFloatFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalFloat.of(mapper.applyAsFloat(value));
        } else {
            return OptionalFloat.empty();
        }
    }

    public <E extends Exception> OptionalDouble mapToDoubleIfNotNull(final Try.ToDoubleFunction<? super T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return OptionalDouble.of(mapper.applyAsDouble(value));
        } else {
            return OptionalDouble.empty();
        }
    }

    public <U, E extends Exception> Nullable<U> flatMap(Try.Function<? super T, Nullable<U>, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <U, E extends Exception> Nullable<U> flatMapIfNotNull(Try.Function<? super T, Nullable<U>, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isNotNull()) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> Nullable<T> or(Try.Supplier<Nullable<? extends T>, E> supplier) throws E {
        Objects.requireNonNull(supplier);

        if (isPresent()) {
            return this;
        } else {
            return Objects.requireNonNull((Nullable<T>) supplier.get());
        }
    }

    public <E extends Exception> Nullable<T> orIfNull(Try.Supplier<Nullable<? extends T>, E> supplier) throws E {
        Objects.requireNonNull(supplier);

        if (isNotNull()) {
            return this;
        } else {
            return Objects.requireNonNull((Nullable<T>) supplier.get());
        }
    }

    public T orNull() {
        return isPresent() ? value : null;
    }

    //    public T orElseNull() {
    //        return isPresent() ? value : null;
    //    }

    public T orElse(T other) {
        return isPresent() ? value : other;
    }

    public <E extends Exception> T orElseGet(Try.Supplier<? extends T, E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent()) {
            return value;
        } else {
            return other.get();
        }
    }

    public T orElseThrow() throws NoSuchElementException {
        if (isPresent()) {
            return value;
        } else {
            throw new NoSuchElementException("No value is present");
        }
    }

    public <X extends Throwable> T orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent()) {
            return value;
        } else {
            throw exceptionSupplier.get();
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

    public T orElseThrowIfNull() throws NoSuchElementException {
        if (isNotNull()) {
            return value;
        } else {
            throw new NoSuchElementException("No value is present");
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

    public Stream<T> stream() {
        if (isPresent()) {
            return Stream.of(value);
        } else {
            return Stream.<T> empty();
        }
    }

    public Stream<T> streamIfNotNull() {
        if (isNotNull()) {
            return Stream.of(value);
        } else {
            return Stream.<T> empty();
        }
    }

    public List<T> toList() {
        if (isPresent()) {
            return N.asList(value);
        } else {
            return new ArrayList<>();
        }
    }

    public List<T> toListIfNotNull() {
        if (isNotNull()) {
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

    public Set<T> toSetIfNotNull() {
        if (isNotNull()) {
            return N.asSet(value);
        } else {
            return new HashSet<>();
        }
    }

    public ImmutableList<T> toImmutableList() {
        if (isPresent()) {
            return ImmutableList.of(value);
        } else {
            return ImmutableList.empty();
        }
    }

    public ImmutableList<T> toImmutableListIfNotNull() {
        if (isNotNull()) {
            return ImmutableList.of(value);
        } else {
            return ImmutableList.empty();
        }
    }

    public ImmutableSet<T> toImmutableSet() {
        if (isPresent()) {
            return ImmutableSet.of(value);
        } else {
            return ImmutableSet.empty();
        }
    }

    public ImmutableSet<T> toImmutableSetIfNotNull() {
        if (isNotNull()) {
            return ImmutableSet.of(value);
        } else {
            return ImmutableSet.empty();
        }
    }

    public Optional<T> toOptional() {
        if (value == null) {
            return Optional.<T> empty();
        } else {
            return Optional.of(value);
        }
    }

    public java.util.Optional<T> toJdkOptional() {
        if (value == null) {
            return java.util.Optional.<T> empty();
        } else {
            return java.util.Optional.of(value);
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof Nullable) {
            final Nullable<?> other = (Nullable<?>) obj;

            return N.equals(isPresent, other.isPresent) && N.equals(value, other.value);
        }

        return false;
    }

    @Override
    public int hashCode() {
        return N.hashCode(isPresent) * 31 + N.hashCode(value);
    }

    @Override
    public String toString() {
        if (value == null) {
            return isPresent ? "Nullable[null]" : "Nullable.empty";
        } else {
            return String.format("Nullable[%s]", value);
        }
    }
}
