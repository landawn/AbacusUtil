/*
 * Copyright (C) 2018 HaiYang Li
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Stream;

@Beta
/**
 * 
 * @since 1.2
 * 
 * @author Haiyang Li
 */
abstract class Any<T> {

    final T value;
    final boolean isPresent;

    /**
     * Constructs an empty instance.
     *
     * @implNote Generally only one empty instance, {@link Any#EMPTY},
     * should exist per VM.
     */
    Any() {
        this.value = null;
        isPresent = false;
    }

    /**
     * Constructs an instance with the value present.
     *
     * @param value the nullable value to be present
     */
    Any(T value) {
        this.value = value;
        isPresent = true;
    }

    /**
     * Returns an empty {@code Any} instance.  No value is present for this
     * Optional.
     *
     * @apiNote Though it may be tempting to do so, avoid testing if an object
     * is empty by comparing with {@code ==} against instances returned by
     * {@code Any.empty()}. There is no guarantee that it is a singleton.
     * Instead, use {@link #isPresent()}.
     *
     * @param <T> Type of the non-existent value
     * @return an empty {@code Any}
     */
    public static <T> Any<T> empty() {
        return Nullable.empty();
    }

    /**
     * Returns an {@code Any} with the specified present nullable value.
     *
     * @param value the value to be present, which could be null
     * @return an {@code Any} with the value present
     */
    public static <T> Any<T> of(T value) {
        return Nullable.of(value);
    }

    /**
     * If a value is present in this {@code Any}, returns the value,
     * otherwise throws {@code NoSuchElementException}.
     *
     * @return the nullable value held by this {@code Any}
     * @throws NoSuchElementException if there is no value present
     *
     * @see Any#isPresent()
     */
    public T get() throws NoSuchElementException {
        return orElseThrow();
    }

    /**
     * Return {@code true} if there is a value present, otherwise {@code false}.
     *
     * @return {@code true} if there is a value present, otherwise {@code false}
     */
    public boolean isPresent() {
        return isPresent;
    }

    /**
     * Return {@code true} if there is a value not null, otherwise {@code false}.
     *
     * @return {@code true} if there is a value not null, otherwise {@code false}
     */
    public boolean isNotNull() {
        return value != null;
    }

    /**
     * If a value is present, invoke the specified consumer with the value,
     * otherwise do nothing.
     *
     * @param consumer block to be executed if a value is present
     * @throws IllegalArgumentException if value is present and {@code consumer} is
     * null
     */
    public <E extends Exception> void ifPresent(Try.Consumer<? super T, E> consumer) throws E {
        if (isPresent()) {
            consumer.accept(value);
        }
    }

    /**
    * If a value is present, performs the given action with the value, otherwise performs the given empty-based action.
    *
    * @param action
    * @param emptyAction
    */
    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.Consumer<? super T, E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        if (isPresent()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    /**
     * If a value is not null, invoke the specified consumer with the value,
     * otherwise do nothing.
     *
     * @param consumer block to be executed if a value is not null.
     * @throws IllegalArgumentException if value is present and {@code consumer} is
     * null
     */
    public <E extends Exception> void ifNotNull(Try.Consumer<? super T, E> consumer) throws E {
        if (isNotNull()) {
            consumer.accept(value);
        }
    }

    /**
    * If a value is not null, performs the given action with the value, otherwise performs the given empty-based action.
    *
    * @param action
    * @param emptyAction
    */
    public <E extends Exception, E2 extends Exception> void ifNotNullOrElse(Try.Consumer<? super T, E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        if (isNotNull()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    /**
     * If a value is present, and the value matches the given predicate,
     * return an {@code Any} describing the value, otherwise return an
     * empty {@code Any}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code Any} describing the value of this {@code Any}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code Any}
     * @throws IllegalArgumentException if the predicate is null
     */
    public abstract <E extends Exception> Any<T> filter(Try.Predicate<? super T, E> predicate) throws E;

    /**
     * If a value is present, apply the provided mapping function to it,
     *
     * @apiNote This method supports post-processing on optional values, without
     * the need to explicitly check for a return status.  For example, the
     * following code traverses a stream of file names, selects one that has
     * not yet been processed, and then opens that file, returning an
     * {@code Optional<FileInputStream>}:
     *
     * <pre>{@code
     *     Any<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code Any<String>}, and then
     * {@code map} returns an {@code Any<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if present
     * @return an {@code Any} describing the result of applying a mapping
     * function to the value of this {@code Any}, if a value is present,
     * otherwise an empty {@code Any}
     * @throws IllegalArgumentException if the mapping function is null
     */
    public abstract <U, E extends Exception> Any<U> map(Try.Function<? super T, ? extends U, E> mapper) throws E;

    public <E extends Exception> OptionalBoolean mapToBoolean(final Try.ToBooleanFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalBoolean.of(mapper.applyAsBoolean(value));
        } else {
            return OptionalBoolean.empty();
        }
    }

    public <E extends Exception> OptionalChar mapToChar(final Try.ToCharFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalChar.of(mapper.applyAsChar(value));
        } else {
            return OptionalChar.empty();
        }
    }

    public <E extends Exception> OptionalByte mapToByte(final Try.ToByteFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalByte.of(mapper.applyAsByte(value));
        } else {
            return OptionalByte.empty();
        }
    }

    public <E extends Exception> OptionalShort mapToShort(final Try.ToShortFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalShort.of(mapper.applyAsShort(value));
        } else {
            return OptionalShort.empty();
        }
    }

    public <E extends Exception> OptionalInt mapToInt(final Try.ToIntFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalInt.of(mapper.applyAsInt(value));
        } else {
            return OptionalInt.empty();
        }
    }

    public <E extends Exception> OptionalLong mapToLong(final Try.ToLongFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalLong.of(mapper.applyAsLong(value));
        } else {
            return OptionalLong.empty();
        }
    }

    public <E extends Exception> OptionalFloat mapToFloat(final Try.ToFloatFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalFloat.of(mapper.applyAsFloat(value));
        } else {
            return OptionalFloat.empty();
        }
    }

    public <E extends Exception> OptionalDouble mapToDouble(final Try.ToDoubleFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isPresent()) {
            return OptionalDouble.of(mapper.applyAsDouble(value));
        } else {
            return OptionalDouble.empty();
        }
    }

    /**
     * If a value is present, apply the provided {@code Any}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code Any}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code Any},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code Any}.
     *
     * @param <U> The type parameter to the {@code Any} returned by
     * @param mapper a mapping function to apply to the value, if present
     *           the mapping function
     * @return the result of applying an {@code Any}-bearing mapping
     * function to the value of this {@code Any}, if a value is present,
     * otherwise an empty {@code Any}
     * @throws IllegalArgumentException if the mapping function is null or returns
     * a null result
     */
    public abstract <U, E extends Exception> Any<U> flatMap(Try.Function<? super T, ? extends Any<U>, E> mapper) throws E;

    /**
     * If a value is not null, and the value matches the given predicate,
     * return an {@code Any} describing the value, otherwise return an
     * empty {@code Any}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code Any} describing the value of this {@code Any}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code Any}
     * @throws IllegalArgumentException if the predicate is null
     */
    public abstract <E extends Exception> Optional<T> filterIfNotNull(Try.Predicate<? super T, E> predicate) throws E;

    /**
     * If a value is not null, apply the provided mapping function to it,
     *
     * @apiNote This method supports post-processing on optional values, without
     * the need to explicitly check for a return status.  For example, the
     * following code traverses a stream of file names, selects one that has
     * not yet been processed, and then opens that file, returning an
     * {@code Optional<FileInputStream>}:
     *
     * <pre>{@code
     *     Any<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code Any<String>}, and then
     * {@code map} returns an {@code Optional<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if not null
     * @return an {@code Any} describing the result of applying a mapping
     * function to the value of this {@code Any}, if a value is not null,
     * otherwise an empty {@code Any}
     * @throws IllegalArgumentException if the mapping function is null
     */
    public abstract <U, E extends Exception> Any<U> mapIfNotNull(Try.Function<? super T, ? extends U, E> mapper) throws E;

    public <E extends Exception> OptionalBoolean mapToBooleanIfNotNull(final Try.ToBooleanFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalBoolean.of(mapper.applyAsBoolean(value));
        } else {
            return OptionalBoolean.empty();
        }
    }

    public <E extends Exception> OptionalChar mapToCharIfNotNull(final Try.ToCharFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalChar.of(mapper.applyAsChar(value));
        } else {
            return OptionalChar.empty();
        }
    }

    public <E extends Exception> OptionalByte mapToByteIfNotNull(final Try.ToByteFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalByte.of(mapper.applyAsByte(value));
        } else {
            return OptionalByte.empty();
        }
    }

    public <E extends Exception> OptionalShort mapToShortIfNotNull(final Try.ToShortFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalShort.of(mapper.applyAsShort(value));
        } else {
            return OptionalShort.empty();
        }
    }

    public <E extends Exception> OptionalInt mapToIntIfNotNull(final Try.ToIntFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalInt.of(mapper.applyAsInt(value));
        } else {
            return OptionalInt.empty();
        }
    }

    public <E extends Exception> OptionalLong mapToLongIfNotNull(final Try.ToLongFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalLong.of(mapper.applyAsLong(value));
        } else {
            return OptionalLong.empty();
        }
    }

    public <E extends Exception> OptionalFloat mapToFloatIfNotNull(final Try.ToFloatFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalFloat.of(mapper.applyAsFloat(value));
        } else {
            return OptionalFloat.empty();
        }
    }

    public <E extends Exception> OptionalDouble mapToDoubleIfNotNull(final Try.ToDoubleFunction<? super T, E> mapper) throws E {
        N.checkArgNotNull(mapper);

        if (isNotNull()) {
            return OptionalDouble.of(mapper.applyAsDouble(value));
        } else {
            return OptionalDouble.empty();
        }
    }

    /**
     * If a value is not null, apply the provided {@code Any}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code Any}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code Any},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code Any}.
     *
     * @param <U> The type parameter to the {@code Any} returned by
     * @param mapper a mapping function to apply to the value, if not null
     *           the mapping function
     * @return the result of applying an {@code Any}-bearing mapping
     * function to the value of this {@code Any}, if a value is not null,
     * otherwise an empty {@code Any}
     * @throws IllegalArgumentException if the mapping function is null or returns
     * a null result
     */
    public abstract <U, E extends Exception> Any<U> flatMapIfNotNull(Try.Function<? super T, ? extends Any<U>, E> mapper) throws E;

    /**
     * If a value is present, returns an Optional describing the value, otherwise returns an Optional produced by the supplying function.
     * 
     * @param supplier
     * @return
     * @throws E
     */
    public abstract <E extends Exception> Any<T> or(Try.Supplier<? extends Any<T>, E> supplier) throws E;

    /**
     * Same as {@code orElseNull}.
     * 
     * @return
     */
    public T orNull() {
        return isPresent() ? value : null;
    }

    //    /**
    //     * Same as {@code orNull}
    //     * 
    //     * @return
    //     */
    //    public T orElseNull() {
    //        return isPresent() ? value : null;
    //    }

    /**
     * If a value is present, returns the value, otherwise throws NoSuchElementException.
     * 
     * @return
     * @throws NoSuchElementException - if no value is present
     */
    public T orElseThrow() throws NoSuchElementException {
        if (isPresent()) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    /**
     * Return the value if present, otherwise return {@code other}.
     *
     * @param other the value to be returned if there is no value present, may be null
     * @return the value, if present, otherwise {@code other}
     */
    public T orElse(T other) {
        return isPresent() ? value : other;
    }

    /**
     * Return the value if present, otherwise invoke {@code other} and return the result of that invocation.
     *
     * @param other a {@code Supplier} whose result is returned if no value is present
     * @return the value if present otherwise the result of {@code other.get()}
     * @throws IllegalArgumentException if value is not present and {@code other} is
     * null
     */
    public <E extends Exception> T orElseGet(Try.Supplier<? extends T, E> other) throws E {
        return isPresent() ? value : other.get();
    }

    /**
     * Return the contained value, if present, otherwise throw an exception to be created by the provided supplier.
     *
     * @apiNote A method reference to the exception constructor with an empty
     * argument list can be used as the supplier. For example,
     * {@code IllegalStateException::new}
     *
     * @param <X> Type of the exception to be thrown
     * @param exceptionSupplier The supplier which will return the exception to
     * be thrown
     * @return the present value
     * @throws X if there is no value present
     * @throws IllegalArgumentException if no value is present and
     * {@code exceptionSupplier} is null
     */
    public <X extends Throwable> T orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        if (isPresent()) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    /**
     * Return the value is not null, otherwise return {@code other}.
     *
     * @param other the value to be returned if not present or null, may be null
     * @return the value, if not present or null, otherwise {@code other}
     */
    public T orIfNull(T other) {
        return isNotNull() ? value : other;
    }

    /**
     * Return the value is not null, otherwise invoke {@code other} and return the result of that invocation.
     *
     * @param other a {@code Supplier} whose result is returned if not present or null
     * @return the value if not present or null otherwise the result of {@code other.get()}
     * @throws IllegalArgumentException if value is not present and {@code other} is null
     */
    public <E extends Exception> T orGetIfNull(Try.Supplier<? extends T, E> other) throws E {
        return isNotNull() ? value : other.get();
    }

    /**
     * Return the value is not null, otherwise throw an exception to be created by the provided supplier.
     *
     * @apiNote A method reference to the exception constructor with an empty
     * argument list can be used as the supplier. For example,
     * {@code IllegalStateException::new}
     *
     * @param <X> Type of the exception to be thrown
     * @param exceptionSupplier The supplier which will return the exception to be thrown
     * @return the present value
     * @throws X if not present or null
     * @throws IllegalArgumentException if not present or null and
     * {@code exceptionSupplier} is null
     */
    public <X extends Throwable> T orThrowIfNull(Supplier<? extends X> exceptionSupplier) throws X {
        if (isNotNull()) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    /**
     * If a value is present, returns the value, otherwise throws NoSuchElementException.
     * 
     * @return
     * @throws NoSuchElementException - if no value is present
     */
    public T orThrowIfNull() throws NoSuchElementException {
        if (isPresent()) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    /**
     * 
     * @return an empty stream if it's not present.
     */
    public Stream<T> stream() {
        return isPresent() ? Stream.of(value) : Stream.<T> empty();
    }

    /**
     * 
     * @return an empty stream if it's null.
     */
    public Stream<T> streamIfNotNull() {
        return isNotNull() ? Stream.of(value) : Stream.<T> empty();
    }

    /**
     * Returns a {@code List} with the value if it presents, otherwise an empty {@code List}.
     * 
     * @return
     */
    public List<T> toList() {
        return isPresent() ? N.asList(value) : new ArrayList<T>();
    }

    /**
     * Returns a {@code List} with the value if the value is not null, otherwise an empty {@code List}.
     * 
     * @return
     */
    public List<T> toListIfNotNull() {
        return isNotNull() ? N.asList(value) : new ArrayList<T>();
    }

    /**
     * Returns a {@code Set} with the value if it presents, otherwise an empty {@code Set}.
     * 
     * @return
     */
    public Set<T> toSet() {
        return isPresent() ? N.asSet(value) : new HashSet<T>();
    }

    /**
     * Returns a {@code Set} with the value if the value is not null, otherwise an empty {@code Set}.
     * 
     * @return
     */
    public Set<T> toSetIfNotNull() {
        return isNotNull() ? N.asSet(value) : new HashSet<T>();
    }

    /**
     * Returns a {@code ImmutableList} with the value if it presents, otherwise an empty {@code List}.
     * 
     * @return
     */
    public ImmutableList<T> toImmutableList() {
        return isPresent() ? ImmutableList.of(value) : ImmutableList.<T> empty();
    }

    /**
     * Returns a {@code ImmutableList} with the value if it presents, otherwise an empty {@code List}.
     * 
     * @return
     */
    public ImmutableList<T> toImmutableListIfNotNull() {
        return isNotNull() ? ImmutableList.of(value) : ImmutableList.<T> empty();
    }

    /**
     * Returns a {@code ImmutableSet} with the value if it presents, otherwise an empty {@code Set}.
     * 
     * @return
     */
    public ImmutableSet<T> toImmutableSet() {
        return isPresent() ? ImmutableSet.of(value) : ImmutableSet.<T> empty();
    }

    /**
     * Returns a {@code ImmutableSet} with the value if it presents, otherwise an empty {@code Set}.
     * 
     * @return
     */
    public ImmutableSet<T> toImmutableSetIfNotNull() {
        return isNotNull() ? ImmutableSet.of(value) : ImmutableSet.<T> empty();
    }

    /**
     * 
     * @return <code>Optional.empty()</code> if the value is not present or {@code null}.
     */
    public Optional<T> toOptional() {
        return value == null ? Optional.<T> empty() : Optional.of(value);
    }

    /**
     * 
     * @return <code>java.util.Optional.empty()</code> if the value is not present or {@code null}.
     */
    public java.util.Optional<T> toJdkOptional() {
        return value == null ? java.util.Optional.<T> empty() : java.util.Optional.of(value);
    }

    /**
     * 
     * @return <code>Optional.empty()</code> if the value is not present or {@code null}.
     */
    public Optional<T> __() {
        return value == null ? Optional.<T> empty() : Optional.of(value);
    }
}
