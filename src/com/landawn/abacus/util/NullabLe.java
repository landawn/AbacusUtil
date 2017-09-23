/*
 * Copyright (c) 2012, 2013, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package com.landawn.abacus.util;

import java.util.NoSuchElementException;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Stream;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 * 
 * A container object which may or may not contain a nullable value.
 * If a value is present, {@code isPresent()} will return {@code true} and
 * {@code get()} will return the value.
 *
 * <p>Additional methods that depend on the presence or absence of a contained
 * value are provided, such as {@link #or(java.lang.Object) orElse()}
 * (return a default value if value not present) and
 * {@link #ifPresent(java.util.function.Consumers) ifPresent()} (execute a block
 * of code if the value is present).
 *
 * <p>This is a <a href="../lang/doc-files/ValueBased.html">value-based</a>
 * class; use of identity-sensitive operations (including reference equality
 * ({@code ==}), identity hash code, or synchronization) on instances of
 * {@code NullabLe} may have unpredictable results and should be avoided.
 *
 * @since 1.8
 */
public final class NullabLe<T> {
    /**
     * Common instance for {@code empty()}.
     */
    private static final NullabLe<?> EMPTY = new NullabLe<>();

    private final T value;
    private final boolean isPresent;

    /**
     * Constructs an empty instance.
     *
     * @implNote Generally only one empty instance, {@link NullabLe#EMPTY},
     * should exist per VM.
     */
    private NullabLe() {
        this.value = null;
        isPresent = false;
    }

    /**
     * Constructs an instance with the value present.
     *
     * @param value the nullable value to be present
     */
    private NullabLe(T value) {
        this.value = value;
        isPresent = true;
    }

    /**
     * Returns an empty {@code NullabLe} instance.  No value is present for this
     * Optional.
     *
     * @apiNote Though it may be tempting to do so, avoid testing if an object
     * is empty by comparing with {@code ==} against instances returned by
     * {@code NullabLe.empty()}. There is no guarantee that it is a singleton.
     * Instead, use {@link #isPresent()}.
     *
     * @param <T> Type of the non-existent value
     * @return an empty {@code NullabLe}
     */
    public static <T> NullabLe<T> empty() {
        return (NullabLe<T>) EMPTY;
    }

    /**
     * Returns an {@code NullabLe} with the specified present nullable value.
     *
     * @param value the value to be present, which could be null
     * @return an {@code NullabLe} with the value present
     */
    public static <T> NullabLe<T> of(T value) {
        return new NullabLe<>(value);
    }

    /**
     * Returns a {@code NullabLe} with the value returned by {@code action} or an empty {@code NullabLe} if exception happens.
     * 
     * @param action
     * @return
     */
    public static <R> NullabLe<R> tryOrEmpty(final Try.Callable<R, ? extends Throwable> action) {
        try {
            return NullabLe.of(action.call());
        } catch (Throwable e) {
            return NullabLe.<R> empty();
        }
    }

    //    public static <T> NullabLe<T> from(Optional<T> optional) {
    //        return optional.isPresent() ? new NullabLe<T>(optional.get()) : (NullabLe<T>) empty();
    //    }

    /**
     * If a value is present in this {@code NullabLe}, returns the value,
     * otherwise throws {@code NoSuchElementException}.
     *
     * @return the nullable value held by this {@code NullabLe}
     * @throws NoSuchElementException if there is no value present
     *
     * @see NullabLe#isPresent()
     */
    public T get() {
        if (isPresent()) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
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
     * @throws NullPointerException if value is present and {@code consumer} is
     * null
     */
    public void ifPresent(Consumer<? super T> consumer) {
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
    public void ifPresentOrElse(Consumer<? super T> action, Runnable emptyAction) {
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
     * @throws NullPointerException if value is present and {@code consumer} is
     * null
     */
    public void ifNotNull(Consumer<? super T> consumer) {
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
    public void ifNotNullOrElse(Consumer<? super T> action, Runnable emptyAction) {
        if (isNotNull()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    /**
     * If a value is present, and the value matches the given predicate,
     * return an {@code NullabLe} describing the value, otherwise return an
     * empty {@code NullabLe}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code NullabLe} describing the value of this {@code NullabLe}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code NullabLe}
     * @throws NullPointerException if the predicate is null
     */
    public NullabLe<T> filter(Predicate<? super T> predicate) {
        N.requireNonNull(predicate);

        if (isPresent() && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

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
     *     NullabLe<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code NullabLe<String>}, and then
     * {@code map} returns an {@code NullabLe<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if present
     * @return an {@code NullabLe} describing the result of applying a mapping
     * function to the value of this {@code NullabLe}, if a value is present,
     * otherwise an empty {@code NullabLe}
     * @throws NullPointerException if the mapping function is null
     */
    public <U> NullabLe<U> map(Function<? super T, ? extends U> mapper) {
        N.requireNonNull(mapper);

        if (isPresent()) {
            return (NullabLe<U>) NullabLe.of(mapper.apply(value));
        } else {
            return empty();
        }
    }

    /**
     * If a value is present, apply the provided {@code NullabLe}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code NullabLe}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code NullabLe},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code NullabLe}.
     *
     * @param <U> The type parameter to the {@code NullabLe} returned by
     * @param mapper a mapping function to apply to the value, if present
     *           the mapping function
     * @return the result of applying an {@code NullabLe}-bearing mapping
     * function to the value of this {@code NullabLe}, if a value is present,
     * otherwise an empty {@code NullabLe}
     * @throws NullPointerException if the mapping function is null or returns
     * a null result
     */
    public <U> NullabLe<U> flatMap(Function<? super T, NullabLe<U>> mapper) {
        N.requireNonNull(mapper);

        if (isPresent()) {
            return N.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    /**
     * If a value is not null, and the value matches the given predicate,
     * return an {@code NullabLe} describing the value, otherwise return an
     * empty {@code NullabLe}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code NullabLe} describing the value of this {@code NullabLe}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code NullabLe}
     * @throws NullPointerException if the predicate is null
     */
    public NullabLe<T> filterIfNotNull(Predicate<? super T> predicate) {
        N.requireNonNull(predicate);

        if (isNotNull() && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

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
     *     NullabLe<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code NullabLe<String>}, and then
     * {@code map} returns an {@code Optional<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if not null
     * @return an {@code NullabLe} describing the result of applying a mapping
     * function to the value of this {@code NullabLe}, if a value is not null,
     * otherwise an empty {@code NullabLe}
     * @throws NullPointerException if the mapping function is null
     */
    public <U> NullabLe<U> mapIfNotNull(Function<? super T, ? extends U> mapper) {
        N.requireNonNull(mapper);

        if (isNotNull()) {
            return (NullabLe<U>) NullabLe.of(mapper.apply(value));
        } else {
            return empty();
        }
    }

    /**
     * If a value is not null, apply the provided {@code NullabLe}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code NullabLe}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code NullabLe},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code NullabLe}.
     *
     * @param <U> The type parameter to the {@code NullabLe} returned by
     * @param mapper a mapping function to apply to the value, if not null
     *           the mapping function
     * @return the result of applying an {@code NullabLe}-bearing mapping
     * function to the value of this {@code NullabLe}, if a value is not null,
     * otherwise an empty {@code NullabLe}
     * @throws NullPointerException if the mapping function is null or returns
     * a null result
     */
    public <U> NullabLe<U> flatMapIfNotNull(Function<? super T, NullabLe<U>> mapper) {
        N.requireNonNull(mapper);

        if (isNotNull()) {
            return N.requireNonNull(mapper.apply(value));
        } else {
            return empty();
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
     * @throws NullPointerException if value is not present and {@code other} is
     * null
     */
    public T orElseGet(Supplier<? extends T> other) {
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
     * @throws NullPointerException if no value is present and
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
     * @throws NullPointerException if value is not present and {@code other} is null
     */
    public T orGetIfNull(Supplier<? extends T> other) {
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
     * @throws NullPointerException if not present or null and
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
     * 
     * @return <code>Optional.empty()</code> if the value is not present or {@code null}.
     */
    public Optional<T> __() {
        return value == null ? Optional.<T> empty() : Optional.of(value);
    }

    /**
     * @return <code>java.util.Optional.empty()</code> if the value is not present or {@code null}.
     * 
     * @return
     * @see java.util.Optional
     */
    public java.util.Optional<T> toJdkOptional() {
        return value == null ? java.util.Optional.<T> empty() : java.util.Optional.of(value);
    }

    /**
     * Indicates whether some other object is "equal to" this Optional. The other object is considered equal if:
     * <ul>
     * <li>it is also an {@code NullabLe} and;
     * <li>both instances have no value present or;
     * <li>the present values are "equal to" each other via {@code equals()}.
     * </ul>
     *
     * @param obj an object to be tested for equality
     * @return {code true} if the other object is "equal to" this object
     * otherwise {@code false}
     */
    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof NullabLe) {
            final NullabLe<?> other = (NullabLe<?>) obj;
            return N.equals(isPresent, other.isPresent) && N.equals(value, other.value);
        }

        return false;
    }

    /**
     * Returns the hash code value of the present value, if any, or 0 (zero) if
     * no value is present.
     *
     * @return hash code value of the present value or 0 if no value is present
     */
    @Override
    public int hashCode() {
        return N.hashCode(isPresent) * 31 + N.hashCode(value);
    }

    /**
     * Returns a non-empty string representation of this Optional suitable for
     * debugging. The exact presentation format is unspecified and may vary
     * between implementations and versions.
     *
     * @implSpec If a value is present the result must include its string
     * representation in the result. Empty and present Optionals must be
     * unambiguously differentiable.
     *
     * @return the string representation of this instance
     */
    @Override
    public String toString() {
        return isPresent ? String.format("NullabLe[%s]", value) : "NullabLe.empty";
    }
}
