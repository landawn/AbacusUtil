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
 * {@link #ifPresent(java.util.function.Consumer) ifPresent()} (execute a block
 * of code if the value is present).
 *
 * <p>This is a <a href="../lang/doc-files/ValueBased.html">value-based</a>
 * class; use of identity-sensitive operations (including reference equality
 * ({@code ==}), identity hash code, or synchronization) on instances of
 * {@code OptionalNullable} may have unpredictable results and should be avoided.
 *
 * @since 1.8
 */
public final class OptionalNullable<T> {
    /**
     * Common instance for {@code empty()}.
     */
    private static final OptionalNullable<?> EMPTY = new OptionalNullable<>();

    private final T value;
    private final boolean isPresent;

    /**
     * Constructs an empty instance.
     *
     * @implNote Generally only one empty instance, {@link OptionalNullable#EMPTY},
     * should exist per VM.
     */
    private OptionalNullable() {
        this.value = null;
        isPresent = false;
    }

    /**
     * Constructs an instance with the value present.
     *
     * @param value the nullable value to be present
     */
    private OptionalNullable(T value) {
        this.value = value;
        isPresent = true;
    }

    /**
     * Returns an empty {@code OptionalNullable} instance.  No value is present for this
     * Optional.
     *
     * @apiNote Though it may be tempting to do so, avoid testing if an object
     * is empty by comparing with {@code ==} against instances returned by
     * {@code OptionalNullable.empty()}. There is no guarantee that it is a singleton.
     * Instead, use {@link #isPresent()}.
     *
     * @param <T> Type of the non-existent value
     * @return an empty {@code OptionalNullable}
     */
    public static <T> OptionalNullable<T> empty() {
        return (OptionalNullable<T>) EMPTY;
    }

    /**
     * Returns an {@code OptionalNullable} with the specified present nullable value.
     *
     * @param value the value to be present, which could be null
     * @return an {@code OptionalNullable} with the value present
     */
    public static <T> OptionalNullable<T> of(T value) {
        return new OptionalNullable<>(value);
    }

    /**
     * If a value is present in this {@code OptionalNullable}, returns the value,
     * otherwise throws {@code NoSuchElementException}.
     *
     * @return the nullable value held by this {@code OptionalNullable}
     * @throws NoSuchElementException if there is no value present
     *
     * @see OptionalNullable#isPresent()
     */
    public T get() {
        if (isPresent == false) {
            throw new NoSuchElementException("No value present");
        }

        return value;
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
        if (isPresent)
            consumer.accept(value);
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
        if (value != null)
            consumer.accept(value);
    }

    /**
     * If a value is present, and the value matches the given predicate,
     * return an {@code OptionalNullable} describing the value, otherwise return an
     * empty {@code OptionalNullable}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code OptionalNullable} describing the value of this {@code OptionalNullable}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code OptionalNullable}
     * @throws NullPointerException if the predicate is null
     */
    public OptionalNullable<T> filter(Predicate<? super T> predicate) {
        N.requireNonNull(predicate);

        if (!isPresent())
            return this;
        else
            return predicate.test(value) ? this : (OptionalNullable<T>) empty();
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
     *     OptionalNullable<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code OptionalNullable<String>}, and then
     * {@code map} returns an {@code OptionalNullable<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if present
     * @return an {@code OptionalNullable} describing the result of applying a mapping
     * function to the value of this {@code OptionalNullable}, if a value is present,
     * otherwise an empty {@code OptionalNullable}
     * @throws NullPointerException if the mapping function is null
     */
    public <U> OptionalNullable<U> map(Function<? super T, ? extends U> mapper) {
        N.requireNonNull(mapper);

        if (!isPresent())
            return empty();
        else {
            return (OptionalNullable<U>) OptionalNullable.of(mapper.apply(value));
        }
    }

    /**
     * If a value is present, apply the provided {@code OptionalNullable}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code OptionalNullable}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code OptionalNullable},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code OptionalNullable}.
     *
     * @param <U> The type parameter to the {@code OptionalNullable} returned by
     * @param mapper a mapping function to apply to the value, if present
     *           the mapping function
     * @return the result of applying an {@code OptionalNullable}-bearing mapping
     * function to the value of this {@code OptionalNullable}, if a value is present,
     * otherwise an empty {@code OptionalNullable}
     * @throws NullPointerException if the mapping function is null or returns
     * a null result
     */
    public <U> OptionalNullable<U> flatMap(Function<? super T, OptionalNullable<U>> mapper) {
        N.requireNonNull(mapper);

        if (!isPresent())
            return empty();
        else {
            return N.requireNonNull(mapper.apply(value));
        }
    }

    /**
     * If a value is not null, and the value matches the given predicate,
     * return an {@code OptionalNullable} describing the value, otherwise return an
     * empty {@code OptionalNullable}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code OptionalNullable} describing the value of this {@code OptionalNullable}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code OptionalNullable}
     * @throws NullPointerException if the predicate is null
     */
    public OptionalNullable<T> filterIfNotNull(Predicate<? super T> predicate) {
        N.requireNonNull(predicate);

        if (value == null)
            return this;
        else
            return predicate.test(value) ? this : (OptionalNullable<T>) empty();
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
     *     OptionalNullable<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code OptionalNullable<String>}, and then
     * {@code map} returns an {@code Optional<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if not null
     * @return an {@code OptionalNullable} describing the result of applying a mapping
     * function to the value of this {@code OptionalNullable}, if a value is not null,
     * otherwise an empty {@code OptionalNullable}
     * @throws NullPointerException if the mapping function is null
     */
    public <U> OptionalNullable<U> mapIfNotNull(Function<? super T, ? extends U> mapper) {
        N.requireNonNull(mapper);

        if (value == null)
            return empty();
        else {
            return (OptionalNullable<U>) OptionalNullable.of(mapper.apply(value));
        }
    }

    /**
     * If a value is not null, apply the provided {@code OptionalNullable}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code OptionalNullable}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code OptionalNullable},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code OptionalNullable}.
     *
     * @param <U> The type parameter to the {@code OptionalNullable} returned by
     * @param mapper a mapping function to apply to the value, if not null
     *           the mapping function
     * @return the result of applying an {@code OptionalNullable}-bearing mapping
     * function to the value of this {@code OptionalNullable}, if a value is not null,
     * otherwise an empty {@code OptionalNullable}
     * @throws NullPointerException if the mapping function is null or returns
     * a null result
     */
    public <U> OptionalNullable<U> flatMapIfNotNull(Function<? super T, OptionalNullable<U>> mapper) {
        N.requireNonNull(mapper);

        if (value == null)
            return empty();
        else {
            return N.requireNonNull(mapper.apply(value));
        }
    }

    /**
     * Return the value if present, otherwise return {@code other}.
     *
     * @param other the value to be returned if there is no value present, may be null
     * @return the value, if present, otherwise {@code other}
     */
    public T or(T other) {
        return isPresent ? value : other;
    }

    /**
     * Return the value if present, otherwise invoke {@code other} and return the result of that invocation.
     *
     * @param other a {@code Supplier} whose result is returned if no value is present
     * @return the value if present otherwise the result of {@code other.get()}
     * @throws NullPointerException if value is not present and {@code other} is
     * null
     */
    public T orGet(Supplier<? extends T> other) {
        return isPresent ? value : other.get();
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
    public <X extends Throwable> T orThrow(Supplier<? extends X> exceptionSupplier) throws X {
        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    /**
     * Return the value if present, otherwise return {@code null}.
     *
     * @return the value, if present, otherwise {@code null}
     */
    public T orNull() {
        return isPresent() ? value : null;
    }

    /**
     * Return the value is not null, otherwise return {@code other}.
     *
     * @param other the value to be returned if not present or null, may be null
     * @return the value, if not present or null, otherwise {@code other}
     */
    public T orIfNull(T other) {
        return value == null ? other : value;
    }

    /**
     * Return the value is not null, otherwise invoke {@code other} and return the result of that invocation.
     *
     * @param other a {@code Supplier} whose result is returned if not present or null
     * @return the value if not present or null otherwise the result of {@code other.get()}
     * @throws NullPointerException if value is not present and {@code other} is null
     */
    public T orGetIfNull(Supplier<? extends T> other) {
        return value == null ? other.get() : value;
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
        if (value == null) {
            throw exceptionSupplier.get();
        } else {
            return value;
        }
    }

    /**
     * Indicates whether some other object is "equal to" this Optional. The other object is considered equal if:
     * <ul>
     * <li>it is also an {@code OptionalNullable} and;
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

        if (!(obj instanceof OptionalNullable)) {
            return false;
        }

        final OptionalNullable<?> other = (OptionalNullable<?>) obj;

        return N.equals(isPresent, other.isPresent) && N.equals(value, other.value);
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
        return isPresent ? String.format("Optional[%s]", value) : "Optional.empty";
    }
}
