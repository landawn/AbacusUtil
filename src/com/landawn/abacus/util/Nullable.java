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
 * {@code Nullable} may have unpredictable results and should be avoided.
 *
 */
public final class Nullable<T> extends Any<T> {
    /**
     * Common instance for {@code empty()}.
     */
    private static final Nullable<?> EMPTY = new Nullable<>();

    /**
     * Constructs an empty instance.
     *
     * @implNote Generally only one empty instance, {@link Nullable#EMPTY},
     * should exist per VM.
     */
    private Nullable() {
        super();
    }

    /**
     * Constructs an instance with the value present.
     *
     * @param value the nullable value to be present
     */
    private Nullable(T value) {
        super(value);
    }

    /**
     * Returns an empty {@code Nullable} instance.  No value is present for this
     * Optional.
     *
     * @apiNote Though it may be tempting to do so, avoid testing if an object
     * is empty by comparing with {@code ==} against instances returned by
     * {@code Nullable.empty()}. There is no guarantee that it is a singleton.
     * Instead, use {@link #isPresent()}.
     *
     * @param <T> Type of the non-existent value
     * @return an empty {@code Nullable}
     */
    public static <T> Nullable<T> empty() {
        return (Nullable<T>) EMPTY;
    }

    /**
     * Returns an {@code Nullable} with the specified present nullable value.
     *
     * @param value the value to be present, which could be null
     * @return an {@code Nullable} with the value present
     */
    public static <T> Nullable<T> of(T value) {
        return new Nullable<>(value);
    }

    /**
     * If a value is present, and the value matches the given predicate,
     * return an {@code Nullable} describing the value, otherwise return an
     * empty {@code Nullable}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code Nullable} describing the value of this {@code Nullable}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code Nullable}
     * @throws NullPointerException if the predicate is null
     */
    @Override
    public <E extends Exception> Nullable<T> filter(Try.Predicate<? super T, E> predicate) throws E {
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
     *     Nullable<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code Nullable<String>}, and then
     * {@code map} returns an {@code Nullable<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if present
     * @return an {@code Nullable} describing the result of applying a mapping
     * function to the value of this {@code Nullable}, if a value is present,
     * otherwise an empty {@code Nullable}
     * @throws NullPointerException if the mapping function is null
     */
    @Override
    public <U, E extends Exception> Nullable<U> map(Try.Function<? super T, ? extends U, E> mapper) throws E {
        N.requireNonNull(mapper);

        if (isPresent()) {
            return Nullable.of((U) mapper.apply(value));
        } else {
            return empty();
        }
    }

    /**
     * If a value is present, apply the provided {@code Nullable}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code Nullable}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code Nullable},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code Nullable}.
     *
     * @param <U> The type parameter to the {@code Nullable} returned by
     * @param mapper a mapping function to apply to the value, if present
     *           the mapping function
     * @return the result of applying an {@code Nullable}-bearing mapping
     * function to the value of this {@code Nullable}, if a value is present,
     * otherwise an empty {@code Nullable}
     * @throws NullPointerException if the mapping function is null or returns
     * a null result
     */
    @Override
    public <U, E extends Exception> Nullable<U> flatMap(Try.Function<? super T, ? extends Any<U>, E> mapper) throws E {
        N.requireNonNull(mapper);

        if (isPresent()) {
            final Any<U> any = N.requireNonNull(mapper.apply(value));
            return any instanceof Nullable ? (Nullable<U>) any : (any.isPresent ? Nullable.of(any.get()) : Nullable.<U> empty());
        } else {
            return empty();
        }
    }

    /**
     * If a value is not null, and the value matches the given predicate,
     * return an {@code Nullable} describing the value, otherwise return an
     * empty {@code Nullable}.
     *
     * @param predicate a predicate to apply to the value, if present
     * @return an {@code Nullable} describing the value of this {@code Nullable}
     * if a value is present and the value matches the given predicate,
     * otherwise an empty {@code Nullable}
     * @throws NullPointerException if the predicate is null
     */
    @Override
    public <E extends Exception> Nullable<T> filterIfNotNull(Try.Predicate<? super T, E> predicate) throws E {
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
     *     Nullable<FileInputStream> fis =
     *         names.stream().filter(name -> !isProcessedYet(name))
     *                       .findFirst()
     *                       .map(name -> new FileInputStream(name));
     * }</pre>
     *
     * Here, {@code findFirst} returns an {@code Nullable<String>}, and then
     * {@code map} returns an {@code Optional<FileInputStream>} for the desired
     * file if one exists.
     *
     * @param <U> The type of the result of the mapping function
     * @param mapper a mapping function to apply to the value, if not null
     * @return an {@code Nullable} describing the result of applying a mapping
     * function to the value of this {@code Nullable}, if a value is not null,
     * otherwise an empty {@code Nullable}
     * @throws NullPointerException if the mapping function is null
     */
    @Override
    public <U, E extends Exception> Nullable<U> mapIfNotNull(Try.Function<? super T, ? extends U, E> mapper) throws E {
        N.requireNonNull(mapper);

        if (isNotNull()) {
            return Nullable.of((U) mapper.apply(value));
        } else {
            return empty();
        }
    }

    /**
     * If a value is not null, apply the provided {@code Nullable}-bearing
     * mapping function to it, return that result, otherwise return an empty
     * {@code Nullable}.  This method is similar to {@link #map(Function)},
     * but the provided mapper is one whose result is already an {@code Nullable},
     * and if invoked, {@code flatMap} does not wrap it with an additional
     * {@code Nullable}.
     *
     * @param <U> The type parameter to the {@code Nullable} returned by
     * @param mapper a mapping function to apply to the value, if not null
     *           the mapping function
     * @return the result of applying an {@code Nullable}-bearing mapping
     * function to the value of this {@code Nullable}, if a value is not null,
     * otherwise an empty {@code Nullable}
     * @throws NullPointerException if the mapping function is null or returns
     * a null result
     */
    @Override
    public <U, E extends Exception> Nullable<U> flatMapIfNotNull(Try.Function<? super T, ? extends Any<U>, E> mapper) throws E {
        N.requireNonNull(mapper);

        if (isNotNull()) {
            final Any<U> any = N.requireNonNull(mapper.apply(value));
            return any instanceof Nullable ? (Nullable<U>) any : (any.isPresent ? Nullable.of(any.get()) : Nullable.<U> empty());
        } else {
            return empty();
        }
    }

    /**
     * Returns {@code this} if it's present or call {@code supplier.get()}.
     * 
     * @param supplier
     * @return
     * @throws E
     */
    @Override
    public <E extends Exception> Nullable<T> or(Try.Supplier<? extends Any<T>, E> supplier) throws E {
        if (isPresent()) {
            return this;
        } else {
            final Any<T> any = N.requireNonNull(supplier.get());
            return any instanceof Nullable ? (Nullable<T>) any : (any.isPresent ? Nullable.of(any.get()) : Nullable.<T> empty());
        }
    }

    /**
     * Indicates whether some other object is "equal to" this Optional. The other object is considered equal if:
     * <ul>
     * <li>it is also an {@code Nullable} and;
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

        if (obj instanceof Nullable) {
            final Nullable<?> other = (Nullable<?>) obj;
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
        return isPresent ? String.format("Nullable[%s]", value) : "Nullable.empty";
    }
}
