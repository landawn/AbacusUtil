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

import com.landawn.abacus.util.stream.CharStream;

/**
 * Note: It's copied from OpenJDK at: http://hg.openjdk.java.net/jdk8u/hs-dev/jdk
 * <br />
 *
 * A container object which may or may not contain a {@code char} value.
 * If a value is present, {@code isPresent()} will return {@code true} and
 * {@code get()} will return the value.
 *
 * <p>Additional methods that depend on the presence or absence of a contained
 * value are provided, such as {@link #or(char) orElse()}
 * (return a default value if value not present) and
 * {@link #ifPresent(java.util.function.CharConsumer) ifPresent()} (execute a block
 * of code if the value is present).
 *
 * <p>This is a <a href="../lang/doc-files/ValueBased.html">value-based</a>
 * class; use of identity-sensitive operations (including reference equality
 * ({@code ==}), identity hash code, or synchronization) on instances of
 * {@code OptionalChar} may have unpredictable results and should be avoided.
 *
 * @since 1.8
 */
public final class OptionalChar implements Comparable<OptionalChar> {
    /**
     * Common instance for {@code empty()}.
     */
    private static final OptionalChar EMPTY = new OptionalChar();

    /**
     * If true then the value is present, otherwise indicates no value is present
     */
    private final boolean isPresent;
    private final char value;

    /**
     * Construct an empty instance.
     *
     * @implNote Generally only one empty instance, {@link OptionalChar#EMPTY},
     * should exist per VM.
     */
    private OptionalChar() {
        this.isPresent = false;
        this.value = 0;
    }

    /**
     * Returns an empty {@code OptionalChar} instance.  No value is present for this
     * OptionalChar.
     *
     * @apiNote Though it may be tempting to do so, avoid testing if an object
     * is empty by comparing with {@code ==} against instances returned by
     * {@code OptionalChar.empty()}. There is no guarantee that it is a singleton.
     * Instead, use {@link #isPresent()}.
     *
     *  @return an empty {@code OptionalChar}
     */
    public static OptionalChar empty() {
        return EMPTY;
    }

    /**
     * Construct an instance with the value present.
     *
     * @param value the char value to be present
     */
    private OptionalChar(char value) {
        this.isPresent = true;
        this.value = value;
    }

    /**
     * Return an {@code OptionalChar} with the specified value present.
     *
     * @param value the value to be present
     * @return an {@code OptionalChar} with the value present
     */
    public static OptionalChar of(char value) {
        return new OptionalChar(value);
    }

    /**
     * If a value is present in this {@code OptionalChar}, returns the value,
     * otherwise throws {@code NoSuchElementException}.
     *
     * @return the value held by this {@code OptionalChar}
     * @throws NoSuchElementException if there is no value present
     *
     * @see OptionalChar#isPresent()
     */
    public char get() {
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
     * Have the specified consumer accept the value if a value is present,
     * otherwise do nothing.
     *
     * @param action block to be executed if a value is present
     * @throws NullPocharerException if value is present and {@code consumer} is
     * null
     */
    public <E extends Exception> void ifPresent(Try.CharConsumer<E> action) throws E {
        if (isPresent()) {
            action.accept(value);
        }
    }

    /**
     * If a value is present, performs the given action with the value, otherwise performs the given empty-based action.
     *
     * @param action
     * @param emptyAction
     */
    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.CharConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        if (isPresent()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    /**
     * Return the value if present, otherwise return {@code other}.
     *
     * @param other the value to be returned if there is no value present
     * @return the value, if present, otherwise {@code other}
     */
    public char orElse(char other) {
        return isPresent() ? value : other;
    }

    /**
     * Return the value if present, otherwise invoke {@code other} and return
     * the result of that invocation.
     *
     * @param other a {@code CharSupplier} whose result is returned if no value
     * is present
     * @return the value if present otherwise the result of {@code other.getAsChar()}
     * @throws NullPocharerException if value is not present and {@code other} is
     * null
     */
    public <E extends Exception> char orElseGet(Try.CharSupplier<E> other) throws E {
        return isPresent() ? value : other.getAsChar();
    }

    /**
     * Return the contained value, if present, otherwise throw an exception
     * to be created by the provided supplier.
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
     * @throws NullPocharerException if no value is present and
     * {@code exceptionSupplier} is null
     */
    public <X extends Throwable, E extends Exception> char orElseThrow(Try.Supplier<X, E> exceptionSupplier) throws X, E {
        if (isPresent()) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalChar optional) {
        if (optional == null || optional.isPresent() == false) {
            return isPresent() ? 1 : 0;
        }

        if (isPresent() == false) {
            return optional.isPresent() ? -1 : 0;
        }

        return Character.compare(this.get(), optional.get());
    }

    public CharStream stream() {
        return isPresent() ? CharStream.of(value) : CharStream.empty();
    }

    public Optional<Character> boxed() {
        return isPresent() ? Optional.of(value) : Optional.<Character> empty();
    }

    /**
     * Indicates whether some other object is "equal to" this OptionalChar. The
     * other object is considered equal if:
     * <ul>
     * <li>it is also an {@code OptionalChar} and;
     * <li>both instances have no value present or;
     * <li>the present values are "equal to" each other via {@code ==}.
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

        if (obj instanceof OptionalChar) {
            OptionalChar other = (OptionalChar) obj;
            return (isPresent && other.isPresent) ? value == other.value : isPresent == other.isPresent;
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
        return isPresent() ? Character.valueOf(value).hashCode() : 0;
    }

    /**
     * {@inheritDoc}
     *
     * Returns a non-empty string representation of this object suitable for
     * debugging. The exact presentation format is unspecified and may vary
     * between implementations and versions.
     *
     * @implSpec If a value is present the result must include its string
     * representation in the result. Empty and present instances must be
     * unambiguously differentiable.
     *
     * @return the string representation of this instance
     */
    @Override
    public String toString() {
        return isPresent() ? String.format("OptionalChar[%s]", value) : "OptionalChar.empty";
    }
}
