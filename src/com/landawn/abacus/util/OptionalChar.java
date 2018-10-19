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

import java.util.NoSuchElementException;
import java.util.Objects;

import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.CharStream;

public final class OptionalChar implements Comparable<OptionalChar> {
    private static final OptionalChar EMPTY = new OptionalChar();

    private final char value;
    private final boolean isPresent;

    private OptionalChar() {
        this.value = 0;
        this.isPresent = false;
    }

    private OptionalChar(char value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalChar empty() {
        return EMPTY;
    }

    public static OptionalChar of(char value) {
        return new OptionalChar(value);
    }

    public static OptionalChar ofNullable(Character val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalChar.of(val);
        }
    }

    public char get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> void ifPresent(Try.CharConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent()) {
            action.accept(value);
        }
    }

    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.CharConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent()) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    public <E extends Exception> OptionalChar filter(Try.CharPredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent() && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalChar map(final Try.CharUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return OptionalChar.of(mapper.applyAsChar(value));
        } else {
            return empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.CharFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalChar flatMap(Try.CharFunction<OptionalChar, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent()) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalChar or(Try.Supplier<OptionalChar, E> supplier) throws E {
        if (isPresent()) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public char orZero() {
        return isPresent() ? value : 0;
    }

    //    public char orElseZero() {
    //        return isPresent() ? value : 0;
    //    }

    public char orElseThrow() throws NoSuchElementException {
        if (isPresent()) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public char orElse(char other) {
        return isPresent() ? value : other;
    }

    public <E extends Exception> char orElseGet(Try.CharSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent()) {
            return value;
        } else {
            return other.getAsChar();
        }
    }

    public <X extends Throwable> char orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent()) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalChar optional) {
        if (optional == null || optional.isPresent() == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return -1;
        }

        return Character.compare(this.get(), optional.get());
    }

    public CharStream stream() {
        if (isPresent()) {
            return CharStream.of(value);
        } else {
            return CharStream.empty();
        }
    }

    public Optional<Character> boxed() {
        if (isPresent()) {
            return Optional.of(value);
        } else {
            return Optional.<Character> empty();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalChar) {
            final OptionalChar other = (OptionalChar) obj;

            return (isPresent && other.isPresent) ? value == other.value : isPresent == other.isPresent;
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
            return String.format("OptionalChar[%s]", value);
        }

        return "OptionalChar.empty";
    }
}
