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
import com.landawn.abacus.util.stream.IntStream;

public final class OptionalInt implements Comparable<OptionalInt> {
    private static final OptionalInt EMPTY = new OptionalInt();

    private final int value;
    private final boolean isPresent;

    private OptionalInt() {
        this.value = 0;
        this.isPresent = false;
    }

    private OptionalInt(int value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalInt empty() {
        return EMPTY;
    }

    public static OptionalInt of(int value) {
        return new OptionalInt(value);
    }

    public static OptionalInt ofNullable(Integer val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalInt.of(val);
        }
    }

    public static OptionalInt from(java.util.OptionalInt optional) {
        return optional.isPresent() ? of(optional.getAsInt()) : OptionalInt.empty();
    }

    public int get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> void ifPresent(Try.IntConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent) {
            action.accept(value);
        }
    }

    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.IntConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    public <E extends Exception> OptionalInt filter(Try.IntPredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalInt map(final Try.IntUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalInt.of(mapper.applyAsInt(value));
        } else {
            return empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.IntFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalInt flatMap(Try.IntFunction<OptionalInt, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalInt or(Try.Supplier<OptionalInt, E> supplier) throws E {
        if (isPresent) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public int orZero() {
        return isPresent ? value : 0;
    }

    //    public int orElseZero() {
    //        return isPresent ? value : 0;
    //    }

    public int orElseThrow() throws NoSuchElementException {
        if (isPresent) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public int orElse(int other) {
        return isPresent ? value : other;
    }

    public <E extends Exception> int orElseGet(Try.IntSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent) {
            return value;
        } else {
            return other.getAsInt();
        }
    }

    public <X extends Throwable> int orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalInt optional) {
        if (optional == null || optional.isPresent == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return -1;
        }

        return Integer.compare(this.get(), optional.get());
    }

    public IntStream stream() {
        if (isPresent) {
            return IntStream.of(value);
        } else {
            return IntStream.empty();
        }
    }

    public Optional<Integer> boxed() {
        if (isPresent) {
            return Optional.of(value);
        } else {
            return Optional.<Integer> empty();
        }
    }

    public java.util.OptionalInt __() {
        return isPresent() ? java.util.OptionalInt.of(value) : java.util.OptionalInt.empty();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalInt) {
            final OptionalInt other = (OptionalInt) obj;

            return (isPresent && other.isPresent) ? value == other.value : isPresent == other.isPresent;
        }

        return false;
    }

    @Override
    public int hashCode() {
        return N.hashCode(isPresent) * 31 + N.hashCode(value);
    }

    @Override
    public String toString() {
        if (isPresent) {
            return String.format("OptionalInt[%s]", value);
        }

        return "OptionalInt.empty";
    }
}
