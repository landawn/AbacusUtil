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
import com.landawn.abacus.util.stream.Stream;

public final class OptionalBoolean implements Comparable<OptionalBoolean> {
    private static final OptionalBoolean EMPTY = new OptionalBoolean();

    private final boolean value;
    private final boolean isPresent;

    private OptionalBoolean() {
        this.value = false;
        this.isPresent = false;
    }

    private OptionalBoolean(boolean value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalBoolean empty() {
        return EMPTY;
    }

    public static OptionalBoolean of(boolean value) {
        return new OptionalBoolean(value);
    }

    public static OptionalBoolean ofNullable(Boolean val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalBoolean.of(val);
        }
    }

    public boolean get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> OptionalBoolean ifPresent(Try.BooleanConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent) {
            action.accept(value);
        }

        return this;
    }

    public <E extends Exception, E2 extends Exception> OptionalBoolean ifPresentOrElse(Try.BooleanConsumer<E> action, Try.Runnable<E2> emptyAction)
            throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent) {
            action.accept(value);
        } else {
            emptyAction.run();
        }

        return this;
    }

    public <E extends Exception> OptionalBoolean filter(Try.BooleanPredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalBoolean map(final Try.BooleanUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalBoolean.of(mapper.applyAsBoolean(value));
        } else {
            return empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.BooleanFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalBoolean flatMap(Try.BooleanFunction<OptionalBoolean, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalBoolean or(Try.Supplier<OptionalBoolean, E> supplier) throws E {
        if (isPresent) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public boolean orFalse() {
        return isPresent ? value : false;
    }

    //    public boolean orElseFalse() {
    //        return isPresent ? value : false;
    //    }

    public boolean orTrue() {
        return isPresent ? value : true;
    }

    //    public boolean orElseTrue() {
    //        return isPresent ? value : true;
    //    }

    public boolean orElseThrow() throws NoSuchElementException {
        if (isPresent) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public boolean orElse(boolean other) {
        return isPresent ? value : other;
    }

    public <E extends Exception> boolean orElseGet(Try.BooleanSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent) {
            return value;
        } else {
            return other.getAsBoolean();
        }
    }

    public <X extends Throwable> boolean orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalBoolean optional) {
        if (optional == null || optional.isPresent == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return -1;
        }

        return Boolean.compare(this.get(), optional.get());
    }

    public Stream<Boolean> stream() {
        if (isPresent) {
            return Stream.of(value);
        } else {
            return Stream.empty();
        }
    }

    public Optional<Boolean> boxed() {
        if (isPresent) {
            return Optional.of(value);
        } else {
            return Optional.<Boolean> empty();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalBoolean) {
            final OptionalBoolean other = (OptionalBoolean) obj;

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
            return String.format("OptionalBoolean[%s]", value);
        }

        return "OptionalBoolean.empty";
    }
}
