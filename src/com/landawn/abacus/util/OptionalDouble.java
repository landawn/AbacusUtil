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
import com.landawn.abacus.util.stream.DoubleStream;

public final class OptionalDouble implements Comparable<OptionalDouble> {
    private static final OptionalDouble EMPTY = new OptionalDouble();

    private final double value;
    private final boolean isPresent;

    private OptionalDouble() {
        this.value = 0;
        this.isPresent = false;
    }

    private OptionalDouble(double value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalDouble empty() {
        return EMPTY;
    }

    public static OptionalDouble of(double value) {
        return new OptionalDouble(value);
    }

    public static OptionalDouble ofNullable(Double val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalDouble.of(val);
        }
    }

    public static OptionalDouble from(java.util.OptionalDouble optional) {
        return optional.isPresent() ? of(optional.getAsDouble()) : OptionalDouble.empty();
    }

    public double get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> void ifPresent(Try.DoubleConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent) {
            action.accept(value);
        }
    }

    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.DoubleConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    public <E extends Exception> OptionalDouble filter(Try.DoublePredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalDouble map(final Try.DoubleUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalDouble.of(mapper.applyAsDouble(value));
        } else {
            return empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.DoubleFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalDouble flatMap(Try.DoubleFunction<OptionalDouble, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalDouble or(Try.Supplier<OptionalDouble, E> supplier) throws E {
        if (isPresent) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public double orZero() {
        return isPresent ? value : 0;
    }

    //    public double orElseZero() {
    //        return isPresent ? value : 0;
    //    }

    public double orElseThrow() throws NoSuchElementException {
        if (isPresent) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public double orElse(double other) {
        return isPresent ? value : other;
    }

    public <E extends Exception> double orElseGet(Try.DoubleSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent) {
            return value;
        } else {
            return other.getAsDouble();
        }
    }

    public <X extends Throwable> double orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalDouble optional) {
        if (optional == null || optional.isPresent == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return optional.isPresent ? -1 : 0;
        }

        return Double.compare(this.get(), optional.get());
    }

    public DoubleStream stream() {
        if (isPresent) {
            return DoubleStream.of(value);
        } else {
            return DoubleStream.empty();
        }
    }

    public Optional<Double> boxed() {
        if (isPresent) {
            return Optional.of(value);
        } else {
            return Optional.<Double> empty();
        }
    }

    public java.util.OptionalDouble __() {
        return isPresent ? java.util.OptionalDouble.of(value) : java.util.OptionalDouble.empty();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalDouble) {
            final OptionalDouble other = (OptionalDouble) obj;

            return (isPresent && other.isPresent) ? N.equals(value, other.value) : isPresent == other.isPresent;
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
            return String.format("OptionalDouble[%s]", value);
        }

        return "OptionalDouble.empty";
    }
}
