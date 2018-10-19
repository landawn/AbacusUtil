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
import com.landawn.abacus.util.stream.FloatStream;

public final class OptionalFloat implements Comparable<OptionalFloat> {
    private static final OptionalFloat EMPTY = new OptionalFloat();

    private final float value;
    private final boolean isPresent;

    private OptionalFloat() {
        this.value = 0;
        this.isPresent = false;
    }

    private OptionalFloat(float value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalFloat empty() {
        return EMPTY;
    }

    public static OptionalFloat of(float value) {
        return new OptionalFloat(value);
    }

    public static OptionalFloat ofNullable(Float val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalFloat.of(val);
        }
    }

    public float get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> void ifPresent(Try.FloatConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent) {
            action.accept(value);
        }
    }

    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.FloatConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    public <E extends Exception> OptionalFloat filter(Try.FloatPredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalFloat map(final Try.FloatUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalFloat.of(mapper.applyAsFloat(value));
        } else {
            return empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.FloatFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalFloat flatMap(Try.FloatFunction<OptionalFloat, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalFloat or(Try.Supplier<OptionalFloat, E> supplier) throws E {
        if (isPresent) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public float orZero() {
        return isPresent ? value : 0;
    }

    //    public float orElseZero() {
    //        return isPresent ? value : 0;
    //    }

    public float orElseThrow() throws NoSuchElementException {
        if (isPresent) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public float orElse(float other) {
        return isPresent ? value : other;
    }

    public <E extends Exception> float orElseGet(Try.FloatSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent) {
            return value;
        } else {
            return other.getAsFloat();
        }
    }

    public <X extends Throwable> float orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalFloat optional) {
        if (optional == null || optional.isPresent == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return -1;
        }

        return Float.compare(this.get(), optional.get());
    }

    public FloatStream stream() {
        if (isPresent) {
            return FloatStream.of(value);
        } else {
            return FloatStream.empty();
        }
    }

    public Optional<Float> boxed() {
        if (isPresent) {
            return Optional.of(value);
        } else {
            return Optional.<Float> empty();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalFloat) {
            final OptionalFloat other = (OptionalFloat) obj;

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
            return String.format("OptionalFloat[%s]", value);
        }

        return "OptionalFloat.empty";
    }
}
