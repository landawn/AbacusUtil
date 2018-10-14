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
import com.landawn.abacus.util.stream.ShortStream;

public final class OptionalShort implements Comparable<OptionalShort> {
    private static final OptionalShort EMPTY = new OptionalShort();

    private final short value;
    private final boolean isPresent;

    private OptionalShort() {
        this.value = 0;
        this.isPresent = false;
    }

    private OptionalShort(short value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalShort empty() {
        return EMPTY;
    }

    public static OptionalShort of(short value) {
        return new OptionalShort(value);
    }

    public static OptionalShort ofNullable(Short val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalShort.of(val);
        }
    }

    public short get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> void ifPresent(Try.ShortConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent) {
            action.accept(value);
        }
    }

    public <E extends Exception, E2 extends Exception> void ifPresentOrElse(Try.ShortConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent) {
            action.accept(value);
        } else {
            emptyAction.run();
        }
    }

    public <E extends Exception> OptionalShort filter(Try.ShortPredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalShort map(final Try.ShortUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalShort.of(mapper.applyAsShort(value));
        } else {
            return empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.ShortFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalShort flatMap(Try.ShortFunction<OptionalShort, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalShort or(Try.Supplier<OptionalShort, E> supplier) throws E {
        if (isPresent) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public short orZero() {
        return isPresent ? value : 0;
    }

    //    public short orElseZero() {
    //        return isPresent ? value : 0;
    //    }

    public short orElseThrow() throws NoSuchElementException {
        if (isPresent) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public short orElse(short other) {
        return isPresent ? value : other;
    }

    public <E extends Exception> short orElseGet(Try.ShortSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent) {
            return value;
        } else {
            return other.getAsShort();
        }
    }

    public <X extends Throwable> short orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalShort optional) {
        if (optional == null || optional.isPresent == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return optional.isPresent ? -1 : 0;
        }

        return Short.compare(this.get(), optional.get());
    }

    public ShortStream stream() {
        if (isPresent) {
            return ShortStream.of(value);
        } else {
            return ShortStream.empty();
        }
    }

    public Optional<Short> boxed() {
        if (isPresent) {
            return Optional.of(value);
        } else {
            return Optional.<Short> empty();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalShort) {
            final OptionalShort other = (OptionalShort) obj;

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
            return String.format("OptionalShort[%s]", value);
        }

        return "OptionalShort.empty";
    }
}
