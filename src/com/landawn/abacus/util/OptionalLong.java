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
import com.landawn.abacus.util.stream.LongStream;

public final class OptionalLong implements Comparable<OptionalLong> {
    private static final OptionalLong EMPTY = new OptionalLong();

    private final long value;
    private final boolean isPresent;

    private OptionalLong() {
        this.value = 0;
        this.isPresent = false;
    }

    private OptionalLong(long value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalLong empty() {
        return EMPTY;
    }

    public static OptionalLong of(long value) {
        return new OptionalLong(value);
    }

    public static OptionalLong ofNullable(Long val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalLong.of(val);
        }
    }

    public static OptionalLong from(java.util.OptionalLong optional) {
        return optional.isPresent() ? of(optional.getAsLong()) : OptionalLong.empty();
    }

    public long get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> OptionalLong ifPresent(Try.LongConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent) {
            action.accept(value);
        }

        return this;
    }

    public <E extends Exception, E2 extends Exception> OptionalLong ifPresentOrElse(Try.LongConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent) {
            action.accept(value);
        } else {
            emptyAction.run();
        }

        return this;
    }

    public <E extends Exception> OptionalLong filter(Try.LongPredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalLong map(final Try.LongUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalLong.of(mapper.applyAsLong(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalInt mapToInt(final Try.ToIntFunction<Long, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalInt.of(mapper.applyAsInt(value));
        } else {
            return OptionalInt.empty();
        }
    }

    public <E extends Exception> OptionalDouble mapToDouble(final Try.ToDoubleFunction<Long, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalDouble.of(mapper.applyAsDouble(value));
        } else {
            return OptionalDouble.empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.LongFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalLong flatMap(Try.LongFunction<OptionalLong, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalLong or(Try.Supplier<OptionalLong, E> supplier) throws E {
        if (isPresent) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public long orZero() {
        return isPresent ? value : 0;
    }

    //    public long orElseZero() {
    //        return isPresent ? value : 0;
    //    }

    public long orElseThrow() throws NoSuchElementException {
        if (isPresent) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public long orElse(long other) {
        return isPresent ? value : other;
    }

    public <E extends Exception> long orElseGet(Try.LongSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent) {
            return value;
        } else {
            return other.getAsLong();
        }
    }

    public <X extends Throwable> long orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalLong optional) {
        if (optional == null || optional.isPresent == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return -1;
        }

        return Long.compare(this.get(), optional.get());
    }

    public LongStream stream() {
        if (isPresent) {
            return LongStream.of(value);
        } else {
            return LongStream.empty();
        }
    }

    public Optional<Long> boxed() {
        if (isPresent) {
            return Optional.of(value);
        } else {
            return Optional.<Long> empty();
        }
    }

    public java.util.OptionalLong __() {
        return isPresent() ? java.util.OptionalLong.of(value) : java.util.OptionalLong.empty();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalLong) {
            final OptionalLong other = (OptionalLong) obj;

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
            return String.format("OptionalLong[%s]", value);
        }

        return "OptionalLong.empty";
    }
}
