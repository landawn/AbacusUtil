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
import com.landawn.abacus.util.stream.ByteStream;

public final class OptionalByte implements Comparable<OptionalByte> {
    private static final OptionalByte EMPTY = new OptionalByte();

    private final byte value;
    private final boolean isPresent;

    private OptionalByte() {
        this.value = 0;
        this.isPresent = false;
    }

    private OptionalByte(byte value) {
        this.value = value;
        this.isPresent = true;
    }

    public static OptionalByte empty() {
        return EMPTY;
    }

    public static OptionalByte of(byte value) {
        return new OptionalByte(value);
    }

    public static OptionalByte ofNullable(Byte val) {
        if (val == null) {
            return empty();
        } else {
            return OptionalByte.of(val);
        }
    }

    public byte get() throws NoSuchElementException {
        return orElseThrow();
    }

    public boolean isPresent() {
        return isPresent;
    }

    public <E extends Exception> OptionalByte ifPresent(Try.ByteConsumer<E> action) throws E {
        Objects.requireNonNull(action);

        if (isPresent) {
            action.accept(value);
        }

        return this;
    }

    public <E extends Exception, E2 extends Exception> OptionalByte ifPresentOrElse(Try.ByteConsumer<E> action, Try.Runnable<E2> emptyAction) throws E, E2 {
        Objects.requireNonNull(action);
        Objects.requireNonNull(emptyAction);

        if (isPresent) {
            action.accept(value);
        } else {
            emptyAction.run();
        }

        return this;
    }

    public <E extends Exception> OptionalByte filter(Try.BytePredicate<E> predicate) throws E {
        Objects.requireNonNull(predicate);

        if (isPresent && predicate.test(value)) {
            return this;
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalByte map(final Try.ByteUnaryOperator<E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalByte.of(mapper.applyAsByte(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalInt mapToInt(final Try.ToIntFunction<Byte, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return OptionalInt.of(mapper.applyAsInt(value));
        } else {
            return OptionalInt.empty();
        }
    }

    public <T, E extends Exception> Nullable<T> mapToObj(final Try.ByteFunction<T, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Nullable.of(mapper.apply(value));
        } else {
            return Nullable.<T> empty();
        }
    }

    public <E extends Exception> OptionalByte flatMap(Try.ByteFunction<OptionalByte, E> mapper) throws E {
        Objects.requireNonNull(mapper);

        if (isPresent) {
            return Objects.requireNonNull(mapper.apply(value));
        } else {
            return empty();
        }
    }

    public <E extends Exception> OptionalByte or(Try.Supplier<OptionalByte, E> supplier) throws E {
        if (isPresent) {
            return this;
        } else {
            return Objects.requireNonNull(supplier.get());
        }
    }

    public byte orZero() {
        return isPresent ? value : 0;
    }

    //    public byte orElseZero() {
    //        return isPresent ? value : 0;
    //    }

    public byte orElseThrow() throws NoSuchElementException {
        if (isPresent) {
            return value;
        } else {
            throw new NoSuchElementException("No value present");
        }
    }

    public byte orElse(byte other) {
        return isPresent ? value : other;
    }

    public <E extends Exception> byte orElseGet(Try.ByteSupplier<E> other) throws E {
        Objects.requireNonNull(other);

        if (isPresent) {
            return value;
        } else {
            return other.getAsByte();
        }
    }

    public <X extends Throwable> byte orElseThrow(Supplier<? extends X> exceptionSupplier) throws X {
        Objects.requireNonNull(exceptionSupplier);

        if (isPresent) {
            return value;
        } else {
            throw exceptionSupplier.get();
        }
    }

    @Override
    public int compareTo(OptionalByte optional) {
        if (optional == null || optional.isPresent == false) {
            return isPresent ? 1 : 0;
        }

        if (isPresent == false) {
            return -1;
        }

        return Byte.compare(this.get(), optional.get());
    }

    public ByteStream stream() {
        if (isPresent) {
            return ByteStream.of(value);
        } else {
            return ByteStream.empty();
        }
    }

    public Optional<Byte> boxed() {
        if (isPresent) {
            return Optional.of(value);
        } else {
            return Optional.<Byte> empty();
        }
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof OptionalByte) {
            final OptionalByte other = (OptionalByte) obj;

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
            return String.format("OptionalByte[%s]", value);
        }

        return "OptionalByte.empty";
    }
}
