package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface ByteFunction<R> {

    R apply(byte value);

    default <V> ByteFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static ByteFunction<Byte> identity() {
        return t -> t;
    }
}
