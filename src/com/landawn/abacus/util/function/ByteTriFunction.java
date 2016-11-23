package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface ByteTriFunction<R> {

    R apply(byte a, byte b, byte c);

    default <V> ByteTriFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (a, b, c) -> after.apply(apply(a, b, c));
    }
}
