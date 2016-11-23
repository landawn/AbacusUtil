package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface ShortTriFunction<R> {

    R apply(short a, short b, short c);

    default <V> ShortTriFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (a, b, c) -> after.apply(apply(a, b, c));
    }
}
