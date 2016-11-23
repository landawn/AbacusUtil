package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface ShortBiFunction<R> {

    R apply(short t, short u);

    default <V> ShortBiFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (t, u) -> after.apply(apply(t, u));
    }
}
