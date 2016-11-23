package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface CharBiFunction<R> {

    R apply(char t, char u);

    default <V> CharBiFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (t, u) -> after.apply(apply(t, u));
    }
}
