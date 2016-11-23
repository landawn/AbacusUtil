package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface FloatBiFunction<R> {

    R apply(float t, float u);

    default <V> FloatBiFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (t, u) -> after.apply(apply(t, u));
    }
}
