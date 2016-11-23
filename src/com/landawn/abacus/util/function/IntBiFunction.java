package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface IntBiFunction<R> {

    R apply(int t, int u);

    default <V> IntBiFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (t, u) -> after.apply(apply(t, u));
    }
}
