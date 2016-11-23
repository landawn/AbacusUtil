package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface BooleanBiFunction<R> {

    R apply(boolean t, boolean u);

    default <V> BooleanBiFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (t, u) -> after.apply(apply(t, u));
    }
}
