package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface IntTriFunction<R> {

    R apply(int a, int b, int c);

    default <V> IntTriFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (a, b, c) -> after.apply(apply(a, b, c));
    }
}
