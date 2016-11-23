package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface IndexedBiFunction<T, A, U, R> {

    R apply(int idx, T e, A a, U u);

    default <V> IndexedBiFunction<T, A, U, V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (idx, e, a, u) -> after.apply(apply(idx, e, a, u));
    }
}
