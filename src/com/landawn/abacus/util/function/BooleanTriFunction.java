package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface BooleanTriFunction<R> {

    R apply(boolean a, boolean b, boolean c);

    default <V> BooleanTriFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (a, b, c) -> after.apply(apply(a, b, c));
    }
}
