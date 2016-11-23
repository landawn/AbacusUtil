package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface BooleanFunction<R> {

    R apply(boolean value);

    default <V> BooleanFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static BooleanFunction<Boolean> identity() {
        return t -> t;
    }
}
