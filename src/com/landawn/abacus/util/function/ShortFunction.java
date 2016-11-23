package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface ShortFunction<R> {

    R apply(short value);

    default <V> ShortFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static ShortFunction<Short> identity() {
        return t -> t;
    }
}
