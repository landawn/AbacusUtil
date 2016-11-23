package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface CharFunction<R> {

    R apply(char value);

    default <V> CharFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static CharFunction<Character> identity() {
        return t -> t;
    }
}
