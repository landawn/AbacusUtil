package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface FloatFunction<R> {

    R apply(float value);

    default <V> FloatFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static FloatFunction<Float> identity() {
        return t -> t;
    }
}
