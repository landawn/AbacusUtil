package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface FloatNFunction<R> {

    R apply(float... args);

    default <V> FloatNFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return args -> after.apply(apply(args));
    }
}
