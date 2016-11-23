package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface CharNFunction<R> {

    R apply(char... args);

    default <V> CharNFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return args -> after.apply(apply(args));
    }
}
