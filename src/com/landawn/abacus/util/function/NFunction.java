package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface NFunction<T, R> {

    R apply(T... args);

    default <V> NFunction<T, V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return args -> after.apply(apply(args));
    }
}
