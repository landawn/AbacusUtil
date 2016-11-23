package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

public interface IntNFunction<R> {

    R apply(int... args);

    default <V> IntNFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return args -> after.apply(apply(args));
    }
}
