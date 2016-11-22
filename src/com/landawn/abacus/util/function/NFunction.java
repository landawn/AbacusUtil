package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface NFunction<R> {

    R apply(Object... args);

    default <V> NFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return args -> after.apply(apply(args));
    }
}
