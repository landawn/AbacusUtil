package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface TriFunction<A, B, C, R> {

    R apply(A a, B b, C c);

    default <V> TriFunction<A, B, C, V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return (a, b, c) -> after.apply(apply(a, b, c));
    }
}
