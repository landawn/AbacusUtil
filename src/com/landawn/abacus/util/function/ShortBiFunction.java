package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface ShortBiFunction<R> {

    R apply(short t, short u);

    default <V> ShortBiFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);
        return (t, u) -> after.apply(apply(t, u));
    }
}
