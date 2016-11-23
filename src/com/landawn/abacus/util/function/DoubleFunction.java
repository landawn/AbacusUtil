package com.landawn.abacus.util.function;

import java.util.function.Function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface DoubleFunction<R> extends java.util.function.DoubleFunction<R> {

    @Override
    R apply(double value);

    default <V> DoubleFunction<V> andThen(Function<? super R, ? extends V> after) {
        N.requireNonNull(after);

        return t -> after.apply(apply(t));
    }

    static DoubleFunction<Double> identity() {
        return t -> t;
    }
}
