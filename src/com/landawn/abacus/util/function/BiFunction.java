package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface BiFunction<T, U, R> extends java.util.function.BiFunction<T, U, R> {

    @Override
    R apply(T t, U u);
}
