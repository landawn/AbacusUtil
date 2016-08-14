package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface Function<T, R> extends java.util.function.Function<T, R> {

    @Override
    R apply(T t);

    static <T> Function<T, T> identity() {
        return t -> t;
    }
}
