package com.landawn.abacus.util.function;

import java.util.Comparator;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface BinaryOperator<T> extends BiFunction<T, T, T>, java.util.function.BinaryOperator<T> {

    public static <T> BinaryOperator<T> minBy(Comparator<? super T> comparator) {
        N.requireNonNull(comparator);

        return (a, b) -> comparator.compare(a, b) <= 0 ? a : b;
    }

    public static <T> BinaryOperator<T> maxBy(Comparator<? super T> comparator) {
        N.requireNonNull(comparator);

        return (a, b) -> comparator.compare(a, b) >= 0 ? a : b;
    }
}