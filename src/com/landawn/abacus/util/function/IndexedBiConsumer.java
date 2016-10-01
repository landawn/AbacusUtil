package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IndexedBiConsumer<T, A, U> {

    void accept(int idx, T t, A a, U u);

    default IndexedBiConsumer<T, A, U> andThen(IndexedBiConsumer<T, A, U> after) {
        N.requireNonNull(after);
        return (int idx, T t, A a, U u) -> {
            accept(idx, t, a, u);
            after.accept(idx, t, a, u);
        };
    }
}
