package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IndexedBiConsumer<T, U> {

    void accept(int idx, T t, U u);

    default IndexedBiConsumer<T, U> andThen(IndexedBiConsumer<T, U> after) {
        N.requireNonNull(after);
        return (int idx, T t, U u) -> {
            accept(idx, t, u);
            after.accept(idx, t, u);
        };
    }
}
