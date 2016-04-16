package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IndexedConsumer<T> {

    void accept(T t, int idx);

    default IndexedConsumer<T> andThen(IndexedConsumer<T> after) {
        N.requireNonNull(after);
        return (T t, int idx) -> {
            accept(t, idx);
            after.accept(t, idx);
        };
    }
}
