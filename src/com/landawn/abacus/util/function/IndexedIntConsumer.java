package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IndexedIntConsumer {

    void accept(int t, int idx);

    default IndexedIntConsumer andThen(IndexedIntConsumer after) {
        N.requireNonNull(after);
        return (int t, int idx) -> {
            accept(t, idx);
            after.accept(t, idx);
        };
    }
}
