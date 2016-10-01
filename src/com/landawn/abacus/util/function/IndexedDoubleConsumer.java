package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IndexedDoubleConsumer {

    void accept(int idx, double t, double[] a);

    default IndexedDoubleConsumer andThen(IndexedDoubleConsumer after) {
        N.requireNonNull(after);
        return (int idx, double t, double[] a) -> {
            accept(idx, t, a);
            after.accept(idx, t, a);
        };
    }
}
