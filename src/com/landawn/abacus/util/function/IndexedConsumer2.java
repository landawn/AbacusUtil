package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IndexedConsumer2<A> {

    void accept(int idx, A ac);

    default IndexedConsumer2<A> andThen(IndexedConsumer2<A> after) {
        N.requireNonNull(after);
        return (int idx, A a) -> {
            accept(idx, a);
            after.accept(idx, a);
        };
    }
}
