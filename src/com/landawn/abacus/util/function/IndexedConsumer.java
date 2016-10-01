package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IndexedConsumer<T, A> {

    void accept(int idx, T t, A ac);

    default IndexedConsumer<T, A> andThen(IndexedConsumer<T, A> after) {
        N.requireNonNull(after);
        return (int idx, T t, A a) -> {
            accept(idx, t, a);
            after.accept(idx, t, a);
        };
    }
}
