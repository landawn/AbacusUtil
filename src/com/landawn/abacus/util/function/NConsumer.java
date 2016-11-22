package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface NConsumer {

    void accept(Object... args);

    default NConsumer andThen(NConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
