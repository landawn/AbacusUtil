package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IntTriConsumer {

    void accept(int a, int b, int c);

    default IntTriConsumer andThen(IntTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
