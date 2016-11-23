package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IntBiConsumer {

    void accept(int t, int u);

    default IntBiConsumer andThen(IntBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
