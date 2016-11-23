package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface LongBiConsumer {

    void accept(long t, long u);

    default LongBiConsumer andThen(LongBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
