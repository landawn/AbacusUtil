package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface FloatBiConsumer {

    void accept(float t, float u);

    default FloatBiConsumer andThen(FloatBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
