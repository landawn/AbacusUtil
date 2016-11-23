package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface FloatTriConsumer {

    void accept(float a, float b, float c);

    default FloatTriConsumer andThen(FloatTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
