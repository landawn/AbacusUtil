package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface FloatConsumer {

    void accept(float t);

    default FloatConsumer andThen(FloatConsumer after) {
        N.requireNonNull(after);

        return (t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
