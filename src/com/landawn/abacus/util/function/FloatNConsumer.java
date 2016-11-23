package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface FloatNConsumer {

    void accept(float... args);

    default FloatNConsumer andThen(FloatNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
