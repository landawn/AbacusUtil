package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface DoubleNConsumer {

    void accept(double... args);

    default DoubleNConsumer andThen(DoubleNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
