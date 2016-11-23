package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IntNConsumer {

    void accept(int... args);

    default IntNConsumer andThen(IntNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
