package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface BooleanNConsumer {

    void accept(boolean... args);

    default BooleanNConsumer andThen(BooleanNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
