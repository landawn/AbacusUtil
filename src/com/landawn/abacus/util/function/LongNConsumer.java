package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface LongNConsumer {

    void accept(long... args);

    default LongNConsumer andThen(LongNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
