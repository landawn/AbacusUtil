package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ShortNConsumer {

    void accept(short... args);

    default ShortNConsumer andThen(ShortNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
