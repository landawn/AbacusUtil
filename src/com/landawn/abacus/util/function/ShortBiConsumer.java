package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ShortBiConsumer {

    void accept(short t, short u);

    default ShortBiConsumer andThen(ShortBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
