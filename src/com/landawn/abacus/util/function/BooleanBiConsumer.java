package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface BooleanBiConsumer {

    void accept(boolean t, boolean u);

    default BooleanBiConsumer andThen(BooleanBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
