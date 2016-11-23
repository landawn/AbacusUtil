package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface CharBiConsumer {

    void accept(char t, char u);

    default CharBiConsumer andThen(CharBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
