package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface CharConsumer {

    void accept(char t);

    default CharConsumer andThen(CharConsumer after) {
        N.requireNonNull(after);

        return (t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
