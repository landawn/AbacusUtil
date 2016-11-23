package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface CharTriConsumer {

    void accept(char a, char b, char c);

    default CharTriConsumer andThen(CharTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
