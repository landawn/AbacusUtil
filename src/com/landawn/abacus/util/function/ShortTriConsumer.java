package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ShortTriConsumer {

    void accept(short a, short b, short c);

    default ShortTriConsumer andThen(ShortTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
