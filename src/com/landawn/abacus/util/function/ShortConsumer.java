package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ShortConsumer {

    void accept(short t);

    default ShortConsumer andThen(ShortConsumer after) {
        N.requireNonNull(after);

        return (t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
