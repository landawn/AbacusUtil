package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface BooleanConsumer {

    void accept(boolean t);

    default BooleanConsumer andThen(BooleanConsumer after) {
        N.requireNonNull(after);

        return (t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
