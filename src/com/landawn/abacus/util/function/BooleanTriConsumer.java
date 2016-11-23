package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface BooleanTriConsumer {

    void accept(boolean a, boolean b, boolean c);

    default BooleanTriConsumer andThen(BooleanTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
