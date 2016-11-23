package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface LongTriConsumer {

    void accept(long a, long b, long c);

    default LongTriConsumer andThen(LongTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
