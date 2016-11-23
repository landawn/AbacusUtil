package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface DoubleTriConsumer {

    void accept(double a, double b, double c);

    default DoubleTriConsumer andThen(DoubleTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
