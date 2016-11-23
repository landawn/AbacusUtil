package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface DoubleBiConsumer {

    void accept(double t, double u);

    default DoubleBiConsumer andThen(DoubleBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
