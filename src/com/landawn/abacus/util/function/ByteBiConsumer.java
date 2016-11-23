package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ByteBiConsumer {

    void accept(byte t, byte u);

    default ByteBiConsumer andThen(ByteBiConsumer after) {
        N.requireNonNull(after);

        return (t, u) -> {
            accept(t, u);
            after.accept(t, u);
        };
    }
}
