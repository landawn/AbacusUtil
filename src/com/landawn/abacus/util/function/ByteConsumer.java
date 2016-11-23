package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ByteConsumer {

    void accept(byte t);

    default ByteConsumer andThen(ByteConsumer after) {
        N.requireNonNull(after);

        return (t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
