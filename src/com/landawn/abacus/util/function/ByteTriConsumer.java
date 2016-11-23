package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ByteTriConsumer {

    void accept(byte a, byte b, byte c);

    default ByteTriConsumer andThen(ByteTriConsumer after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
