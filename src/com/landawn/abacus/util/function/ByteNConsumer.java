package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ByteNConsumer {

    void accept(byte... args);

    default ByteNConsumer andThen(ByteNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
