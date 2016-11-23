package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface CharNConsumer {

    void accept(char... args);

    default CharNConsumer andThen(CharNConsumer after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
