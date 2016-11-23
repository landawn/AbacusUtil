package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface NConsumer<T> {

    void accept(T... args);

    default NConsumer<T> andThen(NConsumer<? super T> after) {
        N.requireNonNull(after);

        return args -> {
            accept(args);
            after.accept(args);
        };
    }
}
