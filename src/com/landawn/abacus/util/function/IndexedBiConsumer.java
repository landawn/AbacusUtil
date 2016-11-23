package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedBiConsumer<T, A, U> {

    void accept(int idx, T e, A a, U u);

    default IndexedBiConsumer<T, A, U> andThen(IndexedBiConsumer<T, A, U> after) {
        N.requireNonNull(after);

        return (idx, e, a, u) -> {
            accept(idx, e, a, u);
            after.accept(idx, e, a, u);
        };
    }
}
