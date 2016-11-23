package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedConsumer<T, A> {

    void accept(int idx, T e, A ac);

    default IndexedConsumer<T, A> andThen(IndexedConsumer<T, A> after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
