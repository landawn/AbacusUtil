package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedBooleanConsumer {

    void accept(int idx, boolean e, boolean[] a);

    default IndexedBooleanConsumer andThen(IndexedBooleanConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
