package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedIntConsumer {

    void accept(int idx, int e, int[] a);

    default IndexedIntConsumer andThen(IndexedIntConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
