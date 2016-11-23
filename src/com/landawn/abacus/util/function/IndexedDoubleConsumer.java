package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedDoubleConsumer {

    void accept(int idx, double e, double[] a);

    default IndexedDoubleConsumer andThen(IndexedDoubleConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
