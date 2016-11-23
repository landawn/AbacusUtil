package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedLongConsumer {

    void accept(int idx, long e, long[] a);

    default IndexedLongConsumer andThen(IndexedLongConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
