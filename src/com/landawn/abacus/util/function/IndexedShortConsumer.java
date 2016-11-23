package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedShortConsumer {

    void accept(int idx, short e, short[] a);

    default IndexedShortConsumer andThen(IndexedShortConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
