package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedFloatConsumer {

    void accept(int idx, float e, float[] a);

    default IndexedFloatConsumer andThen(IndexedFloatConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
