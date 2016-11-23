package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedByteConsumer {

    void accept(int idx, byte e, byte[] a);

    default IndexedByteConsumer andThen(IndexedByteConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
