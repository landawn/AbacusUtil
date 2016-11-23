package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedCharConsumer {

    void accept(int idx, char e, char[] a);

    default IndexedCharConsumer andThen(IndexedCharConsumer after) {
        N.requireNonNull(after);

        return (idx, e, a) -> {
            accept(idx, e, a);
            after.accept(idx, e, a);
        };
    }
}
