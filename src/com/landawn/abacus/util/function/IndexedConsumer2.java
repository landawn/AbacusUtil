package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IndexedConsumer2<A> {

    void accept(int idx, A ac);

    default IndexedConsumer2<A> andThen(IndexedConsumer2<A> after) {
        N.requireNonNull(after);

        return (idx, ac) -> {
            accept(idx, ac);
            after.accept(idx, ac);
        };
    }
}
