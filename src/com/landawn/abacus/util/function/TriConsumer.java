package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface TriConsumer<A, B, C> {

    void accept(A a, B b, C c);

    default TriConsumer<A, B, C> andThen(TriConsumer<? super A, ? super B, ? super C> after) {
        N.requireNonNull(after);

        return (a, b, c) -> {
            accept(a, b, c);
            after.accept(a, b, c);
        };
    }
}
