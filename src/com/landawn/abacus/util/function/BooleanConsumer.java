/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.Consumer
 */
public interface BooleanConsumer {
    /**
     * Performs this operation on the given argument
     *     
     * @param t
     */
    void accept(boolean t);

    default BooleanConsumer andThen(BooleanConsumer after) {
        N.requireNonNull(after);
        return (t) -> {
            accept(t);
            after.accept(t);
        };
    }
}
