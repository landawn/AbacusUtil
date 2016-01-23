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
public interface IndexedDoubleConsumer {
    /**
     * Performs this operation on the given argument
     *     
     * @param t
     * @param idx
     */
    void accept(double t, int idx);

    default IndexedDoubleConsumer andThen(IndexedDoubleConsumer after) {
        N.requireNonNull(after);
        return (double t, int idx) -> {
            accept(t, idx);
            after.accept(t, idx);
        };
    }
}
