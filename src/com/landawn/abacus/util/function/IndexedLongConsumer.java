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
public interface IndexedLongConsumer {
    /**
     * Performs this operation on the given argument
     *     
     * @param t
     * @param idx
     */
    void accept(long t, int idx);

    default IndexedLongConsumer andThen(IndexedLongConsumer after) {
        N.requireNonNull(after);
        return (long t, int idx) -> {
            accept(t, idx);
            after.accept(t, idx);
        };
    }
}
