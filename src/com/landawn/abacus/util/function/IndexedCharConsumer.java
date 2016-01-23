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
public interface IndexedCharConsumer {
    /**
     * Performs this operation on the given argument
     *     
     * @param t
     * @param idx
     */
    void accept(char t, int idx);

    default IndexedCharConsumer andThen(IndexedCharConsumer after) {
        N.requireNonNull(after);
        return (char t, int idx) -> {
            accept(t, idx);
            after.accept(t, idx);
        };
    }
}
