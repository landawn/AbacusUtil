/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.IntConsumer
 */
// public interface IntConsumer {
public interface IntConsumer extends java.util.function.IntConsumer {
    /**
     * Performs this operation on the given argument
     *     
     * @param t
     */
    @Override
    void accept(int t);
}
