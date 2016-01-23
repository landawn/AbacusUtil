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
 * @see java.util.function.LongConsumer
 */
// public interface LongConsumer {
public interface LongConsumer extends java.util.function.LongConsumer {
    /**
     * Performs this operation on the given argument
     *     
     * @param t
     */
    @Override
    void accept(long t);
}
