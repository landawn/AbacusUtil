/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

/**
 * 
 * @param <R>
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.LongFunction
 */
// public interface LongFunction<R> {
public interface LongFunction<R> extends java.util.function.LongFunction<R> {
    @Override
    R apply(long value);
}
