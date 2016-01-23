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
 * @see java.util.function.DoubleFunction
 */
// public interface DoubleFunction<R> {
public interface DoubleFunction<R> extends java.util.function.DoubleFunction<R> {
    @Override
    R apply(double value);
}
