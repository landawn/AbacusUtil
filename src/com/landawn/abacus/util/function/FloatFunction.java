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
public interface FloatFunction<R> {
    R apply(float value);
}
