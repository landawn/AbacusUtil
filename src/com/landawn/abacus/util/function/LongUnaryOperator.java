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
 * @see java.util.function.IntUnaryOperator
 */
// public interface LongUnaryOperator {
public interface LongUnaryOperator extends java.util.function.LongUnaryOperator {
    @Override
    long applyAsLong(long operand);
}
