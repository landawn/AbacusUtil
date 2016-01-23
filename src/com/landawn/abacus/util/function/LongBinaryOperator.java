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
 * @see java.util.function.LongBinaryOperator
 */
// public interface LongBinaryOperator {
public interface LongBinaryOperator extends java.util.function.LongBinaryOperator {
    @Override
    long applyAsLong(long left, long right);
}
