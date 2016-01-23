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
// public interface IntUnaryOperator {
public interface IntUnaryOperator extends java.util.function.IntUnaryOperator {
    @Override
    int applyAsInt(int operand);
}
