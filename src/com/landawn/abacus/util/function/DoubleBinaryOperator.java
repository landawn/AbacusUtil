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
 * @see java.util.function.DoubleBinaryOperator
 */
// public interface DoubleBinaryOperator {
public interface DoubleBinaryOperator extends java.util.function.DoubleBinaryOperator {
    @Override
    double applyAsDouble(double left, double right);
}
