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
public interface FloatBinaryOperator {
    float applyAsFloat(float left, float right);
}
