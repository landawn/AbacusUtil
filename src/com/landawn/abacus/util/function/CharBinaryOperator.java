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
 * @see java.util.function.IntBinaryOperator
 */
public interface CharBinaryOperator {
    char applyAsChar(char left, char right);
}
