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
 * @see java.util.function.DoublePredicate
 */
// public interface DoublePredicate {
public interface DoublePredicate extends java.util.function.DoublePredicate {
    /**
     * Returns {@code true} if the specified {@code value} matches the expectation, otherwise, {@code false} is returned.
     *
     * @param value
     * @return
     */
    @Override
    boolean test(double value);
}
