/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

/**
 *
 * @param <T>
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.Predicate
 */
// public interface Predicate<T> {
public interface Predicate<T> extends java.util.function.Predicate<T> {
    /**
     * Returns {@code true} if the specified {@code value} matches the expectation, otherwise, {@code false} is returned.
     *
     * @param value
     * @return
     */
    @Override
    boolean test(T value);
}
