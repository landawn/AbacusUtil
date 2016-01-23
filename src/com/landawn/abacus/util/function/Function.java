/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

/**
 * 
 * @param <T>
 * @param <R>
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.Function
 */
// public interface Function<T, R> {
public interface Function<T, R> extends java.util.function.Function<T, R> {
    @Override
    R apply(T t);
}
