/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

/**
 * 
 * @param <T>
 * @param <U>
 * @param <R>
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.BiFunction
 */
// public interface BiFunction<T, U, R> {
public interface BiFunction<T, U, R> extends java.util.function.BiFunction<T, U, R> {
    @Override
    R apply(T t, U u);
}
