/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util.function;

/**
 *
 * @param <T>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see java.util.function.Consumer
 */
// public interface Consumer<T> {
public interface Consumer<T> extends java.util.function.Consumer<T> {
    /**
     * Performs this operation on the given argument
     *     
     * @param t
     */
    @Override
    void accept(T t);
}
