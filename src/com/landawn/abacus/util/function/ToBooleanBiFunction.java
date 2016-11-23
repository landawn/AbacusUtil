package com.landawn.abacus.util.function;

public interface ToBooleanBiFunction<T, U> {

    boolean applyAsBoolean(T t, U u);
}
