package com.landawn.abacus.util.function;

public interface ToFloatBiFunction<T, U> {

    float applyAsFloat(T t, U u);
}
