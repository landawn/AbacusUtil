package com.landawn.abacus.util.function;

public interface ToCharBiFunction<T, U> {

    char applyAsChar(T t, U u);
}
