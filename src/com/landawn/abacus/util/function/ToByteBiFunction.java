package com.landawn.abacus.util.function;

public interface ToByteBiFunction<T, U> {

    byte applyAsByte(T t, U u);
}
