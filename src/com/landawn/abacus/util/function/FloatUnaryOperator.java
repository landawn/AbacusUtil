package com.landawn.abacus.util.function;

public interface FloatUnaryOperator {

    float applyAsFloat(float operand);

    static FloatUnaryOperator identity() {
        return t -> t;
    }
}
