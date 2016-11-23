package com.landawn.abacus.util.function;

public interface CharUnaryOperator {

    char applyAsChar(char operand);

    static CharUnaryOperator identity() {
        return t -> t;
    }
}
