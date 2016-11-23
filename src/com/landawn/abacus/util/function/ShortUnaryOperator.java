package com.landawn.abacus.util.function;

public interface ShortUnaryOperator {

    short applyAsShort(short operand);

    static ShortUnaryOperator identity() {
        return t -> t;
    }
}
