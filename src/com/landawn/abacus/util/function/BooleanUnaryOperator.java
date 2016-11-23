package com.landawn.abacus.util.function;

public interface BooleanUnaryOperator {

    boolean applyAsBoolean(boolean operand);

    static BooleanUnaryOperator identity() {
        return t -> t;
    }
}
