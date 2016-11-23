package com.landawn.abacus.util.function;

public interface ByteUnaryOperator {

    byte applyAsByte(byte operand);

    static ByteUnaryOperator identity() {
        return t -> t;
    }
}
