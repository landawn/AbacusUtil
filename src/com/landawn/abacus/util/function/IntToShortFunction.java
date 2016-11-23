package com.landawn.abacus.util.function;

public interface IntToShortFunction {

    public static final IntToShortFunction DEFAULT = new IntToShortFunction() {
        @Override
        public short applyAsShort(int value) {
            return (short) value;
        }
    };

    short applyAsShort(int value);
}
