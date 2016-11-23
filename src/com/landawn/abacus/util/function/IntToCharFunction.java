package com.landawn.abacus.util.function;

public interface IntToCharFunction {

    public static final IntToCharFunction DEFAULT = new IntToCharFunction() {
        @Override
        public char applyAsChar(int value) {
            return (char) value;
        }
    };

    char applyAsChar(int value);
}
