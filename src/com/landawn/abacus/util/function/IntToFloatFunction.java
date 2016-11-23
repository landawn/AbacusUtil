package com.landawn.abacus.util.function;

public interface IntToFloatFunction {

    public static final IntToFloatFunction DEFAULT = new IntToFloatFunction() {
        @Override
        public float applyAsFloat(int value) {
            return value;
        }
    };

    float applyAsFloat(int value);
}
