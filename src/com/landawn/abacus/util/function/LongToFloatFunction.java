package com.landawn.abacus.util.function;

public interface LongToFloatFunction {

    public static final LongToFloatFunction DEFAULT = new LongToFloatFunction() {
        @Override
        public float applyAsFloat(long value) {
            return value;
        }
    };

    float applyAsFloat(long value);
}