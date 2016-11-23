package com.landawn.abacus.util.function;

public interface DoubleToFloatFunction {

    public static final DoubleToFloatFunction DEFAULT = new DoubleToFloatFunction() {
        @Override
        public float applyAsFloat(double value) {
            return (float) value;
        }
    };

    float applyAsFloat(double value);
}