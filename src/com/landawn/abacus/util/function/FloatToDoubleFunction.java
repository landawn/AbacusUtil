package com.landawn.abacus.util.function;

public interface FloatToDoubleFunction {

    public static final FloatToDoubleFunction DEFAULT = new FloatToDoubleFunction() {
        @Override
        public double applyAsDouble(float value) {
            return value;
        }
    };

    double applyAsDouble(float value);
}