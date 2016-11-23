package com.landawn.abacus.util.function;

public interface FloatToIntFunction {

    public static final FloatToIntFunction DEFAULT = new FloatToIntFunction() {
        @Override
        public int applyAsInt(float value) {
            return (int) value;
        }
    };

    int applyAsInt(float value);
}