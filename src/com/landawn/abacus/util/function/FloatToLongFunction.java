package com.landawn.abacus.util.function;

public interface FloatToLongFunction {

    public static final FloatToLongFunction DEFAULT = new FloatToLongFunction() {
        @Override
        public long applyAsLong(float value) {
            return (long) value;
        }
    };

    long applyAsLong(float value);
}