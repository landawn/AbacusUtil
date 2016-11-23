package com.landawn.abacus.util.function;

public interface ToFloatFunction<T> {

    public static final ToFloatFunction<Float> UNBOX = new ToFloatFunction<Float>() {
        @Override
        public float applyAsFloat(Float value) {
            return value == null ? 0 : value.floatValue();
        }
    };

    float applyAsFloat(T value);
}
