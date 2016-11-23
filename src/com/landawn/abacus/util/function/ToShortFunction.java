package com.landawn.abacus.util.function;

public interface ToShortFunction<T> {

    public static final ToShortFunction<Short> UNBOX = new ToShortFunction<Short>() {
        @Override
        public short applyAsShort(Short value) {
            return value == null ? 0 : value.shortValue();
        }
    };

    short applyAsShort(T value);
}
