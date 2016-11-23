package com.landawn.abacus.util.function;

public interface ToBooleanFunction<T> {

    public static final ToBooleanFunction<Boolean> UNBOX = new ToBooleanFunction<Boolean>() {
        @Override
        public boolean applyAsBoolean(Boolean value) {
            return value == null ? false : value.booleanValue();
        }
    };

    boolean applyAsBoolean(T value);
}
