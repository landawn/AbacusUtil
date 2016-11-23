package com.landawn.abacus.util.function;

public interface ShortToIntFunction {

    public static final ShortToIntFunction DEFAULT = new ShortToIntFunction() {
        @Override
        public int applyAsInt(short value) {
            return value;
        }
    };

    int applyAsInt(short value);
}
