package com.landawn.abacus.util.function;

public interface CharToIntFunction {

    public static final CharToIntFunction DEFAULT = new CharToIntFunction() {
        @Override
        public int applyAsInt(char value) {
            return value;
        }
    };

    int applyAsInt(char value);
}
