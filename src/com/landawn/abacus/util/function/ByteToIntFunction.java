package com.landawn.abacus.util.function;

public interface ByteToIntFunction {

    public static final ByteToIntFunction DEFAULT = new ByteToIntFunction() {
        @Override
        public int applyAsInt(byte value) {
            return value;
        }
    };

    int applyAsInt(byte value);
}
