package com.landawn.abacus.util.function;

public interface IntToByteFunction {

    public static final IntToByteFunction DEFAULT = new IntToByteFunction() {
        @Override
        public byte applyAsByte(int value) {
            return (byte) value;
        }
    };

    byte applyAsByte(int value);
}
