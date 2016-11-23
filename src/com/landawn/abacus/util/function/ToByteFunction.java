package com.landawn.abacus.util.function;

public interface ToByteFunction<T> {

    public static final ToByteFunction<Byte> UNBOX = new ToByteFunction<Byte>() {
        @Override
        public byte applyAsByte(Byte value) {
            return value == null ? 0 : value.byteValue();
        }
    };

    byte applyAsByte(T value);
}
