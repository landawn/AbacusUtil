package com.landawn.abacus.util.function;

public interface ByteSupplier {

    public static final ByteSupplier ZERO = new ByteSupplier() {
        @Override
        public byte getAsByte() {
            return 0;
        }
    };

    public static final ByteSupplier RANDOM = new ByteSupplier() {
        @Override
        public byte getAsByte() {
            return (byte) Util.RAND.nextInt();
        }
    };

    byte getAsByte();
}