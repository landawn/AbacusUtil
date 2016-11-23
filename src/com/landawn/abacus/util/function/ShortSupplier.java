package com.landawn.abacus.util.function;

public interface ShortSupplier {

    public static final ShortSupplier ZERO = new ShortSupplier() {
        @Override
        public short getAsShort() {
            return 0;
        }
    };

    public static final ShortSupplier RANDOM = new ShortSupplier() {
        @Override
        public short getAsShort() {
            return (short) Util.RAND.nextInt();
        }
    };

    short getAsShort();
}