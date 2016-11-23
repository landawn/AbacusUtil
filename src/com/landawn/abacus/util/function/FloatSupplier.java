package com.landawn.abacus.util.function;

public interface FloatSupplier {

    public static final FloatSupplier ZERO = new FloatSupplier() {
        @Override
        public float getAsFloat() {
            return 0;
        }
    };

    public static final FloatSupplier RANDOM = new FloatSupplier() {
        @Override
        public float getAsFloat() {
            return Util.RAND.nextFloat();
        }
    };

    float getAsFloat();
}