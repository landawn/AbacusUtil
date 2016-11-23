package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface BooleanSupplier extends java.util.function.BooleanSupplier {

    public static final BooleanSupplier TRUE = new BooleanSupplier() {
        @Override
        public boolean getAsBoolean() {
            return true;
        }
    };

    public static final BooleanSupplier FALSE = new BooleanSupplier() {
        @Override
        public boolean getAsBoolean() {
            return false;
        }
    };

    public static final BooleanSupplier RANDOM = new BooleanSupplier() {
        @Override
        public boolean getAsBoolean() {
            return Util.RAND.nextInt() / 2 == 0 ? false : true;
        }
    };

    @Override
    boolean getAsBoolean();
}
