package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IntSupplier extends java.util.function.IntSupplier {

    public static final IntSupplier ZERO = new IntSupplier() {
        @Override
        public int getAsInt() {
            return 0;
        }
    };

    public static final IntSupplier RANDOM = new IntSupplier() {
        @Override
        public int getAsInt() {
            return Util.RAND.nextInt();
        }
    };

    @Override
    int getAsInt();
}
