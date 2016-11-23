package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface DoubleSupplier extends java.util.function.DoubleSupplier {

    public static final DoubleSupplier ZERO = new DoubleSupplier() {
        @Override
        public double getAsDouble() {
            return 0;
        }
    };

    public static final DoubleSupplier RANDOM = new DoubleSupplier() {
        @Override
        public double getAsDouble() {
            return Util.RAND.nextDouble();
        }
    };

    @Override
    double getAsDouble();
}