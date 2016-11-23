package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface LongSupplier extends java.util.function.LongSupplier {

    public static final LongSupplier ZERO = new LongSupplier() {
        @Override
        public long getAsLong() {
            return 0;
        }
    };

    public static final LongSupplier RANDOM = new LongSupplier() {
        @Override
        public long getAsLong() {
            return Util.RAND.nextLong();
        }
    };

    @Override
    long getAsLong();
}
