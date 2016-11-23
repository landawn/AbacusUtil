package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface DoubleToLongFunction extends java.util.function.DoubleToLongFunction {

    public static final DoubleToLongFunction DEFAULT = new DoubleToLongFunction() {
        @Override
        public long applyAsLong(double value) {
            return (long) value;
        }
    };

    @Override
    long applyAsLong(double value);
}
