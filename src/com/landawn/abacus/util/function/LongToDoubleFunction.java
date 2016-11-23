package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface LongToDoubleFunction extends java.util.function.LongToDoubleFunction {

    public static final LongToDoubleFunction DEFAULT = new LongToDoubleFunction() {
        @Override
        public double applyAsDouble(long value) {
            return value;
        }
    };

    @Override
    double applyAsDouble(long value);
}