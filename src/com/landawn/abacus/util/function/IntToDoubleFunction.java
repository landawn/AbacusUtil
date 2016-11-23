package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IntToDoubleFunction extends java.util.function.IntToDoubleFunction {

    public static final IntToDoubleFunction DEFAULT = new IntToDoubleFunction() {
        @Override
        public double applyAsDouble(int value) {
            return value;
        }
    };

    @Override
    double applyAsDouble(int value);
}
