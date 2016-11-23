package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IntToLongFunction extends java.util.function.IntToLongFunction {

    public static final IntToLongFunction DEFAULT = new IntToLongFunction() {
        @Override
        public long applyAsLong(int value) {
            return value;
        }
    };

    @Override
    long applyAsLong(int value);
}
