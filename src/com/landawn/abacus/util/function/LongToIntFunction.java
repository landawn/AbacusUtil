package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface LongToIntFunction extends java.util.function.LongToIntFunction {

    public static final LongToIntFunction DEFAULT = new LongToIntFunction() {
        @Override
        public int applyAsInt(long value) {
            return (int) value;
        }
    };

    @Override
    int applyAsInt(long value);
}
