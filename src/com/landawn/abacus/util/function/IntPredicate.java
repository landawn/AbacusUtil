package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IntPredicate extends java.util.function.IntPredicate {

    public static final IntPredicate ALWAYS_TRUE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return true;
        }
    };

    public static final IntPredicate ALWAYS_FALSE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return false;
        }
    };

    @Override
    boolean test(int value);
}
