package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface DoublePredicate extends java.util.function.DoublePredicate {

    public static final DoublePredicate ALWAYS_TRUE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return true;
        }
    };

    public static final DoublePredicate ALWAYS_FALSE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return false;
        }
    };

    @Override
    boolean test(double value);
}
