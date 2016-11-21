package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface LongPredicate extends java.util.function.LongPredicate {

    public static final LongPredicate ALWAYS_TRUE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return true;
        }
    };

    public static final LongPredicate ALWAYS_FALSE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return false;
        }
    };

    @Override
    boolean test(long value);
}
