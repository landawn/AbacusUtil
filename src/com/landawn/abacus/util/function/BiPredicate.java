package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface BiPredicate<T, U> extends java.util.function.BiPredicate<T, U> {

    @SuppressWarnings("rawtypes")
    public static final BiPredicate ALWAYS_TRUE = new BiPredicate() {
        @Override
        public boolean test(Object t, Object u) {
            return true;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate ALWAYS_FALSE = new BiPredicate() {
        @Override
        public boolean test(Object t, Object u) {
            return false;
        }
    };

    @Override
    boolean test(T t, U u);
}
