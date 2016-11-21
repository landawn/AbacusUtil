package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface ShortPredicate {

    public static final ShortPredicate ALWAYS_TRUE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return true;
        }
    };

    public static final ShortPredicate ALWAYS_FALSE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return false;
        }
    };

    boolean test(short value);

    default ShortPredicate and(ShortPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) && other.test(t);
    }

    default ShortPredicate negate() {
        return (t) -> !test(t);
    }

    default ShortPredicate or(ShortPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) || other.test(t);
    }
}
