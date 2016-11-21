package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface BooleanPredicate {

    public static final BooleanPredicate ALWAYS_TRUE = new BooleanPredicate() {
        @Override
        public boolean test(boolean value) {
            return true;
        }
    };

    public static final BooleanPredicate ALWAYS_FALSE = new BooleanPredicate() {
        @Override
        public boolean test(boolean value) {
            return false;
        }
    };

    boolean test(boolean value);

    default BooleanPredicate and(BooleanPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) && other.test(t);
    }

    default BooleanPredicate negate() {
        return (t) -> !test(t);
    }

    default BooleanPredicate or(BooleanPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) || other.test(t);
    }
}
