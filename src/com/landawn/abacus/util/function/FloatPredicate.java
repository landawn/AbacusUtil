package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface FloatPredicate {

    public static final FloatPredicate ALWAYS_TRUE = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return true;
        }
    };

    public static final FloatPredicate ALWAYS_FALSE = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return false;
        }
    };

    boolean test(float value);

    default FloatPredicate and(FloatPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) && other.test(t);
    }

    default FloatPredicate negate() {
        return (t) -> !test(t);
    }

    default FloatPredicate or(FloatPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) || other.test(t);
    }
}
