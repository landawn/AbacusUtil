package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface CharPredicate {

    public static final CharPredicate ALWAYS_TRUE = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return true;
        }
    };

    public static final CharPredicate ALWAYS_FALSE = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return false;
        }
    };

    boolean test(char value);

    default CharPredicate and(CharPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) && other.test(t);
    }

    default CharPredicate negate() {
        return (t) -> !test(t);
    }

    default CharPredicate or(CharPredicate other) {
        N.requireNonNull(other);
        return (t) -> test(t) || other.test(t);
    }
}
