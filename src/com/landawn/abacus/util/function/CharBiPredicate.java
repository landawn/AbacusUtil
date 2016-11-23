package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface CharBiPredicate {

    public static final CharBiPredicate ALWAYS_TRUE = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return true;
        }
    };

    public static final CharBiPredicate ALWAYS_FALSE = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return false;
        }
    };

    public static final CharBiPredicate IS_EQUAL = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return t == u;
        }
    };

    public static final CharBiPredicate NOT_EQUAL = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return t != u;
        }
    };

    public static final CharBiPredicate GREATER_THAN = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return t > u;
        }
    };

    public static final CharBiPredicate GREATER_EQUAL = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return t >= u;
        }
    };

    public static final CharBiPredicate LESS_THAN = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return t < u;
        }
    };

    public static final CharBiPredicate LESS_EQUAL = new CharBiPredicate() {
        @Override
        public boolean test(char t, char u) {
            return t <= u;
        }
    };

    boolean test(char t, char u);

    default CharBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default CharBiPredicate and(CharBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default CharBiPredicate or(CharBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
