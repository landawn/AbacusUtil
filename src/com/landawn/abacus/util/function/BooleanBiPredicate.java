package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface BooleanBiPredicate {

    public static final BooleanBiPredicate ALWAYS_TRUE = new BooleanBiPredicate() {
        @Override
        public boolean test(boolean t, boolean u) {
            return true;
        }
    };

    public static final BooleanBiPredicate ALWAYS_FALSE = new BooleanBiPredicate() {
        @Override
        public boolean test(boolean t, boolean u) {
            return false;
        }
    };

    public static final BooleanBiPredicate BOTH_TRUE = new BooleanBiPredicate() {
        @Override
        public boolean test(boolean t, boolean u) {
            return t && u;
        }
    };

    public static final BooleanBiPredicate BOTH_FALSE = new BooleanBiPredicate() {
        @Override
        public boolean test(boolean t, boolean u) {
            return t == false && u == false;
        }
    };

    public static final BooleanBiPredicate IS_EQUAL = new BooleanBiPredicate() {
        @Override
        public boolean test(boolean t, boolean u) {
            return t == u;
        }
    };

    public static final BooleanBiPredicate NOT_EQUAL = new BooleanBiPredicate() {
        @Override
        public boolean test(boolean t, boolean u) {
            return t != u;
        }
    };

    boolean test(boolean t, boolean u);

    default BooleanBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default BooleanBiPredicate and(BooleanBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default BooleanBiPredicate or(BooleanBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
