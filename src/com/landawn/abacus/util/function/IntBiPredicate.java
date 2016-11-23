package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface IntBiPredicate {

    public static final IntBiPredicate ALWAYS_TRUE = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return true;
        }
    };

    public static final IntBiPredicate ALWAYS_FALSE = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return false;
        }
    };

    public static final IntBiPredicate IS_EQUAL = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return t == u;
        }
    };

    public static final IntBiPredicate NOT_EQUAL = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return t != u;
        }
    };

    public static final IntBiPredicate GREATER_THAN = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return t > u;
        }
    };

    public static final IntBiPredicate GREATER_EQUAL = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return t >= u;
        }
    };

    public static final IntBiPredicate LESS_THAN = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return t < u;
        }
    };

    public static final IntBiPredicate LESS_EQUAL = new IntBiPredicate() {
        @Override
        public boolean test(int t, int u) {
            return t <= u;
        }
    };

    boolean test(int t, int u);

    default IntBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default IntBiPredicate and(IntBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default IntBiPredicate or(IntBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
