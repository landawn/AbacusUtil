package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface LongBiPredicate {

    public static final LongBiPredicate ALWAYS_TRUE = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return true;
        }
    };

    public static final LongBiPredicate ALWAYS_FALSE = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return false;
        }
    };

    public static final LongBiPredicate IS_EQUAL = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return t == u;
        }
    };

    public static final LongBiPredicate NOT_EQUAL = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return t != u;
        }
    };

    public static final LongBiPredicate GREATER_THAN = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return t > u;
        }
    };

    public static final LongBiPredicate GREATER_EQUAL = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return t >= u;
        }
    };

    public static final LongBiPredicate LESS_THAN = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return t < u;
        }
    };

    public static final LongBiPredicate LESS_EQUAL = new LongBiPredicate() {
        @Override
        public boolean test(long t, long u) {
            return t <= u;
        }
    };

    boolean test(long t, long u);

    default LongBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default LongBiPredicate and(LongBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default LongBiPredicate or(LongBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
