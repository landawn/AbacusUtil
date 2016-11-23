package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ShortBiPredicate {

    public static final ShortBiPredicate ALWAYS_TRUE = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return true;
        }
    };

    public static final ShortBiPredicate ALWAYS_FALSE = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return false;
        }
    };

    public static final ShortBiPredicate IS_EQUAL = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return t == u;
        }
    };

    public static final ShortBiPredicate NOT_EQUAL = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return t != u;
        }
    };

    public static final ShortBiPredicate GREATER_THAN = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return t > u;
        }
    };

    public static final ShortBiPredicate GREATER_EQUAL = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return t >= u;
        }
    };

    public static final ShortBiPredicate LESS_THAN = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return t < u;
        }
    };

    public static final ShortBiPredicate LESS_EQUAL = new ShortBiPredicate() {
        @Override
        public boolean test(short t, short u) {
            return t <= u;
        }
    };

    boolean test(short t, short u);

    default ShortBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default ShortBiPredicate and(ShortBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default ShortBiPredicate or(ShortBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
