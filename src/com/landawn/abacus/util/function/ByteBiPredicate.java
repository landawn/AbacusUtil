package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ByteBiPredicate {

    public static final ByteBiPredicate ALWAYS_TRUE = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return true;
        }
    };

    public static final ByteBiPredicate ALWAYS_FALSE = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return false;
        }
    };

    public static final ByteBiPredicate IS_EQUAL = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return t == u;
        }
    };

    public static final ByteBiPredicate NOT_EQUAL = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return t != u;
        }
    };

    public static final ByteBiPredicate GREATER_THAN = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return t > u;
        }
    };

    public static final ByteBiPredicate GREATER_EQUAL = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return t >= u;
        }
    };

    public static final ByteBiPredicate LESS_THAN = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return t < u;
        }
    };

    public static final ByteBiPredicate LESS_EQUAL = new ByteBiPredicate() {
        @Override
        public boolean test(byte t, byte u) {
            return t <= u;
        }
    };

    boolean test(byte t, byte u);

    default ByteBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default ByteBiPredicate and(ByteBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default ByteBiPredicate or(ByteBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
