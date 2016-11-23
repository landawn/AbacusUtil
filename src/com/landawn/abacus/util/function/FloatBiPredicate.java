package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface FloatBiPredicate {

    public static final FloatBiPredicate ALWAYS_TRUE = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return true;
        }
    };

    public static final FloatBiPredicate ALWAYS_FALSE = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return false;
        }
    };

    public static final FloatBiPredicate IS_EQUAL = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return Float.compare(t, u) == 0;
        }
    };

    public static final FloatBiPredicate NOT_EQUAL = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return Float.compare(t, u) != 0;
        }
    };

    public static final FloatBiPredicate GREATER_THAN = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return Float.compare(t, u) > 0;
        }
    };

    public static final FloatBiPredicate GREATER_EQUAL = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return Float.compare(t, u) >= 0;
        }
    };

    public static final FloatBiPredicate LESS_THAN = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return Float.compare(t, u) < 0;
        }
    };

    public static final FloatBiPredicate LESS_EQUAL = new FloatBiPredicate() {
        @Override
        public boolean test(float t, float u) {
            return Float.compare(t, u) <= 0;
        }
    };

    boolean test(float t, float u);

    default FloatBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default FloatBiPredicate and(FloatBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default FloatBiPredicate or(FloatBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
