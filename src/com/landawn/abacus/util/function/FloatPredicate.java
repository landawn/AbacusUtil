package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

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

    public static final FloatPredicate IS_ZERO = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return value == 0;
        }
    };

    public static final FloatPredicate NOT_ZERO = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return value != 0;
        }
    };

    public static final FloatPredicate IS_POSITIVE = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return value > 0;
        }
    };

    public static final FloatPredicate NOT_POSITIVE = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return value <= 0;
        }
    };

    public static final FloatPredicate IS_NEGATIVE = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return value < 0;
        }
    };

    public static final FloatPredicate NOT_NEGATIVE = new FloatPredicate() {
        @Override
        public boolean test(float value) {
            return value >= 0;
        }
    };

    boolean test(float value);

    default FloatPredicate negate() {
        return (t) -> !test(t);
    }

    default FloatPredicate and(FloatPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) && other.test(t);
    }

    default FloatPredicate or(FloatPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) || other.test(t);
    }

    static FloatPredicate isEqual(float targetFloat) {
        return value -> value == targetFloat;
    }

    static FloatPredicate notEqual(float targetFloat) {
        return value -> value != targetFloat;
    }

    static FloatPredicate greaterThan(float targetFloat) {
        return value -> N.compare(value, targetFloat) > 0;
    }

    static FloatPredicate greaterEqual(float targetFloat) {
        return value -> N.compare(value, targetFloat) >= 0;
    }

    static FloatPredicate lessThan(float targetFloat) {
        return value -> N.compare(value, targetFloat) < 0;
    }

    static FloatPredicate lessEqual(float targetFloat) {
        return value -> N.compare(value, targetFloat) <= 0;
    }
}
