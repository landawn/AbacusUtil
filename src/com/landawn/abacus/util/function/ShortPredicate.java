package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface ShortPredicate {

    public static final ShortPredicate ALWAYS_TRUE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return true;
        }
    };

    public static final ShortPredicate ALWAYS_FALSE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return false;
        }
    };

    public static final ShortPredicate IS_ZERO = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return value == 0;
        }
    };

    public static final ShortPredicate NOT_ZERO = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return value != 0;
        }
    };

    public static final ShortPredicate IS_POSITIVE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return value > 0;
        }
    };

    public static final ShortPredicate NOT_POSITIVE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return value <= 0;
        }
    };

    public static final ShortPredicate IS_NEGATIVE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return value < 0;
        }
    };

    public static final ShortPredicate NOT_NEGATIVE = new ShortPredicate() {
        @Override
        public boolean test(short value) {
            return value >= 0;
        }
    };

    boolean test(short value);

    default ShortPredicate negate() {
        return (t) -> !test(t);
    }

    default ShortPredicate and(ShortPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) && other.test(t);
    }

    default ShortPredicate or(ShortPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) || other.test(t);
    }

    static ShortPredicate isEqual(short targetShort) {
        return value -> value == targetShort;
    }

    static ShortPredicate notEqual(short targetShort) {
        return value -> value != targetShort;
    }

    static ShortPredicate greaterThan(short targetShort) {
        return value -> N.compare(value, targetShort) > 0;
    }

    static ShortPredicate greaterEqual(short targetShort) {
        return value -> N.compare(value, targetShort) >= 0;
    }

    static ShortPredicate lessThan(short targetShort) {
        return value -> N.compare(value, targetShort) < 0;
    }

    static ShortPredicate lessEqual(short targetShort) {
        return value -> N.compare(value, targetShort) <= 0;
    }
}
