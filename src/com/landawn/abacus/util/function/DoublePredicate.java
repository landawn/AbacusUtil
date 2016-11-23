package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface DoublePredicate extends java.util.function.DoublePredicate {

    public static final DoublePredicate ALWAYS_TRUE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return true;
        }
    };

    public static final DoublePredicate ALWAYS_FALSE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return false;
        }
    };

    public static final DoublePredicate IS_ZERO = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return value == 0;
        }
    };

    public static final DoublePredicate NOT_ZERO = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return value != 0;
        }
    };

    public static final DoublePredicate IS_POSITIVE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return value > 0;
        }
    };

    public static final DoublePredicate NOT_POSITIVE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return value <= 0;
        }
    };

    public static final DoublePredicate IS_NEGATIVE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return value < 0;
        }
    };

    public static final DoublePredicate NOT_NEGATIVE = new DoublePredicate() {
        @Override
        public boolean test(double value) {
            return value >= 0;
        }
    };

    @Override
    boolean test(double value);

    static DoublePredicate isEqual(double targetDouble) {
        return value -> value == targetDouble;
    }

    static DoublePredicate notEqual(double targetDouble) {
        return value -> value != targetDouble;
    }

    static DoublePredicate greaterThan(double targetDouble) {
        return value -> N.compare(value, targetDouble) > 0;
    }

    static DoublePredicate greaterEqual(double targetDouble) {
        return value -> N.compare(value, targetDouble) >= 0;
    }

    static DoublePredicate lessThan(double targetDouble) {
        return value -> N.compare(value, targetDouble) < 0;
    }

    static DoublePredicate lessEqual(double targetDouble) {
        return value -> N.compare(value, targetDouble) <= 0;
    }
}
