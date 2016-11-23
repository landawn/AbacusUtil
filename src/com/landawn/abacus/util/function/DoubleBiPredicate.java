package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface DoubleBiPredicate {

    public static final DoubleBiPredicate ALWAYS_TRUE = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return true;
        }
    };

    public static final DoubleBiPredicate ALWAYS_FALSE = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return false;
        }
    };
    public static final DoubleBiPredicate IS_EQUAL = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return Double.compare(t, u) == 0;
        }
    };

    public static final DoubleBiPredicate NOT_EQUAL = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return Double.compare(t, u) != 0;
        }
    };

    public static final DoubleBiPredicate GREATER_THAN = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return Double.compare(t, u) > 0;
        }
    };

    public static final DoubleBiPredicate GREATER_EQUAL = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return Double.compare(t, u) >= 0;
        }
    };

    public static final DoubleBiPredicate LESS_THAN = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return Double.compare(t, u) < 0;
        }
    };

    public static final DoubleBiPredicate LESS_EQUAL = new DoubleBiPredicate() {
        @Override
        public boolean test(double t, double u) {
            return Double.compare(t, u) <= 0;
        }
    };

    boolean test(double t, double u);

    default DoubleBiPredicate negate() {
        return (t, u) -> !test(t, u);
    }

    default DoubleBiPredicate and(DoubleBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) && other.test(t, u);
    }

    default DoubleBiPredicate or(DoubleBiPredicate other) {
        N.requireNonNull(other);

        return (t, u) -> test(t, u) || other.test(t, u);
    }
}
