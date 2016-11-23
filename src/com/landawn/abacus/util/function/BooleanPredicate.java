package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface BooleanPredicate {

    public static final BooleanPredicate ALWAYS_TRUE = new BooleanPredicate() {
        @Override
        public boolean test(boolean value) {
            return true;
        }
    };

    public static final BooleanPredicate ALWAYS_FALSE = new BooleanPredicate() {
        @Override
        public boolean test(boolean value) {
            return false;
        }
    };

    public static final BooleanPredicate IS_TRUE = new BooleanPredicate() {
        @Override
        public boolean test(boolean value) {
            return value == true;
        }
    };

    public static final BooleanPredicate IS_FALSE = new BooleanPredicate() {
        @Override
        public boolean test(boolean value) {
            return value == false;
        }
    };

    boolean test(boolean value);

    default BooleanPredicate negate() {
        return (t) -> !test(t);
    }

    default BooleanPredicate and(BooleanPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) && other.test(t);
    }

    default BooleanPredicate or(BooleanPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) || other.test(t);
    }
}
