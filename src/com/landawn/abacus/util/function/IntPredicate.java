package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface IntPredicate extends java.util.function.IntPredicate {

    public static final IntPredicate ALWAYS_TRUE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return true;
        }
    };

    public static final IntPredicate ALWAYS_FALSE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return false;
        }
    };

    public static final IntPredicate IS_ZERO = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return value == 0;
        }
    };

    public static final IntPredicate NOT_ZERO = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return value != 0;
        }
    };

    public static final IntPredicate IS_POSITIVE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return value > 0;
        }
    };

    public static final IntPredicate NOT_POSITIVE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return value <= 0;
        }
    };

    public static final IntPredicate IS_NEGATIVE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return value < 0;
        }
    };

    public static final IntPredicate NOT_NEGATIVE = new IntPredicate() {
        @Override
        public boolean test(int value) {
            return value >= 0;
        }
    };

    @Override
    boolean test(int value);

    static IntPredicate isEqual(int targetInt) {
        return value -> value == targetInt;
    }

    static IntPredicate notEqual(int targetInt) {
        return value -> value != targetInt;
    }

    static IntPredicate greaterThan(int targetInt) {
        return value -> N.compare(value, targetInt) > 0;
    }

    static IntPredicate greaterEqual(int targetInt) {
        return value -> N.compare(value, targetInt) >= 0;
    }

    static IntPredicate lessThan(int targetInt) {
        return value -> N.compare(value, targetInt) < 0;
    }

    static IntPredicate lessEqual(int targetInt) {
        return value -> N.compare(value, targetInt) <= 0;
    }
}
