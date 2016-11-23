package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface LongPredicate extends java.util.function.LongPredicate {

    public static final LongPredicate ALWAYS_TRUE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return true;
        }
    };

    public static final LongPredicate ALWAYS_FALSE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return false;
        }
    };

    public static final LongPredicate IS_ZERO = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return value == 0;
        }
    };

    public static final LongPredicate NOT_ZERO = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return value != 0;
        }
    };

    public static final LongPredicate IS_POSITIVE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return value > 0;
        }
    };

    public static final LongPredicate NOT_POSITIVE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return value <= 0;
        }
    };

    public static final LongPredicate IS_NEGATIVE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return value < 0;
        }
    };

    public static final LongPredicate NOT_NEGATIVE = new LongPredicate() {
        @Override
        public boolean test(long value) {
            return value >= 0;
        }
    };

    @Override
    boolean test(long value);

    static LongPredicate isEqual(long targetLong) {
        return value -> value == targetLong;
    }

    static LongPredicate notEqual(long targetLong) {
        return value -> value != targetLong;
    }

    static LongPredicate greaterThan(long targetLong) {
        return value -> N.compare(value, targetLong) > 0;
    }

    static LongPredicate greaterEqual(long targetLong) {
        return value -> N.compare(value, targetLong) >= 0;
    }

    static LongPredicate lessThan(long targetLong) {
        return value -> N.compare(value, targetLong) < 0;
    }

    static LongPredicate lessEqual(long targetLong) {
        return value -> N.compare(value, targetLong) <= 0;
    }
}
