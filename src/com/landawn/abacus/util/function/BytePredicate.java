/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */
package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface BytePredicate {

    public static final BytePredicate ALWAYS_TRUE = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return true;
        }
    };

    public static final BytePredicate ALWAYS_FALSE = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return false;
        }
    };

    public static final BytePredicate IS_ZERO = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return value == 0;
        }
    };

    public static final BytePredicate NOT_ZERO = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return value != 0;
        }
    };

    public static final BytePredicate IS_POSITIVE = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return value > 0;
        }
    };

    public static final BytePredicate NOT_POSITIVE = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return value <= 0;
        }
    };

    public static final BytePredicate IS_NEGATIVE = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return value < 0;
        }
    };

    public static final BytePredicate NOT_NEGATIVE = new BytePredicate() {
        @Override
        public boolean test(byte value) {
            return value >= 0;
        }
    };

    boolean test(byte value);

    default BytePredicate negate() {
        return (t) -> !test(t);
    }

    default BytePredicate and(BytePredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) && other.test(t);
    }

    default BytePredicate or(BytePredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) || other.test(t);
    }

    static BytePredicate isEqual(byte targetByte) {
        return value -> value == targetByte;
    }

    static BytePredicate notEqual(byte targetByte) {
        return value -> value != targetByte;
    }

    static BytePredicate greaterThan(byte targetByte) {
        return value -> N.compare(value, targetByte) > 0;
    }

    static BytePredicate greaterEqual(byte targetByte) {
        return value -> N.compare(value, targetByte) >= 0;
    }

    static BytePredicate lessThan(byte targetByte) {
        return value -> N.compare(value, targetByte) < 0;
    }

    static BytePredicate lessEqual(byte targetByte) {
        return value -> N.compare(value, targetByte) <= 0;
    }
}
