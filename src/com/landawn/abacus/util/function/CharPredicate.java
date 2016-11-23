package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;

public interface CharPredicate {

    public static final CharPredicate ALWAYS_TRUE = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return true;
        }
    };

    public static final CharPredicate ALWAYS_FALSE = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return false;
        }
    };

    public static final CharPredicate IS_ZERO = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return value == 0;
        }
    };

    public static final CharPredicate NOT_ZERO = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return value != 0;
        }
    };

    boolean test(char value);

    default CharPredicate negate() {
        return (t) -> !test(t);
    }

    default CharPredicate and(CharPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) && other.test(t);
    }

    default CharPredicate or(CharPredicate other) {
        N.requireNonNull(other);

        return (t) -> test(t) || other.test(t);
    }

    static CharPredicate isEqual(char targetChar) {
        return value -> value == targetChar;
    }

    static CharPredicate notEqual(char targetChar) {
        return value -> value != targetChar;
    }

    static CharPredicate greaterThan(char targetChar) {
        return value -> N.compare(value, targetChar) > 0;
    }

    static CharPredicate greaterEqual(char targetChar) {
        return value -> N.compare(value, targetChar) >= 0;
    }

    static CharPredicate lessThan(char targetChar) {
        return value -> N.compare(value, targetChar) < 0;
    }

    static CharPredicate lessEqual(char targetChar) {
        return value -> N.compare(value, targetChar) <= 0;
    }
}
