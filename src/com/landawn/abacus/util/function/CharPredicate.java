/*
 * Copyright (C) 2016 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.landawn.abacus.util.function;

import java.util.Objects;

import com.landawn.abacus.util.N;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface CharPredicate {

    static final CharPredicate ALWAYS_TRUE = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return true;
        }
    };

    static final CharPredicate ALWAYS_FALSE = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return false;
        }
    };

    static final CharPredicate IS_ZERO = new CharPredicate() {
        @Override
        public boolean test(char value) {
            return value == 0;
        }
    };

    static final CharPredicate NOT_ZERO = new CharPredicate() {
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
        Objects.requireNonNull(other);

        return (t) -> test(t) && other.test(t);
    }

    default CharPredicate or(CharPredicate other) {
        Objects.requireNonNull(other);

        return (t) -> test(t) || other.test(t);
    }

    static CharPredicate equal(char targetChar) {
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
