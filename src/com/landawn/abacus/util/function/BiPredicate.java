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

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface BiPredicate<T, U> extends java.util.function.BiPredicate<T, U> {

    @SuppressWarnings("rawtypes")
    public static final BiPredicate ALWAYS_TRUE = new BiPredicate() {
        @Override
        public boolean test(Object t, Object u) {
            return true;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate ALWAYS_FALSE = new BiPredicate() {
        @Override
        public boolean test(Object t, Object u) {
            return false;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate IS_EQUAL = new BiPredicate() {
        @Override
        public boolean test(Object t, Object u) {
            return N.equals(t, u);
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate NOT_EQUAL = new BiPredicate() {
        @Override
        public boolean test(Object t, Object u) {
            return !N.equals(t, u);
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate<? extends Comparable, ? extends Comparable> GREATER_THAN = new BiPredicate<Comparable, Comparable>() {
        @Override
        public boolean test(Comparable t, Comparable u) {
            return N.compare(t, u) > 0;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate<? extends Comparable, ? extends Comparable> GREATER_EQUAL = new BiPredicate<Comparable, Comparable>() {
        @Override
        public boolean test(Comparable t, Comparable u) {
            return N.compare(t, u) >= 0;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate<? extends Comparable, ? extends Comparable> LESS_THAN = new BiPredicate<Comparable, Comparable>() {
        @Override
        public boolean test(Comparable t, Comparable u) {
            return N.compare(t, u) < 0;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final BiPredicate<? extends Comparable, ? extends Comparable> LESS_EQUAL = new BiPredicate<Comparable, Comparable>() {
        @Override
        public boolean test(Comparable t, Comparable u) {
            return N.compare(t, u) <= 0;
        }
    };

    @Override
    boolean test(T t, U u);

    static <T> BiPredicate<T, T> isEqual(T t, T u) {
        return IS_EQUAL;
    }

    static <T> BiPredicate<T, T> notEqual(T t, T u) {
        return NOT_EQUAL;
    }

    static <T extends Comparable<? super T>> BiPredicate<T, T> greaterThan(T t, T u) {
        return (BiPredicate<T, T>) GREATER_THAN;
    }

    static <T extends Comparable<? super T>> BiPredicate<T, T> greaterEqual(T t, T u) {
        return (BiPredicate<T, T>) GREATER_EQUAL;
    }

    static <T extends Comparable<? super T>> BiPredicate<T, T> lessThan(T t, T u) {
        return (BiPredicate<T, T>) LESS_THAN;
    }

    static <T extends Comparable<? super T>> BiPredicate<T, T> lessEqual(T t, T u) {
        return (BiPredicate<T, T>) LESS_EQUAL;
    }
}
