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
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface Predicate<T> extends java.util.function.Predicate<T> {

    @SuppressWarnings("rawtypes")
    public static final Predicate ALWAYS_TRUE = new Predicate() {
        @Override
        public boolean test(Object value) {
            return true;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Predicate ALWAYS_FALSE = new Predicate() {
        @Override
        public boolean test(Object value) {
            return false;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Predicate IS_NULL = new Predicate() {
        @Override
        public boolean test(Object value) {
            return value == null;
        }
    };

    @SuppressWarnings("rawtypes")
    public static final Predicate NOT_NULL = new Predicate() {
        @Override
        public boolean test(Object value) {
            return value != null;
        }
    };

    @Override
    boolean test(T value);

    static <T> Predicate<T> isEqual(Object targetRef) {
        return (null == targetRef) ? Objects::isNull : object -> targetRef.equals(object);
    }

    static <T> Predicate<T> notEqual(Object targetRef) {
        return (null == targetRef) ? Objects::nonNull : object -> !targetRef.equals(object);
    }

    static <T extends Comparable<? super T>> Predicate<T> greaterThan(T targetRef) {
        return object -> N.compare(object, targetRef) > 0;
    }

    static <T extends Comparable<? super T>> Predicate<T> greaterEqual(T targetRef) {
        return object -> N.compare(object, targetRef) >= 0;
    }

    static <T extends Comparable<? super T>> Predicate<T> lessThan(T targetRef) {
        return object -> N.compare(object, targetRef) < 0;
    }

    static <T extends Comparable<? super T>> Predicate<T> lessEqual(T targetRef) {
        return object -> N.compare(object, targetRef) <= 0;
    }
}
