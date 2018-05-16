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

import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Try;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface Predicate<T> extends java.util.function.Predicate<T>, Try.Predicate<T, RuntimeException> {

    @Override
    boolean test(T value);

    @Override
    default Predicate<T> negate() {
        return (t) -> !test(t);
    }

    @Override
    default Predicate<T> and(java.util.function.Predicate<? super T> other) {
        N.checkArgNotNull(other);
        return (t) -> test(t) && other.test(t);
    }

    @Override
    default Predicate<T> or(java.util.function.Predicate<? super T> other) {
        N.checkArgNotNull(other);
        return (t) -> test(t) || other.test(t);
    }

    /**
     * Returns the specified instance
     * 
     * @param predicate
     * @return
     */
    static <T> Predicate<T> of(final Predicate<T> predicate) {
        N.checkArgNotNull(predicate);

        return predicate;
    }

    static <T> Predicate<T> alwaysTrue() {
        return Fn.alwaysTrue();
    }

    static <T> Predicate<T> alwaysFalse() {
        return Fn.alwaysFalse();
    }

    static <T> Predicate<T> isNull() {
        return Fn.isNull();
    }

    static <T> Predicate<T> notNull() {
        return Fn.notNull();
    }

    static <T> Predicate<T> equal(Object targetRef) {
        return Fn.equal(targetRef);
    }

    static <T> Predicate<T> notEqual(Object targetRef) {
        return Fn.notEqual(targetRef);
    }

    static <T extends Comparable<? super T>> Predicate<T> greaterThan(T targetRef) {
        return Fn.greaterThan(targetRef);
    }

    static <T extends Comparable<? super T>> Predicate<T> greaterEqual(T targetRef) {
        return Fn.greaterEqual(targetRef);
    }

    static <T extends Comparable<? super T>> Predicate<T> lessThan(T targetRef) {
        return Fn.lessThan(targetRef);
    }

    static <T extends Comparable<? super T>> Predicate<T> lessEqual(T targetRef) {
        return Fn.lessEqual(targetRef);
    }

    static <T extends Comparable<? super T>> Predicate<T> between(T minValue, T maxValue) {
        return value -> N.compare(value, minValue) > 0 && N.compare(value, maxValue) < 0;
    }
}
