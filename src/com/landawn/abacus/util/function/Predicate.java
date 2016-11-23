package com.landawn.abacus.util.function;

import java.util.Objects;

import com.landawn.abacus.util.N;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
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
