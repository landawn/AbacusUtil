package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface ToLongFunction<T> extends java.util.function.ToLongFunction<T> {

    public static final ToLongFunction<Long> UNBOX = new ToLongFunction<Long>() {
        @Override
        public long applyAsLong(Long value) {
            return value == null ? 0 : value.longValue();
        }
    };

    @Override
    long applyAsLong(T value);
}