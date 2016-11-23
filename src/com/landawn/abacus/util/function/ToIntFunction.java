package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface ToIntFunction<T> extends java.util.function.ToIntFunction<T> {

    public static final ToIntFunction<Integer> UNBOX = new ToIntFunction<Integer>() {
        @Override
        public int applyAsInt(Integer value) {
            return value == null ? 0 : value.intValue();
        }
    };

    @Override
    int applyAsInt(T value);
}
