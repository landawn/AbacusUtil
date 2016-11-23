package com.landawn.abacus.util.function;

/**
 * Refer to JDK API documentation at: <a href="https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html">https://docs.oracle.com/javase/8/docs/api/java/util/function/package-summary.html</a>
 */
public interface ToDoubleFunction<T> extends java.util.function.ToDoubleFunction<T> {

    public static final ToDoubleFunction<Double> UNBOX = new ToDoubleFunction<Double>() {
        @Override
        public double applyAsDouble(Double value) {
            return value == null ? 0 : value.doubleValue();
        }
    };

    @Override
    double applyAsDouble(T value);
}
