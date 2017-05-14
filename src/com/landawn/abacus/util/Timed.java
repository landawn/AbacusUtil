package com.landawn.abacus.util;

public class Timed<T> {
    private final T value;
    private final long timeInMillis;

    private Timed(T value, long timeInMillis) {
        this.value = value;
        this.timeInMillis = timeInMillis;
    }

    public static <T> Timed<T> of(T value, long timeInMillis) {
        return new Timed<>(value, timeInMillis);
    }

    public T value() {
        return value;
    }

    public long time() {
        return timeInMillis;
    }
}
