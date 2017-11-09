package com.landawn.abacus.util;

import com.landawn.abacus.util.FloatPair;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Stream;

public final class FloatPair {
    public final float _1;
    public final float _2;

    FloatPair() {
        this(0, 0);
    }

    FloatPair(float _1, float _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public static FloatPair of(float _1, float _2) {
        return new FloatPair(_1, _2);
    }

    public float min() {
        return N.min(_1, _2);
    }

    public float max() {
        return N.max(_1, _2);
    }

    public float sum() {
        return _1 + _2;
    }

    public double average() {
        return sum() / 2d;
    }

    public FloatPair reversed() {
        return new FloatPair(_2, _1);
    }

    public float[] toArray() {
        return new float[] { _1, _2 };
    }

    public FloatList toList() {
        return FloatList.of(_1, _2);
    }

    public void forEach(FloatConsumer comsumer) {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public void accept(Consumer<FloatPair> action) {
        action.accept(this);
    }

    public <U> U map(Function<FloatPair, U> mapper) {
        return mapper.apply(this);
    }

    public Optional<FloatPair> filter(final Predicate<FloatPair> predicate) {
        return predicate.test(this) ? Optional.of(this) : Optional.<FloatPair> empty();
    }

    public Stream<FloatPair> stream() {
        return Stream.of(this);
    }

    @Override
    public int hashCode() {
        return (int) (31 * _1 + this._2);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (!(obj instanceof FloatPair)) {
            return false;
        } else {
            FloatPair other = (FloatPair) obj;
            return this._1 == other._1 && this._2 == other._2;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + "]";
    }
}