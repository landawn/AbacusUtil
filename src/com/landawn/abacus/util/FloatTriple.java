package com.landawn.abacus.util;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Stream;

public final class FloatTriple {
    public final float _1;
    public final float _2;
    public final float _3;

    FloatTriple() {
        this(0, 0, 0);
    }

    FloatTriple(float _1, float _2, float _3) {
        this._1 = _1;
        this._2 = _2;
        this._3 = _3;
    }

    public static FloatTriple of(float _1, float _2, float _3) {
        return new FloatTriple(_1, _2, _3);
    }

    public float min() {
        return N.min(_1, _2, _3);
    }

    public float max() {
        return N.max(_1, _2, _3);
    }

    public float median() {
        return N.median(_1, _2, _3);
    }

    public float sum() {
        return _1 + _2 + _3;
    }

    public double average() {
        return sum() / 3d;
    }

    public FloatTriple reversed() {
        return new FloatTriple(_3, _2, _1);
    }

    public float[] toArray() {
        return new float[] { _1, _2, _3 };
    }

    public FloatList toList() {
        return FloatList.of(_1, _2, _3);
    }

    public void forEach(FloatConsumer comsumer) {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
        comsumer.accept(this._3);
    }

    public void accept(Consumer<FloatTriple> action) {
        action.accept(this);
    }

    public <U> U map(Function<FloatTriple, U> mapper) {
        return mapper.apply(this);
    }

    public Optional<FloatTriple> filter(final Predicate<FloatTriple> predicate) {
        return predicate.test(this) ? Optional.of(this) : Optional.<FloatTriple> empty();
    }

    public Stream<FloatTriple> stream() {
        return Stream.of(this);
    }

    @Override
    public int hashCode() {
        return (int) ((31 * (31 * _1 + this._2)) + _3);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (!(obj instanceof FloatTriple)) {
            return false;
        } else {
            FloatTriple other = (FloatTriple) obj;
            return this._1 == other._1 && this._2 == other._2 && this._3 == other._3;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + ", " + this._3 + "]";
    }
}