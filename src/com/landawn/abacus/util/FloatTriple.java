package com.landawn.abacus.util;

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

    public static FloatTriple from(final float[] a) {
        if (N.isNullOrEmpty(a)) {
            return new FloatTriple();
        } else if (a.length == 1) {
            return new FloatTriple(a[0], 0, 0);
        } else if (a.length == 2) {
            return new FloatTriple(a[0], a[1], 0);
        } else {
            return new FloatTriple(a[0], a[1], a[2]);
        }
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
        return N.sum(_1, _2, _3);
    }

    public double average() {
        return N.average(_1, _2, _3);
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

    public <E extends Exception> void forEach(Try.FloatConsumer<E> comsumer) throws E {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
        comsumer.accept(this._3);
    }

    public <E extends Exception> void accept(Try.Consumer<FloatTriple, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(Try.Function<FloatTriple, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<FloatTriple> filter(final Try.Predicate<FloatTriple, E> predicate) throws E {
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