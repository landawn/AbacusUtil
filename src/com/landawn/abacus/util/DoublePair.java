package com.landawn.abacus.util;

import com.landawn.abacus.util.stream.Stream;

public final class DoublePair {
    public final double _1;
    public final double _2;

    DoublePair() {
        this(0, 0);
    }

    DoublePair(double _1, double _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public static DoublePair of(double _1, double _2) {
        return new DoublePair(_1, _2);
    }

    public double min() {
        return N.min(_1, _2);
    }

    public double max() {
        return N.max(_1, _2);
    }

    public double sum() {
        return _1 + _2;
    }

    public double average() {
        return (0d + _1 + _2) / 2;
    }

    public DoublePair reversed() {
        return new DoublePair(_2, _1);
    }

    public double[] toArray() {
        return new double[] { _1, _2 };
    }

    public DoubleList toList() {
        return DoubleList.of(_1, _2);
    }

    public <E extends Exception> void forEach(Try.DoubleConsumer<E> comsumer) throws E {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public <E extends Exception> void accept(Try.Consumer<DoublePair, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(Try.Function<DoublePair, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<DoublePair> filter(final Try.Predicate<DoublePair, E> predicate) throws E {
        return predicate.test(this) ? Optional.of(this) : Optional.<DoublePair> empty();
    }

    public Stream<DoublePair> stream() {
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
        } else if (!(obj instanceof DoublePair)) {
            return false;
        } else {
            DoublePair other = (DoublePair) obj;
            return this._1 == other._1 && this._2 == other._2;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + "]";
    }
}