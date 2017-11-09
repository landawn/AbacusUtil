package com.landawn.abacus.util;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Stream;

public final class DoubleTriple {
    public final double _1;
    public final double _2;
    public final double _3;

    DoubleTriple() {
        this(0, 0, 0);
    }

    DoubleTriple(double _1, double _2, double _3) {
        this._1 = _1;
        this._2 = _2;
        this._3 = _3;
    }

    public static DoubleTriple of(double _1, double _2, double _3) {
        return new DoubleTriple(_1, _2, _3);
    }

    public double min() {
        return N.min(_1, _2, _3);
    }

    public double max() {
        return N.max(_1, _2, _3);
    }

    public double median() {
        return N.median(_1, _2, _3);
    }

    public double sum() {
        return _1 + _2 + _3;
    }

    public double average() {
        return sum() / 3d;
    }

    public DoubleTriple reversed() {
        return new DoubleTriple(_3, _2, _1);
    }

    public double[] toArray() {
        return new double[] { _1, _2, _3 };
    }

    public DoubleList toList() {
        return DoubleList.of(_1, _2, _3);
    }

    public void forEach(DoubleConsumer comsumer) {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
        comsumer.accept(this._3);
    }

    public void accept(Consumer<DoubleTriple> action) {
        action.accept(this);
    }

    public <U> U map(Function<DoubleTriple, U> mapper) {
        return mapper.apply(this);
    }

    public Optional<DoubleTriple> filter(final Predicate<DoubleTriple> predicate) {
        return predicate.test(this) ? Optional.of(this) : Optional.<DoubleTriple> empty();
    }

    public Stream<DoubleTriple> stream() {
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
        } else if (!(obj instanceof DoubleTriple)) {
            return false;
        } else {
            DoubleTriple other = (DoubleTriple) obj;
            return this._1 == other._1 && this._2 == other._2 && this._3 == other._3;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + ", " + this._3 + "]";
    }
}