package com.landawn.abacus.util;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Stream;

public final class IntPair {
    public final int _1;
    public final int _2;

    IntPair() {
        this(0, 0);
    }

    IntPair(int _1, int _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public static IntPair of(int _1, int _2) {
        return new IntPair(_1, _2);
    }

    public int min() {
        return N.min(_1, _2);
    }

    public int max() {
        return N.max(_1, _2);
    }

    public int sum() {
        return _1 + _2;
    }

    public double average() {
        return sum() / 2d;
    }

    public IntPair reversed() {
        return new IntPair(_2, _1);
    }

    public int[] toArray() {
        return new int[] { _1, _2 };
    }

    public IntList toList() {
        return IntList.of(_1, _2);
    }

    public void forEach(IntConsumer comsumer) {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public void accept(Consumer<IntPair> action) {
        action.accept(this);
    }

    public <U> U map(Function<IntPair, U> mapper) {
        return mapper.apply(this);
    }

    public Optional<IntPair> filter(final Predicate<IntPair> predicate) {
        return predicate.test(this) ? Optional.of(this) : Optional.<IntPair> empty();
    }

    public Stream<IntPair> stream() {
        return Stream.of(this);
    }

    @Override
    public int hashCode() {
        return 31 * _1 + this._2;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (!(obj instanceof IntPair)) {
            return false;
        } else {
            IntPair other = (IntPair) obj;
            return this._1 == other._1 && this._2 == other._2;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + "]";
    }
}