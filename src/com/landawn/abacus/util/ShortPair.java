package com.landawn.abacus.util;

import com.landawn.abacus.util.stream.Stream;

public class ShortPair {
    public final short _1;
    public final short _2;

    ShortPair() {
        this((short) 0, (short) 0);
    }

    ShortPair(short _1, short _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public static ShortPair of(short _1, short _2) {
        return new ShortPair(_1, _2);
    }

    public short min() {
        return N.min(_1, _2);
    }

    public short max() {
        return N.max(_1, _2);
    }

    public int sum() {
        return _1 + _2;
    }

    public double average() {
        return (0d + _1 + _2) / 2;
    }

    public ShortPair reversed() {
        return new ShortPair(_2, _1);
    }

    public short[] toArray() {
        return new short[] { _1, _2 };
    }

    public ShortList toList() {
        return ShortList.of(_1, _2);
    }

    public <E extends Exception> void forEach(Try.ShortConsumer<E> comsumer) throws E {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public <E extends Exception> void accept(Try.Consumer<ShortPair, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(Try.Function<ShortPair, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<ShortPair> filter(final Try.Predicate<ShortPair, E> predicate) throws E {
        return predicate.test(this) ? Optional.of(this) : Optional.<ShortPair> empty();
    }

    public Stream<ShortPair> stream() {
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
        } else if (!(obj instanceof ShortPair)) {
            return false;
        } else {
            ShortPair other = (ShortPair) obj;
            return this._1 == other._1 && this._2 == other._2;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + "]";
    }
}
