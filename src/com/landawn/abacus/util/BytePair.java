package com.landawn.abacus.util;

import com.landawn.abacus.util.stream.Stream;

public class BytePair {
    public final byte _1;
    public final byte _2;

    BytePair() {
        this((byte) 0, (byte) 0);
    }

    BytePair(byte _1, byte _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public static BytePair of(byte _1, byte _2) {
        return new BytePair(_1, _2);
    }

    public static BytePair from(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return new BytePair();
        } else if (a.length == 1) {
            return new BytePair(a[0], (byte) 0);
        } else {
            return new BytePair(a[0], a[1]);
        }
    }

    public byte min() {
        return N.min(_1, _2);
    }

    public byte max() {
        return N.max(_1, _2);
    }

    public int sum() {
        return _1 + _2;
    }

    public double average() {
        return (0d + _1 + _2) / 2;
    }

    public BytePair reversed() {
        return new BytePair(_2, _1);
    }

    public byte[] toArray() {
        return new byte[] { _1, _2 };
    }

    public ByteList toList() {
        return ByteList.of(_1, _2);
    }

    public <E extends Exception> void forEach(Try.ByteConsumer<E> comsumer) throws E {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public <E extends Exception> void accept(Try.Consumer<BytePair, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(Try.Function<BytePair, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<BytePair> filter(final Try.Predicate<BytePair, E> predicate) throws E {
        return predicate.test(this) ? Optional.of(this) : Optional.<BytePair> empty();
    }

    public Stream<BytePair> stream() {
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
        } else if (!(obj instanceof BytePair)) {
            return false;
        } else {
            BytePair other = (BytePair) obj;
            return this._1 == other._1 && this._2 == other._2;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + "]";
    }

}
