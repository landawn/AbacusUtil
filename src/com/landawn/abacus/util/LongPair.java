package com.landawn.abacus.util;

import com.landawn.abacus.util.stream.LongStream;

public final class LongPair {
    public final long _1;
    public final long _2;

    LongPair() {
        this(0, 0);
    }

    LongPair(long _1, long _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public static LongPair of(long _1, long _2) {
        return new LongPair(_1, _2);
    }

    public static LongPair from(final long[] a) {
        if (N.isNullOrEmpty(a)) {
            return new LongPair();
        } else if (a.length == 1) {
            return new LongPair(a[0], 0);
        } else {
            return new LongPair(a[0], a[1]);
        }
    }

    public long min() {
        return N.min(_1, _2);
    }

    public long max() {
        return N.max(_1, _2);
    }

    public long sum() {
        return _1 + _2;
    }

    public double average() {
        return (0d + _1 + _2) / 2;
    }

    public LongPair reversed() {
        return new LongPair(_2, _1);
    }

    public long[] toArray() {
        return new long[] { _1, _2 };
    }

    public LongList toList() {
        return LongList.of(_1, _2);
    }

    public <E extends Exception> void forEach(Try.LongConsumer<E> comsumer) throws E {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public <E extends Exception> void accept(Try.Consumer<LongPair, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(Try.Function<LongPair, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<LongPair> filter(final Try.Predicate<LongPair, E> predicate) throws E {
        return predicate.test(this) ? Optional.of(this) : Optional.<LongPair> empty();
    }

    public LongStream stream() {
        return LongStream.of(_1, _2);
    }

    @Override
    public int hashCode() {
        return (int) (31 * _1 + this._2);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (!(obj instanceof LongPair)) {
            return false;
        } else {
            LongPair other = (LongPair) obj;
            return this._1 == other._1 && this._2 == other._2;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + "]";
    }
}