package com.landawn.abacus.util;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Stream;

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
        return sum() / 2d;
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

    public void forEach(LongConsumer comsumer) {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public void accept(Consumer<LongPair> action) {
        action.accept(this);
    }

    public <U> U map(Function<LongPair, U> mapper) {
        return mapper.apply(this);
    }

    public Optional<LongPair> filter(final Predicate<LongPair> predicate) {
        return predicate.test(this) ? Optional.of(this) : Optional.<LongPair> empty();
    }

    public Stream<LongPair> stream() {
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