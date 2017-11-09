package com.landawn.abacus.util;

import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Stream;

public final class CharPair {
    public final char _1;
    public final char _2;

    CharPair() {
        this((char) 0, (char) 0);
    }

    CharPair(char _1, char _2) {
        this._1 = _1;
        this._2 = _2;
    }

    public static CharPair of(char _1, char _2) {
        return new CharPair(_1, _2);
    }

    public char min() {
        return N.min(_1, _2);
    }

    public char max() {
        return N.max(_1, _2);
    }

    public int sum() {
        return _1 + _2;
    }

    public double average() {
        return sum() / 2d;
    }

    public CharPair reversed() {
        return new CharPair(_2, _1);
    }

    public char[] toArray() {
        return new char[] { _1, _2 };
    }

    public CharList toList() {
        return CharList.of(_1, _2);
    }

    public void forEach(CharConsumer comsumer) {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public void accept(Consumer<CharPair> action) {
        action.accept(this);
    }

    public <U> U map(Function<CharPair, U> mapper) {
        return mapper.apply(this);
    }

    public Optional<CharPair> filter(final Predicate<CharPair> predicate) {
        return predicate.test(this) ? Optional.of(this) : Optional.<CharPair> empty();
    }

    public Stream<CharPair> stream() {
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
        } else if (!(obj instanceof CharPair)) {
            return false;
        } else {
            CharPair other = (CharPair) obj;
            return this._1 == other._1 && this._2 == other._2;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + "]";
    }
}