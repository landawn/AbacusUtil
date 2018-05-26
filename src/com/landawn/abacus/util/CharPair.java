package com.landawn.abacus.util;

import com.landawn.abacus.util.stream.CharStream;

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

    public static CharPair from(final char[] a) {
        if (N.isNullOrEmpty(a)) {
            return new CharPair();
        } else if (a.length == 1) {
            return new CharPair(a[0], (char) 0);
        } else {
            return new CharPair(a[0], a[1]);
        }
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
        return (0d + _1 + _2) / 2;
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

    public <E extends Exception> void forEach(Try.CharConsumer<E> comsumer) throws E {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
    }

    public <E extends Exception> void accept(Try.Consumer<CharPair, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(Try.Function<CharPair, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<CharPair> filter(Try.Predicate<CharPair, E> predicate) throws E {
        return predicate.test(this) ? Optional.of(this) : Optional.<CharPair> empty();
    }

    public CharStream stream() {
        return CharStream.of(_1, _2);
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