package com.landawn.abacus.util;

import com.landawn.abacus.util.stream.CharStream;

public final class CharTriple {
    public final char _1;
    public final char _2;
    public final char _3;

    CharTriple() {
        this((char) 0, (char) 0, (char) 0);
    }

    CharTriple(char _1, char _2, char _3) {
        this._1 = _1;
        this._2 = _2;
        this._3 = _3;
    }

    public static CharTriple of(char _1, char _2, char _3) {
        return new CharTriple(_1, _2, _3);
    }

    public static CharTriple from(final char[] a) {
        if (N.isNullOrEmpty(a)) {
            return new CharTriple();
        } else if (a.length == 1) {
            return new CharTriple(a[0], (char) 0, (char) 0);
        } else if (a.length == 2) {
            return new CharTriple(a[0], a[1], (char) 0);
        } else {
            return new CharTriple(a[0], a[1], a[2]);
        }
    }

    public char min() {
        return N.min(_1, _2, _3);
    }

    public char max() {
        return N.max(_1, _2, _3);
    }

    public char median() {
        return N.median(_1, _2, _3);
    }

    public int sum() {
        return _1 + _2 + _3;
    }

    public double average() {
        return (0d + _1 + _2 + _3) / 3;
    }

    public CharTriple reversed() {
        return new CharTriple(_3, _2, _1);
    }

    public char[] toArray() {
        return new char[] { _1, _2, _3 };
    }

    public CharList toList() {
        return CharList.of(_1, _2, _3);
    }

    public <E extends Exception> void forEach(Try.CharConsumer<E> comsumer) throws E {
        comsumer.accept(this._1);
        comsumer.accept(this._2);
        comsumer.accept(this._3);
    }

    public <E extends Exception> void accept(Try.Consumer<CharTriple, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(Try.Function<CharTriple, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<CharTriple> filter(Try.Predicate<CharTriple, E> predicate) throws E {
        return predicate.test(this) ? Optional.of(this) : Optional.<CharTriple> empty();
    }

    public CharStream stream() {
        return CharStream.of(_1, _2, _3);
    }

    @Override
    public int hashCode() {
        return (31 * (31 * _1 + this._2)) + _3;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (!(obj instanceof CharTriple)) {
            return false;
        } else {
            CharTriple other = (CharTriple) obj;
            return this._1 == other._1 && this._2 == other._2 && this._3 == other._3;
        }
    }

    @Override
    public String toString() {
        return "[" + this._1 + ", " + this._2 + ", " + this._3 + "]";
    }
}