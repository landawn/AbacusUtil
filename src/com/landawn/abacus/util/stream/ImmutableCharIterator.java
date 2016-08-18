package com.landawn.abacus.util.stream;

import com.landawn.abacus.util.CharList;

abstract class ImmutableCharIterator {
    public abstract boolean hasNext();

    public abstract char next();

    public long count() {
        long result = 0;

        while (hasNext()) {
            next();
            result++;
        }

        return result;
    }

    public void skip(long n) {
        while (n > 0 && hasNext()) {
            next();
            n--;
        }
    }

    public char[] toArray() {
        final CharList list = new CharList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
