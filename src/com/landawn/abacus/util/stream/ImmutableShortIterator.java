package com.landawn.abacus.util.stream;

import com.landawn.abacus.util.ShortList;

abstract class ImmutableShortIterator {
    public abstract boolean hasNext();

    public abstract short next();

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

    public short[] toArray() {
        final ShortList list = new ShortList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
