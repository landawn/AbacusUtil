package com.landawn.abacus.util.stream;

import com.landawn.abacus.util.LongList;

abstract class ImmutableLongIterator {
    public abstract boolean hasNext();

    public abstract long next();

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

    public long[] toArray() {
        final LongList list = new LongList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
