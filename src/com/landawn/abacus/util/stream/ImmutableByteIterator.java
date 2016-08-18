package com.landawn.abacus.util.stream;

import com.landawn.abacus.util.ByteList;

abstract class ImmutableByteIterator {
    public abstract boolean hasNext();

    public abstract byte next();

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

    public byte[] toArray() {
        final ByteList list = new ByteList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
