package com.landawn.abacus.util.stream;

import com.landawn.abacus.util.FloatList;

abstract class ImmutableFloatIterator {
    public abstract boolean hasNext();

    public abstract float next();

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

    public float[] toArray() {
        final FloatList list = new FloatList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
