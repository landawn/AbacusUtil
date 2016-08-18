package com.landawn.abacus.util.stream;

import com.landawn.abacus.util.DoubleList;

abstract class ImmutableDoubleIterator {
    public abstract boolean hasNext();

    public abstract double next();

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

    public double[] toArray() {
        final DoubleList list = new DoubleList();

        while (hasNext()) {
            list.add(next());
        }

        return list.trimToSize().array();
    }
}
