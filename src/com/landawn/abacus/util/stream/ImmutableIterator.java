package com.landawn.abacus.util.stream;

import com.landawn.abacus.util.ObjectList;

abstract class ImmutableIterator<T> implements java.util.Iterator<T> {
    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }

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

    public <A> A[] toArray(A[] a) {
        final ObjectList<A> list = new ObjectList<>(a);

        while (hasNext()) {
            list.add((A) next());
        }

        return list.array() == a ? a : list.trimToSize().array();
    }
}
