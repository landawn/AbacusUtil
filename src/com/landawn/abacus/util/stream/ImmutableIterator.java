package com.landawn.abacus.util.stream;

import java.util.Iterator;

abstract class ImmutableIterator<T> implements Iterator<T> {

    @Override
    public void remove() {
        throw new UnsupportedOperationException();
    }
}
