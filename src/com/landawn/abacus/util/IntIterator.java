package com.landawn.abacus.util;

public interface IntIterator {

    boolean hasNext();

    int next();

    void remove();
}
