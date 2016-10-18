package com.landawn.abacus.util;

public interface LongIterator {

    boolean hasNext();

    long next();

    void remove();
}
