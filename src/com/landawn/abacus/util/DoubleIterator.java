package com.landawn.abacus.util;

public interface DoubleIterator {

    boolean hasNext();

    double next();

    void remove();
}
