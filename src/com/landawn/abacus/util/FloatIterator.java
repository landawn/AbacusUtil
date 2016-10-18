package com.landawn.abacus.util;

public interface FloatIterator {

    boolean hasNext();

    float next();

    void remove();
}
