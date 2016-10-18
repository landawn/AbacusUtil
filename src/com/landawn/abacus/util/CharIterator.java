package com.landawn.abacus.util;

public interface CharIterator {

    boolean hasNext();

    char next();

    void remove();
}
