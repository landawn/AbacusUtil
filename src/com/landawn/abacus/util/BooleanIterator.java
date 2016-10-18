package com.landawn.abacus.util;

public interface BooleanIterator {

    boolean hasNext();

    boolean next();

    void remove();
}
