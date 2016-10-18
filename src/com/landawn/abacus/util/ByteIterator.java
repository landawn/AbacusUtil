package com.landawn.abacus.util;

public interface ByteIterator {

    boolean hasNext();

    byte next();

    void remove();
}
