package com.landawn.abacus.util.stream;

public interface SkippableIterator {

    void skip(long n);

    long count();
}
