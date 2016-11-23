package com.landawn.abacus.util.function;

public interface ObjByteConsumer<T> {

    void accept(T t, byte value);
}