package com.landawn.abacus.util.function;

public interface ObjBooleanConsumer<T> {

    void accept(T t, boolean value);
}