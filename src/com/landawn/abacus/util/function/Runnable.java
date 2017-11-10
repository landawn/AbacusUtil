package com.landawn.abacus.util.function;

import com.landawn.abacus.util.Try;

public interface Runnable extends java.lang.Runnable, Try.Runnable<RuntimeException> {

    @Override
    void run();
}
