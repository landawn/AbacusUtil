package com.landawn.abacus.util.function;

import com.landawn.abacus.util.Try;

public interface Callable<R> extends java.util.concurrent.Callable<R>, Try.Callable<R, RuntimeException> {

    @Override
    R call();
}
