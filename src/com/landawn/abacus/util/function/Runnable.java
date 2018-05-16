package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Try;

public interface Runnable extends java.lang.Runnable, Try.Runnable<RuntimeException> {

    @Override
    void run();

    /**
     * Returns the specified instance
     * 
     * @param runnable
     * @return
     */
    public static Runnable of(final Runnable runnable) {
        N.checkArgNotNull(runnable);

        return runnable;
    }

    public static <R> Runnable create(final Callable<R> callable) {
        N.checkArgNotNull(callable);

        return new Runnable() {
            @Override
            public void run() {
                callable.call();
            }
        };
    }
}
