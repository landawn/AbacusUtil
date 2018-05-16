package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Try;

public interface Callable<R> extends java.util.concurrent.Callable<R>, Try.Callable<R, RuntimeException> {

    @Override
    R call();

    /**
     * Returns the specified instance
     * 
     * @param callable
     * @return
     */
    public static <R> Callable<R> of(final Callable<R> callable) {
        N.checkArgNotNull(callable);

        return callable;
    }

    public static Callable<Void> create(Runnable cmd) {
        N.checkArgNotNull(cmd);

        return new Callable<Void>() {
            @Override
            public Void call() {
                cmd.run();
                return null;
            }
        };
    }
}
