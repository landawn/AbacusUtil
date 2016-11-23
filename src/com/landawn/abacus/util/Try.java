package com.landawn.abacus.util;

import java.util.concurrent.Callable;

public final class Try {
    static final Void VOID = null;

    private Try() {
        // singleton.
    }

    public static <T> OptionalNullable<T> of(Callable<T> call) {
        try {
            return OptionalNullable.of(call.call());
        } catch (Throwable e) {
            return OptionalNullable.empty();
        }
    }

    public static OptionalNullable<Void> of(Runnable run) {
        try {
            run.run();
            return OptionalNullable.of(VOID);
        } catch (Throwable e) {
            return OptionalNullable.empty();
        }
    }

    public static interface Runnable {
        void run() throws Exception;
    }
}
