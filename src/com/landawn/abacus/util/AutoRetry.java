package com.landawn.abacus.util;

import java.util.concurrent.Callable;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;

public abstract class AutoRetry {
    private static final Logger logger = LoggerFactory.getLogger(AutoRetry.class);

    private AutoRetry() {
        // singleton.
    }

    static Runnable of(final Runnable runnable, final Function<Throwable, Boolean> ifRetry) {
        return of(runnable, ifRetry, 1, 0);
    }

    /**
     * 
     * @param runnable
     * @param ifRetry
     * @param retryTimes Default value is 1.
     * @param retryInterval Default value is 0.
     * @return
     */
    public static Runnable of(final Runnable runnable, final Function<Throwable, Boolean> ifRetry, final int retryTimes, final long retryInterval) {
        if (retryTimes < 0 || retryInterval < 0) {
            throw new IllegalArgumentException("'retryTimes' and 'retryInterval' can't be negative");
        }

        return new AutoRetry1() {
            @Override
            public void run() {
                try {
                    runnable.run();
                } catch (Throwable e) {
                    logger.error("AutoRetry", e);

                    int retriedTimes = 0;
                    Throwable throwable = e;

                    while (retriedTimes++ < retryTimes && ifRetry.apply(throwable)) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            runnable.run();
                            return;
                        } catch (Throwable e2) {
                            logger.error("AutoRetry", e2);

                            throwable = e2;
                        }
                    }

                    throw N.toRuntimeException(throwable);
                }
            }

        };
    }

    static <T> Callable<T> of(final Callable<T> callable, final BiFunction<Throwable, ? super T, Boolean> ifRetry) {
        return of(callable, ifRetry, 1, 0);
    }

    /**
     * 
     * @param callable
     * @param ifRetry
     * @param retryTimes Default value is 1.
     * @param retryInterval Default value is 0.
     * @return
     */
    public static <T> Callable<T> of(final Callable<T> callable, final BiFunction<Throwable, ? super T, Boolean> ifRetry, final int retryTimes,
            final long retryInterval) {
        if (retryTimes < 0 || retryInterval < 0) {
            throw new IllegalArgumentException("'retryTimes' and 'retryInterval' can't be negative");
        }

        return new AutoRetry2<T>() {
            @Override
            public T call() {
                T result = null;
                int retriedTimes = 0;

                try {
                    result = callable.call();

                    while (retriedTimes++ < retryTimes && (ifRetry.apply(null, result))) {
                        if (retryInterval > 0) {
                            N.sleep(retryInterval);
                        }

                        result = callable.call();

                        if (ifRetry.apply(null, result) == false) {
                            return result;
                        }
                    }
                } catch (Throwable e) {
                    logger.error("AutoRetry", e);

                    Throwable throwable = e;

                    while (retriedTimes++ < retryTimes && ifRetry.apply(throwable, result)) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            result = callable.call();

                            if (ifRetry.apply(null, result) == false) {
                                return result;
                            }
                        } catch (Throwable e2) {
                            logger.error("AutoRetry", e2);

                            throwable = e2;
                        }
                    }

                    throw N.toRuntimeException(throwable);
                }

                if (retryTimes > 0 && ifRetry.apply(null, result)) {
                    throw new RuntimeException("Still failed after retried " + retryTimes + " times for result: " + N.toString(result));
                }

                return result;
            }
        };
    }

    static void execute(final Runnable runnable, final Function<Throwable, Boolean> ifRetry, final int retryTimes, final long retryInterval) {
        of(runnable, ifRetry, retryTimes, retryInterval).run();
    }

    static <T> T execute(final Callable<T> callable, final BiFunction<Throwable, ? super T, Boolean> ifRetry, final int retryTimes, final long retryInterval) {
        try {
            return of(callable, ifRetry, retryTimes, retryInterval).call();
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    static abstract class AutoRetry1 extends AutoRetry implements Runnable {

    }

    static abstract class AutoRetry2<T> extends AutoRetry implements Callable<T> {

    }
}
