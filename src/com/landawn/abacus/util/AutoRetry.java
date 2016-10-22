package com.landawn.abacus.util;

import java.util.Iterator;
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

        return new Runnable() {
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

        return new Callable<T>() {
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

    public static <T> Iterator<T> of(final Iterator<T> iter, final int retryTimes, final long retryInterval, final Function<Throwable, Boolean> ifRetry) {
        return new Iterator<T>() {
            @Override
            public boolean hasNext() {
                try {
                    return iter.hasNext();
                } catch (Throwable e) {
                    logger.error("hasNext", e);

                    int retriedTimes = 0;
                    Throwable throwable = e;

                    while (retriedTimes++ < retryTimes && ifRetry.apply(throwable)) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            return iter.hasNext();
                        } catch (Throwable e2) {
                            logger.error("hasNext", e2);

                            throwable = e2;
                        }
                    }

                    throw N.toRuntimeException(throwable);
                }
            }

            @Override
            public T next() {
                try {
                    return iter.next();
                } catch (Throwable e) {
                    logger.error("next", e);

                    int retriedTimes = 0;
                    Throwable throwable = e;

                    while (retriedTimes++ < retryTimes && ifRetry.apply(throwable)) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            return iter.next();
                        } catch (Throwable e2) {
                            logger.error("next", e2);

                            throwable = e2;
                        }
                    }

                    throw N.toRuntimeException(throwable);
                }
            }

            @Override
            public void remove() {
                try {
                    iter.remove();
                } catch (Throwable e) {
                    logger.error("remove", e);

                    int retriedTimes = 0;
                    Throwable throwable = e;

                    while (retriedTimes++ < retryTimes && ifRetry.apply(throwable)) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            iter.remove();
                        } catch (Throwable e2) {
                            logger.error("remove", e2);

                            throwable = e2;
                        }
                    }

                    throw N.toRuntimeException(throwable);
                }
            }
        };
    }
}
