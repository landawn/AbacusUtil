package com.landawn.abacus.util;

import java.util.Iterator;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.function.Function;

public class AutoRetryIterator<T> implements Iterator<T> {
    private static final Logger logger = LoggerFactory.getLogger(AutoRetryIterator.class);

    private final Iterator<T> iter;

    public AutoRetryIterator(final Iterator<T> iter, final int retryTimes, final long retryInterval, final Function<Throwable, Boolean> ifRetry) {
        this.iter = new Iterator<T>() {

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

    public static <T> AutoRetryIterator<T> of(final Iterator<T> iter, final int retryTimes, final long retryInterval,
            final Function<Throwable, Boolean> ifRetry) {
        return new AutoRetryIterator<>(iter, retryTimes, retryInterval, ifRetry);
    }

    @Override
    public boolean hasNext() {
        return iter.hasNext();
    }

    @Override
    public T next() {
        return iter.next();
    }

    @Override
    public void remove() {
        iter.remove();
    }
}
