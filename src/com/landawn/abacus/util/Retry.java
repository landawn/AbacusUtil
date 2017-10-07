/*
 * Copyright (C) 2016 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.landawn.abacus.util;

import java.util.Iterator;
import java.util.concurrent.Callable;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.Predicate;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Retry<T> {
    private static final Logger logger = LoggerFactory.getLogger(Retry.class);

    private final int retryTimes;
    private final long retryInterval;
    private final Predicate<? super Exception> retryCondition;
    private final BiPredicate<? super T, ? super Exception> retryCondition2;

    Retry(final int retryTimes, final long retryInterval, final Predicate<? super Exception> retryCondition,
            final BiPredicate<? super T, ? super Exception> retryCondition2) {

        this.retryTimes = retryTimes;
        this.retryInterval = retryInterval;
        this.retryCondition = retryCondition;
        this.retryCondition2 = retryCondition2;
    }

    public static Retry<Void> of(final int retryTimes, final long retryInterval, final Predicate<? super Exception> retryCondition) {
        if (retryTimes < 0 || retryInterval < 0) {
            throw new IllegalArgumentException("'retryTimes' and 'retryInterval' can't be negative");
        }

        N.requireNonNull(retryCondition);

        return new Retry<Void>(retryTimes, retryInterval, retryCondition, null);
    }

    public static <R> Retry<R> of(final int retryTimes, final long retryInterval, final BiPredicate<? super R, ? super Exception> retryCondition) {
        if (retryTimes < 0 || retryInterval < 0) {
            throw new IllegalArgumentException("'retryTimes' and 'retryInterval' can't be negative");
        }

        N.requireNonNull(retryCondition);

        return new Retry<R>(retryTimes, retryInterval, null, retryCondition);
    }

    public void run(final Runnable cmd) {
        try {
            cmd.run();
        } catch (RuntimeException e) {
            logger.error("AutoRetry", e);

            int retriedTimes = 0;
            RuntimeException throwable = e;

            while (retriedTimes++ < retryTimes
                    && ((retryCondition != null && retryCondition.test(throwable)) || (retryCondition2 != null && retryCondition2.test(null, throwable)))) {
                try {
                    if (retryInterval > 0) {
                        N.sleep(retryInterval);
                    }

                    cmd.run();
                    return;
                } catch (RuntimeException e2) {
                    logger.error("AutoRetry", e2);

                    throwable = e2;
                }
            }

            throw throwable;
        }
    }

    public T call(final Try.Callable<T, RuntimeException> callable) {
        T result = null;
        int retriedTimes = 0;

        try {
            result = callable.call();

            while (retriedTimes++ < retryTimes && (retryCondition2 != null && retryCondition2.test(result, null))) {
                if (retryInterval > 0) {
                    N.sleep(retryInterval);
                }

                result = callable.call();

                if (retryCondition2 == null || retryCondition2.test(result, null) == false) {
                    return result;
                }
            }
        } catch (RuntimeException e) {
            logger.error("AutoRetry", e);

            RuntimeException throwable = e;

            while (retriedTimes++ < retryTimes
                    && ((retryCondition != null && retryCondition.test(throwable)) || (retryCondition2 != null && retryCondition2.test(null, throwable)))) {
                try {
                    if (retryInterval > 0) {
                        N.sleep(retryInterval);
                    }

                    result = callable.call();

                    if (retryCondition2 == null || retryCondition2.test(result, null) == false) {
                        return result;
                    }
                } catch (RuntimeException e2) {
                    logger.error("AutoRetry", e2);

                    throwable = e2;
                }
            }

            throw throwable;
        }

        if (retryTimes > 0 && (retryCondition2 != null && retryCondition2.test(result, null))) {
            throw new RuntimeException("Still failed after retried " + retryTimes + " times for result: " + N.toString(result));
        }

        return result;
    }

    public <E> Iterator<E> iterator(final Iterator<E> iter) {
        return new Iterator<E>() {
            @Override
            public boolean hasNext() {
                try {
                    return iter.hasNext();
                } catch (RuntimeException e) {
                    logger.error("hasNext", e);

                    int retriedTimes = 0;
                    RuntimeException throwable = e;

                    while (retriedTimes++ < retryTimes && ((retryCondition != null && retryCondition.test(throwable))
                            || (retryCondition2 != null && retryCondition2.test(null, throwable)))) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            return iter.hasNext();
                        } catch (RuntimeException e2) {
                            logger.error("hasNext", e2);

                            throwable = e2;
                        }
                    }

                    throw throwable;
                }
            }

            @Override
            public E next() {
                try {
                    return iter.next();
                } catch (RuntimeException e) {
                    logger.error("next", e);

                    int retriedTimes = 0;
                    RuntimeException throwable = e;

                    while (retriedTimes++ < retryTimes && ((retryCondition != null && retryCondition.test(throwable))
                            || (retryCondition2 != null && retryCondition2.test(null, throwable)))) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            return iter.next();
                        } catch (RuntimeException e2) {
                            logger.error("next", e2);

                            throwable = e2;
                        }
                    }

                    throw throwable;
                }
            }

            @Override
            public void remove() {
                try {
                    iter.remove();
                } catch (RuntimeException e) {
                    logger.error("remove", e);

                    int retriedTimes = 0;
                    RuntimeException throwable = e;

                    while (retriedTimes++ < retryTimes && ((retryCondition != null && retryCondition.test(throwable))
                            || (retryCondition2 != null && retryCondition2.test(null, throwable)))) {
                        try {
                            if (retryInterval > 0) {
                                N.sleep(retryInterval);
                            }

                            iter.remove();
                        } catch (RuntimeException e2) {
                            logger.error("remove", e2);

                            throwable = e2;
                        }
                    }

                    throw throwable;
                }
            }
        };
    }

    public static final class Retry0<T> {
        private final int retryTimes;
        private final long retryInterval;
        private final Predicate<? super Exception> retryCondition;
        private final BiPredicate<? super T, ? super Exception> retryCondition2;

        Retry0(final int retryTimes, final long retryInterval, final Predicate<? super Exception> retryCondition,
                final BiPredicate<? super T, ? super Exception> retryCondition2) {

            this.retryTimes = retryTimes;
            this.retryInterval = retryInterval;
            this.retryCondition = retryCondition;
            this.retryCondition2 = retryCondition2;
        }

        public static Retry0<Void> of(final int retryTimes, final long retryInterval, final Predicate<? super Exception> retryCondition) {
            if (retryTimes < 0 || retryInterval < 0) {
                throw new IllegalArgumentException("'retryTimes' and 'retryInterval' can't be negative");
            }

            N.requireNonNull(retryCondition);

            return new Retry0<Void>(retryTimes, retryInterval, retryCondition, null);
        }

        public static <R> Retry0<R> of(final int retryTimes, final long retryInterval, final BiPredicate<? super R, ? super Exception> retryCondition) {
            if (retryTimes < 0 || retryInterval < 0) {
                throw new IllegalArgumentException("'retryTimes' and 'retryInterval' can't be negative");
            }

            N.requireNonNull(retryCondition);

            return new Retry0<R>(retryTimes, retryInterval, null, retryCondition);
        }

        public void run(final Try.Runnable<? extends Exception> cmd) throws Exception {
            try {
                cmd.run();
            } catch (Exception e) {
                logger.error("AutoRetry", e);

                int retriedTimes = 0;
                Exception throwable = e;

                while (retriedTimes++ < retryTimes
                        && ((retryCondition != null && retryCondition.test(throwable)) || (retryCondition2 != null && retryCondition2.test(null, throwable)))) {
                    try {
                        if (retryInterval > 0) {
                            N.sleep(retryInterval);
                        }

                        cmd.run();
                        return;
                    } catch (Exception e2) {
                        logger.error("AutoRetry", e2);

                        throwable = e2;
                    }
                }

                throw throwable;
            }
        }

        public T call(final Callable<T> callable) throws Exception {
            T result = null;
            int retriedTimes = 0;

            try {
                result = callable.call();

                while (retriedTimes++ < retryTimes && (retryCondition2 != null && retryCondition2.test(result, null))) {
                    if (retryInterval > 0) {
                        N.sleep(retryInterval);
                    }

                    result = callable.call();

                    if (retryCondition2 == null || retryCondition2.test(result, null) == false) {
                        return result;
                    }
                }
            } catch (Exception e) {
                logger.error("AutoRetry", e);

                Exception throwable = e;

                while (retriedTimes++ < retryTimes
                        && ((retryCondition != null && retryCondition.test(throwable)) || (retryCondition2 != null && retryCondition2.test(null, throwable)))) {
                    try {
                        if (retryInterval > 0) {
                            N.sleep(retryInterval);
                        }

                        result = callable.call();

                        if (retryCondition2 == null || retryCondition2.test(result, null) == false) {
                            return result;
                        }
                    } catch (Exception e2) {
                        logger.error("AutoRetry", e2);

                        throwable = e2;
                    }
                }

                throw throwable;
            }

            if (retryTimes > 0 && (retryCondition2 != null && retryCondition2.test(result, null))) {
                throw new RuntimeException("Still failed after retried " + retryTimes + " times for result: " + N.toString(result));
            }

            return result;
        }
    }
}
