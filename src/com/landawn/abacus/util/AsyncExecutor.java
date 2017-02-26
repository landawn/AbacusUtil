/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.Retry.Retry0;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class AsyncExecutor {
    private static final Logger logger = LoggerFactory.getLogger(AsyncExecutor.class);

    private final int maxConcurrentThreadNumber;
    private final long keepAliveTime;
    private final TimeUnit unit;

    private volatile ExecutorService executorService;

    /**
     * Create an instance of with default values: maxConcurrentThreadNumber = 8, keepAliveTime = 300, unit = TimeUnit.SECONDS.
     */
    public AsyncExecutor() {
        this(8, 300, TimeUnit.SECONDS);
    }

    public AsyncExecutor(int maxConcurrentThreadNumber, long keepAliveTime, TimeUnit unit) {
        this.maxConcurrentThreadNumber = maxConcurrentThreadNumber;
        this.keepAliveTime = keepAliveTime;
        this.unit = unit;

        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                if (executorService == null) {
                    return;
                }

                executorService.shutdown();

                try {
                    executorService.awaitTermination(180, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    if (logger.isWarnEnabled()) {
                        logger.warn("Failed to commit task in the queue in class", e);
                    }
                }
            }
        });
    }

    /**
     * 
     * @param asyncExecutor
     */
    public AsyncExecutor(final ExecutorService executorService) {
        this(8, 300, TimeUnit.SECONDS);

        this.executorService = executorService;
    }

    public CompletableFuture<Void> execute(final Runnable command) {
        final CompletableFuture<Void> future = new CompletableFuture<>(this, command, null);

        getExecutorService().execute(future);

        return future;
    }

    public List<CompletableFuture<Void>> execute(final Runnable... commands) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(commands.length);

        for (int i = 0, len = commands.length; i < len; i++) {
            results.add(execute(commands[i]));
        }

        return results;
    }

    public List<CompletableFuture<Void>> execute(final List<? extends Runnable> commands) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(commands.size());

        for (Runnable cmd : commands) {
            results.add(execute(cmd));
        }

        return results;
    }

    public <T> CompletableFuture<T> execute(final Callable<T> command) {
        final CompletableFuture<T> future = new CompletableFuture<>(this, command);

        getExecutorService().execute(future);

        return future;
    }

    public <T> List<CompletableFuture<T>> execute(final Callable<T>... commands) {
        final List<CompletableFuture<T>> results = new ArrayList<>(commands.length);

        for (int i = 0, len = commands.length; i < len; i++) {
            results.add(execute(commands[i]));
        }

        return results;
    }

    public <T> List<CompletableFuture<T>> execute(final Collection<? extends Callable<T>> commands) {
        final List<CompletableFuture<T>> results = new ArrayList<>(commands.size());

        for (Callable<T> cmd : commands) {
            results.add(execute(cmd));
        }

        return results;
    }

    //    public <T> CompletableFuture<T> invoke(final Method method, final Object... args) {
    //        return invoke(null, method, args);
    //    }
    //
    //    public <T> CompletableFuture<T> invoke(final Object instance, final Method method, final Object... args) {
    //        final CompletableFuture<T> future = new CompletableFuture<T>(this, new Callable<T>() {
    //            @Override
    //            @SuppressWarnings("unchecked")
    //            public T call() throws Exception {
    //                return (T) method.invoke(instance, args);
    //            }
    //        });
    //
    //        getExecutorService().execute(future);
    //
    //        return future;
    //    }

    public CompletableFuture<Void> execute(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<Throwable, Boolean> retryCondition) {
        return execute(new Runnable() {
            @Override
            public void run() {
                Retry.of(retryTimes, retryInterval, retryCondition).run(action);
            }
        });
    }

    public <T> CompletableFuture<T> execute(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, Throwable, Boolean> retryCondition) {
        return execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
                return retry.call(action);
            }
        });
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param a
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> firstResult(final CompletableFuture<? extends T>... a) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(a.length);

        for (CompletableFuture<? extends T> future : a) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = a.length; i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null) {
                    return Optional.of(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return Optional.empty();
        }

        return Optional.empty();
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> firstResult(final Collection<? extends CompletableFuture<? extends T>> c) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null) {
                    return Optional.of(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return Optional.empty();
        }

        return Optional.empty();
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @param maxTimeout
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> firstResult(final Collection<? extends CompletableFuture<? extends T>> c, final long maxTimeout) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final long endTime = N.currentMillis() + maxTimeout;

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                long timeout = endTime - N.currentMillis();

                if (timeout <= 0) {
                    return Optional.empty();
                }

                result = queue.poll(timeout, TimeUnit.MILLISECONDS);

                if (result != null) {
                    return Optional.of(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return Optional.empty();
        }

        return Optional.empty();
    }

    /**
     * Returns the first non-exception result or empty if fail to get result for all futures.
     * 
     * @param a
     * @return
     */
    public static <T> OptionalNullable<T> firstSuccessResult(final CompletableFuture<? extends T>... a) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(a.length);

        for (CompletableFuture<? extends T> future : a) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = a.length; i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    return OptionalNullable.of(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return OptionalNullable.empty();
        }

        return OptionalNullable.empty();
    }

    /**
     * Returns the first non-exception result or empty if fail to get result for all futures.
     * 
     * @param c
     * @return
     */
    public static <T> OptionalNullable<T> firstSuccessResult(final Collection<? extends CompletableFuture<? extends T>> c) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    return OptionalNullable.of(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return OptionalNullable.empty();
        }

        return OptionalNullable.empty();
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @param maxTimeout
     * @return
     */
    public static <T> OptionalNullable<T> firstSuccessResult(final Collection<? extends CompletableFuture<? extends T>> c, final long maxTimeout) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final long endTime = N.currentMillis() + maxTimeout;

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                long timeout = endTime - N.currentMillis();

                if (timeout <= 0) {
                    return OptionalNullable.empty();
                }

                result = queue.poll(timeout, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    return OptionalNullable.of(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return OptionalNullable.empty();
        }

        return OptionalNullable.empty();
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param a
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> lastResult(final CompletableFuture<? extends T>... a) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(a.length);

        for (CompletableFuture<? extends T> future : a) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final Holder<Pair<T, Throwable>> holder = new Holder<>((Pair<T, Throwable>) N.NULL_MASK);

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = a.length; i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null) {
                    holder.setValue(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return Optional.empty();
        }

        if (holder.value == N.NULL_MASK) {
            return Optional.empty();
        } else {
            return Optional.of(holder.value);
        }
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> lastResult(final Collection<? extends CompletableFuture<? extends T>> c) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final Holder<Pair<T, Throwable>> holder = new Holder<>((Pair<T, Throwable>) N.NULL_MASK);

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null) {
                    holder.setValue(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return Optional.empty();
        }

        if (holder.value == N.NULL_MASK) {
            return Optional.empty();
        } else {
            return Optional.of(holder.value);
        }
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @param maxTimeout
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> lastResult(final Collection<? extends CompletableFuture<? extends T>> c, final long maxTimeout) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final Holder<Pair<T, Throwable>> holder = new Holder<>((Pair<T, Throwable>) N.NULL_MASK);
        final long endTime = N.currentMillis() + maxTimeout;

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                long timeout = endTime - N.currentMillis();

                if (timeout <= 0) {
                    break;
                }

                result = queue.poll(timeout, TimeUnit.MILLISECONDS);

                if (result != null) {
                    holder.setValue(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return Optional.empty();
        }

        if (holder.value == N.NULL_MASK) {
            return Optional.empty();
        } else {
            return Optional.of(holder.value);
        }
    }

    /**
     * Returns the first non-exception result or empty if fail to get result for all futures.
     * 
     * @param a
     * @return
     */
    public static <T> OptionalNullable<T> lastSuccessResult(final CompletableFuture<? extends T>... a) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(a.length);

        for (CompletableFuture<? extends T> future : a) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final Holder<T> holder = new Holder<>((T) N.NULL_MASK);

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = a.length; i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    holder.setValue(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return OptionalNullable.empty();
        }

        if (holder.value == N.NULL_MASK) {
            return OptionalNullable.empty();
        } else {
            return OptionalNullable.of(holder.value);
        }
    }

    /**
     * Returns the first non-exception result or empty if fail to get result for all futures.
     * 
     * @param c
     * @return
     */
    public static <T> OptionalNullable<T> lastSuccessResult(final Collection<? extends CompletableFuture<? extends T>> c) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final Holder<T> holder = new Holder<>((T) N.NULL_MASK);

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    holder.setValue(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return OptionalNullable.empty();
        }

        if (holder.value == N.NULL_MASK) {
            return OptionalNullable.empty();
        } else {
            return OptionalNullable.of(holder.value);
        }
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @param maxTimeout
     * @return
     */
    public static <T> OptionalNullable<T> lastSuccessResult(final Collection<? extends CompletableFuture<? extends T>> c, final long maxTimeout) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        final Holder<T> holder = new Holder<>((T) N.NULL_MASK);
        final long endTime = N.currentMillis() + maxTimeout;

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                long timeout = endTime - N.currentMillis();

                if (timeout <= 0) {
                    break;
                }

                result = queue.poll(timeout, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    holder.setValue(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            return OptionalNullable.empty();
        }

        if (holder.value == N.NULL_MASK) {
            return OptionalNullable.empty();
        } else {
            return OptionalNullable.of(holder.value);
        }
    }

    public static <T> BlockingQueue<Pair<T, Throwable>> concat(final CompletableFuture<? extends T>... a) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(a.length);

        for (CompletableFuture<? extends T> future : a) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        return queue;
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @return
     */
    public static <T> BlockingQueue<Pair<T, Throwable>> concat(final Collection<? extends CompletableFuture<? extends T>> c) {
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> future : c) {
            ((CompletableFuture<T>) future).callback(new Callback<T>() {
                @Override
                public void on(Throwable e, T result) {
                    queue.offer(Pair.of(result, e));
                }
            });
        }

        return queue;
    }

    ExecutorService getExecutorService() {
        if (executorService == null) {
            synchronized (this) {
                if (executorService == null) {
                    final ThreadPoolExecutor threadPoolExecutor = new ThreadPoolExecutor(maxConcurrentThreadNumber, maxConcurrentThreadNumber, keepAliveTime,
                            unit, new LinkedBlockingQueue<Runnable>());
                    threadPoolExecutor.allowCoreThreadTimeOut(true);
                    executorService = threadPoolExecutor;
                }
            }
        }

        return executorService;
    }

    /**
     * Short name for AsyncExecutor
     */
    @Beta
    static final class Asyn extends AsyncExecutor {

    }
}
