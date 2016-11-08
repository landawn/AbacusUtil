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
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
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
    }

    /**
     * 
     * @param asyncExecutor
     */
    public AsyncExecutor(final ExecutorService executorService) {
        this(8, 300, TimeUnit.SECONDS);

        this.executorService = executorService;

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

    public CompletableFuture<Void> execute(final Runnable command) {
        final CompletableFuture<Void> future = new CompletableFuture<Void>(this, command, null);

        getExecutorService().execute(future);

        return future;
    }

    public List<CompletableFuture<Void>> execute(final Runnable... commands) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(commands.length);
        CompletableFuture<Void> future = null;

        for (int i = 0, len = commands.length; i < len; i++) {
            future = new CompletableFuture<Void>(this, commands[i], null);
            getExecutorService().execute(future);
            results.add(future);
        }

        return results;
    }

    public List<CompletableFuture<Void>> execute(final List<? extends Runnable> commands) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(commands.size());
        CompletableFuture<Void> future = null;

        for (Runnable cmd : commands) {
            future = new CompletableFuture<Void>(this, cmd, null);

            getExecutorService().execute(future);

            results.add(future);
        }

        return results;
    }

    public CompletableFuture<Void> execute(final Runnable action, final Function<Throwable, Boolean> ifRetry, final int retryTimes, final long retryInterval) {
        return execute(AutoRetry.of(action, ifRetry, retryTimes, retryInterval));
    }

    public <T> CompletableFuture<T> execute(final Callable<T> command) {
        final CompletableFuture<T> future = new CompletableFuture<T>(this, command);

        getExecutorService().execute(future);

        return future;
    }

    public <T> List<CompletableFuture<T>> execute(final Callable<T>... commands) {
        final List<CompletableFuture<T>> results = new ArrayList<>(commands.length);
        CompletableFuture<T> future = null;

        for (int i = 0, len = commands.length; i < len; i++) {
            future = new CompletableFuture<T>(this, commands[i]);
            getExecutorService().execute(future);
            results.add(future);
        }

        return results;
    }

    public <T> List<CompletableFuture<T>> execute(final Collection<? extends Callable<T>> commands) {
        final List<CompletableFuture<T>> results = new ArrayList<>(commands.size());
        CompletableFuture<T> future = null;

        for (Callable<T> cmd : commands) {
            future = new CompletableFuture<T>(this, cmd);

            getExecutorService().execute(future);

            results.add(future);
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

    public <T> CompletableFuture<T> execute(final Callable<T> action, final BiFunction<Throwable, ? super T, Boolean> ifRetry, final int retryTimes,
            final long retryInterval) {
        return execute(AutoRetry.of(action, ifRetry, retryTimes, retryInterval));
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
    static final class Async extends AsyncExecutor {

    }
}
