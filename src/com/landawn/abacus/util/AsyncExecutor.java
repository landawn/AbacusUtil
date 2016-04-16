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

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class AsyncExecutor {
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
                    logger.warn("Failed to commit task in the queue in class", e);
                }
            }
        });
    }

    public CallbackFuture<Void> execute(final Runnable command) {
        final CallbackFuture<Void> future = new CallbackFuture<Void>(command, null);

        getExecutorService().execute(future);

        return future;
    }

    public CallbackFuture<Void>[] execute(final Runnable... commands) {
        final CallbackFuture<Void>[] results = new CallbackFuture[commands.length];

        for (int i = 0, len = commands.length; i < len; i++) {
            results[i] = new CallbackFuture<Void>(commands[i], null);

            getExecutorService().execute(results[i]);
        }

        return results;
    }

    public List<CallbackFuture<Void>> execute(final List<? extends Runnable> commands) {
        final List<CallbackFuture<Void>> results = N.newArrayList(commands.size());
        CallbackFuture<Void> future = null;

        for (Runnable cmd : commands) {
            future = new CallbackFuture<Void>(cmd, null);

            getExecutorService().execute(future);

            results.add(future);
        }

        return results;
    }

    public <T> CallbackFuture<T> execute(final Callable<T> command) {
        final CallbackFuture<T> future = new CallbackFuture<T>(command);

        getExecutorService().execute(future);

        return future;
    }

    public <T> CallbackFuture<T>[] execute(final Callable<T>... commands) {
        final CallbackFuture<T>[] results = new CallbackFuture[commands.length];

        for (int i = 0, len = commands.length; i < len; i++) {
            results[i] = new CallbackFuture<T>(commands[i]);

            getExecutorService().execute(results[i]);
        }

        return results;
    }

    public <T> List<CallbackFuture<T>> execute(final Collection<? extends Callable<T>> commands) {
        final List<CallbackFuture<T>> results = N.newArrayList(commands.size());
        CallbackFuture<T> future = null;

        for (Callable<T> cmd : commands) {
            future = new CallbackFuture<T>(cmd);

            getExecutorService().execute(future);

            results.add(future);
        }

        return results;
    }

    public <T> CallbackFuture<T> invoke(final Method method, final Object... args) {
        return invoke(null, method, args);
    }

    public <T> CallbackFuture<T> invoke(final Object instance, final Method method, final Object... args) {
        final CallbackFuture<T> future = new CallbackFuture<T>(new Callable<T>() {
            @Override
            @SuppressWarnings("unchecked")
            public T call() throws Exception {
                return (T) method.invoke(instance, args);
            }
        });

        getExecutorService().execute(future);

        return future;
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
}
