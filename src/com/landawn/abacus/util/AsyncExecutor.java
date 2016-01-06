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
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
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

    private final int corePoolSize;
    private final int maximumPoolSize;
    private final long keepAliveTime;
    private final TimeUnit unit;

    private volatile ExecutorService executorService;

    /**
     * Create an instance of with default values: corePoolSize = 8, maximumPoolSize = 64, keepAliveTime = 300, unit = TimeUnit.SECONDS.
     */
    public AsyncExecutor() {
        this(8, 64, 300, TimeUnit.SECONDS);
    }

    public AsyncExecutor(int corePoolSize, int maximumPoolSize, long keepAliveTime, TimeUnit unit) {
        this.corePoolSize = corePoolSize;
        this.maximumPoolSize = maximumPoolSize;
        this.keepAliveTime = keepAliveTime;
        this.unit = unit;
    }

    /**
     * 
     * @param asyncExecutor
     */
    public AsyncExecutor(final ExecutorService executorService) {
        this(8, 64, 300, TimeUnit.SECONDS);

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

    public Future<Void> execute(final Runnable command) {
        final FutureTask<Void> future = new FutureTask<Void>(command, null);

        getExecutorService().execute(future);

        return future;
    }

    public Future<Void>[] execute(final Runnable... commands) {
        final Future<Void>[] results = new Future[commands.length];

        for (int i = 0, len = commands.length; i < len; i++) {
            final FutureTask<Void> future = new FutureTask<Void>(commands[i], null);

            getExecutorService().execute(future);

            results[i] = future;
        }

        return results;
    }

    public List<Future<Void>> execute(final List<? extends Runnable> commands) {
        final List<Future<Void>> results = N.newArrayList(commands.size());

        for (Runnable cmd : commands) {
            final FutureTask<Void> future = new FutureTask<Void>(cmd, null);

            getExecutorService().execute(future);

            results.add(future);
        }

        return results;
    }

    public <T> Future<T> execute(final Callable<T> command) {
        final FutureTask<T> future = new FutureTask<T>(command);

        getExecutorService().execute(future);

        return future;
    }

    @SuppressWarnings("rawtypes")
    public <T> Future<T>[] execute(final Callable... commands) {
        final Future<T>[] results = new Future[commands.length];

        for (int i = 0, len = commands.length; i < len; i++) {
            final FutureTask<T> future = new FutureTask<T>(commands[i]);

            getExecutorService().execute(future);

            results[i] = future;
        }

        return results;
    }

    @SuppressWarnings("rawtypes")
    public <T> List<Future<T>> execute(final Collection<? extends Callable> commands) {
        final List<Future<T>> results = N.newArrayList(commands.size());

        for (Callable<T> cmd : commands) {
            final FutureTask<T> future = new FutureTask<T>(cmd);

            getExecutorService().execute(future);

            results.add(future);
        }

        return results;
    }

    public <T> Future<T> invoke(final Method method, final Object... args) {
        return invoke(null, method, args);
    }

    public <T> Future<T> invoke(final Object instance, final Method method, final Object... args) {
        final FutureTask<T> future = new FutureTask<T>(new Callable<T>() {
            @Override
            @SuppressWarnings("unchecked")
            public T call() throws Exception {
                return (T) method.invoke(instance, args);
            }
        });

        getExecutorService().execute(future);

        return future;
    }

    private ExecutorService getExecutorService() {
        if (executorService == null) {
            synchronized (this) {
                if (executorService == null) {
                    executorService = new ThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, new LinkedBlockingQueue<Runnable>());
                }
            }
        }

        return executorService;
    }
}
