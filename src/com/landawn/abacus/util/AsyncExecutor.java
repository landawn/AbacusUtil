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
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

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

    private static final ScheduledExecutorService SCHEDULED_EXECUTOR = Executors.newScheduledThreadPool(N.CPU_CORES);

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

                logger.warn("Starting to shutdown task in AsyncExecutor");

                try {
                    executorService.shutdown();
                    executorService.awaitTermination(180, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    logger.error("Failed to commit the tasks in queue in ExecutorService before shutdown", e);
                } finally {
                    logger.warn("Completed to shutdown task in AsyncExecutor");
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
        return execute(new FutureTask<Void>(command, null));
    }

    public CompletableFuture<Void> execute(final Runnable action, final long delay) {
        final ExecutorService executor = getExecutorService();

        final Callable<CompletableFuture<Void>> scheduledAction = new Callable<CompletableFuture<Void>>() {
            @Override
            public CompletableFuture<Void> call() throws Exception {
                return execute(action);
            }
        };

        final ScheduledFuture<CompletableFuture<Void>> scheduledFuture = SCHEDULED_EXECUTOR.schedule(scheduledAction, delay, TimeUnit.MILLISECONDS);

        return new CompletableFuture<>(wrap(scheduledFuture), null, executor);
    }

    @SafeVarargs
    public final List<CompletableFuture<Void>> execute(final Runnable... commands) {
        if (N.isNullOrEmpty(commands)) {
            return new ArrayList<>();
        }

        final List<CompletableFuture<Void>> results = new ArrayList<>(commands.length);

        for (int i = 0, len = commands.length; i < len; i++) {
            results.add(execute(commands[i]));
        }

        return results;
    }

    public List<CompletableFuture<Void>> execute(final List<? extends Runnable> commands) {
        if (N.isNullOrEmpty(commands)) {
            return new ArrayList<>();
        }

        final List<CompletableFuture<Void>> results = new ArrayList<>(commands.size());

        for (Runnable cmd : commands) {
            results.add(execute(cmd));
        }

        return results;
    }

    public <T> CompletableFuture<T> execute(final Callable<T> command) {
        return execute(new FutureTask<>(command));
    }

    public <T> CompletableFuture<T> execute(final Callable<T> action, final long delay) {
        final ExecutorService executor = getExecutorService();

        final Callable<CompletableFuture<T>> scheduledAction = new Callable<CompletableFuture<T>>() {
            @Override
            public CompletableFuture<T> call() throws Exception {
                return execute(action);
            }
        };

        final ScheduledFuture<CompletableFuture<T>> scheduledFuture = SCHEDULED_EXECUTOR.schedule(scheduledAction, delay, TimeUnit.MILLISECONDS);

        return new CompletableFuture<>(wrap(scheduledFuture), null, executor);
    }

    @SafeVarargs
    public final <T> List<CompletableFuture<T>> execute(final Callable<T>... commands) {
        if (N.isNullOrEmpty(commands)) {
            return new ArrayList<>();
        }

        final List<CompletableFuture<T>> results = new ArrayList<>(commands.length);

        for (int i = 0, len = commands.length; i < len; i++) {
            results.add(execute(commands[i]));
        }

        return results;
    }

    public <T> List<CompletableFuture<T>> execute(final Collection<? extends Callable<T>> commands) {
        if (N.isNullOrEmpty(commands)) {
            return new ArrayList<>();
        }

        final List<CompletableFuture<T>> results = new ArrayList<>(commands.size());

        for (Callable<T> cmd : commands) {
            results.add(execute(cmd));
        }

        return results;
    }

    public CompletableFuture<Void> execute(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<? super Exception, Boolean> retryCondition) {
        return execute(new Runnable() {
            @Override
            public void run() {
                Retry.of(retryTimes, retryInterval, retryCondition).run(action);
            }
        });
    }

    public <T> CompletableFuture<T> execute(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, ? super Exception, Boolean> retryCondition) {
        return execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
                return retry.call(action);
            }
        });
    }

    private <T> CompletableFuture<T> execute(final FutureTask<T> futureTask) {
        final ExecutorService executor = getExecutorService();

        executor.execute(futureTask);

        return new CompletableFuture<>(futureTask, null, executor);
    }

    private static <T> Future<T> wrap(final ScheduledFuture<CompletableFuture<T>> scheduledFuture) {
        return new Future<T>() {
            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                if (scheduledFuture.cancel(mayInterruptIfRunning) && scheduledFuture.isDone()) {
                    try {
                        final CompletableFuture<T> resFuture = scheduledFuture.get();
                        return resFuture == null || resFuture.cancel(mayInterruptIfRunning);
                    } catch (Exception e) {
                        return false;
                    }
                }

                return false;
            }

            @Override
            public boolean isCancelled() {
                if (scheduledFuture.isCancelled() && scheduledFuture.isDone()) {
                    try {
                        final CompletableFuture<T> resFuture = scheduledFuture.get();
                        return resFuture == null || resFuture.isCancelled();
                    } catch (Exception e) {
                        return false;
                    }
                }

                return false;
            }

            @Override
            public boolean isDone() {
                if (scheduledFuture.isDone()) {
                    try {
                        final CompletableFuture<T> resFuture = scheduledFuture.get();
                        return resFuture == null || resFuture.isDone();
                    } catch (Exception e) {
                        return false;
                    }
                }

                return false;
            }

            @Override
            public T get() throws InterruptedException, ExecutionException {
                final CompletableFuture<T> resFuture = scheduledFuture.get();
                return resFuture == null ? null : resFuture.get();
            }

            @Override
            public T get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                final long beginTime = N.currentMillis();

                final CompletableFuture<T> resFuture = scheduledFuture.get(timeout, unit);

                final long remainingTimeout = unit.toMillis(timeout) - (N.currentMillis() - beginTime);

                return resFuture == null ? null : (remainingTimeout > 0 ? resFuture.get(remainingTimeout, TimeUnit.MILLISECONDS) : resFuture.get());
            }
        };
    }

    private ExecutorService getExecutorService() {
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
