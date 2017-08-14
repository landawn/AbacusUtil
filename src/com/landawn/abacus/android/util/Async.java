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

package com.landawn.abacus.android.util;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Retry;
import com.landawn.abacus.util.Retry.Retry0;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;

import android.os.AsyncTask;
import android.os.Handler;
import android.os.Looper;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class Async {

    static final ScheduledExecutorService SCHEDULED_EXECUTOR = Executors.newScheduledThreadPool(N.CPU_CORES);
    static final _UIExecutor _UI_EXECUTOR = new _UIExecutor();

    public static final Executor SERIAL_EXECUTOR = AsyncTask.SERIAL_EXECUTOR;
    public static final Executor TP_EXECUTOR = AsyncTask.THREAD_POOL_EXECUTOR;
    public static final Executor UI_EXECUTOR = _UI_EXECUTOR;

    private Async() {
        // Singleton
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    static CompletableFuture<Void> execute(final Runnable action) {
        return execute(new FutureTask<Void>(action, null), SERIAL_EXECUTOR);
    }

    static CompletableFuture<Void> execute(final Runnable action, final long delay) {
        final Callable<CompletableFuture<Void>> scheduledAction = new Callable<CompletableFuture<Void>>() {
            @Override
            public CompletableFuture<Void> call() throws Exception {
                return Async.execute(action);
            }
        };

        final ScheduledFuture<CompletableFuture<Void>> scheduledFuture = SCHEDULED_EXECUTOR.schedule(scheduledAction, delay, TimeUnit.MILLISECONDS);

        return new CompletableFuture<>(wrap(scheduledFuture), null, SERIAL_EXECUTOR);
    }

    /**
     * 
     * @param action
     * @param retryTimes
     * @param retryInterval
     * @param retryCondition
     * @return
     */
    static CompletableFuture<Void> execute(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<? super Exception, Boolean> retryCondition) {
        return execute(new Runnable() {
            @Override
            public void run() {
                Retry.of(retryTimes, retryInterval, retryCondition).run(action);
            }
        });
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    static <T> CompletableFuture<T> execute(final Callable<T> action) {
        return execute(new FutureTask<>(action), SERIAL_EXECUTOR);
    }

    static <T> CompletableFuture<T> execute(final Callable<T> action, final long delay) {
        final Callable<CompletableFuture<T>> scheduledAction = new Callable<CompletableFuture<T>>() {
            @Override
            public CompletableFuture<T> call() throws Exception {
                return Async.execute(action);
            }
        };

        final ScheduledFuture<CompletableFuture<T>> scheduledFuture = SCHEDULED_EXECUTOR.schedule(scheduledAction, delay, TimeUnit.MILLISECONDS);

        return new CompletableFuture<>(wrap(scheduledFuture), null, SERIAL_EXECUTOR);
    }

    /**
     * 
     * @param action
     * @param retryTimes
     * @param retryInterval
     * @param retryCondition
     * @return
     */
    static <T> CompletableFuture<T> execute(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, ? super Exception, Boolean> retryCondition) {
        return execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
                return retry.call(action);
            }
        });
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    static CompletableFuture<Void> executeWithThreadPool(final Runnable action) {
        return execute(new FutureTask<Void>(action, null), TP_EXECUTOR);
    }

    static CompletableFuture<Void> executeWithThreadPool(final Runnable action, final long delay) {
        final Callable<CompletableFuture<Void>> scheduledAction = new Callable<CompletableFuture<Void>>() {
            @Override
            public CompletableFuture<Void> call() throws Exception {
                return Async.executeWithThreadPool(action);
            }
        };

        final ScheduledFuture<CompletableFuture<Void>> scheduledFuture = SCHEDULED_EXECUTOR.schedule(scheduledAction, delay, TimeUnit.MILLISECONDS);

        return new CompletableFuture<>(wrap(scheduledFuture), null, TP_EXECUTOR);
    }

    /**
     * 
     * @param action
     * @param retryTimes
     * @param retryInterval
     * @param retryCondition
     * @return
     */
    static CompletableFuture<Void> executeWithThreadPool(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<? super Exception, Boolean> retryCondition) {
        return executeWithThreadPool(new Runnable() {
            @Override
            public void run() {
                Retry.of(retryTimes, retryInterval, retryCondition).run(action);
            }
        });
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    static <T> CompletableFuture<T> executeWithThreadPool(final Callable<T> action) {
        return execute(new FutureTask<>(action), TP_EXECUTOR);
    }

    static <T> CompletableFuture<T> executeWithThreadPool(final Callable<T> action, final long delay) {
        final Callable<CompletableFuture<T>> scheduledAction = new Callable<CompletableFuture<T>>() {
            @Override
            public CompletableFuture<T> call() throws Exception {
                return Async.executeWithThreadPool(action);
            }
        };

        final ScheduledFuture<CompletableFuture<T>> scheduledFuture = SCHEDULED_EXECUTOR.schedule(scheduledAction, delay, TimeUnit.MILLISECONDS);

        return new CompletableFuture<>(wrap(scheduledFuture), null, TP_EXECUTOR);
    }

    /**
     * 
     * @param action
     * @param retryTimes
     * @param retryInterval
     * @param retryCondition
     * @return
     */
    static <T> CompletableFuture<T> executeWithThreadPool(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, ? super Exception, Boolean> retryCondition) {
        return executeWithThreadPool(new Callable<T>() {
            @Override
            public T call() throws Exception {
                final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
                return retry.call(action);
            }
        });
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @return
     */
    static CompletableFuture<Void> executeOnUiThread(final Runnable action) {
        return executeOnUiThread(action, 0);
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    static CompletableFuture<Void> executeOnUiThread(final Runnable action, final long delay) {
        return execute(new FutureTask<Void>(action, null), _UI_EXECUTOR, delay);
    }

    /**
     * 
     * @param action
     * @param retryTimes
     * @param retryInterval
     * @param retryCondition
     * @return
     */
    static CompletableFuture<Void> executeOnUiThread(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<? super Exception, Boolean> retryCondition) {
        return executeOnUiThread(new Runnable() {
            @Override
            public void run() {
                Retry.of(retryTimes, retryInterval, retryCondition).run(action);
            }
        });
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @return
     */
    static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action) {
        return executeOnUiThread(action, 0);
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action, final long delay) {
        return execute(new FutureTask<>(action), _UI_EXECUTOR, delay);
    }

    /**
     * 
     * @param action
     * @param retryTimes
     * @param retryInterval
     * @param retryCondition
     * @return
     */
    static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, ? super Exception, Boolean> retryCondition) {
        return executeOnUiThread(new Callable<T>() {
            @Override
            public T call() throws Exception {
                final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
                return retry.call(action);
            }
        });
    }

    private static <T> CompletableFuture<T> execute(final FutureTask<T> futureTask, final Executor executor) {
        executor.execute(futureTask);

        return new CompletableFuture<>(futureTask, null, executor);
    }

    private static <T> CompletableFuture<T> execute(final FutureTask<T> futureTask, final _UIExecutor executor, final long delay) {
        executor.execute(futureTask, delay);

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

    static final class _UIExecutor implements Executor {
        private static final Handler HANDLER = new Handler(Looper.getMainLooper());

        private _UIExecutor() {
            // Singleton.
        }

        @Override
        public void execute(Runnable command) {
            HANDLER.post(command);
        }

        public void execute(Runnable command, final long delay) {
            if (delay > 0) {
                HANDLER.postDelayed(command, delay);
            } else {
                HANDLER.post(command);
            }
        }
    }

    public static final class SerialExecutor {
        private SerialExecutor() {
            // singleton
        }

        /**
         * The action will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background.
         * 
         * @param action
         * @return
         */
        public static CompletableFuture<Void> execute(final Runnable action) {
            return Async.execute(action);
        }

        public static CompletableFuture<Void> execute(final Runnable action, final long delay) {
            return Async.execute(action, delay);
        }

        public static CompletableFuture<Void> execute(final Runnable action, final int retryTimes, final long retryInterval,
                final Function<? super Exception, Boolean> retryCondition) {
            return Async.execute(action, retryTimes, retryInterval, retryCondition);

        }

        /**
         * The action will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background.
         * 
         * @param action
         * @return
         */
        public static <T> CompletableFuture<T> execute(final Callable<T> action) {
            return Async.execute(action);
        }

        public static <T> CompletableFuture<T> execute(final Callable<T> action, final long delay) {
            return Async.execute(action, delay);
        }

        public static <T> CompletableFuture<T> execute(final Callable<T> action, final int retryTimes, final long retryInterval,
                final BiFunction<? super T, ? super Exception, Boolean> retryCondition) {
            return Async.execute(action, retryTimes, retryInterval, retryCondition);
        }
    }

    public static final class TPExecutor {
        private TPExecutor() {
            // singleton
        }

        /**
         * The action will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
         * 
         * @param action
         * @return
         */
        public static CompletableFuture<Void> execute(final Runnable action) {
            return Async.executeWithThreadPool(action);
        }

        public static CompletableFuture<Void> execute(final Runnable action, final long delay) {
            return Async.executeWithThreadPool(action, delay);
        }

        public static CompletableFuture<Void> execute(final Runnable action, final int retryTimes, final long retryInterval,
                final Function<? super Exception, Boolean> retryCondition) {
            return Async.executeWithThreadPool(action, retryTimes, retryInterval, retryCondition);

        }

        /**
         * The action will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
         * 
         * @param action
         * @return
         */
        public static <T> CompletableFuture<T> execute(final Callable<T> action) {
            return Async.executeWithThreadPool(action);
        }

        public static <T> CompletableFuture<T> execute(final Callable<T> action, final long delay) {
            return Async.executeWithThreadPool(action, delay);
        }

        public static <T> CompletableFuture<T> execute(final Callable<T> action, final int retryTimes, final long retryInterval,
                final BiFunction<? super T, ? super Exception, Boolean> retryCondition) {
            return Async.executeWithThreadPool(action, retryTimes, retryInterval, retryCondition);
        }
    }

    public static final class UIExecutor {
        private UIExecutor() {
            // singleton
        }

        /**
         * The action will be asynchronously executed in UI thread.
         * 
         * @param action
         * @return
         */
        public static CompletableFuture<Void> execute(final Runnable action) {
            return Async.executeOnUiThread(action);
        }

        /**
         * The action will be asynchronously executed in UI thread.
         * 
         * @param action
         * @param delay unit is milliseconds
         * @return
         */
        public static CompletableFuture<Void> execute(final Runnable action, final long delay) {
            return Async.executeOnUiThread(action, delay);
        }

        public static CompletableFuture<Void> execute(final Runnable action, final int retryTimes, final long retryInterval,
                final Function<? super Exception, Boolean> retryCondition) {
            return Async.executeOnUiThread(action, retryTimes, retryInterval, retryCondition);
        }

        /**
         * The action will be asynchronously executed in UI thread.
         * 
         * @param action
         * @return
         */
        public static <T> CompletableFuture<T> execute(final Callable<T> action) {
            return Async.executeOnUiThread(action);
        }

        /**
         * The action will be asynchronously executed in UI thread.
         * 
         * @param action
         * @param delay
         * @return
         */
        public static <T> CompletableFuture<T> execute(final Callable<T> action, final long delay) {
            return Async.executeOnUiThread(action, delay);
        }

        public static <T> CompletableFuture<T> execute(final Callable<T> action, final int retryTimes, final long retryInterval,
                final BiFunction<? super T, ? super Exception, Boolean> retryCondition) {
            return Async.executeOnUiThread(action, retryTimes, retryInterval, retryCondition);
        }
    }

    //    /**
    //     * Short name for AsyncExecutor
    //     * 
    //     * @deprecated replaced with SerialExecutor/TPExecutor/UIExecutor.
    //     */
    //    @Deprecated
    //    @Beta
    //    public static class Asyn extends AsyncExecutor {
    //        private Asyn() {
    //            // singleton
    //        }
    //    }
}
