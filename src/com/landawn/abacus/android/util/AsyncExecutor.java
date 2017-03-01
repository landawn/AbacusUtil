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
import java.util.concurrent.Executor;
import java.util.concurrent.FutureTask;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.Retry;
import com.landawn.abacus.util.Retry.Retry0;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;

import android.os.AsyncTask;
import android.os.Handler;
import android.os.Looper;

/**
 * <pre>
 * AsyncExecutor.executeWithThreadPool(() -> { // download image })
    .executeOnUiThread((e, image) -> {// refresh UI })
    .execute((e, image) -> {// convert image to bitmap format})
    .callbackOnUiThread((e, bitmap) -> {// update UI});
 * </pre>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class AsyncExecutor {

    public static final UIExecutor UI_EXECUTOR = new UIExecutor();
    public static final Executor SERIAL_EXECUTOR = AsyncTask.SERIAL_EXECUTOR;
    public static final Executor TP_EXECUTOR = AsyncTask.THREAD_POOL_EXECUTOR;

    private AsyncExecutor() {
        // Singleton
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    public static CompletableFuture<Void> execute(final Runnable action) {
        return execute(new FutureTask<Void>(action, null), SERIAL_EXECUTOR);
    }

    public static CompletableFuture<Void> execute(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<Throwable, Boolean> retryCondition) {
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
    public static <T> CompletableFuture<T> execute(final Callable<T> action) {
        return execute(new FutureTask<>(action), SERIAL_EXECUTOR);
    }

    public static <T> CompletableFuture<T> execute(final Callable<T> action, final int retryTimes, final long retryInterval,
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
     * The action will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    public static CompletableFuture<Void> executeWithThreadPool(final Runnable action) {
        return execute(new FutureTask<Void>(action, null), TP_EXECUTOR);
    }

    public static CompletableFuture<Void> executeWithThreadPool(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<Throwable, Boolean> retryCondition) {
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
    public static <T> CompletableFuture<T> executeWithThreadPool(final Callable<T> action) {
        return execute(new FutureTask<>(action), TP_EXECUTOR);
    }

    public static <T> CompletableFuture<T> executeWithThreadPool(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, Throwable, Boolean> retryCondition) {
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
    public static CompletableFuture<Void> executeOnUiThread(final Runnable action) {
        return executeOnUiThread(action, 0);
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    public static CompletableFuture<Void> executeOnUiThread(final Runnable action, final long delay) {
        return execute(new FutureTask<Void>(action, null), UI_EXECUTOR, delay);
    }

    public static CompletableFuture<Void> executeOnUiThread(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<Throwable, Boolean> retryCondition) {
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
    public static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action) {
        return executeOnUiThread(action, 0);
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    public static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action, final long delay) {
        return execute(new FutureTask<>(action), UI_EXECUTOR, delay);
    }

    public static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, Throwable, Boolean> retryCondition) {
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

        return new CompletableFuture<>(futureTask, executor);
    }

    private static <T> CompletableFuture<T> execute(final FutureTask<T> futureTask, final UIExecutor executor, final long delay) {
        executor.execute(futureTask, delay);

        return new CompletableFuture<>(futureTask, executor);
    }

    public static final class UIExecutor implements Executor {
        private static final Handler HANDLER = new Handler(Looper.getMainLooper());

        private UIExecutor() {
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

    /**
     * Short name for AsyncExecutor
     */
    @Beta
    public static class Asyn extends AsyncExecutor {
    }
}
