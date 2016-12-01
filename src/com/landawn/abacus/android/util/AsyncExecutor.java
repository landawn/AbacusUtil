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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;

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
 * AsyncExecutor.executeInParallel(() -> { // download image })
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
    private static final Handler HANDLER = new Handler(Looper.getMainLooper());

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
        return execute(new CompletableFuture<Void>(action, null));
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

    @Beta
    static List<CompletableFuture<Void>> execute(final List<? extends Runnable> actions) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(actions.size());

        for (Runnable cmd : actions) {
            results.add(execute(cmd));
        }

        return results;
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    public static <T> CompletableFuture<T> execute(final Callable<T> action) {
        return execute(new CompletableFuture<T>(action));
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
     * The actions will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> List<CompletableFuture<T>> execute(final Collection<? extends Callable<T>> actions) {
        final List<CompletableFuture<T>> results = new ArrayList<>(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(execute(cmd));
        }

        return results;
    }

    private static <T> CompletableFuture<T> execute(final CompletableFuture<T> callableFuture) {
        AsyncTask.SERIAL_EXECUTOR.execute(callableFuture);

        return callableFuture;
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    public static CompletableFuture<Void> executeInParallel(final Runnable action) {
        return executeInParallel(new CompletableFuture<Void>(action, null));
    }

    public static CompletableFuture<Void> executeInParallel(final Runnable action, final int retryTimes, final long retryInterval,
            final Function<Throwable, Boolean> retryCondition) {
        return executeInParallel(new Runnable() {
            @Override
            public void run() {
                Retry.of(retryTimes, retryInterval, retryCondition).run(action);
            }
        });
    }

    /**
     * The actions will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
     * 
     * @param actions
     * @return
     */
    @Beta
    static List<CompletableFuture<Void>> executeInParallel(final List<? extends Runnable> actions) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(actions.size());

        for (Runnable cmd : actions) {
            results.add(executeInParallel(cmd));
        }

        return results;
    }

    /**
     * The action will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
     * 
     * @param action
     * @return
     */
    public static <T> CompletableFuture<T> executeInParallel(final Callable<T> action) {
        return executeInParallel(new CompletableFuture<T>(action));
    }

    public static <T> CompletableFuture<T> executeInParallel(final Callable<T> action, final int retryTimes, final long retryInterval,
            final BiFunction<? super T, Throwable, Boolean> retryCondition) {
        return executeInParallel(new Callable<T>() {
            @Override
            public T call() throws Exception {
                final Retry0<T> retry = Retry0.of(retryTimes, retryInterval, retryCondition);
                return retry.call(action);
            }
        });
    }

    /**
     * The actions will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> List<CompletableFuture<T>> executeInParallel(final Collection<? extends Callable<T>> actions) {
        final List<CompletableFuture<T>> results = new ArrayList<>(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(executeInParallel(cmd));
        }

        return results;
    }

    private static <T> CompletableFuture<T> executeInParallel(final CompletableFuture<T> callableFuture) {
        AsyncTask.THREAD_POOL_EXECUTOR.execute(callableFuture);

        return callableFuture;
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @return
     */
    public static CompletableFuture<Void> executeOnUiThread(final Runnable action) {
        return executeOnUiThread(new CompletableFuture<Void>(action, null), 0);
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    public static CompletableFuture<Void> executeOnUiThread(final Runnable action, final long delay) {
        return executeOnUiThread(new CompletableFuture<Void>(action, null), delay);
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
     * The actions will be asynchronously executed in UI thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static List<CompletableFuture<Void>> executeOnUiThread(final List<? extends Runnable> actions) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(actions.size());

        for (Runnable cmd : actions) {
            results.add(executeOnUiThread(cmd));
        }

        return results;
    }

    /**
     * The actions will be asynchronously executed in UI thread.
     * 
     * @param actions
     * @param delay
     * @return
     */
    @Beta
    static List<CompletableFuture<Void>> executeOnUiThread(final List<? extends Runnable> actions, final long delay) {
        final List<CompletableFuture<Void>> results = new ArrayList<>(actions.size());

        for (Runnable cmd : actions) {
            results.add(executeOnUiThread(cmd, delay));
        }

        return results;
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @return
     */
    public static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action) {
        return executeOnUiThread(new CompletableFuture<T>(action), 0);
    }

    /**
     * The action will be asynchronously executed in UI thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    public static <T> CompletableFuture<T> executeOnUiThread(final Callable<T> action, final long delay) {
        return executeOnUiThread(new CompletableFuture<T>(action), delay);
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

    /**
     * The actions will be asynchronously executed in UI thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> List<CompletableFuture<T>> executeOnUiThread(final Collection<? extends Callable<T>> actions) {
        final List<CompletableFuture<T>> results = new ArrayList<>(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(executeOnUiThread(cmd));
        }

        return results;
    }

    /**
     * The actions will be asynchronously executed in UI thread.
     * 
     * @param actions
     * @param delay
     * @return
     */
    @Beta
    static <T> List<CompletableFuture<T>> executeOnUiThread(final Collection<? extends Callable<T>> actions, final long delay) {
        final List<CompletableFuture<T>> results = new ArrayList<>(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(executeOnUiThread(cmd, delay));
        }

        return results;
    }

    private static <T> CompletableFuture<T> executeOnUiThread(final CompletableFuture<T> callableFuture, final long delay) {
        if (delay > 0) {
            HANDLER.postDelayed(callableFuture, delay);
        } else {
            HANDLER.post(callableFuture);
        }

        return callableFuture;
    }

    /**
     * Short name for AsyncExecutor
     */
    @Beta
    static class Async extends AsyncExecutor {
    }
}
