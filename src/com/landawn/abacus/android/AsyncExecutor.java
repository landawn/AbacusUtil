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

package com.landawn.abacus.android;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;

import com.landawn.abacus.util.N;

import android.os.AsyncTask;
import android.os.Handler;
import android.os.Looper;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class AsyncExecutor {
    private static final Handler m_handler = new Handler(Looper.getMainLooper());

    private AsyncExecutor() {
        // Singleton
    }

    public static CallbackFuture<Void> execute(final Runnable action) {
        return execute(new CallbackFuture<Void>(action, null));
    }

    public static List<CallbackFuture<Void>> execute(final List<? extends Runnable> actions) {
        final List<CallbackFuture<Void>> results = N.newArrayList(actions.size());

        for (Runnable cmd : actions) {
            results.add(execute(cmd));
        }

        return results;
    }

    public static <T> CallbackFuture<T> execute(final Callable<T> action) {
        return execute(new CallbackFuture<T>(action));
    }

    public static <T> List<CallbackFuture<T>> execute(final Collection<? extends Callable<T>> actions) {
        final List<CallbackFuture<T>> results = N.newArrayList(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(execute(cmd));
        }

        return results;
    }

    private static <T> CallbackFuture<T> execute(CallbackFuture<T> callableFuture) {
        AsyncTask.SERIAL_EXECUTOR.execute(callableFuture);

        return callableFuture;
    }

    public static CallbackFuture<Void> executeInParallel(final Runnable action) {
        return executeInParallel(new CallbackFuture<Void>(action, null));
    }

    public static List<CallbackFuture<Void>> executeInParallel(final List<? extends Runnable> actions) {
        final List<CallbackFuture<Void>> results = N.newArrayList(actions.size());

        for (Runnable cmd : actions) {
            results.add(executeInParallel(cmd));
        }

        return results;
    }

    public static <T> CallbackFuture<T> executeInParallel(final Callable<T> action) {
        return executeInParallel(new CallbackFuture<T>(action));
    }

    public static <T> List<CallbackFuture<T>> executeInParallel(final Collection<? extends Callable<T>> actions) {
        final List<CallbackFuture<T>> results = N.newArrayList(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(executeInParallel(cmd));
        }

        return results;
    }

    private static <T> CallbackFuture<T> executeInParallel(CallbackFuture<T> callableFuture) {
        AsyncTask.THREAD_POOL_EXECUTOR.execute(callableFuture);

        return callableFuture;
    }

    public static CallbackFuture<Void> executeOnUiThread(final Runnable action) {
        return executeOnUiThread(new CallbackFuture<Void>(action, null), 0);
    }

    public static CallbackFuture<Void> executeOnUiThread(final Runnable action, long delay) {
        return executeOnUiThread(new CallbackFuture<Void>(action, null), delay);
    }

    public static List<CallbackFuture<Void>> executeOnUiThread(final List<? extends Runnable> actions) {
        final List<CallbackFuture<Void>> results = N.newArrayList(actions.size());

        for (Runnable cmd : actions) {
            results.add(executeOnUiThread(cmd));
        }

        return results;
    }

    public static List<CallbackFuture<Void>> executeOnUiThread(final List<? extends Runnable> actions, long delay) {
        final List<CallbackFuture<Void>> results = N.newArrayList(actions.size());

        for (Runnable cmd : actions) {
            results.add(executeOnUiThread(cmd, delay));
        }

        return results;
    }

    public static <T> CallbackFuture<T> executeOnUiThread(final Callable<T> action) {
        return executeOnUiThread(new CallbackFuture<T>(action), 0);
    }

    public static <T> CallbackFuture<T> executeOnUiThread(final Callable<T> action, long delay) {
        return executeOnUiThread(new CallbackFuture<T>(action), delay);
    }

    public static <T> List<CallbackFuture<T>> executeOnUiThread(final Collection<? extends Callable<T>> actions) {
        final List<CallbackFuture<T>> results = N.newArrayList(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(executeOnUiThread(cmd));
        }

        return results;
    }

    public static <T> List<CallbackFuture<T>> executeOnUiThread(final Collection<? extends Callable<T>> actions, long delay) {
        final List<CallbackFuture<T>> results = N.newArrayList(actions.size());

        for (Callable<T> cmd : actions) {
            results.add(executeOnUiThread(cmd, delay));
        }

        return results;
    }

    private static <T> CallbackFuture<T> executeOnUiThread(CallbackFuture<T> callableFuture, long delay) {
        if (delay > 0) {
            m_handler.postDelayed(callableFuture, delay);
        } else {
            m_handler.post(callableFuture);
        }

        return callableFuture;
    }
}
