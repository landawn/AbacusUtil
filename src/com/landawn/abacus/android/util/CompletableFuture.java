package com.landawn.abacus.android.util;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.landawn.abacus.util.Callback;
import com.landawn.abacus.util.Callback2;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ThreadMode;

public class CompletableFuture<T> implements RunnableFuture<T> {
    private final FutureTask<T> futureTask;
    private volatile Callback<T> callback;
    private volatile ThreadMode threadMode;
    private volatile boolean actionExecuted = false;

    CompletableFuture(Callable<T> callable) {
        this.futureTask = new FutureTask<T>(callable);
    }

    CompletableFuture(Runnable runnable, T result) {
        this.futureTask = new FutureTask<T>(runnable, result);
    }

    /**
     * 
     * @param result
     * @return a {@code CompletableFuture} which is already completed.
     */
    public static <T> CompletableFuture<T> completed(T result) {
        return new CompletedFuture<T>(result);
    }

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
        return futureTask.cancel(mayInterruptIfRunning);
    }

    @Override
    public boolean isCancelled() {
        return futureTask.isCancelled();
    }

    @Override
    public boolean isDone() {
        return futureTask.isDone();
    }

    @Override
    public T get() throws InterruptedException, ExecutionException {
        return futureTask.get();
    }

    @Override
    public T get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
        return futureTask.get(timeout, unit);
    }

    public T get(final Callback.Action<T> action) {
        try {
            final T result = get();
            action.on(result);
            return result;
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }
    }

    public T get(long timeout, TimeUnit unit, final Callback.Action<T> action) {
        try {
            final T result = get(timeout, unit);
            action.on(result);
            return result;
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            throw N.toRuntimeException(e);
        }
    }

    public T get(final Callback<T> callback) {
        T result = null;
        RuntimeException runtimeException = null;

        try {
            result = get();
        } catch (Throwable e) {
            runtimeException = N.toRuntimeException(e);
        }

        callback.on(runtimeException, result);

        return result;
    }

    public T get(long timeout, TimeUnit unit, final Callback<T> callback) {
        T result = null;
        RuntimeException runtimeException = null;

        try {
            result = get(timeout, unit);
        } catch (Throwable e) {
            runtimeException = N.toRuntimeException(e);
        }

        callback.on(runtimeException, result);

        return result;
    }

    @Override
    public void run() {
        try {
            futureTask.run();
        } finally {
            if (this.callback != null && actionExecuted == false) {
                synchronized (this.callback) {
                    if (actionExecuted == false) {
                        actionExecuted = true;

                        T result = null;
                        RuntimeException runtimeException = null;

                        try {
                            result = get();
                        } catch (Throwable e) {
                            runtimeException = N.toRuntimeException(e);
                        }

                        callback(runtimeException, result);
                    }
                }
            }
        }
    }

    /**
     * The callback will be executed in target future task thread or current thread if task has been completed before this callback is set.
     * 
     * @param action
     */
    public void callback(final Callback.Action<T> action) {
        callback(action, ThreadMode.DEFAULT);
    }

    /**
     * The callback will be executed in target future task thread or current thread if task has been completed before this callback is set.
     * 
     * @param callback
     */
    public void callback(final Callback<T> callback) {
        callback(callback, ThreadMode.DEFAULT);
    }

    /**
     * The callback will be executed in background thread which could be the future task thread or current thread if it's background thread, otherwise it will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR}.
     * 
     * @param action
     */
    public void callbackInBackground(final Callback.Action<T> action) {
        callback(action, ThreadMode.SERIAL_EXECUTOR);
    }

    /**
     * The callback will be executed in background thread which could be the future task thread or current thread if it's background thread, otherwise it will be asynchronously executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR}.
     * 
     * @param callback
     */
    public void callbackInBackground(final Callback<T> callback) {
        callback(callback, ThreadMode.SERIAL_EXECUTOR);
    }

    /**
     * The callback will be executed in background thread which could be the future task thread or current thread if it's background thread, otherwise it will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR}.
     * 
     * @param action
     */
    public void callbackInParallel(final Callback.Action<T> action) {
        callback(action, ThreadMode.THREAD_POOL_EXECUTOR);
    }

    /**
     * The callback will be executed in background thread which could be the future task thread or current thread if it's background thread, otherwise it will be asynchronously executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR}.
     * 
     * @param callback
     */
    public void callbackInParallel(final Callback<T> callback) {
        callback(callback, ThreadMode.THREAD_POOL_EXECUTOR);
    }

    /**
     * The callback will be executed in UI thread which could be the future task thread or current thread if it's UI thread, otherwise it will be asynchronously executed in UI thread.
     * 
     * @param action
     */
    public void callbackOnUiThread(final Callback.Action<T> action) {
        callback(action, ThreadMode.UI_THREAD);
    }

    /**
     * The callback will be executed in UI thread which could be the future task thread or current thread if it's UI thread, otherwise it will be asynchronously executed in UI thread.
     * 
     * @param callback
     */
    public void callbackOnUiThread(final Callback<T> callback) {
        callback(callback, ThreadMode.UI_THREAD);
    }

    private void callback(final Callback.Action<T> action, final ThreadMode threadMode) {
        callback(new Callback<T>() {
            @Override
            public void on(RuntimeException e, T result) {
                if (e != null) {
                    throw e;
                }

                action.on(result);
            }
        }, threadMode);
    }

    private void callback(final Callback<T> callback, final ThreadMode threadMode) {
        this.threadMode = threadMode;
        this.callback = callback;

        synchronized (this) {
            if (isDone() && isCancelled() == false && actionExecuted == false) {
                actionExecuted = true;

                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                callback(runtimeException, result);
            }
        }
    }

    private void callback(final RuntimeException runtimeException, final T result) {
        if (this.isCancelled()) {
            return;
        }

        switch (threadMode) {
            case DEFAULT:
                callback.on(runtimeException, result);

                break;

            case SERIAL_EXECUTOR:
                if (Util.isUiThread()) {
                    AsyncExecutor.execute(new Runnable() {
                        @Override
                        public void run() {
                            callback.on(runtimeException, result);
                        }
                    });
                } else {
                    callback.on(runtimeException, result);
                }

                break;

            case THREAD_POOL_EXECUTOR:
                if (Util.isUiThread()) {
                    AsyncExecutor.executeInParallel(new Runnable() {

                        @Override
                        public void run() {
                            callback.on(runtimeException, result);
                        }
                    });
                } else {
                    callback.on(runtimeException, result);
                }

                break;

            case UI_THREAD:
                if (Util.isUiThread()) {
                    callback.on(runtimeException, result);
                } else {
                    AsyncExecutor.executeOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            callback.on(runtimeException, result);
                        }
                    });
                }

                break;

            default:
                throw new RuntimeException("Unsupported thread mode");

        }
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background thread.
     * 
     * @param action
     * @return
     */
    public CompletableFuture<Void> execute(final Callback.Action<T> action) {
        return AsyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                try {
                    action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public CompletableFuture<Void> execute(final Callback<T> callback) {
        return AsyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                callback.on(runtimeException, result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param action
     * @return
     */
    public CompletableFuture<Void> executeInParallel(final Callback.Action<T> action) {
        return AsyncExecutor.executeInParallel(new Runnable() {
            @Override
            public void run() {
                try {
                    action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public CompletableFuture<Void> executeInParallel(final Callback<T> callback) {
        return AsyncExecutor.executeInParallel(new Runnable() {
            @Override
            public void run() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                callback.on(runtimeException, result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param action
     * @return
     */
    public CompletableFuture<Void> executeOnUiThread(final Callback.Action<T> action) {
        return AsyncExecutor.executeOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param callback
     * @return
     */
    public CompletableFuture<Void> executeOnUiThread(final Callback<T> callback) {
        return AsyncExecutor.executeOnUiThread(new Runnable() {
            @Override
            public void run() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                callback.on(runtimeException, result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    public CompletableFuture<Void> executeOnUiThread(final Callback.Action<T> action, final long delay) {
        return AsyncExecutor.executeOnUiThread(new Runnable() {
            @Override
            public void run() {
                try {
                    action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        }, delay);
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param callback
     * @param delay
     * @return
     */
    public CompletableFuture<Void> executeOnUiThread(final Callback<T> callback, final long delay) {
        return AsyncExecutor.executeOnUiThread(new Runnable() {
            @Override
            public void run() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                callback.on(runtimeException, result);
            }
        }, delay);
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background thread.
     * 
     * @param action
     * @return
     */
    public <R> CompletableFuture<R> execute(final Callback2.Action<T, R> action) {
        return AsyncExecutor.execute(new Callable<R>() {
            @Override
            public R call() {
                try {
                    return action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public <R> CompletableFuture<R> execute(final Callback2<T, R> callback) {
        return AsyncExecutor.execute(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                return callback.on(runtimeException, result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param action
     * @return
     */
    public <R> CompletableFuture<R> executeInParallel(final Callback2.Action<T, R> action) {
        return AsyncExecutor.executeInParallel(new Callable<R>() {
            @Override
            public R call() {
                try {
                    return action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public <R> CompletableFuture<R> executeInParallel(final Callback2<T, R> callback) {
        return AsyncExecutor.executeInParallel(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                return callback.on(runtimeException, result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param action
     * @return
     */
    public <R> CompletableFuture<R> executeOnUiThread(final Callback2.Action<T, R> action) {
        return AsyncExecutor.executeOnUiThread(new Callable<R>() {
            @Override
            public R call() {
                try {
                    return action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param callback
     * @return
     */
    public <R> CompletableFuture<R> executeOnUiThread(final Callback2<T, R> callback) {
        return AsyncExecutor.executeOnUiThread(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                return callback.on(runtimeException, result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param action
     * @param delay
     * @return
     */
    public <R> CompletableFuture<R> executeOnUiThread(final Callback2.Action<T, R> action, final long delay) {
        return AsyncExecutor.executeOnUiThread(new Callable<R>() {
            @Override
            public R call() {
                try {
                    return action.on(get());
                } catch (InterruptedException | ExecutionException e) {
                    throw N.toRuntimeException(e);
                }
            }
        }, delay);
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param callback
     * @param delay
     * @return
     */
    public <R> CompletableFuture<R> executeOnUiThread(final Callback2<T, R> callback, final long delay) {
        return AsyncExecutor.executeOnUiThread(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                RuntimeException runtimeException = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                return callback.on(runtimeException, result);
            }
        }, delay);
    }

    static class CompletedFuture<T> extends CompletableFuture<T> {
        private static final Runnable EMPTY_CALLABLE = new Runnable() {
            @Override
            public void run() {
                // do nothing
            }
        };

        private final T result;

        CompletedFuture(T result) {
            super(EMPTY_CALLABLE, null);

            this.result = result;
        }

        @Override
        public boolean cancel(boolean mayInterruptIfRunning) {
            return false;
        }

        @Override
        public boolean isCancelled() {
            return false;
        }

        @Override
        public boolean isDone() {
            return true;
        }

        @Override
        public T get() {
            return result;
        }

        @Override
        public T get(long timeout, TimeUnit unit) {
            return result;
        }

        @Override
        public void run() {
            // do nothing. it's already done.
        }
    }
}
