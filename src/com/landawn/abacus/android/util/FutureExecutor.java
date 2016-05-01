package com.landawn.abacus.android.util;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.landawn.abacus.util.Callback;
import com.landawn.abacus.util.Callback2;

public class FutureExecutor<T> implements RunnableFuture<T> {
    private final FutureTask<T> futureTask;
    private volatile Callback<T> callback;
    private volatile ThreadMode threadMode;
    private volatile boolean actionExecuted = false;

    FutureExecutor(Callable<T> callable) {
        this.futureTask = new FutureTask<T>(callable);
    }

    FutureExecutor(Runnable runnable, T result) {
        this.futureTask = new FutureTask<T>(runnable, result);
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
                        Throwable throwable = null;

                        try {
                            result = futureTask.get();
                        } catch (Throwable e) {
                            throwable = e;
                        }

                        callback(throwable, result);
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
        callback(action, ThreadMode.CURRENT_THREAD);
    }

    /**
     * The callback will be executed in target future task thread or current thread if task has been completed before this callback is set.
     * 
     * @param callback
     */
    public void callback(final Callback<T> callback) {
        callback(callback, ThreadMode.CURRENT_THREAD);
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
            public void on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                action.on(result);
            }
        });
    }

    private void callback(final Callback<T> callback, final ThreadMode threadMode) {
        this.threadMode = threadMode;
        this.callback = callback;

        synchronized (this) {
            if (futureTask.isDone() && futureTask.isCancelled() == false && actionExecuted == false) {
                actionExecuted = true;

                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                callback(throwable, result);
            }
        }
    }

    private void callback(final Throwable throwable, final T result) {
        if (this.isCancelled()) {
            return;
        }

        switch (threadMode) {
            case CURRENT_THREAD:
                callback.on(throwable, result);

                break;

            case SERIAL_EXECUTOR:
                if (Util.isUiThread()) {
                    AsyncExecutor.execute(new Runnable() {
                        @Override
                        public void run() {
                            callback.on(throwable, result);
                        }
                    });
                } else {
                    callback.on(throwable, result);
                }

                break;

            case THREAD_POOL_EXECUTOR:
                if (Util.isUiThread()) {
                    AsyncExecutor.executeInParallel(new Runnable() {
                        @Override
                        public void run() {
                            callback.on(throwable, result);
                        }
                    });
                } else {
                    callback.on(throwable, result);
                }

                break;

            case UI_THREAD:
                if (Util.isUiThread()) {
                    callback.on(throwable, result);
                } else {
                    AsyncExecutor.executeOnUiThread(new Runnable() {
                        @Override
                        public void run() {
                            callback.on(throwable, result);
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
    public FutureExecutor<Void> execute(final Callback.Action<T> action) {
        return execute(new Callback<T>() {
            @Override
            public void on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                action.on(result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public FutureExecutor<Void> execute(final Callback<T> callback) {
        return AsyncExecutor.execute(new Runnable() {
            @Override
            public void run() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                callback.on(throwable, result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param action
     * @return
     */
    public FutureExecutor<Void> executeInParallel(final Callback.Action<T> action) {
        return executeInParallel(new Callback<T>() {
            @Override
            public void on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                action.on(result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public FutureExecutor<Void> executeInParallel(final Callback<T> callback) {
        return AsyncExecutor.executeInParallel(new Runnable() {
            @Override
            public void run() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                callback.on(throwable, result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param action
     * @return
     */
    public FutureExecutor<Void> executeOnUiThread(final Callback.Action<T> action) {
        return executeOnUiThread(new Callback<T>() {
            @Override
            public void on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                action.on(result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param callback
     * @return
     */
    public FutureExecutor<Void> executeOnUiThread(final Callback<T> callback) {
        return AsyncExecutor.executeOnUiThread(new Runnable() {
            @Override
            public void run() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                callback.on(throwable, result);
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
    public FutureExecutor<Void> executeOnUiThread(final Callback.Action<T> action, final long delay) {
        return executeOnUiThread(new Callback<T>() {
            @Override
            public void on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                action.on(result);
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
    public FutureExecutor<Void> executeOnUiThread(final Callback<T> callback, final long delay) {
        return AsyncExecutor.executeOnUiThread(new Runnable() {
            @Override
            public void run() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                callback.on(throwable, result);
            }
        }, delay);
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background thread.
     * 
     * @param action
     * @return
     */
    public <R> FutureExecutor<R> execute(final Callback2.Action<T, R> action) {
        return execute(new Callback2<T, R>() {
            @Override
            public R on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                return action.on(result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#SERIAL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public <R> FutureExecutor<R> execute(final Callback2<T, R> callback) {
        return AsyncExecutor.execute(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                return callback.on(throwable, result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param action
     * @return
     */
    public <R> FutureExecutor<R> executeInParallel(final Callback2.Action<T, R> action) {
        return executeInParallel(new Callback2<T, R>() {
            @Override
            public R on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                return action.on(result);
            }
        });
    }

    /**
     * The callback will be executed with {@code android.io.AsyncTask#THREAD_POOL_EXECUTOR} in background thread.
     * 
     * @param callback
     * @return
     */
    public <R> FutureExecutor<R> executeInParallel(final Callback2<T, R> callback) {
        return AsyncExecutor.executeInParallel(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                return callback.on(throwable, result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param action
     * @return
     */
    public <R> FutureExecutor<R> executeOnUiThread(final Callback2.Action<T, R> action) {
        return executeOnUiThread(new Callback2<T, R>() {
            @Override
            public R on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                return action.on(result);
            }
        });
    }

    /**
     * The callback will be executed in main(UI) thread.
     * 
     * @param callback
     * @return
     */
    public <R> FutureExecutor<R> executeOnUiThread(final Callback2<T, R> callback) {
        return AsyncExecutor.executeOnUiThread(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                return callback.on(throwable, result);
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
    public <R> FutureExecutor<R> executeOnUiThread(final Callback2.Action<T, R> action, final long delay) {
        return executeOnUiThread(new Callback2<T, R>() {
            @Override
            public R on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                return action.on(result);
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
    public <R> FutureExecutor<R> executeOnUiThread(final Callback2<T, R> callback, final long delay) {
        return AsyncExecutor.executeOnUiThread(new Callable<R>() {
            @Override
            public R call() {
                T result = null;
                Throwable throwable = null;

                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                }

                return callback.on(throwable, result);
            }
        }, delay);
    }

    static enum ThreadMode {
        CURRENT_THREAD, SERIAL_EXECUTOR, THREAD_POOL_EXECUTOR, UI_THREAD;
    }
}
