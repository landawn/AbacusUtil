package com.landawn.abacus.android;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.landawn.abacus.util.Callback;

public class CallbackFuture<T> implements RunnableFuture<T> {
    private final FutureTask<T> futureTask;
    private volatile Callback<T> callback;
    private volatile ThreadMode threadMode;
    private volatile boolean actionExecuted = false;

    public CallbackFuture(Callable<T> callable) {
        this.futureTask = new FutureTask<T>(callable);
    }

    public CallbackFuture(Runnable runnable, T result) {
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
                        } finally {
                            threadMode = threadMode == ThreadMode.NEW_THREAD ? ThreadMode.DEFAULT : threadMode;
                            callback(throwable, result);
                        }
                    }
                }
            }
        }
    }

    public void callback(final Callback<T> callback) {
        callback(callback, ThreadMode.NEW_THREAD);
    }

    public void callbackOnUiThread(final Callback<T> callback) {
        callback(callback, ThreadMode.UI_THREAD);
    }

    private void callback(final Callback<T> callback, final ThreadMode threadMode) {
        this.threadMode = threadMode;
        this.callback = callback;

        synchronized (this) {
            if (futureTask.isDone() && actionExecuted == false) {
                actionExecuted = true;

                T result = null;
                Throwable throwable = null;
                try {
                    result = futureTask.get();
                } catch (Throwable e) {
                    throwable = e;
                } finally {
                    callback(throwable, result);
                }
            }
        }
    }

    public void callback(final Callback.Action<T> action) {
        callback(action, ThreadMode.NEW_THREAD);
    }

    public void callbackOnUiThread(final Callback.Action<T> action) {
        callback(action, ThreadMode.UI_THREAD);
    }

    private void callback(final Callback.Action<T> action, final ThreadMode threadMode) {
        this.callback = new Callback<T>() {
            @Override
            public void on(Throwable e, T result) {
                if (e != null) {
                    throw (RuntimeException) e instanceof RuntimeException ? (RuntimeException) e : new RuntimeException(e);
                }

                action.on(result);
            }
        };

        callback(callback, threadMode);
    }

    private void callback(final Throwable throwable, final T result) {
        switch (threadMode) {
            case DEFAULT:
                callback.on(throwable, result);

                break;

            case NEW_THREAD:
                AsyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        callback.on(throwable, result);
                    }
                });

                break;

            case UI_THREAD:
                AsyncExecutor.executeOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        callback.on(throwable, result);
                    }
                });

                break;

            default:
                throw new RuntimeException("Unsupported thread mode");
        }
    }

    static enum ThreadMode {
        DEFAULT, NEW_THREAD, UI_THREAD;
    }
}
