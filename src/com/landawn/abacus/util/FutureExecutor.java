package com.landawn.abacus.util;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class FutureExecutor<T> implements RunnableFuture<T> {
    private final AsyncExecutor asyncExecutor;
    private final FutureTask<T> futureTask;
    private volatile Callback<T> callback;
    private volatile boolean actionExecuted = false;

    FutureExecutor(final AsyncExecutor asyncExecutor, final Callable<T> callable) {
        this.asyncExecutor = asyncExecutor;
        this.futureTask = new FutureTask<T>(callable);
    }

    FutureExecutor(final AsyncExecutor asyncExecutor, final Runnable runnable, final T result) {
        this.asyncExecutor = asyncExecutor;
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

                        callback.on(throwable, result);
                    }
                }
            }
        }
    }

    /**
     * The callback could be executed in target future task thread or current thread if task has been completed before this callback is set.
     * 
     * @param action
     */
    public void callback(final Callback.Action<T> action) {
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

    /**
     * The callback could be executed in target future task thread or current thread if task has been completed before this callback is set.
     * 
     * @param callback
     */
    public void callback(final Callback<T> callback) {
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

                this.callback.on(throwable, result);
            }
        }
    }

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

    public FutureExecutor<Void> execute(final Callback<T> callback) {
        return asyncExecutor.execute(new Runnable() {
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

    public <R> FutureExecutor<R> execute(final Callback2<T, R> callback) {
        return asyncExecutor.execute(new Callable<R>() {
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

}
