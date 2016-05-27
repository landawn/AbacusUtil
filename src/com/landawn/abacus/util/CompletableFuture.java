package com.landawn.abacus.util;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class CompletableFuture<T> implements RunnableFuture<T> {
    private final AsyncExecutor asyncExecutor;
    private final FutureTask<T> futureTask;
    private volatile Callback<T> callback;
    private volatile boolean actionExecuted = false;

    CompletableFuture(final AsyncExecutor asyncExecutor, final Callable<T> callable) {
        this.asyncExecutor = asyncExecutor;
        this.futureTask = new FutureTask<T>(callable);
    }

    CompletableFuture(final AsyncExecutor asyncExecutor, final Runnable runnable, final T result) {
        this.asyncExecutor = asyncExecutor;
        this.futureTask = new FutureTask<T>(runnable, result);
    }

    /**
     * 
     * @param result
     * @param asyncExecutor
     * @return a CompletableFuture which is already done by passing the result to it directly.
     */
    public static <T> CompletableFuture<T> of(final T result, final AsyncExecutor asyncExecutor) {
        return new FinishedFuture<T>(asyncExecutor, result);
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

                        callback.on(runtimeException, result);
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
            public void on(RuntimeException e, T result) {
                if (e != null) {
                    throw e;
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
            if (isDone() && isCancelled() == false && actionExecuted == false) {
                actionExecuted = true;

                T result = null;
                RuntimeException runtimeException = null;
                try {
                    result = get();
                } catch (Throwable e) {
                    runtimeException = N.toRuntimeException(e);
                }

                this.callback.on(runtimeException, result);
            }
        }
    }

    public CompletableFuture<Void> execute(final Callback.Action<T> action) {
        return asyncExecutor.execute(new Runnable() {
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

    public CompletableFuture<Void> execute(final Callback<T> callback) {
        return asyncExecutor.execute(new Runnable() {
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

    public <R> CompletableFuture<R> execute(final Callback2.Action<T, R> action) {
        return asyncExecutor.execute(new Callable<R>() {
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

    public <R> CompletableFuture<R> execute(final Callback2<T, R> callback) {
        return asyncExecutor.execute(new Callable<R>() {
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

    static class FinishedFuture<T> extends CompletableFuture<T> {
        private static final Runnable EMPTY_CALLABLE = new Runnable() {
            @Override
            public void run() {
                // do nothing
            }
        };

        private final T result;

        FinishedFuture(final AsyncExecutor asyncExecutor, T result) {
            super(asyncExecutor, EMPTY_CALLABLE, null);

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
