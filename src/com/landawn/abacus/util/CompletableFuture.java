/*
 * Copyright (C) 2016 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package com.landawn.abacus.util;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.Tuple.Tuple4;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/CompletableFuture.html">https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/CompletableFuture.html</a>
 */
public class CompletableFuture<T> implements Future<T> {
    private static final Logger logger = LoggerFactory.getLogger(CompletableFuture.class);

    private static final ExecutorService commonPool = Executors.newFixedThreadPool(64);

    static {
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                logger.warn("Starting to shutdown task in CompletableFuture");

                try {
                    commonPool.shutdown();

                    while (commonPool.isTerminated() == false) {
                        N.sleepUninterruptibly(100);
                    }
                } finally {
                    logger.warn("Completed to shutdown task in CompletableFuture");
                }
            }
        });
    }

    final Future<T> future;
    final List<CompletableFuture<?>> upFutures;
    final Executor asyncExecutor;

    CompletableFuture(final Future<T> future) {
        this(future, null, null);
    }

    CompletableFuture(final Future<T> future, final List<CompletableFuture<?>> upFutures, final Executor asyncExecutor) {
        this.future = future;
        this.upFutures = upFutures;
        this.asyncExecutor = asyncExecutor == null ? commonPool : asyncExecutor;
    }

    public static <E extends Exception> CompletableFuture<Void> run(final Try.Runnable<E> action) {
        return run(action, commonPool);
    }

    public static <E extends Exception> CompletableFuture<Void> run(final Try.Runnable<E> action, final Executor executor) {
        final FutureTask<Void> futureTask = new FutureTask<>(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                action.run();
                return null;
            }
        });

        executor.execute(futureTask);

        return new CompletableFuture<>(futureTask, null, executor);
    }

    public static <T, E extends Exception> CompletableFuture<T> call(final Try.Callable<T, E> action) {
        return call(action, commonPool);
    }

    public static <T, E extends Exception> CompletableFuture<T> call(final Try.Callable<T, E> action, final Executor executor) {
        final FutureTask<T> futureTask = new FutureTask<>(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return action.call();
            }
        });

        executor.execute(futureTask);

        return new CompletableFuture<>(futureTask, null, executor);
    }

    /**
     * 
     * @param result
     * @param asyncExecutor
     * @return a CompletableFuture which is already done by passing the result to it directly.
     */
    public static <T> CompletableFuture<T> completed(final T result) {
        return new CompletableFuture<>(new Future<T>() {
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
            public T get(final long timeout, final TimeUnit unit) {
                return result;
            }
        }, null, commonPool);
    }

    public static <T> CompletableFuture<T> wrap(Future<T> future) {
        return new CompletableFuture<T>(future);
    }

    @Override
    public boolean cancel(boolean mayInterruptIfRunning) {
        return future.cancel(mayInterruptIfRunning);
    }

    @Override
    public boolean isCancelled() {
        return future.isCancelled();
    }

    /**
     * Cancel this future and all the previous stage future recursively.
     * 
     * @param mayInterruptIfRunning
     * @return
     */
    public boolean cancelAll(boolean mayInterruptIfRunning) {
        boolean res = true;

        if (N.notNullOrEmpty(upFutures)) {
            for (CompletableFuture<?> preFuture : upFutures) {
                res = res & preFuture.cancelAll(mayInterruptIfRunning);
            }
        }

        return cancel(mayInterruptIfRunning) && res;
    }

    /**
     * Returns true if this future and all previous stage futures have been recursively cancelled, otherwise false is returned.
     * 
     * @return
     */
    public boolean isAllCancelled() {
        boolean res = true;

        if (N.notNullOrEmpty(upFutures)) {
            for (CompletableFuture<?> preFuture : upFutures) {
                res = res & preFuture.isAllCancelled();
            }
        }

        return isCancelled() && res;
    }

    @Override
    public boolean isDone() {
        return future.isDone();
    }

    @Override
    public T get() throws InterruptedException, ExecutionException {
        return future.get();
    }

    @Override
    public T get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
        return future.get(timeout, unit);
    }

    public Pair<T, Exception> gett() {
        try {
            return Pair.of(get(), null);
        } catch (Exception e) {
            return Pair.of(null, e);
        }
    }

    public Pair<T, Exception> gett(final long timeout, final TimeUnit unit) {
        try {
            return Pair.of(get(timeout, unit), null);
        } catch (Exception e) {
            return Pair.of(null, e);
        }
    }

    public <U, E extends Exception> U get(final Try.Function<? super T, ? extends U, E> action) throws E {
        try {
            return action.apply(get());
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }
    }

    public <U, E extends Exception> U get(final long timeout, final TimeUnit unit, final Try.Function<? super T, ? extends U, E> action) throws E {
        try {
            return action.apply(get(timeout, unit));
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            throw N.toRuntimeException(e);
        }
    }

    public <U, E extends Exception> U get(final Try.BiFunction<? super T, Exception, ? extends U, E> action) throws E {
        final Pair<T, Exception> result = gett();
        return action.apply(result.left, result.right);
    }

    public <U, E extends Exception> U get(final long timeout, final TimeUnit unit, final Try.BiFunction<? super T, Exception, ? extends U, E> action) throws E {
        final Pair<T, Exception> result = gett(timeout, unit);
        return action.apply(result.left, result.right);
    }

    public T getNow(T defaultValue) {
        try {
            return isDone() ? get() : defaultValue;
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }
    }

    public <E extends Exception> void getAndThen(final Try.Consumer<T, E> action) throws InterruptedException, ExecutionException, E {
        action.accept(get());
    }

    public <E extends Exception> void getAndThen(final long timeout, final TimeUnit unit, final Try.Consumer<T, E> action)
            throws InterruptedException, ExecutionException, TimeoutException, E {
        action.accept(get(timeout, unit));
    }

    //    public void complete() throws InterruptedException, ExecutionException {
    //        get();
    //    }
    //
    //    public void complete(Consumer<? super T> action) {
    //        try {
    //            action.accept(get());
    //        } catch (InterruptedException | ExecutionException e) {
    //            throw N.toRuntimeException(e);
    //        }
    //    }
    //
    //    public void complete(BiConsumer<? super T, Exception> action) {
    //        final Pair<T, Exception> result = gett();
    //        action.accept(result.left, result.right);
    //    }

    <U, E extends Exception> CompletableFuture<U> thenApply(final Try.Function<? super T, ? extends U, E> action) {
        return new CompletableFuture<U>(new Future<U>() {
            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                return future.cancel(mayInterruptIfRunning);
            }

            @Override
            public boolean isCancelled() {
                return future.isCancelled();
            }

            @Override
            public boolean isDone() {
                return future.isDone();
            }

            @Override
            public U get() throws InterruptedException, ExecutionException {
                try {
                    return action.apply(future.get());
                } catch (InterruptedException | ExecutionException e) {
                    throw e;
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }

            @Override
            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                try {
                    return action.apply(future.get(timeout, unit));
                } catch (InterruptedException | ExecutionException | TimeoutException e) {
                    throw e;
                } catch (Exception e) {
                    throw N.toRuntimeException(e);
                }
            }
        }, null, asyncExecutor) {
            @Override
            public boolean cancelAll(boolean mayInterruptIfRunning) {
                return super.cancelAll(mayInterruptIfRunning);
            }

            @Override
            public boolean isAllCancelled() {
                return super.isAllCancelled();
            }
        };
    }

    //    public <U> CompletableFuture<U> thenApply(final BiFunction<? super T, Exception, ? extends U> action) {
    //        return new CompletableFuture<U>(new Future<U>() {
    //            @Override
    //            public boolean cancel(boolean mayInterruptIfRunning) {
    //                return future.cancel(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isCancelled() {
    //                return future.isCancelled();
    //            }
    //
    //            @Override
    //            public boolean isDone() {
    //                return future.isDone();
    //            }
    //
    //            @Override
    //            public U get() throws InterruptedException, ExecutionException {
    //                final Pair<T, Exception> result = gett();
    //                return action.apply(result.left, result.right);
    //            }
    //
    //            @Override
    //            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                final Pair<T, Exception> result = gett(timeout, unit);
    //                return action.apply(result.left, result.right);
    //            }
    //        }, null, asyncExecutor) {
    //            @Override
    //            public boolean cancelAll(boolean mayInterruptIfRunning) {
    //                return super.cancelAll(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isAllCancelled() {
    //                return super.isAllCancelled();
    //            }
    //        };
    //    }

    //    public <U> CompletableFuture<Void> thenAccept(final Consumer<? super T> action) {
    //        return thenApply(new Function<T, Void>() {
    //            @Override
    //            public Void apply(T t) {
    //                action.accept(t);
    //                return null;
    //            }
    //        });
    //    }
    //
    //    public <U> CompletableFuture<Void> thenAccept(final BiConsumer<? super T, Exception> action) {
    //        return thenApply(new BiFunction<T, Exception, Void>() {
    //            @Override
    //            public Void apply(T t, Exception e) {
    //                action.accept(t, e);
    //                return null;
    //            }
    //        });
    //    }
    //
    //    public <U, R> CompletableFuture<R> thenCombine(final CompletableFuture<? extends U> other, final BiFunction<? super T, ? super U, ? extends R> action) {
    //        return new CompletableFuture<R>(new Future<R>() {
    //            @Override
    //            public boolean cancel(boolean mayInterruptIfRunning) {
    //                return future.cancel(mayInterruptIfRunning) && other.future.cancel(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isCancelled() {
    //                return future.isCancelled() || other.future.isCancelled();
    //            }
    //
    //            @Override
    //            public boolean isDone() {
    //                return future.isDone() && other.future.isDone();
    //            }
    //
    //            @Override
    //            public R get() throws InterruptedException, ExecutionException {
    //                return action.apply(future.get(), other.future.get());
    //            }
    //
    //            @Override
    //            public R get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                final long timeoutInMillis = unit.toMillis(timeout);
    //                final long now = N.currentMillis();
    //                final long endTime = timeoutInMillis > Long.MAX_VALUE - now ? Long.MAX_VALUE : now + timeoutInMillis;
    //
    //                final T result = future.get(timeout, unit);
    //                final U otherResult = other.future.get(N.max(0, endTime - N.currentMillis()), TimeUnit.MILLISECONDS);
    //
    //                return action.apply(result, otherResult);
    //            }
    //        }, null, asyncExecutor) {
    //            @Override
    //            public boolean cancelAll(boolean mayInterruptIfRunning) {
    //                return super.cancelAll(mayInterruptIfRunning) && other.cancelAll(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isAllCancelled() {
    //                return super.isAllCancelled() && other.isAllCancelled();
    //            }
    //        };
    //    }
    //
    //    public <U, R> CompletableFuture<R> thenCombine(final CompletableFuture<? extends U> other, final Function<Tuple4<T, Exception, U, Exception>, R> action) {
    //        return new CompletableFuture<R>(new Future<R>() {
    //            @Override
    //            public boolean cancel(boolean mayInterruptIfRunning) {
    //                return future.cancel(mayInterruptIfRunning) && other.future.cancel(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isCancelled() {
    //                return future.isCancelled() || other.future.isCancelled();
    //            }
    //
    //            @Override
    //            public boolean isDone() {
    //                return future.isDone() && other.future.isDone();
    //            }
    //
    //            @Override
    //            public R get() throws InterruptedException, ExecutionException {
    //                final Pair<T, Exception> result = gett();
    //                final Pair<? extends U, Exception> result2 = other.gett();
    //
    //                return action.apply(Tuple.of(result.left, result.right, (U) result2.left, result.right));
    //            }
    //
    //            @Override
    //            public R get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                final long timeoutInMillis = unit.toMillis(timeout);
    //                final long now = N.currentMillis();
    //                final long endTime = timeoutInMillis > Long.MAX_VALUE - now ? Long.MAX_VALUE : now + timeoutInMillis;
    //
    //                final Pair<T, Exception> result = CompletableFuture.this.gett(timeout, unit);
    //                final Pair<? extends U, Exception> result2 = other.gett(N.max(0, endTime - N.currentMillis()), TimeUnit.MILLISECONDS);
    //
    //                return action.apply(Tuple.of(result.left, result.right, (U) result2.left, result.right));
    //            }
    //        }, null, asyncExecutor) {
    //            @Override
    //            public boolean cancelAll(boolean mayInterruptIfRunning) {
    //                return super.cancelAll(mayInterruptIfRunning) && other.cancelAll(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isAllCancelled() {
    //                return super.isAllCancelled() && other.isAllCancelled();
    //            }
    //        };
    //    }
    //
    //    public <U> CompletableFuture<Void> thenAcceptBoth(final CompletableFuture<? extends U> other, final BiConsumer<? super T, ? super U> action) {
    //        return thenCombine(other, new BiFunction<T, U, Void>() {
    //            @Override
    //            public Void apply(T t, U u) {
    //                action.accept(t, u);
    //                return null;
    //            }
    //        });
    //    }
    //
    //    public <U> CompletableFuture<Void> thenAcceptBoth(final CompletableFuture<? extends U> other, final Consumer<Tuple4<T, Exception, U, Exception>> action) {
    //        return thenCombine(other, new Function<Tuple4<T, Exception, U, Exception>, Void>() {
    //            @Override
    //            public Void apply(Tuple4<T, Exception, U, Exception> t) {
    //                action.accept(t);
    //                return null;
    //            }
    //        });
    //    }

    public <E extends Exception> CompletableFuture<Void> thenRun(final Try.Runnable<E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                get();
                action.run();
                return null;
            }
        });
    }

    public <E extends Exception> CompletableFuture<Void> thenRun(final Try.Consumer<? super T, E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                action.accept(get());
                return null;
            }
        });
    }

    public <E extends Exception> CompletableFuture<Void> thenRun(final Try.BiConsumer<? super T, Exception, E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                final Pair<T, Exception> result = gett();
                action.accept(result.left, result.right);
                return null;
            }
        });
    }

    public <R, E extends Exception> CompletableFuture<R> thenCall(final Try.Callable<R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                get();
                return action.call();
            }
        });
    }

    public <R, E extends Exception> CompletableFuture<R> thenCall(final Try.Function<? super T, R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                return action.apply(get());
            }
        });
    }

    public <R, E extends Exception> CompletableFuture<R> thenCall(final Try.BiFunction<? super T, Exception, R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Exception> result = gett();
                return action.apply(result.left, result.right);
            }
        });
    }

    public <E extends Exception> CompletableFuture<Void> runAfterBoth(final CompletableFuture<?> other, final Try.Runnable<E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                get();
                other.get();
                action.run();
                return null;
            }
        }, other);
    }

    public <U, E extends Exception> CompletableFuture<Void> runAfterBoth(final CompletableFuture<U> other, final Try.BiConsumer<T, U, E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                action.accept(get(), other.get());
                return null;
            }
        }, other);
    }

    public <U, E extends Exception> CompletableFuture<Void> runAfterBoth(final CompletableFuture<U> other,
            final Try.Consumer<Tuple4<T, Exception, U, Exception>, E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                final Pair<T, Exception> result = gett();
                final Pair<U, Exception> result2 = other.gett();

                action.accept(Tuple.of(result.left, result.right, result2.left, result.right));
                return null;
            }
        }, other);
    }

    public <R, E extends Exception> CompletableFuture<R> callAfterBoth(final CompletableFuture<?> other, final Try.Callable<R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                get();
                other.get();
                return action.call();
            }
        }, other);
    }

    public <U, R, E extends Exception> CompletableFuture<R> callAfterBoth(final CompletableFuture<U> other, final Try.BiFunction<T, U, R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                return action.apply(get(), other.get());
            }
        }, other);
    }

    public <U, R, E extends Exception> CompletableFuture<R> callAfterBoth(final CompletableFuture<U> other,
            final Try.Function<Tuple4<T, Exception, U, Exception>, R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Exception> result = gett();
                final Pair<U, Exception> result2 = other.gett();

                return action.apply(Tuple.of(result.left, result.right, result2.left, result.right));
            }
        }, other);
    }

    public <E extends Exception> CompletableFuture<Void> runAfterEither(final CompletableFuture<?> other, final Try.Runnable<E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                Futures.anyOf(N.asList(CompletableFuture.this, other)).get();

                action.run();
                return null;
            }
        }, other);
    }

    public <E extends Exception> CompletableFuture<Void> runAfterEither(final CompletableFuture<? extends T> other, final Try.Consumer<? super T, E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                final T result = Futures.anyOf(N.asList(CompletableFuture.this, other)).get();

                action.accept(result);
                return null;
            }
        }, other);
    }

    public <E extends Exception> CompletableFuture<Void> runAfterEither(final CompletableFuture<? extends T> other,
            final Try.BiConsumer<? super T, Exception, E> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                final Pair<T, Exception> result = Futures.anyOf(N.asList(CompletableFuture.this, other)).gett();

                action.accept(result.left, result.right);
                return null;
            }
        }, other);
    }

    public <R, E extends Exception> CompletableFuture<R> callAfterEither(final CompletableFuture<?> other, final Try.Callable<R, RuntimeException> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                Futures.anyOf(N.asList(CompletableFuture.this, other)).get();

                return action.call();
            }
        }, other);
    }

    public <R, E extends Exception> CompletableFuture<R> callAfterEither(final CompletableFuture<? extends T> other,
            final Try.Function<? super T, R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final T result = Futures.anyOf(N.asList(CompletableFuture.this, other)).get();

                return action.apply(result);
            }
        }, other);
    }

    public <R, E extends Exception> CompletableFuture<R> callAfterEither(final CompletableFuture<? extends T> other,
            final Try.BiFunction<? super T, Exception, R, E> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Exception> result = Futures.anyOf(N.asList(CompletableFuture.this, other)).gett();

                return action.apply(result.left, result.right);
            }
        }, other);
    }

    //    /**
    //     * Returns a new CompletableFuture that, when either this or the
    //     * other given CompletableFuture complete normally. If both of the given
    //     * CompletableFutures complete exceptionally, then the returned
    //     * CompletableFuture also does so.
    //     * 
    //     * @param other
    //     * @param action
    //     * @return
    //     */
    //    public <U> CompletableFuture<U> applyToEither(final CompletableFuture<? extends T> other, final Function<? super T, U> action) {
    //        return Futures.anyOf(N.asList(this, other)).thenApply(action);
    //    }
    //
    //    /**
    //     * Returns a new CompletableFuture that, when either this or the
    //     * other given CompletableFuture complete normally. If both of the given
    //     * CompletableFutures complete exceptionally, then the returned
    //     * CompletableFuture also does so.
    //     * 
    //     * @param other
    //     * @param action
    //     * @return
    //     */
    //    public CompletableFuture<Void> acceptEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
    //        return Futures.anyOf(N.asList(this, other)).thenAccept(action);
    //    }

    //    /**
    //     * Returns a new CompletableFuture that, when this CompletableFuture completes
    //     * exceptionally, is executed with this CompletableFuture's exception as the
    //     * argument to the supplied function. Otherwise, if this CompletableFuture
    //     * completes normally, then the returned CompletableFuture also completes
    //     * normally with the same value.
    //     * 
    //     * @param action
    //     * @return
    //     */
    //    public CompletableFuture<T> exceptionally(final Function<Exception, ? extends T> action) {
    //        return new CompletableFuture<T>(new Future<T>() {
    //            @Override
    //            public boolean cancel(boolean mayInterruptIfRunning) {
    //                return future.cancel(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isCancelled() {
    //                return future.isCancelled();
    //            }
    //
    //            @Override
    //            public boolean isDone() {
    //                return future.isDone();
    //            }
    //
    //            @Override
    //            public T get() throws InterruptedException, ExecutionException {
    //                try {
    //                    return future.get();
    //                } catch (Exception e) {
    //                    return action.apply(e);
    //                }
    //            }
    //
    //            @Override
    //            public T get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                try {
    //                    return future.get(timeout, unit);
    //                } catch (Exception e) {
    //                    return action.apply(e);
    //                }
    //            }
    //        }, null, asyncExecutor) {
    //            @Override
    //            public boolean cancelAll(boolean mayInterruptIfRunning) {
    //                return super.cancelAll(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isAllCancelled() {
    //                return super.isAllCancelled();
    //            }
    //        };
    //    }

    //    public CompletableFuture<T> whenComplete(final BiConsumer<? super T, ? super Exception> action) {
    //        return new CompletableFuture<>(new Future<T>() {
    //            @Override
    //            public boolean cancel(boolean mayInterruptIfRunning) {
    //                return future.cancel(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isCancelled() {
    //                return future.isCancelled();
    //            }
    //
    //            @Override
    //            public boolean isDone() {
    //                return future.isDone();
    //            }
    //
    //            @Override
    //            public T get() throws InterruptedException, ExecutionException {
    //                final Pair<T, Exception> result = gett();
    //
    //                if (result.right != null) {
    //                    try {
    //                        action.accept(result.left, result.right);
    //                    } catch (Exception e) {
    //                        // ignore.
    //                    }
    //
    //                    if (result.right instanceof InterruptedException) {
    //                        throw ((InterruptedException) result.right);
    //                    } else if (result.right instanceof ExecutionException) {
    //                        throw ((ExecutionException) result.right);
    //                    } else {
    //                        throw N.toRuntimeException(result.right);
    //                    }
    //                } else {
    //                    action.accept(result.left, result.right);
    //                    return result.left;
    //                }
    //            }
    //
    //            @Override
    //            public T get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                final Pair<T, Exception> result = gett(timeout, unit);
    //
    //                if (result.right != null) {
    //                    try {
    //                        action.accept(result.left, result.right);
    //                    } catch (Exception e) {
    //                        // ignore.
    //                    }
    //
    //                    if (result.right instanceof InterruptedException) {
    //                        throw ((InterruptedException) result.right);
    //                    } else if (result.right instanceof ExecutionException) {
    //                        throw ((ExecutionException) result.right);
    //                    } else {
    //                        throw N.toRuntimeException(result.right);
    //                    }
    //                } else {
    //                    action.accept(result.left, result.right);
    //                    return result.left;
    //                }
    //            }
    //        }, asyncExecutor);
    //    }
    //
    //    public <U> CompletableFuture<U> handle(final BiFunction<? super T, Exception, ? extends U> action) {
    //        return new CompletableFuture<>(new Future<U>() {
    //            @Override
    //            public boolean cancel(boolean mayInterruptIfRunning) {
    //                return future.cancel(mayInterruptIfRunning);
    //            }
    //
    //            @Override
    //            public boolean isCancelled() {
    //                return future.isCancelled();
    //            }
    //
    //            @Override
    //            public boolean isDone() {
    //                return future.isDone();
    //            }
    //
    //            @Override
    //            public U get() throws InterruptedException, ExecutionException {
    //                final Pair<T, Exception> result = gett();
    //                return action.apply(result.left, result.right);
    //            }
    //
    //            @Override
    //            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                final Pair<T, Exception> result = gett(timeout, unit);
    //                return action.apply(result.left, result.right);
    //            }
    //        }, asyncExecutor);
    //    }

    private <R> CompletableFuture<R> execute(final Callable<R> command) {
        return execute(command, null);
    }

    private <R> CompletableFuture<R> execute(final Callable<R> command, final CompletableFuture<?> other) {
        return execute(new FutureTask<>(command), other);
    }

    private <U> CompletableFuture<U> execute(final FutureTask<U> futureTask, final CompletableFuture<?> other) {
        asyncExecutor.execute(futureTask);

        @SuppressWarnings("rawtypes")
        final List<CompletableFuture<?>> upFutures = other == null ? (List) Arrays.asList(this) : Arrays.asList(this, other);
        return new CompletableFuture<>(futureTask, upFutures, asyncExecutor);
    }

    public CompletableFuture<T> delayed(long delay, TimeUnit unit) {
        if (delay <= 0) {
            return this;
        }

        return with(asyncExecutor, delay, TimeUnit.MILLISECONDS);
    }

    public CompletableFuture<T> with(Executor executor) {
        return with(executor, 0, TimeUnit.MILLISECONDS);
    }

    @Deprecated
    public CompletableFuture<T> with(final Executor executor, final long delay, final TimeUnit unit) {
        N.checkArgNotNull(executor);

        return new CompletableFuture<T>(new Future<T>() {
            private final long delayEndTime = DateUtil.currentMillis() + unit.toMillis(delay);
            private volatile boolean isDelayed = false;

            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                return future.cancel(mayInterruptIfRunning);
            }

            @Override
            public boolean isCancelled() {
                return future.isCancelled();
            }

            @Override
            public boolean isDone() {
                boolean isDone = future.isDone();

                if (isDone) {
                    delay();
                }

                return isDone;
            }

            @Override
            public T get() throws InterruptedException, ExecutionException {
                delay();

                return future.get();
            }

            @Override
            public T get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                delay();

                return future.get(timeout, unit);
            }

            private void delay() {
                if (isDelayed == false) {
                    isDelayed = true;

                    N.sleep(delayEndTime - DateUtil.currentMillis());
                }
            }
        }, null, executor) {
            @Override
            public boolean cancelAll(boolean mayInterruptIfRunning) {
                return super.cancelAll(mayInterruptIfRunning);
            }

            @Override
            public boolean isAllCancelled() {
                return super.isAllCancelled();
            }
        };
    }
}
