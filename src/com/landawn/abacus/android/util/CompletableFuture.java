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

package com.landawn.abacus.android.util;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.Tuple;
import com.landawn.abacus.util.Tuple.Tuple4;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;

/**
 * <br />
 * <code>TP</code> -> <code>Thread Pool</code>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class CompletableFuture<T> implements Future<T> {
    static final Logger logger = LoggerFactory.getLogger(CompletableFuture.class);

    final Future<T> future;
    final List<CompletableFuture<?>> upFutures;
    final Executor asyncExecutor;

    CompletableFuture(final Future<T> future, final List<CompletableFuture<?>> upFutures, final Executor asyncExecutor) {
        this.future = future;
        this.upFutures = upFutures;
        this.asyncExecutor = asyncExecutor;
    }

    //    public static CompletableFuture<Void> run(final Runnable action) {
    //        return run(action, Async.SERIAL_EXECUTOR);
    //    }
    //
    //    public static <T> CompletableFuture<T> run(final Try.Callable<T> action) {
    //        return run(action, Async.SERIAL_EXECUTOR);
    //    }
    //
    //    public static CompletableFuture<Void> run(final Runnable action, final Executor executor) {
    //        final FutureTask<Void> futureTask = new FutureTask<>(action, null);
    //
    //        executor.execute(futureTask);
    //
    //        return new CompletableFuture<>(futureTask, null, executor);
    //    }
    //
    //    public static <T> CompletableFuture<T> run(final Try.Callable<T> action, final Executor executor) {
    //        final FutureTask<T> futureTask = new FutureTask<>(action);
    //
    //        executor.execute(futureTask);
    //
    //        return new CompletableFuture<>(futureTask, null, executor);
    //    }

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
        }, null, Async.SERIAL_EXECUTOR);
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

    public Pair<T, Throwable> get2() {
        try {
            return Pair.of(get(), null);
        } catch (Throwable e) {
            return Pair.of(null, e);
        }
    }

    public Pair<T, Throwable> get2(final long timeout, final TimeUnit unit) {
        try {
            return Pair.of(get(timeout, unit), null);
        } catch (Throwable e) {
            return Pair.of(null, e);
        }
    }

    public T getNow(T defaultValue) {
        try {
            return isDone() ? get() : defaultValue;
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }
    }

    public <U> U get(final Function<? super T, ? extends U> action) {
        try {
            return action.apply(get());
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }
    }

    public <U> U get(final long timeout, final TimeUnit unit, final Function<? super T, ? extends U> action) {
        try {
            return action.apply(get(timeout, unit));
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            throw N.toRuntimeException(e);
        }
    }

    public <U> U get(final BiFunction<? super T, Throwable, ? extends U> action) {
        final Pair<T, Throwable> result = get2();
        return action.apply(result.left, result.right);
    }

    public <U> U get(final long timeout, final TimeUnit unit, final BiFunction<? super T, Throwable, ? extends U> action) {
        final Pair<T, Throwable> result = get2(timeout, unit);
        return action.apply(result.left, result.right);
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
    //    public void complete(BiConsumer<? super T, Throwable> action) {
    //        final Pair<T, Throwable> result = get2();
    //        action.accept(result.left, result.right);
    //    }

    <U> CompletableFuture<U> thenApply(final Function<? super T, ? extends U> action) {
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
                return action.apply(future.get());
            }

            @Override
            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                return action.apply(future.get(timeout, unit));
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

    //    <U> CompletableFuture<U> thenApply(final BiFunction<? super T, Throwable, ? extends U> action) {
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
    //                final Pair<T, Throwable> result = get2();
    //
    //                return action.apply(result.left, result.right);
    //            }
    //
    //            @Override
    //            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                final Pair<T, Throwable> result = get2(timeout, unit);
    //
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
    //    public <U> CompletableFuture<Void> thenAccept(final BiConsumer<? super T, Throwable> action) {
    //        return thenApply(new BiFunction<T, Throwable, Void>() {
    //            @Override
    //            public Void apply(T t, Throwable e) {
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
    //    public <U, R> CompletableFuture<R> thenCombine(final CompletableFuture<? extends U> other, final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
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
    //                final Pair<T, Throwable> result = get2();
    //                final Pair<? extends U, Throwable> result2 = other.get2();
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
    //                final Pair<T, Throwable> result = CompletableFuture.this.get2(timeout, unit);
    //                final Pair<? extends U, Throwable> result2 = other.get2(N.max(0, endTime - N.currentMillis()), TimeUnit.MILLISECONDS);
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
    //    public <U> CompletableFuture<Void> thenAcceptBoth(final CompletableFuture<? extends U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
    //        return thenCombine(other, new Function<Tuple4<T, Throwable, U, Throwable>, Void>() {
    //            @Override
    //            public Void apply(Tuple4<T, Throwable, U, Throwable> t) {
    //                action.accept(t);
    //                return null;
    //            }
    //        });
    //    }

    public CompletableFuture<Void> thenRun(final Runnable action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                get();
                action.run();
                return null;
            }
        });
    }

    public CompletableFuture<Void> thenRun(final Consumer<? super T> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                action.accept(get());
                return null;
            }
        });
    }

    public CompletableFuture<Void> thenRun(final BiConsumer<? super T, Throwable> action) {
        return execute(new Runnable() {
            @Override
            public void run() {
                final Pair<T, Throwable> result = get2();
                action.accept(result.left, result.right);
            }
        });
    }

    public <U> CompletableFuture<U> thenCall(final Try.Callable<U, RuntimeException> action) {
        return execute(new Callable<U>() {
            @Override
            public U call() throws Exception {
                get();

                return action.call();
            }
        });
    }

    public <R> CompletableFuture<R> thenCall(final Function<? super T, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                return action.apply(get());
            }
        });
    }

    public <R> CompletableFuture<R> thenCall(final BiFunction<? super T, Throwable, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Throwable> result = get2();
                return action.apply(result.left, result.right);
            }
        });
    }

    public CompletableFuture<Void> thenRunOnUI(final Runnable action) {
        return withUIExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunOnUI(final Consumer<? super T> action) {
        return withUIExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunOnUI(final BiConsumer<? super T, Throwable> action) {
        return withUIExecutor().thenRun(action);
    }

    public <U> CompletableFuture<U> thenCallOnUI(final Try.Callable<U, RuntimeException> action) {
        return withUIExecutor().thenCall(action);
    }

    public <R> CompletableFuture<R> thenCallOnUI(final Function<? super T, R> action) {
        return withUIExecutor().thenCall(action);
    }

    public <R> CompletableFuture<R> thenCallOnUI(final BiFunction<? super T, Throwable, R> action) {
        return withUIExecutor().thenCall(action);
    }

    public CompletableFuture<Void> thenRunByTP(final Runnable action) {
        return withTPExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunByTP(final Consumer<? super T> action) {
        return withTPExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunByTP(final BiConsumer<? super T, Throwable> action) {
        return withTPExecutor().thenRun(action);
    }

    public <U> CompletableFuture<U> thenCallByTP(final Try.Callable<U, RuntimeException> action) {
        return withTPExecutor().thenCall(action);
    }

    public <R> CompletableFuture<R> thenCallByTP(final Function<? super T, R> action) {
        return withTPExecutor().thenCall(action);
    }

    public <R> CompletableFuture<R> thenCallByTP(final BiFunction<? super T, Throwable, R> action) {
        return withTPExecutor().thenCall(action);
    }

    public CompletableFuture<Void> runAfterBoth(final CompletableFuture<?> other, final Runnable action) {
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

    public <U> CompletableFuture<Void> runAfterBoth(final CompletableFuture<U> other, final BiConsumer<T, U> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                action.accept(get(), other.get());
                return null;
            }
        }, other);
    }

    public <U> CompletableFuture<Void> runAfterBoth(final CompletableFuture<U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return execute(new Runnable() {
            @Override
            public void run() {
                final Pair<T, Throwable> result = get2();
                final Pair<U, Throwable> result2 = other.get2();

                action.accept(Tuple.of(result.left, result.right, result2.left, result.right));
            }
        }, other);
    }

    public <U> CompletableFuture<U> callAfterBoth(final CompletableFuture<?> other, final Try.Callable<U, RuntimeException> action) {
        return execute(new Callable<U>() {
            @Override
            public U call() throws Exception {
                get();
                other.get();
                return action.call();
            }
        }, other);
    }

    public <U, R> CompletableFuture<R> callAfterBoth(final CompletableFuture<U> other, final BiFunction<T, U, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                return action.apply(get(), other.get());
            }
        }, other);
    }

    public <U, R> CompletableFuture<R> callAfterBoth(final CompletableFuture<U> other, final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Throwable> result = get2();
                final Pair<U, Throwable> result2 = other.get2();

                return action.apply(Tuple.of(result.left, result.right, result2.left, result.right));
            }
        }, other);
    }

    public CompletableFuture<Void> runOnUIAfterBoth(final CompletableFuture<?> other, final Runnable action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runOnUIAfterBoth(final CompletableFuture<U> other, final BiConsumer<T, U> action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runOnUIAfterBoth(final CompletableFuture<U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<U> callOnUIAfterBoth(final CompletableFuture<?> other, final Try.Callable<U, RuntimeException> action) {
        return withUIExecutor().callAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> callOnUIAfterBoth(final CompletableFuture<U> other, final BiFunction<T, U, R> action) {
        return withUIExecutor().callAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> callOnUIAfterBoth(final CompletableFuture<U> other, final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return withUIExecutor().callAfterBoth(other, action);
    }

    public CompletableFuture<Void> runByTPAfterBoth(final CompletableFuture<?> other, final Runnable action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runByTPAfterBoth(final CompletableFuture<U> other, final BiConsumer<T, U> action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runByTPAfterBoth(final CompletableFuture<U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<U> callByTPAfterBoth(final CompletableFuture<?> other, final Try.Callable<U, RuntimeException> action) {
        return withTPExecutor().callAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> callByTPAfterBoth(final CompletableFuture<U> other, final BiFunction<T, U, R> action) {
        return withTPExecutor().callAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> callByTPAfterBoth(final CompletableFuture<U> other, final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return withTPExecutor().callAfterBoth(other, action);
    }

    public CompletableFuture<Void> runAfterEither(final CompletableFuture<?> other, final Runnable action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                Futures.anyOf(N.asList(CompletableFuture.this, other)).get();
                action.run();
                return null;
            }
        }, other);
    }

    public CompletableFuture<Void> runAfterEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                final T result = Futures.anyOf(N.asList(CompletableFuture.this, other)).get();
                action.accept(result);
                return null;
            }
        }, other);
    }

    public CompletableFuture<Void> runAfterEither(final CompletableFuture<? extends T> other, final BiConsumer<? super T, Throwable> action) {
        return execute(new Runnable() {
            @Override
            public void run() {
                final Pair<T, Throwable> result = Futures.anyOf(N.asList(CompletableFuture.this, other)).get2();

                action.accept(result.left, result.right);
            }
        }, other);
    }

    public <U> CompletableFuture<U> callAfterEither(final CompletableFuture<?> other, final Try.Callable<U, RuntimeException> action) {
        return execute(new Callable<U>() {
            @Override
            public U call() throws Exception {
                Futures.anyOf(N.asList(CompletableFuture.this, other)).get();

                return action.call();
            }
        }, other);
    }

    public <R> CompletableFuture<R> callAfterEither(final CompletableFuture<? extends T> other, final Function<? super T, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final T result = Futures.anyOf(N.asList(CompletableFuture.this, other)).get();

                return action.apply(result);
            }
        }, other);
    }

    public <R> CompletableFuture<R> callAfterEither(final CompletableFuture<? extends T> other, final BiFunction<? super T, Throwable, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Throwable> result = Futures.anyOf(N.asList(CompletableFuture.this, other)).get2();

                return action.apply(result.left, result.right);
            }
        }, other);
    }

    public CompletableFuture<Void> runOnUIAfterEither(final CompletableFuture<?> other, final Runnable action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runOnUIAfterEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runOnUIAfterEither(final CompletableFuture<? extends T> other, final BiConsumer<? super T, Throwable> action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<U> callOnUIAfterEither(final CompletableFuture<?> other, final Try.Callable<U, RuntimeException> action) {
        return withUIExecutor().callAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> callOnUIAfterEither(final CompletableFuture<? extends T> other, final Function<? super T, R> action) {
        return withUIExecutor().callAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> callOnUIAfterEither(final CompletableFuture<? extends T> other, final BiFunction<? super T, Throwable, R> action) {
        return withUIExecutor().callAfterEither(other, action);
    }

    public CompletableFuture<Void> runByTPAfterEither(final CompletableFuture<?> other, final Runnable action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runByTPAfterEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runByTPAfterEither(final CompletableFuture<? extends T> other, final BiConsumer<? super T, Throwable> action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<U> callByTPAfterEither(final CompletableFuture<?> other, final Try.Callable<U, RuntimeException> action) {
        return withTPExecutor().callAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> callByTPAfterEither(final CompletableFuture<? extends T> other, final Function<? super T, R> action) {
        return withTPExecutor().callAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> callByTPAfterEither(final CompletableFuture<? extends T> other, final BiFunction<? super T, Throwable, R> action) {
        return withTPExecutor().callAfterEither(other, action);
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
    //    public CompletableFuture<T> exceptionally(final Function<Throwable, ? extends T> action) {
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
    //                } catch (Throwable e) {
    //                    return action.apply(e);
    //                }
    //            }
    //
    //            @Override
    //            public T get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                try {
    //                    return future.get(timeout, unit);
    //                } catch (Throwable e) {
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

    //    public CompletableFuture<T> whenComplete(final BiConsumer<? super T, ? super Throwable> action) {
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
    //                final Pair<T, Throwable> result = get2();
    //
    //                if (result.right != null) {
    //                    try {
    //                        action.accept(result.left, result.right);
    //                    } catch (Throwable e) {
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
    //                final Pair<T, Throwable> result = get2(timeout, unit);
    //
    //                if (result.right != null) {
    //                    try {
    //                        action.accept(result.left, result.right);
    //                    } catch (Throwable e) {
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
    //    public <U> CompletableFuture<U> handle(final BiFunction<? super T, Throwable, ? extends U> action) {
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
    //                final Pair<T, Throwable> result = get2();
    //                return action.apply(result.left, result.right);
    //            }
    //
    //            @Override
    //            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                final Pair<T, Throwable> result = get2(timeout, unit);
    //                return action.apply(result.left, result.right);
    //            }
    //        }, asyncExecutor);
    //    }

    private CompletableFuture<Void> execute(final Runnable command) {
        return execute(command, null);
    }

    private CompletableFuture<Void> execute(final Runnable command, final CompletableFuture<?> other) {
        return execute(new FutureTask<Void>(command, null), other);
    }

    private <U> CompletableFuture<U> execute(final Callable<U> command) {
        return execute(command, null);
    }

    private <U> CompletableFuture<U> execute(final Callable<U> command, final CompletableFuture<?> other) {
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

    public CompletableFuture<T> with(final Executor executor, final long delay, final TimeUnit unit) {
        N.requireNonNull(executor);

        return new CompletableFuture<T>(new Future<T>() {
            private final long delayEndTime = N.currentMillis() + unit.toMillis(delay);
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

                    N.sleep(delayEndTime - N.currentMillis());
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

    public CompletableFuture<T> withUIExecutor() {
        return with(Async.UI_EXECUTOR);
    }

    public CompletableFuture<T> withUIExecutor(final long delay) {
        return with(Async.UI_EXECUTOR, delay, TimeUnit.MILLISECONDS);
    }

    public CompletableFuture<T> withSerialExecutor() {
        return with(Async.SERIAL_EXECUTOR);
    }

    public CompletableFuture<T> withSerialExecutor(final long delay) {
        return with(Async.SERIAL_EXECUTOR, delay, TimeUnit.MILLISECONDS);
    }

    /**
     * With Thread Pool Executor.
     * @return
     */
    public CompletableFuture<T> withTPExecutor() {
        return with(Async.TP_EXECUTOR);
    }

    public CompletableFuture<T> withTPExecutor(final long delay) {
        return with(Async.TP_EXECUTOR, delay, TimeUnit.MILLISECONDS);
    }
}
