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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import com.landawn.abacus.android.util.AsyncExecutor.UIExecutor;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.MutableBoolean;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Pair;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.Tuple;
import com.landawn.abacus.util.Tuple.Tuple2;
import com.landawn.abacus.util.Tuple.Tuple3;
import com.landawn.abacus.util.Tuple.Tuple4;
import com.landawn.abacus.util.Tuple.Tuple5;
import com.landawn.abacus.util.Tuple.Tuple6;
import com.landawn.abacus.util.Tuple.Tuple7;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.TriFunction;

/**
 * The <code>action</code> in all <code>*run*</code> methods will be executed asynchronously by the specified or default <code>Executor</code> eventually.
 * The <code>action</code> in other methods (<code>apply/accept/combine/</code>) won't be executed until they're called/continued by <code>complete/get/*run*</code> methods and executed in the thread where the <code>complete/get/run/call</code> is called.
 * 
 * <br />
 * <br />
 * <code>TP</code> -> <code>Thread Pool</code>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class CompletableFuture<T> implements Future<T> {
    private static final Logger logger = LoggerFactory.getLogger(CompletableFuture.class);

    private static final Consumer<Object> EMPTY_CONSUMER = new Consumer<Object>() {
        @Override
        public void accept(Object t) {
            // Do nothing.                
        }
    };

    @SuppressWarnings("rawtypes")
    private static final Function IDENTITY_FUNCTION = new Function<Object, Object>() {
        @Override
        public Object apply(Object t) {
            return t;
        }
    };

    private final Future<T> future;
    private final Executor asyncExecutor;
    private final long delay;

    CompletableFuture(final Future<T> future, final Executor asyncExecutor) {
        this(future, asyncExecutor, 0);
    }

    CompletableFuture(final Future<T> future, final Executor asyncExecutor, final long delay) {
        this.future = future;
        this.asyncExecutor = asyncExecutor;
        this.delay = delay;
    }

    public static CompletableFuture<Void> run(final Runnable action) {
        return run(action, AsyncExecutor.SERIAL_EXECUTOR);
    }

    public static <T> CompletableFuture<T> run(final Try.Callable<T> action) {
        return run(action, AsyncExecutor.SERIAL_EXECUTOR);
    }

    public static CompletableFuture<Void> runWithUIExecutor(final Runnable action) {
        return run(action, AsyncExecutor.UI_EXECUTOR);
    }

    public static <T> CompletableFuture<T> runWithUIExecutor(final Try.Callable<T> action) {
        return run(action, AsyncExecutor.UI_EXECUTOR);
    }

    //    public static CompletableFuture<Void> runWithSerialExecutor(final Runnable action) {
    //        return run(action, AsyncExecutor.SERIAL_EXECUTOR);
    //    }
    //
    //    public static <T> CompletableFuture<T> runWithSerialExecutor(final Try.Callable<T> action) {
    //        return run(action, AsyncExecutor.SERIAL_EXECUTOR);
    //    }

    public static CompletableFuture<Void> runWithTPExecutor(final Runnable action) {
        return run(action, AsyncExecutor.TP_EXECUTOR);
    }

    public static <T> CompletableFuture<T> runWithTPExecutor(final Try.Callable<T> action) {
        return run(action, AsyncExecutor.TP_EXECUTOR);
    }

    public static CompletableFuture<Void> run(final Runnable action, final Executor executor) {
        final FutureTask<Void> futureTask = new FutureTask<>(action, null);

        executor.execute(futureTask);

        return new CompletableFuture<>(futureTask, executor);
    }

    public static <T> CompletableFuture<T> run(final Try.Callable<T> action, final Executor executor) {
        final FutureTask<T> futureTask = new FutureTask<>(action);

        executor.execute(futureTask);

        return new CompletableFuture<>(futureTask, executor);
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
        }, AsyncExecutor.SERIAL_EXECUTOR);
    }

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

    public void complete() throws InterruptedException, ExecutionException {
        get();
    }

    public void complete(Consumer<? super T> action) {
        try {
            action.accept(get());
        } catch (InterruptedException | ExecutionException e) {
            throw N.toRuntimeException(e);
        }
    }

    public void complete(BiConsumer<? super T, Throwable> action) {
        T result = null;

        try {
            result = get();
        } catch (Throwable e) {
            action.accept(null, e);
            return;
        }

        action.accept(result, null);
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
        T result = null;

        try {
            result = get();
        } catch (Throwable e) {
            return action.apply(null, e);
        }

        return action.apply(result, null);
    }

    public <U> U get(final long timeout, final TimeUnit unit, final BiFunction<? super T, Throwable, ? extends U> action) {
        T result = null;

        try {
            result = get(timeout, unit);
        } catch (Throwable e) {
            return action.apply(null, e);
        }

        return action.apply(result, null);
    }

    public <U> CompletableFuture<U> thenApply(final Function<? super T, ? extends U> action) {
        return new CompletableFuture<>(new Future<U>() {
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
        }, asyncExecutor);
    }

    public <U> CompletableFuture<U> thenApply(final BiFunction<? super T, Throwable, ? extends U> action) {
        return new CompletableFuture<>(new Future<U>() {
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
                final Pair<T, Throwable> result = get2();

                return action.apply(result.left, result.right);
            }

            @Override
            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                final Pair<T, Throwable> result = get2(timeout, unit);

                return action.apply(result.left, result.right);
            }
        }, asyncExecutor);
    }

    public <U> CompletableFuture<Void> thenAccept(final Consumer<? super T> action) {
        return thenApply(new Function<T, Void>() {
            @Override
            public Void apply(T t) {
                action.accept(t);
                return null;
            }
        });
    }

    public <U> CompletableFuture<Void> thenAccept(final BiConsumer<? super T, Throwable> action) {
        return thenApply(new BiFunction<T, Throwable, Void>() {
            @Override
            public Void apply(T t, Throwable e) {
                action.accept(t, e);
                return null;
            }
        });
    }

    public <U, R> CompletableFuture<R> thenCombine(final CompletableFuture<? extends U> other, final BiFunction<? super T, ? super U, ? extends R> action) {
        return new CompletableFuture<>(new Future<R>() {
            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                return future.cancel(mayInterruptIfRunning) && other.future.cancel(mayInterruptIfRunning);
            }

            @Override
            public boolean isCancelled() {
                return future.isCancelled() || other.future.isCancelled();
            }

            @Override
            public boolean isDone() {
                return future.isDone() && other.future.isDone();
            }

            @Override
            public R get() throws InterruptedException, ExecutionException {
                return action.apply(future.get(), other.future.get());
            }

            @Override
            public R get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                final long timeoutInMillis = unit.toMillis(timeout);
                final long now = N.currentMillis();
                final long endTime = timeoutInMillis > Long.MAX_VALUE - now ? Long.MAX_VALUE : now + timeoutInMillis;

                final T result = future.get(timeout, unit);
                final U otherResult = other.future.get(N.max(0, endTime - N.currentMillis()), TimeUnit.MILLISECONDS);

                return action.apply(result, otherResult);
            }
        }, asyncExecutor);
    }

    public <U, R> CompletableFuture<R> thenCombine(final CompletableFuture<? extends U> other, final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return new CompletableFuture<>(new Future<R>() {
            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                return future.cancel(mayInterruptIfRunning) && other.future.cancel(mayInterruptIfRunning);
            }

            @Override
            public boolean isCancelled() {
                return future.isCancelled() || other.future.isCancelled();
            }

            @Override
            public boolean isDone() {
                return future.isDone() && other.future.isDone();
            }

            @Override
            public R get() throws InterruptedException, ExecutionException {
                final Pair<T, Throwable> result = get2();
                final Pair<? extends U, Throwable> result2 = other.get2();

                return action.apply(Tuple.of(result.left, result.right, (U) result2.left, result.right));
            }

            @Override
            public R get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                final long timeoutInMillis = unit.toMillis(timeout);
                final long now = N.currentMillis();
                final long endTime = timeoutInMillis > Long.MAX_VALUE - now ? Long.MAX_VALUE : now + timeoutInMillis;

                final Pair<T, Throwable> result = CompletableFuture.this.get2(timeout, unit);
                final Pair<? extends U, Throwable> result2 = other.get2(N.max(0, endTime - N.currentMillis()), TimeUnit.MILLISECONDS);

                return action.apply(Tuple.of(result.left, result.right, (U) result2.left, result.right));
            }
        }, asyncExecutor);
    }

    public <U> CompletableFuture<Void> thenAcceptBoth(final CompletableFuture<? extends U> other, final BiConsumer<? super T, ? super U> action) {
        return thenCombine(other, new BiFunction<T, U, Void>() {
            @Override
            public Void apply(T t, U u) {
                action.accept(t, u);
                return null;
            }
        });
    }

    public <U> CompletableFuture<Void> thenAcceptBoth(final CompletableFuture<? extends U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return thenCombine(other, new Function<Tuple4<T, Throwable, U, Throwable>, Void>() {
            @Override
            public Void apply(Tuple4<T, Throwable, U, Throwable> t) {
                action.accept(t);
                return null;
            }
        });
    }

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

    public <U> CompletableFuture<U> thenRun(final Try.Callable<U> action) {
        return execute(new Callable<U>() {
            @Override
            public U call() throws Exception {
                get();

                return action.call();
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
                T result = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    action.accept(null, e);
                    return;
                }

                action.accept(result, null);
            }
        });
    }

    public <R> CompletableFuture<R> thenRun(final Function<? super T, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                return action.apply(get());
            }
        });
    }

    public <R> CompletableFuture<R> thenRun(final BiFunction<? super T, Throwable, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                T result = null;

                try {
                    result = get();
                } catch (Throwable e) {
                    return action.apply(null, e);
                }

                return action.apply(result, null);
            }
        });
    }

    public CompletableFuture<Void> thenRunWithUIExecutor(final Runnable action) {
        return withUIExecutor().thenRun(action);
    }

    public <U> CompletableFuture<U> thenRunWithUIExecutor(final Try.Callable<U> action) {
        return withUIExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithUIExecutor(final Consumer<? super T> action) {
        return withUIExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithUIExecutor(final BiConsumer<? super T, Throwable> action) {
        return withUIExecutor().thenRun(action);
    }

    public <R> CompletableFuture<R> thenRunWithUIExecutor(final Function<? super T, R> action) {
        return withUIExecutor().thenRun(action);
    }

    public <R> CompletableFuture<R> thenRunWithUIExecutor(final BiFunction<? super T, Throwable, R> action) {
        return withUIExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithSerialExecutor(final Runnable action) {
        return withSerialExecutor().thenRun(action);
    }

    public <U> CompletableFuture<U> thenRunWithSerialExecutor(final Try.Callable<U> action) {
        return withSerialExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithSerialExecutor(final Consumer<? super T> action) {
        return withSerialExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithSerialExecutor(final BiConsumer<? super T, Throwable> action) {
        return withSerialExecutor().thenRun(action);
    }

    public <R> CompletableFuture<R> thenRunWithSerialExecutor(final Function<? super T, R> action) {
        return withSerialExecutor().thenRun(action);
    }

    public <R> CompletableFuture<R> thenRunWithSerialExecutor(final BiFunction<? super T, Throwable, R> action) {
        return withSerialExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithTPExecutor(final Runnable action) {
        return withTPExecutor().thenRun(action);
    }

    public <U> CompletableFuture<U> thenRunWithTPExecutor(final Try.Callable<U> action) {
        return withTPExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithTPExecutor(final Consumer<? super T> action) {
        return withTPExecutor().thenRun(action);
    }

    public CompletableFuture<Void> thenRunWithTPExecutor(final BiConsumer<? super T, Throwable> action) {
        return withTPExecutor().thenRun(action);
    }

    public <R> CompletableFuture<R> thenRunWithTPExecutor(final Function<? super T, R> action) {
        return withTPExecutor().thenRun(action);
    }

    public <R> CompletableFuture<R> thenRunWithTPExecutor(final BiFunction<? super T, Throwable, R> action) {
        return withTPExecutor().thenRun(action);
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
        });
    }

    public <U> CompletableFuture<U> runAfterBoth(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return execute(new Callable<U>() {
            @Override
            public U call() throws Exception {
                get();
                other.get();
                return action.call();
            }
        });
    }

    public <U> CompletableFuture<Void> runAfterBoth(final CompletableFuture<U> other, final BiConsumer<T, U> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                action.accept(get(), other.get());
                return null;
            }
        });
    }

    public <U> CompletableFuture<Void> runAfterBoth(final CompletableFuture<U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return execute(new Runnable() {
            @Override
            public void run() {
                final Pair<T, Throwable> result = get2();
                final Pair<U, Throwable> result2 = other.get2();

                action.accept(Tuple.of(result.left, result.right, result2.left, result.right));
            }
        });
    }

    public <U, R> CompletableFuture<R> runAfterBoth(final CompletableFuture<U> other, final BiFunction<T, U, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                return action.apply(get(), other.get());
            }
        });
    }

    public <U, R> CompletableFuture<R> runAfterBoth(final CompletableFuture<U> other, final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Throwable> result = get2();
                final Pair<U, Throwable> result2 = other.get2();

                return action.apply(Tuple.of(result.left, result.right, result2.left, result.right));
            }
        });
    }

    public CompletableFuture<Void> runWithUIExecutorAfterBoth(final CompletableFuture<?> other, final Runnable action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<U> runWithUIExecutorAfterBoth(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runWithUIExecutorAfterBoth(final CompletableFuture<U> other, final BiConsumer<T, U> action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runWithUIExecutorAfterBoth(final CompletableFuture<U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> runWithUIExecutorAfterBoth(final CompletableFuture<U> other, final BiFunction<T, U, R> action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> runWithUIExecutorAfterBoth(final CompletableFuture<U> other,
            final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return withUIExecutor().runAfterBoth(other, action);
    }

    public CompletableFuture<Void> runWithSerialExecutorAfterBoth(final CompletableFuture<?> other, final Runnable action) {
        return withSerialExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<U> runWithSerialExecutorAfterBoth(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return withSerialExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runWithSerialExecutorAfterBoth(final CompletableFuture<U> other, final BiConsumer<T, U> action) {
        return withSerialExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runWithSerialExecutorAfterBoth(final CompletableFuture<U> other,
            final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return withSerialExecutor().runAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> runWithSerialExecutorAfterBoth(final CompletableFuture<U> other, final BiFunction<T, U, R> action) {
        return withSerialExecutor().runAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> runWithSerialExecutorAfterBoth(final CompletableFuture<U> other,
            final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return withSerialExecutor().runAfterBoth(other, action);
    }

    public CompletableFuture<Void> runWithTPExecutorAfterBoth(final CompletableFuture<?> other, final Runnable action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<U> runWithTPExecutorAfterBoth(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runWithTPExecutorAfterBoth(final CompletableFuture<U> other, final BiConsumer<T, U> action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U> CompletableFuture<Void> runWithTPExecutorAfterBoth(final CompletableFuture<U> other, final Consumer<Tuple4<T, Throwable, U, Throwable>> action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> runWithTPExecutorAfterBoth(final CompletableFuture<U> other, final BiFunction<T, U, R> action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public <U, R> CompletableFuture<R> runWithTPExecutorAfterBoth(final CompletableFuture<U> other,
            final Function<Tuple4<T, Throwable, U, Throwable>, R> action) {
        return withTPExecutor().runAfterBoth(other, action);
    }

    public CompletableFuture<Void> runAfterEither(final CompletableFuture<?> other, final Runnable action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                ((CompletableFuture<Object>) CompletableFuture.this).acceptEither((other), EMPTY_CONSUMER).get();
                action.run();
                return null;
            }
        });
    }

    public <U> CompletableFuture<U> runAfterEither(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return execute(new Callable<U>() {
            @Override
            public U call() throws Exception {
                ((CompletableFuture<Object>) CompletableFuture.this).acceptEither((other), EMPTY_CONSUMER).get();

                return action.call();
            }
        });
    }

    public CompletableFuture<Void> runAfterEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                final T result = applyToEither(other, (Function<T, T>) IDENTITY_FUNCTION).get();
                action.accept(result);
                return null;
            }
        });
    }

    public CompletableFuture<Void> runAfterEither(final CompletableFuture<? extends T> other, final BiConsumer<? super T, Throwable> action) {
        return execute(new Runnable() {
            @Override
            public void run() {
                final Pair<T, Throwable> result = applyToEither(other, (Function<T, T>) IDENTITY_FUNCTION).get2();

                action.accept(result.left, result.right);
            }
        });
    }

    public <R> CompletableFuture<R> runAfterEither(final CompletableFuture<? extends T> other, final Function<? super T, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final T result = applyToEither(other, (Function<T, T>) IDENTITY_FUNCTION).get();

                return action.apply(result);
            }
        });
    }

    public <R> CompletableFuture<R> runAfterEither(final CompletableFuture<? extends T> other, final BiFunction<? super T, Throwable, R> action) {
        return execute(new Callable<R>() {
            @Override
            public R call() throws Exception {
                final Pair<T, Throwable> result = applyToEither(other, (Function<T, T>) IDENTITY_FUNCTION).get2();

                return action.apply(result.left, result.right);
            }
        });
    }

    public CompletableFuture<Void> runWithUIExecutorAfterEither(final CompletableFuture<?> other, final Runnable action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<U> runWithUIExecutorAfterEither(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runWithUIExecutorAfterEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runWithUIExecutorAfterEither(final CompletableFuture<? extends T> other, final BiConsumer<? super T, Throwable> action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> runWithUIExecutorAfterEither(final CompletableFuture<? extends T> other, final Function<? super T, R> action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> runWithUIExecutorAfterEither(final CompletableFuture<? extends T> other,
            final BiFunction<? super T, Throwable, R> action) {
        return withUIExecutor().runAfterEither(other, action);
    }

    public CompletableFuture<Void> runWithSerialExecutorAfterEither(final CompletableFuture<?> other, final Runnable action) {
        return withSerialExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<U> runWithSerialExecutorAfterEither(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return withSerialExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runWithSerialExecutorAfterEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return withSerialExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runWithSerialExecutorAfterEither(final CompletableFuture<? extends T> other,
            final BiConsumer<? super T, Throwable> action) {
        return withSerialExecutor().runAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> runWithSerialExecutorAfterEither(final CompletableFuture<? extends T> other, final Function<? super T, R> action) {
        return withSerialExecutor().runAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> runWithSerialExecutorAfterEither(final CompletableFuture<? extends T> other,
            final BiFunction<? super T, Throwable, R> action) {
        return withSerialExecutor().runAfterEither(other, action);
    }

    public CompletableFuture<Void> runWithTPExecutorAfterEither(final CompletableFuture<?> other, final Runnable action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<U> runWithTPExecutorAfterEither(final CompletableFuture<?> other, final Try.Callable<U> action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runWithTPExecutorAfterEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<Void> runWithTPExecutorAfterEither(final CompletableFuture<? extends T> other, final BiConsumer<? super T, Throwable> action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> runWithTPExecutorAfterEither(final CompletableFuture<? extends T> other, final Function<? super T, R> action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U, R> CompletableFuture<R> runWithTPExecutorAfterEither(final CompletableFuture<? extends T> other,
            final BiFunction<? super T, Throwable, R> action) {
        return withTPExecutor().runAfterEither(other, action);
    }

    public <U> CompletableFuture<U> applyToEither(final CompletableFuture<? extends T> other, final Function<? super T, U> action) {
        return new CompletableFuture<>(new Future<U>() {
            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                return future.cancel(mayInterruptIfRunning) && other.future.cancel(mayInterruptIfRunning);
            }

            @Override
            public boolean isCancelled() {
                return future.isCancelled() && other.future.isCancelled();
            }

            @Override
            public boolean isDone() {
                return future.isDone() || other.future.isDone();
            }

            @Override
            public U get() throws InterruptedException, ExecutionException {
                final OptionalNullable<T> anySuccessResultOf = resultOfAnySuccess(CompletableFuture.this, other);

                if (anySuccessResultOf.isPresent()) {
                    return action.apply(anySuccessResultOf.get());
                }

                return action.apply(CompletableFuture.this.get());
            }

            @Override
            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                final OptionalNullable<T> anySuccessResultOf = resultOfAnySuccess(Arrays.asList(CompletableFuture.this, other), unit.toMillis(timeout));

                if (anySuccessResultOf.isPresent()) {
                    return action.apply(anySuccessResultOf.get());
                }

                return action.apply(CompletableFuture.this.get());
            }
        }, asyncExecutor);
    }

    public CompletableFuture<Void> acceptEither(final CompletableFuture<? extends T> other, final Consumer<? super T> action) {
        return applyToEither(other, new Function<T, Void>() {
            @Override
            public Void apply(T t) {
                action.accept(t);
                return null;
            }
        });
    }

    public CompletableFuture<T> exceptionally(final Function<Throwable, ? extends T> action) {
        return new CompletableFuture<>(new Future<T>() {
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
            public T get() throws InterruptedException, ExecutionException {
                try {
                    return future.get();
                } catch (Throwable e) {
                    return action.apply(e);
                }
            }

            @Override
            public T get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                try {
                    return future.get(timeout, unit);
                } catch (Throwable e) {
                    return action.apply(e);
                }
            }
        }, asyncExecutor);
    }

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
    //                T result = null;
    //
    //                try {
    //                    result = future.get();
    //                } catch (Throwable e) {
    //                    return action.apply(null, e);
    //                }
    //
    //                return action.apply(result, null);
    //            }
    //
    //            @Override
    //            public U get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
    //                T result = null;
    //
    //                try {
    //                    result = future.get(timeout, unit);
    //                } catch (Throwable e) {
    //                    return action.apply(null, e);
    //                }
    //
    //                return action.apply(result, null);
    //            }
    //        }, asyncExecutor);
    //    }

    private CompletableFuture<Void> execute(final Runnable command) {
        return execute(new FutureTask<Void>(command, null));
    }

    private <U> CompletableFuture<U> execute(final Callable<U> command) {
        return execute(new FutureTask<>(command));
    }

    private <U> CompletableFuture<U> execute(final FutureTask<U> futureTask) {
        if (delay > 0) {
            if (asyncExecutor instanceof UIExecutor) {
                ((UIExecutor) asyncExecutor).execute(futureTask, delay);
            } else {
                N.sleep(delay);
                asyncExecutor.execute(futureTask);
            }
        } else {
            asyncExecutor.execute(futureTask);
        }

        return new CompletableFuture<>(futureTask, asyncExecutor);
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

    public CompletableFuture<T> with(Executor executor, long delay, TimeUnit unit) {
        N.requireNonNull(executor);

        return new CompletableFuture<>(new Future<T>() {
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
            public T get() throws InterruptedException, ExecutionException {
                return future.get();
            }

            @Override
            public T get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                return future.get(timeout, unit);
            }
        }, executor, delay <= 0 ? 0 : unit.toMillis(delay));
    }

    public CompletableFuture<T> withUIExecutor() {
        return with(AsyncExecutor.UI_EXECUTOR);
    }

    public CompletableFuture<T> withUIExecutor(final long delay) {
        return with(AsyncExecutor.UI_EXECUTOR, delay, TimeUnit.MILLISECONDS);
    }

    public CompletableFuture<T> withSerialExecutor() {
        return with(AsyncExecutor.SERIAL_EXECUTOR);
    }

    /**
     * With Thread Pool Executor.
     * @return
     */
    public CompletableFuture<T> withTPExecutor() {
        return with(AsyncExecutor.TP_EXECUTOR);
    }

    public static <T1, T2> CompletableFuture<Tuple2<T1, T2>> combine(final CompletableFuture<? extends T1> cf1, final CompletableFuture<? extends T2> cf2) {
        return ofAll(N.asList(cf1, cf2)).thenApply(new Function<List<Object>, Tuple2<T1, T2>>() {
            @Override
            public Tuple2<T1, T2> apply(List<Object> t) {
                return Tuple.of((T1) t.get(0), (T2) t.get(1));
            }
        });
    }

    public static <T1, T2, T3> CompletableFuture<Tuple3<T1, T2, T3>> combine(final CompletableFuture<? extends T1> cf1,
            final CompletableFuture<? extends T2> cf2, final CompletableFuture<? extends T3> cf3) {
        return ofAll(N.asList(cf1, cf2, cf3)).thenApply(new Function<List<Object>, Tuple3<T1, T2, T3>>() {
            @Override
            public Tuple3<T1, T2, T3> apply(List<Object> t) {
                return Tuple.of((T1) t.get(0), (T2) t.get(1), (T3) t.get(2));
            }
        });
    }

    public static <T1, T2, T3, T4> CompletableFuture<Tuple4<T1, T2, T3, T4>> combine(final CompletableFuture<? extends T1> cf1,
            final CompletableFuture<? extends T2> cf2, final CompletableFuture<? extends T3> cf3, final CompletableFuture<? extends T4> cf4) {
        return ofAll(N.asList(cf1, cf2, cf3, cf4)).thenApply(new Function<List<Object>, Tuple4<T1, T2, T3, T4>>() {
            @Override
            public Tuple4<T1, T2, T3, T4> apply(List<Object> t) {
                return Tuple.of((T1) t.get(0), (T2) t.get(1), (T3) t.get(2), (T4) t.get(3));
            }
        });
    }

    public static <T1, T2, T3, T4, T5> CompletableFuture<Tuple5<T1, T2, T3, T4, T5>> combine(final CompletableFuture<? extends T1> cf1,
            final CompletableFuture<? extends T2> cf2, final CompletableFuture<? extends T3> cf3, final CompletableFuture<? extends T4> cf4,
            final CompletableFuture<? extends T5> cf5) {
        return ofAll(N.asList(cf1, cf2, cf3, cf4, cf5)).thenApply(new Function<List<Object>, Tuple5<T1, T2, T3, T4, T5>>() {
            @Override
            public Tuple5<T1, T2, T3, T4, T5> apply(List<Object> t) {
                return Tuple.of((T1) t.get(0), (T2) t.get(1), (T3) t.get(2), (T4) t.get(3), (T5) t.get(4));
            }
        });
    }

    public static <T1, T2, T3, T4, T5, T6> CompletableFuture<Tuple6<T1, T2, T3, T4, T5, T6>> combine(final CompletableFuture<? extends T1> cf1,
            final CompletableFuture<? extends T2> cf2, final CompletableFuture<? extends T3> cf3, final CompletableFuture<? extends T4> cf4,
            final CompletableFuture<? extends T5> cf5, final CompletableFuture<? extends T6> cf6) {
        return ofAll(N.asList(cf1, cf2, cf3, cf4, cf5, cf6)).thenApply(new Function<List<Object>, Tuple6<T1, T2, T3, T4, T5, T6>>() {
            @Override
            public Tuple6<T1, T2, T3, T4, T5, T6> apply(List<Object> t) {
                return Tuple.of((T1) t.get(0), (T2) t.get(1), (T3) t.get(2), (T4) t.get(3), (T5) t.get(4), (T6) t.get(5));
            }
        });
    }

    public static <T1, T2, T3, T4, T5, T6, T7> CompletableFuture<Tuple7<T1, T2, T3, T4, T5, T6, T7>> combine(final CompletableFuture<? extends T1> cf1,
            final CompletableFuture<? extends T2> cf2, final CompletableFuture<? extends T3> cf3, final CompletableFuture<? extends T4> cf4,
            final CompletableFuture<? extends T5> cf5, final CompletableFuture<? extends T6> cf6, final CompletableFuture<? extends T7> cf7) {
        return ofAll(N.asList(cf1, cf2, cf3, cf4, cf5, cf6, cf7)).thenApply(new Function<List<Object>, Tuple7<T1, T2, T3, T4, T5, T6, T7>>() {
            @Override
            public Tuple7<T1, T2, T3, T4, T5, T6, T7> apply(List<Object> t) {
                return Tuple.of((T1) t.get(0), (T2) t.get(1), (T3) t.get(2), (T4) t.get(3), (T5) t.get(4), (T6) t.get(5), (T7) t.get(6));
            }
        });
    }

    public static <T1, T2, R> CompletableFuture<R> combine(final CompletableFuture<? extends T1> cf1, final CompletableFuture<? extends T2> cf2,
            final BiFunction<? super T1, ? super T2, ? extends R> action) {
        return ofAll(N.asList(cf1, cf2)).thenApply(new Function<List<Object>, R>() {
            @Override
            public R apply(List<Object> t) {
                return action.apply((T1) t.get(0), (T2) t.get(1));
            }
        });
    }

    public static <T1, T2, T3, R> CompletableFuture<R> combine(final CompletableFuture<? extends T1> cf1, final CompletableFuture<? extends T2> cf2,
            final CompletableFuture<? extends T3> cf3, final TriFunction<? super T1, ? super T2, ? super T3, ? extends R> action) {
        return ofAll(N.asList(cf1, cf2, cf3)).thenApply(new Function<List<Object>, R>() {
            @Override
            public R apply(List<Object> t) {
                return action.apply((T1) t.get(0), (T2) t.get(1), (T3) t.get(2));
            }
        });
    }

    public static <R> CompletableFuture<R> combine(final Collection<? extends CompletableFuture<?>> cfs, final Function<List<Object>, ? extends R> action) {
        return ofAll(cfs).thenApply(new Function<List<Object>, R>() {
            @Override
            public R apply(List<Object> t) {
                return action.apply(t);
            }
        });
    }

    public static CompletableFuture<List<Object>> ofAll(final CompletableFuture<?>... cfs) {
        N.checkArgument(N.notNullOrEmpty(cfs), "'cfs' can't be null or empty");

        return ofAll(N.asList(cfs));
    }

    public static CompletableFuture<List<Object>> ofAll(final Collection<? extends CompletableFuture<?>> cfs) {
        N.checkArgument(N.notNullOrEmpty(cfs), "'cfs' can't be null or empty");

        return new CompletableFuture<>(new Future<List<Object>>() {
            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                for (CompletableFuture<?> future : cfs) {
                    if (future.cancel(mayInterruptIfRunning) == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public boolean isCancelled() {
                for (CompletableFuture<?> future : cfs) {
                    if (future.isCancelled()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public boolean isDone() {
                for (CompletableFuture<?> future : cfs) {
                    if (future.isDone() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public List<Object> get() throws InterruptedException, ExecutionException {
                final List<Object> result = new ArrayList<>(cfs.size());

                for (CompletableFuture<?> future : cfs) {
                    result.add(future.get());
                }

                return result;
            }

            @Override
            public List<Object> get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                final long timeoutInMillis = unit.toMillis(timeout);
                final long now = N.currentMillis();
                final long endTime = timeoutInMillis > Long.MAX_VALUE - now ? Long.MAX_VALUE : now + timeoutInMillis;

                final List<Object> result = new ArrayList<>(cfs.size());

                for (CompletableFuture<?> future : cfs) {
                    result.add(future.get(N.max(0, endTime - N.currentMillis()), TimeUnit.MILLISECONDS));
                }

                return result;
            }
        }, ((CompletableFuture<?>) cfs.iterator().next()).asyncExecutor);
    }

    public static CompletableFuture<Object> ofAny(final CompletableFuture<?>... cfs) {
        return ofAny2(N.asList(cfs));
    }

    public static CompletableFuture<Object> ofAny(final Collection<? extends CompletableFuture<?>> cfs) {
        return ofAny2(cfs);
    }

    public static <T> CompletableFuture<T> ofAny(final List<? extends CompletableFuture<? extends T>> cfs) {
        return (CompletableFuture<T>) ofAny2(cfs);
    }

    private static CompletableFuture<Object> ofAny2(final Collection<? extends CompletableFuture<?>> cfs) {
        N.checkArgument(N.notNullOrEmpty(cfs), "'cfs' can't be null or empty");

        return new CompletableFuture<>(new Future<Object>() {
            @Override
            public boolean cancel(boolean mayInterruptIfRunning) {
                for (CompletableFuture<?> future : cfs) {
                    if (future.cancel(mayInterruptIfRunning) == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public boolean isCancelled() {
                for (CompletableFuture<?> future : cfs) {
                    if (future.isCancelled() == false) {
                        return false;
                    }
                }

                return true;
            }

            @Override
            public boolean isDone() {
                for (CompletableFuture<?> future : cfs) {
                    if (future.isDone()) {
                        return true;
                    }
                }

                return false;
            }

            @Override
            public Object get() throws InterruptedException, ExecutionException {
                return get(resultOfAny(cfs));
            }

            @Override
            public Object get(final long timeout, final TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
                return get(resultOfAny(cfs, unit.toMillis(timeout)));
            }

            private Object get(final Optional<Pair<Object, Throwable>> op) throws InterruptedException, ExecutionException {
                if (op.isPresent()) {
                    if (op.get().right != null) {
                        if (op.get().right instanceof InterruptedException) {
                            throw ((InterruptedException) op.get().right);
                        } else if (op.get().right instanceof ExecutionException) {
                            throw ((ExecutionException) op.get().right);
                        } else {
                            throw N.toRuntimeException(op.get().right);
                        }
                    }

                    return op.get().left;
                } else {
                    throw new InterruptedException("Failed to get result");
                }
            }
        }, ((CompletableFuture<?>) cfs.iterator().next()).asyncExecutor);
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param a
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> resultOfAny(final CompletableFuture<? extends T>... a) {
        return resultOfAny(Arrays.asList(a));
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> resultOfAny(final Collection<? extends CompletableFuture<? extends T>> c) {
        final MutableBoolean stopSign = MutableBoolean.of(false);
        final BlockingQueue<Pair<T, Throwable>> queue = resultOfAll(c, stopSign);

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null) {
                    stopSign.setTrue();
                    return Optional.of(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            stopSign.setTrue();
            return Optional.empty();
        }

        stopSign.setTrue();
        return Optional.empty();
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @param maxTimeout
     * @return
     */
    public static <T> Optional<Pair<T, Throwable>> resultOfAny(final Collection<? extends CompletableFuture<? extends T>> c, final long maxTimeout) {
        final MutableBoolean stopSign = MutableBoolean.of(false);
        final BlockingQueue<Pair<T, Throwable>> queue = resultOfAll(c, stopSign);
        final long now = N.currentMillis();
        final long endTime = maxTimeout > Long.MAX_VALUE - now ? Long.MAX_VALUE : now + maxTimeout;

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                long timeout = endTime - N.currentMillis();

                if (timeout <= 0) {
                    stopSign.setTrue();
                    return Optional.empty();
                }

                result = queue.poll(timeout, TimeUnit.MILLISECONDS);

                if (result != null) {
                    stopSign.setTrue();
                    return Optional.of(result);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            stopSign.setTrue();
            return Optional.empty();
        }

        stopSign.setTrue();
        return Optional.empty();
    }

    /**
     * Returns the first non-exception result or empty if fail to get result for all futures.
     * 
     * @param a
     * @return
     */
    public static <T> OptionalNullable<T> resultOfAnySuccess(final CompletableFuture<? extends T>... a) {
        return resultOfAnySuccess(Arrays.asList(a));
    }

    /**
     * Returns the first non-exception result or empty if fail to get result for all futures.
     * 
     * @param c
     * @return
     */
    public static <T> OptionalNullable<T> resultOfAnySuccess(final Collection<? extends CompletableFuture<? extends T>> c) {
        final MutableBoolean stopSign = MutableBoolean.of(false);
        final BlockingQueue<Pair<T, Throwable>> queue = resultOfAll(c, stopSign);

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                result = queue.poll(Integer.MAX_VALUE, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    stopSign.setTrue();
                    return OptionalNullable.of(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            stopSign.setTrue();
            return OptionalNullable.empty();
        }

        stopSign.setTrue();
        return OptionalNullable.empty();
    }

    /**
     * Returns the first result, which could be an exception.
     * 
     * @param c
     * @param maxTimeout
     * @return
     */
    public static <T> OptionalNullable<T> resultOfAnySuccess(final Collection<? extends CompletableFuture<? extends T>> c, final long maxTimeout) {
        final MutableBoolean stopSign = MutableBoolean.of(false);
        final BlockingQueue<Pair<T, Throwable>> queue = resultOfAll(c, stopSign);
        final long now = N.currentMillis();
        final long endTime = maxTimeout > Long.MAX_VALUE - now ? Long.MAX_VALUE : now + maxTimeout;

        try {
            Pair<T, Throwable> result = null;

            for (int i = 0, len = c.size(); i < len; i++) {
                long timeout = endTime - N.currentMillis();

                if (timeout <= 0) {
                    stopSign.setTrue();
                    return OptionalNullable.empty();
                }

                result = queue.poll(timeout, TimeUnit.MILLISECONDS);

                if (result != null && result.right == null) {
                    stopSign.setTrue();
                    return OptionalNullable.of(result.left);
                }
            }
        } catch (InterruptedException e) {
            // throw N.toRuntimeException(e);
            logger.error("Thread is interrupted while retriving result from queue", e);
            stopSign.setTrue();
            return OptionalNullable.empty();
        }

        stopSign.setTrue();
        return OptionalNullable.empty();
    }

    public static <T> BlockingQueue<Pair<T, Throwable>> resultOfAll(final CompletableFuture<? extends T>... a) {
        return resultOfAll(Arrays.asList(a));
    }

    /**
     * 
     * @param c
     * @return
     */
    public static <T> BlockingQueue<Pair<T, Throwable>> resultOfAll(final Collection<? extends CompletableFuture<? extends T>> c) {
        return resultOfAll(c, null);
    }

    private static <T> BlockingQueue<Pair<T, Throwable>> resultOfAll(final Collection<? extends CompletableFuture<? extends T>> c,
            final MutableBoolean stopSign) {
        final ExecutorService executor = Executors.newFixedThreadPool(c.size());
        final BlockingQueue<Pair<T, Throwable>> queue = new ArrayBlockingQueue<>(c.size());

        for (CompletableFuture<? extends T> e : c) {
            final CompletableFuture<T> futuer = (CompletableFuture<T>) e;

            executor.execute(new Runnable() {
                @Override
                public void run() {
                    // Most likely, the below condition will always be true.
                    if (stopSign == null || stopSign.value() == false) {
                        queue.offer(futuer.get2());
                    }
                }
            });
        }

        return queue;
    }
}
