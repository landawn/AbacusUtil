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

import com.landawn.abacus.util.function.Callable;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Runnable;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Synchronized<T> {
    private final T target;

    Synchronized(final T target) {
        this.target = target;
    }

    public static <T> Synchronized<T> on(final T target) {
        N.requireNonNull(target);

        return new Synchronized<>(target);
    }

    /**
     * 
     * @param target to locked on.
     * @param cmd
     * @return
     */
    public static <U> Runnable run(final U target, final Try.Runnable<RuntimeException> cmd) {
        N.requireNonNull(target);
        N.requireNonNull(cmd);

        return new Runnable() {
            @Override
            public void run() {
                synchronized (target) {
                    cmd.run();
                }
            }
        };
    }

    /**
     * 
     * @param target to locked on.
     * @param cmd
     * @return
     */
    public static <U, R> Callable<R> call(final U target, final Try.Callable<R, RuntimeException> cmd) {
        N.requireNonNull(target);
        N.requireNonNull(cmd);

        return new Callable<R>() {
            @Override
            public R call() {
                synchronized (target) {
                    return cmd.call();
                }
            }
        };
    }

    /**
     * 
     * @param target to locked on.
     * @param predicate
     * @return
     */
    public static <U, T> Predicate<T> test(final U target, final Try.Predicate<T, RuntimeException> predicate) {
        N.requireNonNull(target);
        N.requireNonNull(predicate);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                synchronized (target) {
                    return predicate.test(t);
                }
            }
        };
    }

    /**
     * 
     * @param target to locked on.
     * @param predicate
     * @return
     */
    public static <U, T> Predicate<T> test(final U target, final Try.BiPredicate<U, T, RuntimeException> predicate) {
        N.requireNonNull(target);
        N.requireNonNull(predicate);

        return new Predicate<T>() {
            @Override
            public boolean test(T t) {
                synchronized (target) {
                    return predicate.test(target, t);
                }
            }
        };
    }

    /**
     * 
     * @param target to locked on.
     * @param consumer
     * @return
     */
    public static <U, T> Consumer<T> accept(final U target, final Try.Consumer<T, RuntimeException> consumer) {
        N.requireNonNull(target);
        N.requireNonNull(consumer);

        return new Consumer<T>() {
            @Override
            public void accept(T t) {
                synchronized (target) {
                    consumer.accept(t);
                }
            }
        };
    }

    /**
     * 
     * @param target to locked on.
     * @param consumer
     * @return
     */
    public static <U, T> Consumer<T> accept(final U target, final Try.BiConsumer<U, T, RuntimeException> consumer) {
        N.requireNonNull(target);
        N.requireNonNull(consumer);

        return new Consumer<T>() {
            @Override
            public void accept(T t) {
                synchronized (target) {
                    consumer.accept(target, t);
                }
            }
        };
    }

    /**
     * 
     * @param target to locked on.
     * @param funciton
     * @return
     */
    public static <U, T, R> Function<T, R> apply(final U target, final Try.Function<T, R, RuntimeException> funciton) {
        N.requireNonNull(target);
        N.requireNonNull(funciton);

        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                synchronized (target) {
                    return funciton.apply(t);
                }
            }
        };
    }

    /**
     * 
     * @param target to locked on.
     * @param funciton
     * @return
     */
    public static <U, T, R> Function<T, R> apply(final U target, final Try.BiFunction<U, T, R, RuntimeException> funciton) {
        N.requireNonNull(target);
        N.requireNonNull(funciton);

        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                synchronized (target) {
                    return funciton.apply(target, t);
                }
            }
        };
    }

    public <E extends Exception> void run(final Try.Runnable<E> cmd) throws E {
        N.requireNonNull(target);
        N.requireNonNull(cmd);

        synchronized (target) {
            cmd.run();
        }
    }

    public <R, E extends Exception> R call(final Try.Callable<R, E> cmd) throws E {
        N.requireNonNull(target);
        N.requireNonNull(cmd);

        synchronized (target) {
            return cmd.call();
        }
    }

    public <E extends Exception> boolean test(final Try.Predicate<? super T, E> predicate) throws E {
        N.requireNonNull(target);
        N.requireNonNull(predicate);

        synchronized (target) {
            return predicate.test(target);
        }
    }

    public <E extends Exception> void accept(final Try.Consumer<? super T, E> consumer) throws E {
        N.requireNonNull(target);
        N.requireNonNull(consumer);

        synchronized (target) {
            consumer.accept(target);
        }
    }

    public <R, E extends Exception> R apply(final Try.Function<? super T, R, E> function) throws E {
        N.requireNonNull(target);
        N.requireNonNull(function);

        synchronized (target) {
            return function.apply(target);
        }
    }
}
