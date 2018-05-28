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
 * @deprecated replaced by {@code Fn#sp(Object, Predicate), Fn#sc(Object, Consumer), Fn#sf(Object, Function)}
 */
@Deprecated
public final class Synchronized<T> {
    private final T target;

    Synchronized(final T target) {
        this.target = target;
    }

    public static <T> Synchronized<T> on(final T target) {
        N.checkArgNotNull(target);

        return new Synchronized<>(target);
    }

    /**
     * 
     * @param target to locked on.
     * @param cmd
     * @return
     */
    public static <T> Runnable run(final T target, final Try.Runnable<RuntimeException> cmd) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(cmd);

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
    public static <T, R> Callable<R> call(final T target, final Try.Callable<R, RuntimeException> cmd) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(cmd);

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
    public static <T> Predicate<T> test(final T target, final Try.Predicate<T, RuntimeException> predicate) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(predicate);

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
    public static <T, U> Predicate<U> test(final T target, final Try.BiPredicate<T, U, RuntimeException> predicate) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(predicate);

        return new Predicate<U>() {
            @Override
            public boolean test(U t) {
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
    public static <T, U> Consumer<U> accept(final T target, final Try.Consumer<U, RuntimeException> consumer) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(consumer);

        return new Consumer<U>() {
            @Override
            public void accept(U t) {
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
    public static <T, U> Consumer<U> accept(final T target, final Try.BiConsumer<T, U, RuntimeException> consumer) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(consumer);

        return new Consumer<U>() {
            @Override
            public void accept(U t) {
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
    public static <T, U, R> Function<U, R> apply(final T target, final Try.Function<U, R, RuntimeException> funciton) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(funciton);

        return new Function<U, R>() {
            @Override
            public R apply(U t) {
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
    public static <T, U, R> Function<U, R> apply(final T target, final Try.BiFunction<T, U, R, RuntimeException> funciton) {
        N.checkArgNotNull(target);
        N.checkArgNotNull(funciton);

        return new Function<U, R>() {
            @Override
            public R apply(U t) {
                synchronized (target) {
                    return funciton.apply(target, t);
                }
            }
        };
    }

    public <E extends Exception> void run(final Try.Runnable<E> cmd) throws E {
        N.checkArgNotNull(target);
        N.checkArgNotNull(cmd);

        synchronized (target) {
            cmd.run();
        }
    }

    public <R, E extends Exception> R call(final Try.Callable<R, E> cmd) throws E {
        N.checkArgNotNull(target);
        N.checkArgNotNull(cmd);

        synchronized (target) {
            return cmd.call();
        }
    }

    public <E extends Exception> boolean test(final Try.Predicate<? super T, E> predicate) throws E {
        N.checkArgNotNull(target);
        N.checkArgNotNull(predicate);

        synchronized (target) {
            return predicate.test(target);
        }
    }

    public <E extends Exception> void accept(final Try.Consumer<? super T, E> consumer) throws E {
        N.checkArgNotNull(target);
        N.checkArgNotNull(consumer);

        synchronized (target) {
            consumer.accept(target);
        }
    }

    public <R, E extends Exception> R apply(final Try.Function<? super T, R, E> function) throws E {
        N.checkArgNotNull(target);
        N.checkArgNotNull(function);

        synchronized (target) {
            return function.apply(target);
        }
    }
}
