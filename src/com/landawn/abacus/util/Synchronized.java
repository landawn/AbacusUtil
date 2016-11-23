package com.landawn.abacus.util;

import java.util.concurrent.Callable;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;

public final class Synchronized {

    private Synchronized() {
        // singleton.
    }

    /**
     * 
     * @param target object to be synchronized.
     * @param callable
     * @return
     */
    public static <T> Callable<T> of(final Object target, final Callable<T> callable) {
        return new Callable<T>() {
            @Override
            public T call() throws Exception {
                synchronized (target) {
                    return callable.call();
                }
            }
        };
    }

    /**
     * 
     * @param target object to be synchronized.
     * @param runnable
     * @return
     */
    public static Runnable of(final Object target, final Runnable runnable) {
        return new Runnable() {
            @Override
            public void run() {
                synchronized (target) {
                    runnable.run();
                }
            }
        };
    }

    /**
     * 
     * @param target object to be synchronized and operated
     * @param func
     * @return
     */
    public static <T, R> Function<T, R> of(final T target, final Function<T, R> func) {
        return new Function<T, R>() {
            @Override
            public R apply(T t) {
                synchronized (target) {
                    return func.apply(target);
                }
            }
        };
    }

    /**
     * 
     * @param target object to be synchronized and operated
     * @param consumer
     * @return
     */
    public static <T> Consumer<T> of(final T target, final Consumer<T> consumer) {
        return new Consumer<T>() {
            @Override
            public void accept(T t) {
                synchronized (target) {
                    consumer.accept(target);
                }
            }
        };
    }

    //    public static <T> T execute(final Object target, final Callable<T> callable) {
    //        synchronized (target) {
    //            try {
    //                return callable.call();
    //            } catch (Exception e) {
    //                throw N.toRuntimeException(e);
    //            }
    //        }
    //    }
    //
    //    public static void execute(final Object target, final Runnable runnable) {
    //        synchronized (target) {
    //            runnable.run();
    //        }
    //    }
    //
    //    public static <T, R> R execute(final T target, final Function<T, R> func) {
    //        synchronized (target) {
    //            return func.apply(target);
    //        }
    //    }
    //
    //    public static <T> void execute(final T target, final Consumer<T> consumer) {
    //        synchronized (target) {
    //            consumer.accept(target);
    //        }
    //    }
}
