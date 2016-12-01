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

import java.util.concurrent.Callable;

import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;

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

    public void run(final Runnable cmd) {
        synchronized (target) {
            cmd.run();
        }
    }

    public void run(final Consumer<? super T> cmd) {
        synchronized (target) {
            cmd.accept(target);
        }
    }

    public <R> R call(final Try.Callable<R> cmd) {
        synchronized (target) {
            return cmd.call();
        }
    }

    public <R> R call(final Function<? super T, R> cmd) {
        synchronized (target) {
            return cmd.apply(target);
        }
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

    public static final class Synchronized0<T> {
        private final T target;

        Synchronized0(final T target) {
            this.target = target;
        }

        public static <T> Synchronized0<T> on(final T target) {
            N.requireNonNull(target);

            return new Synchronized0<>(target);
        }

        public void run(final Try.Runnable cmd) throws Exception {
            synchronized (target) {
                cmd.run();
            }
        }

        public void run(final Try.Consumer<? super T> cmd) throws Exception {
            synchronized (target) {
                cmd.accept(target);
            }
        }

        public <R> R call(final Callable<R> cmd) throws Exception {
            synchronized (target) {
                return cmd.call();
            }
        }

        public <R> R call(final Try.Function<? super T, R> cmd) throws Exception {
            synchronized (target) {
                return cmd.apply(target);
            }
        }
    }
}
