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

    public <E extends Exception> void run(final Try.Runnable<E> cmd) throws E {
        synchronized (target) {
            cmd.run();
        }
    }

    public <E extends Exception> void run(final Try.Consumer<? super T, E> cmd) throws E {
        synchronized (target) {
            cmd.accept(target);
        }
    }

    public <R, E extends Exception> R call(final Try.Callable<R, E> cmd) throws E {
        synchronized (target) {
            return cmd.call();
        }
    }

    public <R, E extends Exception> R call(final Try.Function<? super T, R, E> cmd) throws E {
        synchronized (target) {
            return cmd.apply(target);
        }
    }
}
