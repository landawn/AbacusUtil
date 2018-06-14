/*
 * Copyright (C) 2018 HaiYang Li
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

package com.landawn.abacus.util.function;

import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Try;

/**
 * 
 * @since 1.2
 * 
 * @author Haiyang Li
 */
public interface Callable<R> extends java.util.concurrent.Callable<R>, Try.Callable<R, RuntimeException> {

    @Override
    R call();

    /**
     * Returns the specified instance
     * 
     * @param callable
     * @return
     */
    public static <R> Callable<R> of(final Callable<R> callable) {
        N.checkArgNotNull(callable);

        return callable;
    }

    public static Callable<Void> create(Runnable cmd) {
        N.checkArgNotNull(cmd);

        return new Callable<Void>() {
            @Override
            public Void call() {
                cmd.run();
                return null;
            }
        };
    }
}
