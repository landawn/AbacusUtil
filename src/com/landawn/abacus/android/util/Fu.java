/*
 * Copyright (C) 2017 HaiYang Li
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

import com.landawn.abacus.util.function.Consumer;

/**
 * 
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 * 
 */
public final class Fu {
    private Fu() {
        // singleton
    }

    static final Consumer<Object> EMPTY_CONSUMER = new Consumer<Object>() {
        @Override
        public void accept(Object t) {
            // Do nothing;            
        }
    };

    public static final Runnable EMPTY_ACTION = new Runnable() {
        @Override
        public void run() {
            // Do nothing;            
        }
    };

    public static final Consumer<Throwable> ON_ERROR_MISSING = new Consumer<Throwable>() {
        @Override
        public void accept(Throwable t) {
            throw new RuntimeException(t);
        }
    };

    public static <T> Consumer<T> emptyConsumer() {
        return (Consumer<T>) EMPTY_CONSUMER;
    }
}
