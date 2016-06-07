/*
 * Copyright (C) 2007 The Guava Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.android.util;

import java.lang.reflect.Method;

import com.landawn.abacus.util.ThreadMode;

public class EventBus extends com.landawn.abacus.eventBus.EventBus {
    private static final EventBus INSTANCE = new EventBus();

    public EventBus() {
        super();
    }

    public static EventBus getDefault() {
        return INSTANCE;
    }

    @Override
    protected boolean isSupportedThreadMode(final ThreadMode threadMode) {
        return threadMode == ThreadMode.DEFAULT || threadMode == ThreadMode.SERIAL_EXECUTOR || threadMode == ThreadMode.THREAD_POOL_EXECUTOR
                || threadMode == ThreadMode.UI_THREAD;
    }

    @Override
    protected void executeEvent(final Object obj, final Method method, final Object event, final ThreadMode threadMode) throws Throwable {
        switch (threadMode) {
            case DEFAULT:
                invokeMethod(obj, method, event);

                return;

            case SERIAL_EXECUTOR:
                AsyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        invokeMethod(obj, method, event);
                    }
                });

                return;

            case THREAD_POOL_EXECUTOR:
                AsyncExecutor.executeInParallel(new Runnable() {
                    @Override
                    public void run() {
                        invokeMethod(obj, method, event);
                    }
                });

                return;

            case UI_THREAD:
                AsyncExecutor.executeOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        invokeMethod(obj, method, event);
                    }
                });

                return;

            default:
                throw new RuntimeException("Unsupported thread mode");
        }
    }
}
