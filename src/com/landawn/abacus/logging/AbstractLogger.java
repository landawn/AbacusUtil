/*
 * Copyright (C) 2015 HaiYang Li
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

package com.landawn.abacus.logging;

import com.landawn.abacus.util.function.Supplier;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class AbstractLogger implements Logger {
    protected final String name;

    protected AbstractLogger(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    @SafeVarargs
    public final void trace(String format, Object... args) {
        if (isTraceEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            trace(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void trace(Supplier<String> supplier) {
        if (isTraceEnabled()) {
            trace(supplier.get());
        }
    }

    @Override
    public void trace(Supplier<String> supplier, Throwable t) {
        if (isTraceEnabled()) {
            trace(supplier.get(), t);
        }
    }

    @Override
    @SafeVarargs
    public final void debug(String format, Object... args) {
        if (isDebugEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            debug(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void debug(Supplier<String> supplier) {
        if (isDebugEnabled()) {
            debug(supplier.get());
        }
    }

    @Override
    public void debug(Supplier<String> supplier, Throwable t) {
        if (isDebugEnabled()) {
            debug(supplier.get(), t);
        }
    }

    @Override
    @SafeVarargs
    public final void info(String format, Object... args) {
        if (isInfoEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            info(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void info(Supplier<String> supplier) {
        if (isInfoEnabled()) {
            info(supplier.get());
        }
    }

    @Override
    public void info(Supplier<String> supplier, Throwable t) {
        if (isInfoEnabled()) {
            info(supplier.get(), t);
        }
    }

    @Override
    @SafeVarargs
    public final void warn(String format, Object... args) {
        if (isWarnEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            warn(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void warn(Supplier<String> supplier) {
        if (isWarnEnabled()) {
            warn(supplier.get());
        }
    }

    @Override
    public void warn(Supplier<String> supplier, Throwable t) {
        if (isWarnEnabled()) {
            warn(supplier.get(), t);
        }
    }

    @Override
    @SafeVarargs
    public final void error(String format, Object... args) {
        if (isErrorEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            error(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void error(Supplier<String> supplier) {
        if (isErrorEnabled()) {
            error(supplier.get());
        }
    }

    @Override
    public void error(Supplier<String> supplier, Throwable t) {
        if (isErrorEnabled()) {
            error(supplier.get(), t);
        }
    }
}
