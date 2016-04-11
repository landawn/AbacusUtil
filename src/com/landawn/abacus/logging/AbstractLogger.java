/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.logging;

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
    public void trace(String format, Object... args) {
        if (isTraceEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            trace(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void debug(String format, Object... args) {
        if (isDebugEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            debug(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void info(String format, Object... args) {
        if (isInfoEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            info(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void warn(String format, Object... args) {
        if (isWarnEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            warn(ft.getMessage(), ft.getThrowable());
        }
    }

    @Override
    public void error(String format, Object... args) {
        if (isErrorEnabled()) {
            FormattedMessage ft = MessageFormatter.arrayFormat(format, args);
            error(ft.getMessage(), ft.getThrowable());
        }
    }
}
