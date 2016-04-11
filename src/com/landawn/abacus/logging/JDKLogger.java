/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.logging;

import java.util.logging.Level;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class JDKLogger extends AbstractLogger {
    private final java.util.logging.Logger loggerImpl;

    public JDKLogger(String name) {
        super(name);
        loggerImpl = java.util.logging.Logger.getLogger(name);
    }

    @Override
    public boolean isTraceEnabled() {
        return loggerImpl.isLoggable(Level.FINEST);
    }

    @Override
    public void trace(String msg) {
        log(Level.FINEST, msg);
    }

    @Override
    public void trace(String msg, Throwable t) {
        log(Level.FINEST, msg, t);
    }

    @Override
    public boolean isDebugEnabled() {
        return loggerImpl.isLoggable(Level.FINE);
    }

    @Override
    public void debug(String msg) {
        log(Level.FINE, msg);
    }

    @Override
    public void debug(String msg, Throwable t) {
        log(Level.FINE, msg, t);
    }

    @Override
    public boolean isInfoEnabled() {
        return loggerImpl.isLoggable(Level.INFO);
    }

    @Override
    public void info(String msg) {
        log(Level.INFO, msg);
    }

    @Override
    public void info(String msg, Throwable t) {
        log(Level.INFO, msg, t);
    }

    @Override
    public boolean isWarnEnabled() {
        return loggerImpl.isLoggable(Level.WARNING);
    }

    @Override
    public void warn(String msg) {
        log(Level.WARNING, msg);
    }

    @Override
    public void warn(String msg, Throwable t) {
        log(Level.WARNING, msg, t);
    }

    @Override
    public boolean isErrorEnabled() {
        return loggerImpl.isLoggable(Level.SEVERE);
    }

    @Override
    public void error(String msg) {
        log(Level.SEVERE, msg);
    }

    @Override
    public void error(String msg, Throwable t) {
        log(Level.SEVERE, msg, t);
    }

    private void log(Level level, String msg) {
        loggerImpl.log(level, msg);
    }

    private void log(Level level, String msg, Throwable t) {
        loggerImpl.log(level, msg, t);
    }
}
