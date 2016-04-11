/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.logging;

import org.slf4j.helpers.NOPLoggerFactory;

import com.landawn.abacus.exception.AbacusException;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class SLF4JLogger extends AbstractLogger {
    private final org.slf4j.Logger loggerImpl;

    public SLF4JLogger(String name) {
        super(name);
        if (org.slf4j.LoggerFactory.getILoggerFactory() instanceof NOPLoggerFactory) {
            throw new AbacusException("Failed to initilze SLF4J Logger Factory");
        }

        loggerImpl = org.slf4j.LoggerFactory.getLogger(name);
    }

    @Override
    public boolean isTraceEnabled() {
        return loggerImpl.isTraceEnabled();
    }

    @Override
    public void trace(String msg) {
        loggerImpl.trace(msg);
    }

    @Override
    public void trace(String msg, Throwable t) {
        loggerImpl.trace(msg, t);
    }

    @Override
    public boolean isDebugEnabled() {
        return loggerImpl.isDebugEnabled();
    }

    @Override
    public void debug(String msg) {
        loggerImpl.debug(msg);
    }

    @Override
    public void debug(String msg, Throwable t) {
        loggerImpl.debug(msg, t);
    }

    @Override
    public boolean isInfoEnabled() {
        return loggerImpl.isInfoEnabled();
    }

    @Override
    public void info(String msg) {
        loggerImpl.info(msg);
    }

    @Override
    public void info(String msg, Throwable t) {
        loggerImpl.info(msg, t);
    }

    @Override
    public boolean isWarnEnabled() {
        return loggerImpl.isWarnEnabled();
    }

    @Override
    public void warn(String msg) {
        loggerImpl.warn(msg);
    }

    @Override
    public void warn(String msg, Throwable t) {
        loggerImpl.warn(msg, t);
    }

    @Override
    public boolean isErrorEnabled() {
        return loggerImpl.isErrorEnabled();
    }

    @Override
    public void error(String msg) {
        loggerImpl.error(msg);
    }

    @Override
    public void error(String msg, Throwable t) {
        loggerImpl.error(msg, t);
    }
}
