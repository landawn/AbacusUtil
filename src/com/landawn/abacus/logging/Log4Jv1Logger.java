/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.logging;

import org.apache.log4j.Level;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class Log4Jv1Logger extends AbstractLogger {
    // private static final String LOG4J_XML = "log4j.xml";
    // private static final boolean existsLog4JFile;
    //
    // static {
    // boolean temp = false;
    //
    // try {
    // URL url = Loader.getResource(LOG4J_XML);
    // temp = (url != null) && (url.getFile() != null);
    // } catch (Throwable e) {
    // // ignore
    // }
    //
    // existsLog4JFile = temp;
    // }
    //
    private final org.apache.log4j.Logger loggerImpl;

    public Log4Jv1Logger(String name) {
        super(name);
        // if (!existsLog4JFile) {
        // throw new AbacusException("Failed to initilze Log4j Logger Factory");
        // }
        //
        loggerImpl = org.apache.log4j.Logger.getLogger(name);
    }

    @Override
    public boolean isTraceEnabled() {
        return loggerImpl.isEnabledFor(Level.TRACE);
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
        return loggerImpl.isEnabledFor(Level.DEBUG);
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
        return loggerImpl.isEnabledFor(Level.INFO);
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
        return loggerImpl.isEnabledFor(Level.WARN);
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
        return loggerImpl.isEnabledFor(Level.ERROR);
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
