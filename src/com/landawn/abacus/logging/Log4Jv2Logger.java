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

import org.apache.logging.log4j.Level;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class Log4Jv2Logger extends AbstractLogger {
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
    private final org.apache.logging.log4j.Logger loggerImpl;

    public Log4Jv2Logger(String name) {
        super(name);
        // if (!existsLog4JFile) {
        // throw new AbacusException("Failed to initilze Log4j Logger Factory");
        // }
        //
        loggerImpl = org.apache.logging.log4j.LogManager.getLogger(name);
    }

    @Override
    public boolean isTraceEnabled() {
        return loggerImpl.isEnabled(Level.TRACE);
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
        return loggerImpl.isEnabled(Level.DEBUG);
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
        return loggerImpl.isEnabled(Level.INFO);
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
        return loggerImpl.isEnabled(Level.WARN);
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
        return loggerImpl.isEnabled(Level.ERROR);
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
