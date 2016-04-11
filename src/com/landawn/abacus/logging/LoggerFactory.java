/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.logging;

import java.util.HashMap;
import java.util.Map;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class LoggerFactory {
    private static final String JAVA_VENDOR = System.getProperty("java.vendor");
    private static final String JAVA_VM_VENDOR = System.getProperty("java.vm.vendor");
    private static final boolean IS_ANDROID_PLATFORM = JAVA_VENDOR.toUpperCase().contains("ANDROID") || JAVA_VM_VENDOR.contains("ANDROID");

    private static final Logger jdkLogger = new JDKLogger(LoggerFactory.class.getName());
    private static final Map<String, Logger> namedLoggers = new HashMap<String, Logger>();
    private static volatile int logType = 0;
    private static volatile boolean initialized = false;

    public static synchronized Logger getLogger(Class<?> clazz) {
        return getLogger(clazz.getName());
    }

    public static synchronized Logger getLogger(String name) {
        Logger logger = namedLoggers.get(name);

        if (logger == null) {
            switch (logType) {
                case 0:
                    try {
                        logger = new SLF4JLogger(name);

                        if (initialized == false) {
                            jdkLogger.info("Initialized with SLF4J Logger");
                        }

                        logType = 0;
                        initialized = true;

                        break;
                    } catch (Throwable e) {
                        // ignore
                    }

                case 1:
                    if (logger == null) {
                        try {
                            logger = new Log4Jv2Logger(name);

                            if (initialized == false) {
                                jdkLogger.info("Initialized with Log4j v2 Logger");
                            }

                            logType = 1;
                            initialized = true;

                            break;
                        } catch (Throwable e) {
                            // ignore
                        }
                    }

                case 2:
                    if (logger == null) {
                        try {
                            logger = new Log4Jv1Logger(name);

                            if (initialized == false) {
                                jdkLogger.info("Initialized with Log4j v1 Logger");
                            }

                            logType = 2;
                            initialized = true;

                            break;
                        } catch (Throwable e) {
                            // ignore
                        }
                    }

                case 3:
                    if (logger == null && IS_ANDROID_PLATFORM) {
                        try {
                            logger = new AndroidLogger(name);

                            if (initialized == false) {
                                jdkLogger.info("Initialized with Android Logger");
                            }

                            logType = 3;
                            initialized = true;

                            break;
                        } catch (Throwable e) {
                            // ignore
                        }
                    }

                case 4:
                    if (logger == null) {
                        logger = new JDKLogger(name);

                        if (initialized == false) {
                            jdkLogger.info("Initialized with JDK Logger");
                        }

                        logType = 4;
                        initialized = true;

                        break;
                    }
            }

            namedLoggers.put(name, logger);
        }

        return logger;
    }
}
