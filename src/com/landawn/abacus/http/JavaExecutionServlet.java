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

package com.landawn.abacus.http;

import static com.landawn.abacus.http.HTTP.jsonParser;
import static com.landawn.abacus.http.HTTP.kryoParser;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.UncheckedIOException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.DateUtil;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.RemoteExecutionRequest;
import com.landawn.abacus.util.RemoteExecutionResponse;
import com.landawn.abacus.util.RemoteTask;

/**
 * Deploy <code>JavaExecutionServlet</code> under Tomcat.
 *
 * <pre>
 *  {@code
 *     <servlet>
 *         <description>Hello javaExecution</description>
 *         <display-name>javaExecution</display-name>
 *         <servlet-name>javaExecution</servlet-name>
 *         <servlet-class>com.landawn.abacus.http.JavaExecutionServlet</servlet-class>
 *     </servlet>
 *
 *     <servlet-mapping>
 *         <servlet-name>javaExecution</servlet-name>
 *         <url-pattern>/javaExecution/*</url-pattern>
 *     </servlet-mapping>
 * }
 * </pre>
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
@SuppressWarnings("deprecation")
public class JavaExecutionServlet extends AbstractHttpServlet {
    private static final long serialVersionUID = 778742360481398056L;

    private static final Logger logger = LoggerFactory.getLogger(JavaExecutionServlet.class);

    private static final Method defineClassMethod;

    static {
        defineClassMethod = getDeclaredMethod(ClassLoader.class, "defineClass", String.class, byte[].class, int.class, int.class);
        defineClassMethod.setAccessible(true);
    }

    private static final ClassLoader rootClassLoader = JavaExecutionServlet.class.getClassLoader();

    static {
        logger.warn(IOUtil.JAVA_VERSION);
    }

    @Override
    public void init() throws ServletException {
        super.init();
    }

    @Override
    protected void doPost(final HttpServletRequest request, final HttpServletResponse response) throws ServletException {
        execute(request, response);
    }

    protected void execute(final HttpServletRequest request, final HttpServletResponse response) {
        final ContentFormat contentFormat = getContentFormat(request);
        final long startTime = DateUtil.currentMillis();
        final RemoteExecutionResponse remoteResponse = new RemoteExecutionResponse();

        RemoteExecutionRequest remoteRequest = null;
        InputStream is = null;

        try {
            is = getInputStream(request, contentFormat);

            switch (contentFormat) {
                case KRYO:
                    remoteRequest = kryoParser.deserialize(RemoteExecutionRequest.class, is);
                    break;

                case JSON:
                    remoteRequest = jsonParser.deserialize(RemoteExecutionRequest.class, is);
                    break;

                default:
                    remoteResponse.setErrorCode(getClassName(AbacusException.class));
                    remoteResponse.setErrorMessage("Unsupported content format: " + contentFormat + ". Only Kryo is supported");
            }

            if (remoteResponse.getErrorCode() == null) {
                final String requestId = N.isNullOrEmpty(remoteRequest.getRequestId()) ? String.valueOf(DateUtil.currentMillis()) : remoteRequest.getRequestId();
                // remoteRequest.getRunMode() // TODO
                // remoteRequest.getSchedule() // TODO

                if (logger.isInfoEnabled()) {
                    logger.info("Start to execute task: " + requestId + " from host: " + remoteRequest.getRequestHost());
                }

                final DynamicClassLoader dynamicClassLoader = new DynamicClassLoader(rootClassLoader);
                final Map<String, byte[]> classBytesMap = remoteRequest.getClassBytesMap();

                for (Map.Entry<String, byte[]> entry : classBytesMap.entrySet()) {
                    defineClass(dynamicClassLoader, entry.getKey(), entry.getValue());
                }

                @SuppressWarnings("rawtypes")
                final Class<? extends RemoteTask<?, ?>> remoteTask = (Class) dynamicClassLoader.loadClass(remoteRequest.getClassName());
                final RemoteTask<Object, Object> instance = (RemoteTask<Object, Object>) newInstance(remoteTask);
                remoteResponse.setResult(instance.run(remoteRequest.getParameter()));

                if (logger.isInfoEnabled()) {
                    logger.info("Task: " + requestId + " is completed");
                }
            }
        } catch (Exception e) {
            String msg = "Failed to execute task on server: " + IOUtil.HOST_NAME;
            logger.error(msg, e);
            remoteResponse.setErrorCode(getClassName(e.getClass()));
            remoteResponse.setErrorMessage(msg + ". " + e.getMessage());
        } finally {
            IOUtil.close(is);
        }

        // remoteResponse.setRequestHost(remoteRequest.getRequestHost());
        // remoteResponse.setRequestTime(remoteRequest.getRequestTime());
        remoteResponse.setResponseTime(DateUtil.currentMillis());
        remoteResponse.setExecutionTime(remoteResponse.getResponseTime() - startTime);
        remoteResponse.setExecutionHost(IOUtil.HOST_NAME);

        OutputStream os = null;

        try {
            os = getOutputStream(response, contentFormat);

            switch (contentFormat) {
                case KRYO:
                    kryoParser.serialize(os, remoteResponse);
                    break;

                default:
                    jsonParser.serialize(os, remoteResponse);
            }

            flush(os);
        } catch (IOException e) {
            String msg = "Failed to execute task on server: " + IOUtil.HOST_NAME;
            logger.error(msg, e);
            throw new UncheckedIOException(msg, e);
        } finally {
            IOUtil.close(os);
        }
    }

    private static <T> Class<T> defineClass(ClassLoader classLoader, String className, byte[] bytes) {
        return invoke(classLoader, defineClassMethod, className, bytes, 0, bytes.length);
    }

    private static String getClassName(final Class<?> cls) {
        return cls.getName();
    }

    private static Method getDeclaredMethod(final Class<?> cls, final String methodName, final Class<?>... parameterTypes) {
        Method method = null;

        try {
            method = cls.getDeclaredMethod(methodName, parameterTypes);
        } catch (NoSuchMethodException e) {
            // ignore.
        }

        if (method == null) {
            Method[] methods = cls.getDeclaredMethods();

            for (Method m : methods) {
                if (m.getName().equalsIgnoreCase(methodName) && N.equals(parameterTypes, m.getParameterTypes())) {
                    method = m;

                    break;
                }
            }
        }

        return method;
    }

    private static <T> T newInstance(final Class<T> cls) {
        try {
            if (Modifier.isStatic(cls.getModifiers()) == false && (cls.isAnonymousClass() || cls.isMemberClass())) {
                // http://stackoverflow.com/questions/2097982/is-it-possible-to-create-an-instance-of-nested-class-using-java-reflection

                final List<Class<?>> toInstantiate = new ArrayList<>();
                Class<?> parent = cls.getEnclosingClass();

                do {
                    toInstantiate.add(parent);
                    parent = parent.getEnclosingClass();
                } while (parent != null && Modifier.isStatic(parent.getModifiers()) == false && (parent.isAnonymousClass() || parent.isMemberClass()));

                if (parent != null) {
                    toInstantiate.add(parent);
                }

                N.reverse(toInstantiate);

                Object instance = null;
                for (Class<?> current : toInstantiate) {
                    instance = instance == null ? invoke(current.getDeclaredConstructor())
                            : invoke(current.getDeclaredConstructor(instance.getClass()), instance);
                }

                return invoke(cls.getDeclaredConstructor(instance.getClass()), instance);
            } else {
                return invoke(cls.getDeclaredConstructor());
            }
        } catch (Exception e) {
            throw N.toRuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private static <T> T invoke(final Object instance, final Method method, final Object... args) {
        try {
            if (method.isAccessible() == false) {
                method.setAccessible(true);
            }

            return (T) method.invoke(instance, args);
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw N.toRuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private static <T> T invoke(final Constructor<T> c, final Object... args) {
        try {
            if (c.isAccessible() == false) {
                c.setAccessible(true);
            }

            return c.newInstance(args);
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException e) {
            throw N.toRuntimeException(e);
        }
    }

    static class DynamicClassLoader extends ClassLoader {
        public DynamicClassLoader(ClassLoader parent) {
            super(parent);
        }
    }
}
