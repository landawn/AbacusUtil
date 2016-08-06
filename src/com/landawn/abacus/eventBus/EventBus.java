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

package com.landawn.abacus.eventBus;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ThreadMode;

public class EventBus {
    private static final Logger logger = LoggerFactory.getLogger(EventBus.class);

    private static final Multimap<Class<?>, Method, List<Method>> classSubscriberMethodMap = new Multimap<>(ConcurrentHashMap.class, ArrayList.class);

    private final Multimap<Object, MethodIdentifier, Set<MethodIdentifier>> subscriberMethodMap = new Multimap<>(LinkedHashMap.class, HashSet.class);

    private static final EventBus INSTANCE = new EventBus();

    public EventBus() {
    }

    public static EventBus getDefault() {
        return INSTANCE;
    }

    public EventBus register(final Object subscriber) {
        return register(subscriber, ThreadMode.DEFAULT);
    }

    public EventBus register(final Object subscriber, final String eventId) {
        return register(subscriber, eventId, ThreadMode.DEFAULT);
    }

    public EventBus register(final Object subscriber, ThreadMode threadMode) {
        return register(subscriber, null, threadMode);
    }

    public EventBus register(final Object subscriber, final String eventId, ThreadMode threadMode) {
        if (threadMode == null) {
            threadMode = ThreadMode.DEFAULT;
        }

        if (!isSupportedThreadMode(threadMode)) {
            throw new RuntimeException("Unsupported thread mode");
        }

        synchronized (subscriberMethodMap) {
            final Class<?> cls = subscriber.getClass();
            List<Method> methods = classSubscriberMethodMap.get(cls);

            if (methods == null) {
                methods = new ArrayList<>();

                if (subscriber instanceof Subscriber) {
                    for (Method method : cls.getDeclaredMethods()) {
                        if (method.getName().equals("on") && method.getParameterTypes().length == 1) {
                            if (Object.class.equals(method.getParameterTypes()[0]) && N.isNullOrEmpty(eventId)) {
                                throw new RuntimeException("General subscriber (parameter type is Object) only can be registered with event id");
                            }

                            if (!methods.contains(method)) {
                                methods.add(method);
                            }

                            break;
                        }
                    }
                }

                final Set<Class<?>> allTypes = N.getSuperTypes(cls);
                allTypes.add(cls);

                for (Class<?> supertype : allTypes) {
                    for (Method method : supertype.getDeclaredMethods()) {
                        if (method.isAnnotationPresent(Subscribe.class) && !method.isSynthetic() && Modifier.isPublic(method.getModifiers())) {
                            Class<?>[] parameterTypes = method.getParameterTypes();
                            if (parameterTypes.length != 1) {
                                throw new RuntimeException(
                                        method.getName() + " has " + parameterTypes.length + " parameters. Subscriber methods must have exactly 1 parameter.");
                            }

                            if (!methods.contains(method)) {
                                methods.add(method);
                            }
                        }
                    }
                }

                classSubscriberMethodMap.putAll(cls, methods);
            }

            if (N.isNullOrEmpty(methods)) {
                throw new RuntimeException("No subscriber method found in class: " + N.getCanonicalClassName(cls));
            }

            for (Method method : methods) {
                subscriberMethodMap.put(subscriber, new MethodIdentifier(subscriber, method, eventId, threadMode));
            }
        }

        return this;
    }

    /**
     * 
     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
     * @param eventId
     * @return
     */
    public <T> EventBus register(final Subscriber<T> subscriber, final String eventId) {
        return register(subscriber, eventId, ThreadMode.DEFAULT);
    }

    /**
     * 
     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
     * @param eventId
     * @param threadMode
     * @return
     */
    public <T> EventBus register(final Subscriber<T> subscriber, final String eventId, ThreadMode threadMode) {
        return register((Object) subscriber, eventId, threadMode);
    }

    public EventBus unregister(final Object subscriber) {
        synchronized (subscriberMethodMap) {
            subscriberMethodMap.removeAll(subscriber);
        }

        return this;
    }

    public EventBus post(final Object event) {
        return post(event, null);
    }

    public EventBus post(final Object event, final String eventId) {
        final Class<?> cls = event.getClass();

        synchronized (subscriberMethodMap) {
            for (Set<MethodIdentifier> methodList : subscriberMethodMap.values()) {
                for (MethodIdentifier identifier : methodList) {
                    if (identifier.isMyEvent(cls, eventId)) {
                        try {
                            executeEvent(identifier.obj, identifier.method, event, identifier.threadMode);
                        } catch (Throwable e) {
                            logger.error("Failed to execute event(" + N.toString(event) + ") for subscriber: " + N.toString(identifier.obj), e);
                        }
                    }
                }
            }

            return this;
        }
    }

    protected boolean isSupportedThreadMode(final ThreadMode threadMode) {
        return threadMode == ThreadMode.DEFAULT || threadMode == ThreadMode.THREAD_POOL_EXECUTOR;
    }

    protected void executeEvent(final Object obj, final Method method, final Object event, final ThreadMode threadMode) throws Throwable {
        switch (threadMode) {
            case DEFAULT:
                invokeMethod(obj, method, event);

                return;

            case THREAD_POOL_EXECUTOR:
                N.asyncExecute(new Runnable() {
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

    protected void invokeMethod(final Object obj, final Method method, final Object event) {
        final boolean accessible = method.isAccessible();

        try {
            method.setAccessible(true);

            method.invoke(obj, event);
        } catch (Throwable e) {
            logger.error("Failed to execute event(" + N.toString(event) + ") for subscriber: " + N.toString(obj), e);
        } finally {
            method.setAccessible(accessible);
        }
    }

    private static final class MethodIdentifier {
        final Map<Class<?>, Boolean> cachedClasses = new HashMap<>();
        final Object obj;
        final Method method;
        final Class<?> parameterType;
        final Class<?> parameterType2;
        final String eventId;
        final ThreadMode threadMode;

        MethodIdentifier(Object obj, Method method, String eventId, ThreadMode threadMode) {
            this.obj = obj;
            this.method = method;
            this.parameterType = method.getParameterTypes()[0];
            this.parameterType2 = N.isPrimitive(parameterType) ? Array.wrap(parameterType)
                    : (N.isPrimitiveWapper(parameterType) ? Array.unwrap(parameterType) : null);
            this.eventId = eventId;
            this.threadMode = threadMode;
        }

        boolean isMyEvent(final Class<?> cls, final String eventId) {
            if (N.equals(this.eventId, eventId) == false) {
                return false;
            }

            Boolean b = cachedClasses.get(cls);

            if (b == null) {
                b = parameterType.isAssignableFrom(cls) || (parameterType2 != null && parameterType2.isAssignableFrom(cls));

                cachedClasses.put(cls, b);
            }

            return b;
        }
    }
}
