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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.Array;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.ThreadMode;

/**
 * <pre>
 * <code>
 * final Object strSubscriber_1 = new Subscriber<String>() {
 *     public void on(String event) {
 *         System.out.println("Subscriber: strSubscriber_1, event: " + event);
 *     }
 * };
 *
 * final Object anySubscriber_2 = new Object() {
 *    Subscribe(threadMode = ThreadMode.DEFAULT)
 *    public void anyMethod(Object event) {
 *        System.out.println("Subscriber: anySubscriber_2, event: " + event);
 *    }
 * };
 *
 * final EventBus eventBus = EventBus.getDefault();
 *
 * eventBus.register(strSubscriber_1);
 * eventBus.register(anySubscriber_2, "eventId_2");
 *
 * eventBus.post("abc");
 * eventBus.post("abc", "eventId_2");
 *
 * eventBus.post(123);
 * eventBus.post(123, "eventId_2");
 * </code>
 * </pre>
 * 
 * @author haiyang li
 *
 */
public class EventBus {
    private static final Logger logger = LoggerFactory.getLogger(EventBus.class);

    private static final ExecutorService asyncExecutor = Executors.newFixedThreadPool(32);

    private static final Map<Class<?>, List<MethodIdentifier>> classSubscriberMethodMap = new ConcurrentHashMap<>();

    private static final EventBus INSTANCE = new EventBus();

    private final Map<Object, List<MethodIdentifier>> subscriberEventMap = Collections.synchronizedMap(new LinkedHashMap<Object, List<MethodIdentifier>>());
    private final String identifier;

    static {
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                asyncExecutor.shutdown();

                try {
                    asyncExecutor.awaitTermination(180, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    logger.error("Failed to commit the tasks in queue in ExecutorService before shutdown", e);
                }
            }
        });
    }

    public EventBus() {
        this("default");
    }

    public EventBus(String identifier) {
        this.identifier = identifier;
    }

    public static EventBus getDefault() {
        return INSTANCE;
    }

    public String identifier() {
        return identifier;
    }

    public EventBus register(final Object subscriber) {
        return register(subscriber, (ThreadMode) null);
    }

    public EventBus register(final Object subscriber, final String eventId) {
        return register(subscriber, eventId, (ThreadMode) null);
    }

    public EventBus register(final Object subscriber, ThreadMode threadMode) {
        return register(subscriber, null, threadMode);
    }

    public EventBus register(final Object subscriber, final String eventId, ThreadMode threadMode) {
        if (!isSupportedThreadMode(threadMode)) {
            throw new RuntimeException("Unsupported thread mode: " + threadMode);
        }

        if (logger.isInfoEnabled()) {
            logger.info("Registering subscriber: " + subscriber);
        }

        final Class<?> cls = subscriber.getClass();
        final List<MethodIdentifier> subMethodList = getSubscriberMethodList(cls);

        if (N.isNullOrEmpty(subMethodList)) {
            throw new RuntimeException("No subscriber method found in class: " + N.getCanonicalClassName(cls));
        }

        final List<MethodIdentifier> eventSubList = new ArrayList<>(subMethodList.size());

        for (MethodIdentifier e : subMethodList) {
            if (e.isPossibleLambdaSubscriber && N.isNullOrEmpty(eventId)) {
                throw new RuntimeException(
                        "General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id");
            }

            eventSubList.add(new MethodIdentifier(subscriber, e.method, eventId, threadMode == null ? e.threadMode : threadMode));
        }

        subscriberEventMap.put(subscriber, eventSubList);

        return this;
    }

    private List<MethodIdentifier> getSubscriberMethodList(final Class<?> cls) {
        List<MethodIdentifier> methods = classSubscriberMethodMap.get(cls);

        if (methods == null) {
            methods = new ArrayList<>();
            final Set<Method> added = new HashSet<>();

            final Set<Class<?>> allTypes = N.getSuperTypes(cls);
            allTypes.add(cls);

            for (Class<?> supertype : allTypes) {
                for (Method method : supertype.getDeclaredMethods()) {
                    if (method.isAnnotationPresent(Subscribe.class) && Modifier.isPublic(method.getModifiers()) && !method.isSynthetic()) {
                        final Class<?>[] parameterTypes = method.getParameterTypes();

                        if (parameterTypes.length != 1) {
                            throw new RuntimeException(
                                    method.getName() + " has " + parameterTypes.length + " parameters. Subscriber method must have exactly 1 parameter.");
                        }

                        if (added.add(method)) {
                            methods.add(new MethodIdentifier(method));
                        }
                    }
                }
            }

            if (Subscriber.class.isAssignableFrom(cls)) {
                for (Method method : cls.getDeclaredMethods()) {
                    if (method.getName().equals("on") && method.getParameterTypes().length == 1) {
                        if (added.add(method)) {
                            methods.add(new MethodIdentifier(method));
                        }

                        break;
                    }
                }
            }

            classSubscriberMethodMap.put(cls, methods);
        }

        return methods;
    }

    /**
     * 
     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
     * @param eventId
     * @return
     */
    public <T> EventBus register(final Subscriber<T> subscriber) {
        return register(subscriber, (String) null);
    }

    /**
     * 
     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
     * @param eventId
     * @return
     */
    public <T> EventBus register(final Subscriber<T> subscriber, final String eventId) {
        return register(subscriber, eventId, (ThreadMode) null);
    }

    public <T> EventBus register(final Subscriber<T> subscriber, ThreadMode threadMode) {
        return register(subscriber, (String) null, threadMode);
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
        if (logger.isInfoEnabled()) {
            logger.info("Unregistering subscriber: " + subscriber);
        }

        subscriberEventMap.remove(subscriber);

        return this;
    }

    public EventBus post(final Object event) {
        return post(event, null);
    }

    public EventBus post(final Object event, final String eventId) {
        final Class<?> cls = event.getClass();
        final List<List<MethodIdentifier>> tmp = new ArrayList<>(subscriberEventMap.values()); // in case concurrent register/unregister.

        for (List<MethodIdentifier> subEventList : tmp) {
            for (MethodIdentifier identifier : subEventList) {
                if (identifier.isMyEvent(cls, eventId)) {
                    try {
                        dispatch(identifier, event);
                    } catch (Throwable e) {
                        logger.error("Failed to post event(" + N.toString(event) + ") to subscriber: " + N.toString(identifier), e);
                    }
                }
            }
        }

        return this;
    }

    protected boolean isSupportedThreadMode(final ThreadMode threadMode) {
        return threadMode == null || threadMode == ThreadMode.DEFAULT || threadMode == ThreadMode.THREAD_POOL_EXECUTOR;
    }

    protected void dispatch(final MethodIdentifier identifier, final Object event) throws Throwable {
        switch (identifier.threadMode) {
            case DEFAULT:
                post(identifier, event);

                return;

            case THREAD_POOL_EXECUTOR:
                asyncExecutor.execute(new Runnable() {
                    @Override
                    public void run() {
                        post(identifier, event);
                    }
                });

                return;

            default:
                throw new RuntimeException("Unsupported thread mode");
        }
    }

    protected void post(final MethodIdentifier identifier, final Object event) {
        if (logger.isInfoEnabled()) {
            logger.info("Posting event: " + N.toString(event) + " to subscriber: " + N.toString(identifier));
        }

        try {
            identifier.method.invoke(identifier.obj, event);
        } catch (Throwable e) {
            logger.error("Failed to post event(" + N.toString(event) + ") to subscriber: " + N.toString(identifier), e);
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
        final boolean isPossibleLambdaSubscriber;

        MethodIdentifier(Method method) {
            this(null, method, null, method.isAnnotationPresent(Subscribe.class) ? method.getAnnotation(Subscribe.class).threadMode() : ThreadMode.DEFAULT);
        }

        MethodIdentifier(Object obj, Method method, String eventId, ThreadMode threadMode) {
            this.obj = obj;
            this.method = method;
            this.parameterType = method.getParameterTypes()[0];
            this.parameterType2 = N.isPrimitive(parameterType) ? Array.box(parameterType)
                    : (N.isPrimitiveWapper(parameterType) ? Array.unbox(parameterType) : null);
            this.eventId = eventId;
            this.threadMode = threadMode;

            isPossibleLambdaSubscriber = Subscriber.class.isAssignableFrom(method.getDeclaringClass()) && method.getName().equals("on")
                    && parameterType.equals(Object.class) && method.isAnnotationPresent(Subscribe.class) == false;

            if (method.isAccessible() == false) {
                method.setAccessible(true);
            }
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

        @Override
        public String toString() {
            return "{method=" + method + ", eventId=" + eventId + ", threadMode=" + threadMode + "}";
        }
    }
}
