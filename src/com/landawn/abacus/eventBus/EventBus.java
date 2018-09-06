/*
 * Copyright (C) 2016 HaiYang Li
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

package com.landawn.abacus.eventBus;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.ClassUtil;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Primitives;
import com.landawn.abacus.util.ThreadMode;

/**
 * <pre>
 * <code>
 * final Object strSubscriber_1 = new Subscriber<String>() {
 *     &#64;Override
 *     public void on(String event) {
 *     System.out.println("Subscriber: strSubscriber_1, event: " + event);
 *     }
 * };
 * 
 * final Object anySubscriber_2 = new Object() {
 *     &#64;Subscribe(threadMode = ThreadMode.DEFAULT, interval = 1000)
 *     public void anyMethod(Object event) {
 *     System.out.println("Subscriber: anySubscriber_2, event: " + event);
 *     }
 * };
 * 
 * final Object anySubscriber_3 = new Object() {
 *     &#64;Subscribe(threadMode = ThreadMode.DEFAULT, sticky = true)
 *     public void anyMethod(Object event) {
 *     System.out.println("Subscriber: anySubscriber_3, event: " + event);
 *     }
 * };
 * 
 * final EventBus eventBus = EventBus.getDefault();
 * 
 * eventBus.register(strSubscriber_1);
 * eventBus.register(strSubscriber_1);
 * eventBus.register(anySubscriber_2, "eventId_2");
 * 
 * eventBus.post("abc");
 * eventBus.postSticky("sticky");
 * eventBus.post("eventId_2", "abc");
 * 
 * eventBus.post(123);
 * eventBus.post("eventId_2", 123);
 * 
 * eventBus.register(anySubscriber_3);
 * </code>
 * </pre>
 * 
 * @author haiyang li
 */
public class EventBus {
    private static final Logger logger = LoggerFactory.getLogger(EventBus.class);

    private static final ExecutorService executorService = Executors.newFixedThreadPool(32);

    private static final Map<Class<?>, List<SubIdentifier>> classMetaSubMap = new ConcurrentHashMap<>();

    private final Map<Object, List<SubIdentifier>> registeredSubMap = new LinkedHashMap<>();
    private final Map<String, Set<SubIdentifier>> registeredEventIdSubMap = new HashMap<>();
    private final Map<Object, String> stickyEventMap = new IdentityHashMap<>();
    private final String identifier;
    private final Executor executor;

    private final Map<String, List<SubIdentifier>> listOfEventIdSubMap = new ConcurrentHashMap<>();
    private List<List<SubIdentifier>> listOfSubEventSubs = null;
    private Map<Object, String> mapOfStickyEvent = null;

    private static final EventBus INSTANCE = new EventBus("default");

    static {
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                logger.warn("Starting to shutdown task in EventBus");

                try {
                    executorService.shutdown();

                    while (executorService.isTerminated() == false) {
                        N.sleepUninterruptibly(100);
                    }
                } finally {
                    logger.warn("Completed to shutdown task in EventBus");
                }
            }
        });
    }

    public EventBus() {
        this(N.guid());
    }

    public EventBus(final String identifier) {
        this(identifier, executorService);
    }

    public EventBus(final String identifier, final Executor executor) {
        this.identifier = identifier;
        this.executor = executor == null ? executorService : executor;

        if (executor != executorService && executor instanceof ExecutorService) {
            Runtime.getRuntime().addShutdownHook(new Thread() {
                @Override
                public void run() {
                    logger.warn("Starting to shutdown task in EventBus");

                    try {
                        ((ExecutorService) executor).shutdown();

                        while (((ExecutorService) executor).isTerminated() == false) {
                            N.sleepUninterruptibly(100);
                        }
                    } finally {
                        logger.warn("Completed to shutdown task in EventBus");
                    }
                }
            });
        }
    }

    public static EventBus getDefault() {
        return INSTANCE;
    }

    public String identifier() {
        return identifier;
    }

    /**
     * Returns the subscriber which is registered with specified <code>eventType</code>(or its sub types) and <code>null</code> event id.
     * 
     * @param eventType
     * @return
     */
    public List<Object> getSubscribers(final Class<?> eventType) {
        return getSubscribers(eventType, null);
    }

    /**
     * Returns the subscriber which is registered with specified <code>eventType</code>(or its sub types) and <code>eventId</code>.
     * 
     * @param eventType
     * @param eventId
     * @return
     */
    public List<Object> getSubscribers(final Class<?> eventType, final String eventId) {
        final List<Object> eventSubs = new ArrayList<>();

        synchronized (registeredSubMap) {
            for (Map.Entry<Object, List<SubIdentifier>> entry : registeredSubMap.entrySet()) {
                for (SubIdentifier sub : entry.getValue()) {
                    if (sub.isMyEvent(eventType, eventId)) {
                        eventSubs.add(entry.getKey());

                        break;
                    }
                }
            }
        }

        return eventSubs;
    }

    /**
     * Returns all registered subscribers.
     * 
     * @param eventType
     * @return
     */
    public List<Object> getAllSubscribers(final Class<?> eventType) {
        synchronized (registeredSubMap) {
            return new ArrayList<>(registeredSubMap.keySet());
        }
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

    /**
     * Register the subscriber with the specified <code>eventId</code> and <code>threadMode</code>. 
     * If the same register has been registered before, it be over-written with the new specified <code>eventId</code> and <code>threadMode</code>.
     * 
     * @param subscriber
     * @param eventId
     * @param threadMode
     * @return itself
     */
    public EventBus register(final Object subscriber, final String eventId, ThreadMode threadMode) {
        if (!isSupportedThreadMode(threadMode)) {
            throw new RuntimeException("Unsupported thread mode: " + threadMode);
        }

        if (logger.isInfoEnabled()) {
            logger.info("Registering subscriber: " + subscriber + " with eventId: " + eventId + " and thread mode: " + threadMode);
        }

        final Class<?> cls = subscriber.getClass();
        final List<SubIdentifier> subList = getClassSubList(cls);

        if (N.isNullOrEmpty(subList)) {
            throw new RuntimeException("No subscriber method found in class: " + ClassUtil.getCanonicalClassName(cls));
        }

        final List<SubIdentifier> eventSubList = new ArrayList<>(subList.size());

        for (SubIdentifier sub : subList) {
            if (sub.isPossibleLambdaSubscriber && N.isNullOrEmpty(eventId)) {
                throw new RuntimeException(
                        "General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id");
            }

            eventSubList.add(new SubIdentifier(sub, subscriber, eventId, threadMode));
        }

        synchronized (registeredSubMap) {
            registeredSubMap.put(subscriber, eventSubList);
            listOfSubEventSubs = null;
        }

        if (N.isNullOrEmpty(eventId)) {
            synchronized (registeredEventIdSubMap) {
                for (SubIdentifier sub : eventSubList) {
                    if (N.isNullOrEmpty(sub.eventId)) {
                        continue;
                    }

                    final Set<SubIdentifier> eventSubs = registeredEventIdSubMap.get(sub.eventId);

                    if (eventSubs == null) {
                        registeredEventIdSubMap.put(sub.eventId, N.asLinkedHashSet(sub));
                    } else {
                        eventSubs.add(sub);
                    }

                    listOfEventIdSubMap.remove(sub.eventId);
                }
            }
        } else {
            synchronized (registeredEventIdSubMap) {
                final Set<SubIdentifier> eventSubs = registeredEventIdSubMap.get(eventId);

                if (eventSubs == null) {
                    registeredEventIdSubMap.put(eventId, new LinkedHashSet<>(eventSubList));
                } else {
                    eventSubs.addAll(eventSubList);
                }

                listOfEventIdSubMap.remove(eventId);
            }
        }

        Map<Object, String> mapOfStickyEvent = this.mapOfStickyEvent;

        for (SubIdentifier sub : eventSubList) {
            if (sub.sticky) {
                if (mapOfStickyEvent == null) {
                    synchronized (stickyEventMap) {
                        mapOfStickyEvent = new IdentityHashMap<>(stickyEventMap);
                        this.mapOfStickyEvent = mapOfStickyEvent;
                    }
                }

                for (Map.Entry<Object, String> entry : mapOfStickyEvent.entrySet()) {
                    if (sub.isMyEvent(entry.getKey().getClass(), entry.getValue())) {
                        try {
                            dispatch(sub, entry.getKey());
                        } catch (Throwable e) {
                            logger.error("Failed to post sticky event: " + N.toString(entry.getValue()) + " to subscriber: " + N.toString(sub), e);
                        }
                    }
                }
            }
        }

        return this;
    }

    private List<SubIdentifier> getClassSubList(final Class<?> cls) {
        synchronized (classMetaSubMap) {
            List<SubIdentifier> subs = classMetaSubMap.get(cls);

            if (subs == null) {
                subs = new ArrayList<>();
                final Set<Method> added = new HashSet<>();

                final Set<Class<?>> allTypes = ClassUtil.getAllSuperTypes(cls);
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
                                subs.add(new SubIdentifier(method));
                            }
                        }
                    }
                }

                if (Subscriber.class.isAssignableFrom(cls)) {
                    for (Method method : cls.getDeclaredMethods()) {
                        if (method.getName().equals("on") && method.getParameterTypes().length == 1) {
                            if (added.add(method)) {
                                subs.add(new SubIdentifier(method));
                            }

                            break;
                        }
                    }
                }

                classMetaSubMap.put(cls, subs);
            }

            return subs;
        }
    }

    //    /**
    //     * 
    //     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
    //     * @param eventId
    //     * @return
    //     */
    //    public <T> EventBus register(final Subscriber<T> subscriber) {
    //        return register(subscriber, (ThreadMode) null);
    //    }

    /**
     * 
     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
     * @param eventId
     * @return
     */
    public <T> EventBus register(final Subscriber<T> subscriber, final String eventId) {
        return register(subscriber, eventId, (ThreadMode) null);
    }

    //    /**
    //     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
    //     * @param threadMode
    //     * @return
    //     */
    //    public <T> EventBus register(final Subscriber<T> subscriber, ThreadMode threadMode) {
    //        return register(subscriber, (String) null, threadMode);
    //    }

    /**
     * 
     * @param subscriber General subscriber (type is {@code Subscriber} and parameter type is Object, mostly created by lambda) only can be registered with event id
     * @param eventId
     * @param threadMode
     * @return
     */
    public <T> EventBus register(final Subscriber<T> subscriber, final String eventId, ThreadMode threadMode) {
        final Object tmp = subscriber;
        return register(tmp, eventId, threadMode);
    }

    public EventBus unregister(final Object subscriber) {
        if (logger.isInfoEnabled()) {
            logger.info("Unregistering subscriber: " + subscriber);
        }

        List<SubIdentifier> subEvents = null;

        synchronized (registeredSubMap) {
            subEvents = registeredSubMap.remove(subscriber);
            listOfSubEventSubs = null;
        }

        if (N.notNullOrEmpty(subEvents)) {
            synchronized (registeredEventIdSubMap) {
                final List<String> keyToRemove = new ArrayList<>();

                for (Map.Entry<String, Set<SubIdentifier>> entry : registeredEventIdSubMap.entrySet()) {
                    entry.getValue().removeAll(subEvents);

                    if (entry.getValue().isEmpty()) {
                        keyToRemove.add(entry.getKey());
                    }

                    listOfEventIdSubMap.remove(entry.getKey());
                }

                if (N.notNullOrEmpty(keyToRemove)) {
                    for (String key : keyToRemove) {
                        registeredEventIdSubMap.remove(key);
                    }
                }
            }
        }

        return this;
    }

    public EventBus post(final Object event) {
        return post((String) null, event);
    }

    public EventBus post(final String eventId, final Object event) {
        final Class<?> cls = event.getClass();

        List<List<SubIdentifier>> listOfSubs = this.listOfSubEventSubs;

        if (N.isNullOrEmpty(eventId)) {
            if (listOfSubs == null) {
                synchronized (registeredSubMap) {
                    listOfSubs = new ArrayList<>(registeredSubMap.values()); // in case concurrent register/unregister.
                    this.listOfSubEventSubs = listOfSubs;
                }
            }
        } else {
            List<SubIdentifier> listOfEventIdSub = listOfEventIdSubMap.get(eventId);

            if (listOfEventIdSub == null) {
                synchronized (registeredEventIdSubMap) {
                    if (registeredEventIdSubMap.containsKey(eventId)) {
                        listOfEventIdSub = new ArrayList<>(registeredEventIdSubMap.get(eventId)); // in case concurrent register/unregister.
                    } else {
                        listOfEventIdSub = N.emptyList();
                    }

                    this.listOfEventIdSubMap.put(eventId, listOfEventIdSub);
                }

                listOfSubs = Arrays.asList(listOfEventIdSub);
            }
        }

        for (List<SubIdentifier> subs : listOfSubs) {
            for (SubIdentifier sub : subs) {
                if (sub.isMyEvent(cls, eventId)) {
                    try {
                        dispatch(sub, event);
                    } catch (Throwable e) {
                        logger.error("Failed to post event: " + N.toString(event) + " to subscriber: " + N.toString(sub), e);
                    }
                }
            }
        }

        return this;
    }

    public EventBus postSticky(final Object event) {
        return postSticky((String) null, event);
    }

    public EventBus postSticky(final String eventId, final Object event) {
        synchronized (stickyEventMap) {
            stickyEventMap.put(event, eventId);

            this.mapOfStickyEvent = null;
        }

        post(eventId, event);

        return this;
    }

    /**
     * Remove the sticky event posted with <code>null</code> event id.
     * 
     * @param event
     * @return
     */
    public boolean removeStickyEvent(final Object event) {
        return removeStickyEvent(event, null);
    }

    /**
     * Remove the sticky event posted with the specified <code>eventId</code>.
     * 
     * @param event
     * @param eventId
     * @return
     */
    public boolean removeStickyEvent(final Object event, final String eventId) {
        synchronized (stickyEventMap) {
            final String val = stickyEventMap.get(event);

            if (N.equals(val, eventId) && (val != null || stickyEventMap.containsKey(event))) {
                stickyEventMap.remove(event);

                this.mapOfStickyEvent = null;
                return true;
            }
        }

        return false;
    }

    /**
     * Remove the sticky events which can be assigned to specified <code>eventType</code> and posted with <code>null</code> event id.
     * 
     * @param eventType
     * @return true if one or one more than sticky events are removed, otherwise, <code>false</code>.
     */
    public boolean removeStickyEvents(final Class<?> eventType) {
        return removeStickyEvents(eventType, null);
    }

    /**
     * Remove the sticky events which can be assigned to specified <code>eventType</code> and posted with the specified <code>eventId</code>.
     * 
     * @param eventType
     * @param eventId
     * @return true if one or one more than sticky events are removed, otherwise, <code>false</code>.
     */
    public boolean removeStickyEvents(final Class<?> eventType, final String eventId) {
        final List<Object> keyToRemove = new ArrayList<>();

        synchronized (stickyEventMap) {
            for (Map.Entry<Object, String> entry : stickyEventMap.entrySet()) {
                if (N.equals(entry.getValue(), eventId) && eventType.isAssignableFrom(entry.getKey().getClass())) {
                    keyToRemove.add(entry);
                }
            }

            if (N.notNullOrEmpty(keyToRemove)) {
                synchronized (stickyEventMap) {
                    for (Object event : keyToRemove) {
                        stickyEventMap.remove(event);
                    }

                    this.mapOfStickyEvent = null;
                }

                return true;
            }
        }

        return false;
    }

    /**
     * Removes all sticky events.
     */
    public void removeAllStickyEvents() {
        synchronized (stickyEventMap) {
            stickyEventMap.clear();

            this.mapOfStickyEvent = null;
        }
    }

    /**
     * Returns the sticky events which can be assigned to specified <code>eventType</code> and posted with <code>null</code> event id.
     * 
     * 
     * @param eventType
     * @return
     */
    public List<Object> getStickyEvents(Class<?> eventType) {
        return getStickyEvents(eventType, null);
    }

    /**
     * Returns the sticky events which can be assigned to specified <code>eventType</code> and posted with the specified <code>eventId</code>.
     * 
     * @param eventType
     * @param eventId
     * @return
     */
    public List<Object> getStickyEvents(Class<?> eventType, String eventId) {
        final List<Object> result = new ArrayList<>();

        synchronized (stickyEventMap) {
            for (Map.Entry<Object, String> entry : stickyEventMap.entrySet()) {
                if (N.equals(entry.getValue(), eventId) && eventType.isAssignableFrom(entry.getKey().getClass())) {
                    result.add(entry.getKey());
                }
            }
        }

        return result;
    }

    protected boolean isSupportedThreadMode(final ThreadMode threadMode) {
        return threadMode == null || threadMode == ThreadMode.DEFAULT || threadMode == ThreadMode.THREAD_POOL_EXECUTOR;
    }

    protected void dispatch(final SubIdentifier identifier, final Object event) throws Throwable {
        switch (identifier.threadMode) {
            case DEFAULT:
                post(identifier, event);

                return;

            case THREAD_POOL_EXECUTOR:
                executor.execute(new Runnable() {
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

    protected void post(final SubIdentifier sub, final Object event) {
        try {
            if (sub.interval > 0 || sub.deduplicate) {
                synchronized (sub) {
                    if (sub.interval > 0 && System.currentTimeMillis() - sub.lastPostTime < sub.interval) {
                        // ignore.
                        if (logger.isInfoEnabled()) {
                            logger.info("Ignoring event: " + N.toString(event) + " to subscriber: " + N.toString(sub) + " because it's in the interval: "
                                    + sub.interval);
                        }
                    } else if (sub.deduplicate && (sub.previousEvent != null || sub.lastPostTime > 0) && N.equals(sub.previousEvent, event)) {
                        // ignore.
                        if (logger.isInfoEnabled()) {
                            logger.info("Ignoring event: " + N.toString(event) + " to subscriber: " + N.toString(sub) + " because it's same as previous event");
                        }
                    } else {
                        if (logger.isInfoEnabled()) {
                            logger.info("Posting event: " + N.toString(event) + " to subscriber: " + N.toString(sub));
                        }

                        sub.lastPostTime = System.currentTimeMillis();

                        if (sub.deduplicate) {
                            sub.previousEvent = event;
                        }

                        sub.method.invoke(sub.obj, event);
                    }
                }
            } else {
                if (logger.isInfoEnabled()) {
                    logger.info("Posting event: " + N.toString(event) + " to subscriber: " + N.toString(sub));
                }

                sub.method.invoke(sub.obj, event);
            }
        } catch (Throwable e) {
            logger.error("Failed to post event: " + N.toString(event) + " to subscriber: " + N.toString(sub), e);
        }
    }

    private static final class SubIdentifier {
        final Map<Class<?>, Boolean> cachedClasses = new ConcurrentHashMap<>();

        final Object obj;
        final Method method;
        final Class<?> parameterType;
        final String eventId;
        final ThreadMode threadMode;
        final boolean strictEventType;
        final boolean sticky;
        final long interval;
        final boolean deduplicate;
        final boolean isPossibleLambdaSubscriber;
        long lastPostTime = 0;
        Object previousEvent = null;

        SubIdentifier(Method method) {
            final Subscribe subscribe = method.getAnnotation(Subscribe.class);
            this.obj = null;
            this.method = method;
            this.parameterType = Primitives.isPrimitiveType(method.getParameterTypes()[0]) ? Primitives.wrap(method.getParameterTypes()[0])
                    : method.getParameterTypes()[0];
            this.eventId = subscribe == null || N.isNullOrEmpty(subscribe.eventId()) ? null : subscribe.eventId();
            this.threadMode = subscribe == null ? ThreadMode.DEFAULT : subscribe.threadMode();
            this.strictEventType = subscribe == null ? false : subscribe.strictEventType();
            this.sticky = subscribe == null ? false : subscribe.sticky();
            this.interval = subscribe == null ? 0 : subscribe.interval();
            this.deduplicate = subscribe == null ? false : subscribe.deduplicate();

            this.isPossibleLambdaSubscriber = Subscriber.class.isAssignableFrom(method.getDeclaringClass()) && method.getName().equals("on")
                    && parameterType.equals(Object.class) && subscribe == null;

            if (method.isAccessible() == false) {
                method.setAccessible(true);
            }
        }

        SubIdentifier(SubIdentifier sub, Object obj, String eventId, ThreadMode threadMode) {
            this.obj = obj;
            this.method = sub.method;
            this.parameterType = sub.parameterType;
            this.eventId = N.isNullOrEmpty(eventId) ? sub.eventId : eventId;
            this.threadMode = threadMode == null ? sub.threadMode : threadMode;
            this.strictEventType = sub.strictEventType;
            this.sticky = sub.sticky;
            this.interval = sub.interval;
            this.deduplicate = sub.deduplicate;
            this.isPossibleLambdaSubscriber = sub.isPossibleLambdaSubscriber;
        }

        boolean isMyEvent(final Class<?> eventType, final String eventId) {
            if (N.equals(this.eventId, eventId) == false) {
                return false;
            }

            Boolean b = cachedClasses.get(eventType);

            if (b == null) {
                b = strictEventType ? parameterType.equals(eventType) : parameterType.isAssignableFrom(eventType);

                cachedClasses.put(eventType, b);
            }

            return b;
        }

        @Override
        public int hashCode() {
            int h = 17;
            h = 31 * h + N.hashCode(obj);
            h = 31 * h + N.hashCode(method);
            h = 31 * h + N.hashCode(parameterType);
            h = 31 * h + N.hashCode(eventId);
            h = 31 * h + N.hashCode(threadMode);
            h = 31 * h + N.hashCode(strictEventType);
            h = 31 * h + N.hashCode(sticky);
            h = 31 * h + N.hashCode(interval);
            h = 31 * h + N.hashCode(deduplicate);
            h = 31 * h + N.hashCode(isPossibleLambdaSubscriber);

            return h;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj instanceof SubIdentifier) {
                final SubIdentifier other = (SubIdentifier) obj;

                return N.equals(obj, other.obj) && N.equals(method, other.method) && N.equals(parameterType, other.parameterType)
                        && N.equals(eventId, other.eventId) && N.equals(threadMode, other.threadMode) && N.equals(strictEventType, other.strictEventType)
                        && N.equals(sticky, other.sticky) && N.equals(interval, other.interval) && N.equals(deduplicate, other.deduplicate)
                        && N.equals(isPossibleLambdaSubscriber, other.isPossibleLambdaSubscriber);
            }

            return false;
        }

        @Override
        public String toString() {
            return "{obj=" + N.toString(obj) + ", method=" + N.toString(method) + ", parameterType=" + N.toString(parameterType) + ", eventId="
                    + N.toString(eventId) + ", threadMode=" + N.toString(threadMode) + ", strictEventType=" + N.toString(strictEventType) + ", sticky="
                    + N.toString(sticky) + ", interval=" + N.toString(interval) + ", deduplicate=" + N.toString(deduplicate) + ", isPossibleLambdaSubscriber="
                    + N.toString(isPossibleLambdaSubscriber) + "}";
        }

        //        public static void main(String[] args) {
        //            CodeGenerator.printClassMethod(SubIdentifier.class);
        //        }
    }
}
