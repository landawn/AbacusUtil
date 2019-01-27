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

package com.landawn.abacus.pool;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.MoreExecutors;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Objectory;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class AbstractPool<K, E extends Poolable> implements Pool<K, E> {
    private static final long serialVersionUID = -7780250223658416202L;

    static final Logger logger = LoggerFactory.getLogger(AbstractPool.class);
    static final long DEFAULT_EVICT_DELAY = 3000;
    static final float DEFAULT_BALANCE_FACTOR = 0.2f;

    static final ScheduledExecutorService scheduledExecutor;
    static {
        final ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(64);
        executor.setKeepAliveTime(180, TimeUnit.SECONDS);
        executor.allowCoreThreadTimeOut(true);
        executor.setRemoveOnCancelPolicy(true);
        scheduledExecutor = MoreExecutors.getExitingScheduledExecutorService(executor);
    }

    private ScheduledFuture<?> scheduleFuture;

    final AtomicLong putCount = new AtomicLong();
    final AtomicLong hitCount = new AtomicLong();
    final AtomicLong missCount = new AtomicLong();
    final AtomicLong evictionCount = new AtomicLong();

    final ReentrantLock lock = new ReentrantLock();
    final Condition notEmpty = lock.newCondition();
    final Condition notFull = lock.newCondition();
    final int capacity;
    final EvictionPolicy evictionPolicy;
    final boolean autoBalance;
    final float balanceFactor;
    boolean isClosed = false;

    final Map<K, E> pool;

    final Comparator<Map.Entry<K, E>> cmp;

    protected AbstractPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy, boolean autoBalance, float balanceFactor, Map<K, E> pool) {
        if (capacity < 0 || evictDelay < 0 || balanceFactor < 0) {
            throw new IllegalArgumentException(
                    "Capacity(" + capacity + "), evict delay(" + evictDelay + "), balanc factor(" + balanceFactor + ") can not be negative");
        }

        this.capacity = capacity;
        this.evictionPolicy = evictionPolicy == null ? EvictionPolicy.LAST_ACCESS_TIME : evictionPolicy;
        this.autoBalance = autoBalance;
        this.balanceFactor = balanceFactor == 0f ? DEFAULT_BALANCE_FACTOR : balanceFactor;

        this.pool = pool;

        switch (this.evictionPolicy) {
            //            case LAST_ACCESS_TIME:
            //
            //                cmp = new Comparator<Map.Entry<K, E>>() {
            //                    @Override
            //                    public int compare(Map.Entry<K, E> o1, Map.Entry<K, E> o2) {
            //                        long value1 = o1.getValue().activityPrint().getLastAccessTime();
            //                        long value2 = o2.getValue().activityPrint().getLastAccessTime();
            //
            //                        return value1 > value2 ? 1 : (value1 == value2 ? 0 : -1);
            //                    }
            //                };
            //
            //                break;
            //
            //            case ACCESS_COUNT:
            //                cmp = new Comparator<Map.Entry<K, E>>() {
            //                    @Override
            //                    public int compare(Map.Entry<K, E> o1, Map.Entry<K, E> o2) {
            //                        int value1 = o1.getValue().activityPrint().getAccessCount();
            //                        int value2 = o2.getValue().activityPrint().getAccessCount();
            //
            //                        return value1 > value2 ? 1 : (value1 == value2 ? 0 : -1);
            //                    }
            //                };
            //
            //                break;
            //
            //            case EXPIRATION_TIME:
            //
            //                cmp = new Comparator<Map.Entry<K, E>>() {
            //                    @Override
            //                    public int compare(Map.Entry<K, E> o1, Map.Entry<K, E> o2) {
            //                        long value1 = o1.getValue().activityPrint().getExpirationTime();
            //                        long value2 = o2.getValue().activityPrint().getExpirationTime();
            //
            //                        return value1 > value2 ? 1 : (value1 == value2 ? 0 : -1);
            //                    }
            //                };
            //
            //                break;

            // =============================================== For Priority Queue
            case LAST_ACCESS_TIME:

                cmp = new Comparator<Map.Entry<K, E>>() {
                    @Override
                    public int compare(Map.Entry<K, E> o1, Map.Entry<K, E> o2) {
                        long value1 = o1.getValue().activityPrint().getLastAccessTime();
                        long value2 = o2.getValue().activityPrint().getLastAccessTime();

                        return value1 > value2 ? -1 : (value1 == value2 ? 0 : 1);
                    }
                };

                break;

            case ACCESS_COUNT:
                cmp = new Comparator<Map.Entry<K, E>>() {
                    @Override
                    public int compare(Map.Entry<K, E> o1, Map.Entry<K, E> o2) {
                        int value1 = o1.getValue().activityPrint().getAccessCount();
                        int value2 = o2.getValue().activityPrint().getAccessCount();

                        return value1 > value2 ? -1 : (value1 == value2 ? 0 : 1);
                    }
                };

                break;

            case EXPIRATION_TIME:

                cmp = new Comparator<Map.Entry<K, E>>() {
                    @Override
                    public int compare(Map.Entry<K, E> o1, Map.Entry<K, E> o2) {
                        long value1 = o1.getValue().activityPrint().getExpirationTime();
                        long value2 = o2.getValue().activityPrint().getExpirationTime();

                        return value1 > value2 ? -1 : (value1 == value2 ? 0 : 1);
                    }
                };

                break;

            default:
                throw new AbacusException("Unsupproted eviction policy: " + evictionPolicy.name());
        }

        if (evictDelay > 0) {
            final Runnable evictTask = new Runnable() {
                @Override
                public void run() {
                    // Evict from the pool
                    try {
                        evict();
                    } catch (Exception e) {
                        // ignore
                        if (logger.isWarnEnabled()) {
                            logger.warn(AbacusException.getErrorMsg(e));
                        }
                    }
                }
            };

            scheduleFuture = scheduledExecutor.scheduleWithFixedDelay(evictTask, evictDelay, evictDelay, TimeUnit.MILLISECONDS);
        }

        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                logger.warn("Starting to shutdown task in QueryCachePool");

                try {
                    close();
                } finally {
                    logger.warn("Completed to shutdown task in QueryCachePool");
                }
            }
        });
    }

    @Override
    public void lock() {
        lock.lock();
    }

    @Override
    public void unlock() {
        lock.unlock();
    }

    @Override
    public int getCapacity() {
        return capacity;
    }

    @Override
    public int size() {
        // assertNotClosed();

        return pool.size();
    }

    @Override
    public boolean isEmpty() {
        return size() == 0;
    }

    @Override
    public void vacate() {
        assertNotClosed();

        lock.lock();

        try {
            vacate((int) (pool.size() * balanceFactor));

            notFull.signalAll();
        } finally {
            lock.unlock();
        }
    }

    @Override
    public void clear() {
        assertNotClosed();

        removeAll();
    }

    private void removeAll() {
        lock.lock();

        try {
            destroyObject(new HashMap<>(pool));

            pool.clear();

            notFull.signalAll();
        } finally {
            lock.unlock();
        }
    }

    @Override
    public void close() {
        if (isClosed) {
            return;
        }

        isClosed = true;

        try {
            if (scheduleFuture != null) {
                scheduleFuture.cancel(true);
            }
        } finally {
            removeAll();
        }
    }

    @Override
    public boolean isClosed() {
        return isClosed;
    }

    @Override
    public long putCount() {
        return putCount.get();
    }

    @Override
    public long hitCount() {
        return hitCount.get();
    }

    @Override
    public long missCount() {
        return missCount.get();
    }

    @Override
    public long evictionCount() {
        return evictionCount.get();
    }

    @Override
    public int hashCode() {
        return pool.hashCode();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean equals(Object obj) {
        return this == obj || (obj instanceof AbstractPool && N.equals(((AbstractPool<K, E>) obj).pool, pool));
    }

    @Override
    public String toString() {
        return pool.toString();
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();

        if (!isClosed) {
            close();
        }
    }

    protected void assertNotClosed() {
        if (isClosed) {
            throw new AbacusException("This object pool has been closed");
        }
    }

    protected void destroyObject(K key, E value) {
        evictionCount.incrementAndGet();

        if (value != null) {
            if (logger.isInfoEnabled()) {
                logger.info("Destroying cached object with activity print: " + value.activityPrint());
            }

            try {
                value.destroy();
            } catch (Exception e) {

                if (logger.isWarnEnabled()) {
                    logger.warn(AbacusException.getErrorMsg(e));
                }
            }
        }
    }

    protected void destroyObject(Map<K, E> map) {
        if (N.notNullOrEmpty(map)) {
            for (Map.Entry<K, E> entry : map.entrySet()) {
                destroyObject(entry.getKey(), entry.getValue());
            }
        }
    }

    protected void vacate(int vacationNumber) {
        int size = pool.size();

        if (vacationNumber >= size) {
            destroyObject(new HashMap<>(pool));
            pool.clear();
        } else {
            final Queue<Map.Entry<K, E>> heap = new PriorityQueue<>(vacationNumber, cmp);

            for (Map.Entry<K, E> entry : pool.entrySet()) {
                if (heap.size() < vacationNumber) {
                    heap.offer(entry);
                } else if (cmp.compare(entry, heap.peek()) > 0) {
                    heap.poll();
                    heap.offer(entry);
                }
            }

            final Map<K, E> removingObjects = new HashMap<>(N.initHashCapacity(heap.size()));

            for (Map.Entry<K, E> entry : heap) {
                pool.remove(entry.getKey());
                removingObjects.put(entry.getKey(), entry.getValue());
            }

            destroyObject(removingObjects);
        }
    }

    /**
     * scan the object pool to find the idle object which inactive time greater than permitted the inactive time for it
     * or it's time out.
     * 
     */
    protected void evict() {
        lock.lock();

        Map<K, E> removingObjects = null;
        try {
            for (Map.Entry<K, E> entry : pool.entrySet()) {
                if (entry.getValue().activityPrint().isExpired()) {
                    if (removingObjects == null) {
                        removingObjects = Objectory.createMap();
                    }

                    removingObjects.put(entry.getKey(), entry.getValue());
                }
            }

            if (N.notNullOrEmpty(removingObjects)) {
                for (K key : removingObjects.keySet()) {
                    E e = pool.remove(key);

                    if (logger.isInfoEnabled()) {
                        logger.info("Evicting expired cached object. KEY: " + key + ", VALUE: " + N.toString(e) + ". Activity print: " + e.activityPrint());
                    }

                }

                destroyObject(removingObjects);

                notFull.signalAll();
            }
        } finally {
            lock.unlock();

            Objectory.recycle(removingObjects);
        }
    }

    private void writeObject(java.io.ObjectOutputStream os) throws java.io.IOException {
        lock.lock();

        try {
            os.defaultWriteObject();
        } finally {
            lock.unlock();
        }
    }

    private void readObject(java.io.ObjectInputStream is) throws java.io.IOException, ClassNotFoundException {
        lock.lock();

        try {
            is.defaultReadObject();
        } finally {
            lock.unlock();
        }
    }
}
