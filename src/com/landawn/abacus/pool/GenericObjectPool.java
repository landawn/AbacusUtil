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

import java.util.IdentityHashMap;
import java.util.concurrent.TimeUnit;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class GenericObjectPool<E extends Poolable> extends AbstractPool<E, E> implements ObjectPool<E> {
    private static final long serialVersionUID = -5055744987721643286L;
    private final long maxMemorySize;
    private final ObjectPool.MemoryMeasure<E> memoryMeasure;
    private volatile long usedMemorySize = 0;

    protected GenericObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy) {
        this(capacity, evictDelay, evictionPolicy, 0, null);
    }

    protected GenericObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy, long maxMemorySize, ObjectPool.MemoryMeasure<E> memoryMeasure) {
        this(capacity, evictDelay, evictionPolicy, true, DEFAULT_BALANCE_FACTOR, maxMemorySize, memoryMeasure);
    }

    protected GenericObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy, boolean autoBalance, float balanceFactor) {
        this(capacity, evictDelay, evictionPolicy, autoBalance, balanceFactor, 0, null);
    }

    protected GenericObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy, boolean autoBalance, float balanceFactor, long maxMemorySize,
            ObjectPool.MemoryMeasure<E> memoryMeasure) {
        super(capacity, evictDelay, evictionPolicy, autoBalance, balanceFactor, new IdentityHashMap<E, E>((capacity > 1000) ? 1000 : capacity));

        this.maxMemorySize = maxMemorySize;
        this.memoryMeasure = memoryMeasure;
    }

    @Override
    public boolean add(E e) {
        assertNotClosed();

        if (e == null) {
            throw new NullPointerException();
        }

        if (e.activityPrint().isExpired()) {
            return false;
        }

        putCount.incrementAndGet();

        lock.lock();

        try {
            if (pool.size() >= capacity) {
                if (autoBalance) {
                    vacate();
                } else {
                    return false;
                }
            }

            if (memoryMeasure != null && memoryMeasure.sizeOf(e) > maxMemorySize - usedMemorySize) {
                // ignore.

                return false;
            } else {
                pool.put(e, e);

                if (memoryMeasure != null) {
                    usedMemorySize += memoryMeasure.sizeOf(e);
                }

                notEmpty.signal();

                return true;
            }
        } finally {
            lock.unlock();
        }
    }

    @Override
    public boolean add(E e, boolean autoDestroyOnFailedToAdd) {
        boolean sucess = false;

        try {
            sucess = add(e);
        } finally {
            if (autoDestroyOnFailedToAdd && sucess == false && e != null) {
                e.destroy();
            }
        }

        return sucess;
    }

    @Override
    public boolean add(E e, long timeout, TimeUnit unit) throws InterruptedException {
        assertNotClosed();

        if (e == null) {
            throw new NullPointerException();
        }

        if (e.activityPrint().isExpired()) {
            return false;
        }

        putCount.incrementAndGet();

        long nanos = unit.toNanos(timeout);
        lock.lock();

        try {
            if ((pool.size() >= capacity) && autoBalance) {
                vacate();
            }

            while (true) {
                if (pool.size() < capacity) {
                    if (memoryMeasure != null && memoryMeasure.sizeOf(e) > maxMemorySize - usedMemorySize) {
                        // ignore.

                        return false;
                    } else {
                        pool.put(e, e);

                        if (memoryMeasure != null) {
                            usedMemorySize += memoryMeasure.sizeOf(e);
                        }

                        notEmpty.signal();

                        return true;
                    }
                }

                if (nanos <= 0) {
                    return false;
                }

                nanos = notFull.awaitNanos(nanos);
            }
        } finally {
            lock.unlock();
        }
    }

    @Override
    public boolean add(E e, long timeout, TimeUnit unit, boolean autoDestroyOnFailedToAdd) throws InterruptedException {
        boolean sucess = false;

        try {
            sucess = add(e, timeout, unit);
        } finally {
            if (autoDestroyOnFailedToAdd && sucess == false && e != null) {
                e.destroy();
            }
        }

        return sucess;
    }

    @Override
    public E take() {
        assertNotClosed();

        E e = null;

        lock.lock();

        try {
            e = pool.size() > 0 ? pool.remove(pool.keySet().iterator().next()) : null;

            if (e != null) {
                ActivityPrint activityPrint = e.activityPrint();
                activityPrint.updateLastAccessTime();
                activityPrint.updateAccessCount();

                if (memoryMeasure != null) {
                    usedMemorySize -= memoryMeasure.sizeOf(e);
                }

                hitCount.incrementAndGet();

                notFull.signal();
            } else {
                missCount.incrementAndGet();
            }
        } finally {
            lock.unlock();
        }

        return e;
    }

    @Override
    public E take(long timeout, TimeUnit unit) throws InterruptedException {
        assertNotClosed();

        E e = null;
        long nanos = unit.toNanos(timeout);

        lock.lock();

        try {
            while (true) {
                e = pool.size() > 0 ? pool.remove(pool.keySet().iterator().next()) : null;

                if (e != null) {
                    ActivityPrint activityPrint = e.activityPrint();
                    activityPrint.updateLastAccessTime();
                    activityPrint.updateAccessCount();

                    if (memoryMeasure != null) {
                        usedMemorySize -= memoryMeasure.sizeOf(e);
                    }

                    hitCount.incrementAndGet();

                    notFull.signal();

                    return e;
                }

                if (nanos <= 0) {
                    missCount.incrementAndGet();

                    return null;
                }

                nanos = notEmpty.awaitNanos(nanos);
            }
        } finally {
            lock.unlock();
        }
    }

    @Override
    public boolean contains(E e) {
        assertNotClosed();

        lock.lock();

        try {
            return pool.containsKey(e);
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected void destroyObject(E key, E value) {
        if (memoryMeasure != null) {
            usedMemorySize -= memoryMeasure.sizeOf(value);
        }

        super.destroyObject(key, value);
    }
}
