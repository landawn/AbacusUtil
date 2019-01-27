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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class GenericKeyedObjectPool<K, E extends Poolable> extends AbstractPool<K, E> implements KeyedObjectPool<K, E> {
    private static final long serialVersionUID = 2208516321399679864L;
    private final long maxMemorySize;
    private final KeyedObjectPool.MemoryMeasure<K, E> memoryMeasure;
    private volatile long usedMemorySize = 0;

    protected GenericKeyedObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy) {
        this(capacity, evictDelay, evictionPolicy, 0, null);
    }

    protected GenericKeyedObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy, long maxMemorySize,
            KeyedObjectPool.MemoryMeasure<K, E> memoryMeasure) {
        this(capacity, evictDelay, evictionPolicy, true, DEFAULT_BALANCE_FACTOR, maxMemorySize, memoryMeasure);
    }

    protected GenericKeyedObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy, boolean autoBalance, float balanceFactor) {
        this(capacity, evictDelay, evictionPolicy, autoBalance, balanceFactor, 0, null);
    }

    protected GenericKeyedObjectPool(int capacity, long evictDelay, EvictionPolicy evictionPolicy, boolean autoBalance, float balanceFactor, long maxMemorySize,
            KeyedObjectPool.MemoryMeasure<K, E> memoryMeasure) {
        super(capacity, evictDelay, evictionPolicy, autoBalance, balanceFactor, new HashMap<K, E>((capacity > 1000) ? 1000 : capacity));

        this.maxMemorySize = maxMemorySize;
        this.memoryMeasure = memoryMeasure;
    }

    @Override
    public boolean put(K key, E e) {
        assertNotClosed();

        if (key == null || e == null) {
            throw new NullPointerException();
        }

        if (e.activityPrint().isExpired()) {
            return false;
        }

        putCount.incrementAndGet();

        lock.lock();

        try {
            if (pool.size() >= capacity || usedMemorySize > maxMemorySize) {
                if (autoBalance) {
                    vacate();
                } else {
                    return false;
                }
            }

            if (memoryMeasure != null && memoryMeasure.sizeOf(key, e) > maxMemorySize - usedMemorySize) {
                // ignore.

                return false;
            } else {
                E oldValue = pool.put(key, e);

                if (oldValue != null) {
                    destroyObject(key, oldValue);
                }

                if (memoryMeasure != null) {
                    usedMemorySize += memoryMeasure.sizeOf(key, e);
                }

                notEmpty.signal();

                return true;
            }
        } finally {
            lock.unlock();
        }

    }

    @Override
    public boolean put(K key, E e, boolean autoDestroyOnFailedToPut) {
        boolean sucess = false;

        try {
            sucess = put(key, e);
        } finally {
            if (autoDestroyOnFailedToPut && sucess == false && e != null) {
                e.destroy();
            }
        }

        return sucess;
    }

    @Override
    public E get(K key) {
        assertNotClosed();

        E e = null;

        lock.lock();

        try {
            e = pool.get(key);

            if (e != null) {
                ActivityPrint activityPrint = e.activityPrint();
                activityPrint.updateLastAccessTime();
                activityPrint.updateAccessCount();

                hitCount.incrementAndGet();
            } else {
                missCount.incrementAndGet();
            }

            return e;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public E remove(K key) {
        assertNotClosed();

        E e = null;

        lock.lock();

        try {
            e = pool.remove(key);

            if (e != null) {
                ActivityPrint activityPrint = e.activityPrint();
                activityPrint.updateLastAccessTime();
                activityPrint.updateAccessCount();

                if (memoryMeasure != null) {
                    usedMemorySize -= memoryMeasure.sizeOf(key, e);
                }

                notFull.signal();
            }

            return e;
        } finally {
            lock.unlock();
        }
    }

    @Override
    public E peek(K key) {
        assertNotClosed();

        lock.lock();

        try {
            return pool.get(key);
        } finally {
            lock.unlock();
        }
    }

    @Override
    public boolean containsKey(K key) {
        assertNotClosed();

        lock.lock();

        try {
            return pool.containsKey(key);
        } finally {
            lock.unlock();
        }
    }

    @Override
    public boolean containsValue(E e) {
        assertNotClosed();

        lock.lock();

        try {
            return pool.containsValue(e);
        } finally {
            lock.unlock();
        }
    }

    @Override
    public Set<K> keySet() {
        assertNotClosed();

        lock.lock();

        try {
            return new HashSet<K>(pool.keySet());
        } finally {
            lock.unlock();
        }
    }

    @Override
    public Collection<E> values() {
        assertNotClosed();

        lock.lock();

        try {
            return new ArrayList<E>(pool.values());
        } finally {
            lock.unlock();
        }
    }

    @Override
    protected void destroyObject(K key, E value) {
        if (memoryMeasure != null) {
            usedMemorySize -= memoryMeasure.sizeOf(key, value);
        }

        super.destroyObject(key, value);
    }
}
