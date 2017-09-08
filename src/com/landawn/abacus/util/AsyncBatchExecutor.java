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

package com.landawn.abacus.util;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.EntityManager;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class AsyncBatchExecutor<E> {
    private static final Logger logger = LoggerFactory.getLogger(AsyncBatchExecutor.class);

    public static final int DEFAULT_CAPACITY = 8192;
    public static final int DEFAULT_EVICT_DELAY = 3000;
    /**
     * @see Options#DEFAULT_BATCH_SIZE
     */
    public static final int DEFAULT_BATCH_SIZE = Options.DEFAULT_BATCH_SIZE;

    protected final EntityManager<E> em;
    protected final Map<String, Object> options;
    protected final List<E> addQueue;
    protected final List<E> updateQueue;
    protected final List<E> deleteQueue;

    /**
     * Unit is millisecond
     */
    private final int capacity;
    private final long evictDelay;
    private final int batchSize;

    private final ScheduledExecutorService scheduledExecutor = Executors.newScheduledThreadPool(3);

    public AsyncBatchExecutor(final EntityManager<E> em) {
        this(em, DEFAULT_CAPACITY, DEFAULT_EVICT_DELAY, DEFAULT_BATCH_SIZE);
    }

    public AsyncBatchExecutor(final EntityManager<E> em, final int capacity, final long evictDelay, final int batchSize) {
        if ((em == null) || (evictDelay <= 0) || (batchSize <= 0) || (capacity <= 0)) {
            throw new IllegalArgumentException();
        }

        this.em = em;

        this.addQueue = new ArrayList<>(capacity);
        this.updateQueue = new ArrayList<>(capacity);
        this.deleteQueue = new ArrayList<>(capacity);

        this.evictDelay = evictDelay;
        this.batchSize = batchSize;
        this.capacity = capacity;

        this.options = N.asProps(Options.BATCH_SIZE, batchSize);

        // start evict process.
        final Runnable commitTask = new Runnable() {
            @Override
            public void run() {
                if ((addQueue.size() > 0) || (updateQueue.size() > 0) || (deleteQueue.size() > 0)) {
                    commit();
                }
            }
        };

        scheduledExecutor.scheduleWithFixedDelay(commitTask, evictDelay, evictDelay, TimeUnit.MILLISECONDS);

        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                commit();

                scheduledExecutor.shutdown();

                try {
                    scheduledExecutor.awaitTermination(180, TimeUnit.SECONDS);
                } catch (InterruptedException e) {
                    logger.error("Failed to commit the tasks in queue in ExecutorService before shutdown", e);
                }
            }
        });
    }

    public long getEvictDelay() {
        return evictDelay;
    }

    public int getBatchSize() {
        return batchSize;
    }

    public int getCapacity() {
        return capacity;
    }

    public void add(E e) {
        synchronized (addQueue) {
            addElement(addQueue, e);
        }
    }

    public void update(E e) {
        synchronized (updateQueue) {
            addElement(updateQueue, e);
        }
    }

    public void delete(E e) {
        synchronized (deleteQueue) {
            addElement(deleteQueue, e);
        }
    }

    @SuppressWarnings("unchecked")
    public void commit() {
        if (addQueue.size() > 0) {
            List<E> entities = new ArrayList<>();

            synchronized (addQueue) {
                entities.addAll(addQueue);
                addQueue.clear();
            }

            if (logger.isInfoEnabled()) {
                logger.info("START-BATCH-ADD[" + entities.size() + "]");
            }

            for (int size = entities.size(), from = 0, to = Math.min(from + batchSize, size); from < size; from = to, to = Math.min(from + batchSize, size)) {
                try {
                    em.add(entities.subList(from, to), options);
                } catch (Exception e) {
                    if (logger.isWarnEnabled()) {
                        logger.warn("FAILED-BATCH-ADD[" + entities.size() + "]. " + AbacusException.getErrorMsg(e));
                    }
                }
            }

            if (logger.isInfoEnabled()) {
                logger.info("END-BATCH-ADD[" + entities.size() + "]");
            }
        }

        if (updateQueue.size() > 0) {
            List<E> entities = new ArrayList<>();

            synchronized (updateQueue) {
                entities.addAll(updateQueue);
                updateQueue.clear();
            }

            if (logger.isInfoEnabled()) {
                logger.info("START-BATCH-UPDATE[" + entities.size() + "]");
            }

            for (int size = entities.size(), from = 0, to = Math.min(from + batchSize, size); from < size; from = to, to = Math.min(from + batchSize, size)) {
                try {
                    em.update(entities.subList(from, to), options);
                } catch (Exception e) {
                    if (logger.isWarnEnabled()) {
                        logger.warn("FAILED-BATCH-UPDATE[" + entities.size() + "]. " + AbacusException.getErrorMsg(e));
                    }
                }
            }

            if (logger.isInfoEnabled()) {
                logger.info("END-BATCH-UPDATE[" + entities.size() + "]");
            }
        }

        if (deleteQueue.size() > 0) {
            List<E> entities = new ArrayList<>();

            synchronized (deleteQueue) {
                entities.addAll(deleteQueue);
                deleteQueue.clear();
            }

            if (logger.isInfoEnabled()) {
                logger.info("START-BATCH-DELETE[" + entities.size() + "]");
            }

            for (int size = entities.size(), from = 0, to = Math.min(from + batchSize, size); from < size; from = to, to = Math.min(from + batchSize, size)) {
                try {
                    em.delete(entities.subList(from, to), options);
                } catch (Exception e) {
                    if (logger.isWarnEnabled()) {
                        logger.warn("FAILED-BATCH-DELETE[" + entities.size() + "]. " + AbacusException.getErrorMsg(e));
                    }
                }
            }

            if (logger.isInfoEnabled()) {
                logger.info("END-BATCH-DELETE[" + entities.size() + "]");
            }
        }
    }

    protected void addElement(List<E> queue, E e) {
        if ((addQueue.size() + updateQueue.size() + deleteQueue.size()) > capacity) {
            String msg = "Queue is full. The capacity is " + capacity;
            logger.error(msg);
            throw new AbacusException(msg);
        }

        queue.add(e);
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        commit();
    }
}
