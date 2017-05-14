/*
 * Copyright (C) 2017 HaiYang Li
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

import java.util.Collection;
import java.util.Iterator;
import java.util.Queue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;

import com.landawn.abacus.annotation.NonNull;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;

/**
 * 
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 * 
 */
public abstract class Observer<T> {

    private static final Logger logger = LoggerFactory.getLogger(Observer.class);

    private static final Object COMPLETE_FLAG = N.NULL_MASK;

    protected static final double INTERVAL_FACTOR = 3;

    protected static final Executor asyncExecutor = Executors.newFixedThreadPool(N.IS_PLATFORM_ANDROID ? N.CPU_CORES : 32);

    protected static final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(N.IS_PLATFORM_ANDROID ? N.CPU_CORES : 32);

    protected final Dispatcher<Object> dispatcher = new Dispatcher<>();

    protected Observer() {

    }

    @SuppressWarnings("rawtypes")
    public static void complete(Queue<?> queue) {
        ((Queue) queue).offer(COMPLETE_FLAG);
    }

    public static <T> Observer<T> of(final BlockingQueue<T> queue) {
        N.requireNonNull(queue, "queue");

        return new BlockingQueueObserver<T>(queue);
    }

    public static <T> Observer<T> of(final Iterator<T> iter) {
        N.requireNonNull(iter, "iterator");

        return new IteratorObserver<T>(iter);
    }

    /**
     * 
     * @param intervalDurationInMillis
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#debounce(long,%20java.util.concurrent.TimeUnit,%20io.reactivex.Scheduler)">RxJava#debounce</a>
     */
    public Observer<T> debounce(final long intervalDurationInMillis) {
        return debounce(intervalDurationInMillis, TimeUnit.MILLISECONDS);
    }

    /**
     * 
     * @param intervalDuration
     * @param unit
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#debounce(long,%20java.util.concurrent.TimeUnit,%20io.reactivex.Scheduler)">RxJava#debounce</a>
     */
    public Observer<T> debounce(final long intervalDuration, final TimeUnit unit) {
        N.checkArgument(intervalDuration >= 0, "Interval can't be negative");
        N.requireNonNull(unit, "Time unit can't be null");

        if (intervalDuration == 0) {
            return this;
        }

        final long intervalDurationInMillis = unit.toMillis(intervalDuration);

        dispatcher.append(new Dispatcher<Object>() {
            private long prevTimestamp = 0;
            private long lastScheduledTime = 0;

            @Override
            public void onNext(final Object param) {
                synchronized (holder) {
                    final long now = N.currentMillis();

                    if (holder.value() == N.NULL_MASK || now - lastScheduledTime > intervalDurationInMillis * INTERVAL_FACTOR) {
                        holder.setValue(param);
                        prevTimestamp = now;

                        schedule(intervalDuration, unit);
                    } else {
                        holder.setValue(param);
                        prevTimestamp = now;
                    }
                }
            }

            private void schedule(final long delay, final TimeUnit unit) {
                try {
                    scheduler.schedule(new Runnable() {
                        @Override
                        public void run() {
                            final long pastIntervalInMills = N.currentMillis() - prevTimestamp;

                            if (pastIntervalInMills >= intervalDurationInMillis) {
                                Object lastParam = null;

                                synchronized (holder) {
                                    lastParam = holder.value();
                                    holder.setValue(N.NULL_MASK);
                                }

                                if (lastParam != N.NULL_MASK && downDispatcher != null) {
                                    downDispatcher.onNext(lastParam);
                                }
                            } else {
                                schedule(intervalDurationInMillis - pastIntervalInMills, TimeUnit.MILLISECONDS);
                            }
                        }
                    }, delay, unit);

                    lastScheduledTime = N.currentMillis();
                } catch (Throwable e) {
                    holder.setValue(N.NULL_MASK);

                    if (downDispatcher != null) {
                        downDispatcher.onError(e);
                    }
                }
            }
        });

        return this;
    }

    /**
     * 
     * @param intervalDurationInMillis
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#throttleFirst(long,%20java.util.concurrent.TimeUnit)">RxJava#throttleFirst</a>
     */
    public Observer<T> throttleFirst(final long intervalDurationInMillis) {
        return throttleFirst(intervalDurationInMillis, TimeUnit.MILLISECONDS);
    }

    /**
     * 
     * @param intervalDuration
     * @param unit
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#throttleFirst(long,%20java.util.concurrent.TimeUnit)">RxJava#throttleFirst</a>
     */
    public Observer<T> throttleFirst(final long intervalDuration, final TimeUnit unit) {
        N.checkArgument(intervalDuration >= 0, "Interval can't be negative");
        N.requireNonNull(unit, "Time unit can't be null");

        if (intervalDuration == 0) {
            return this;
        }

        final long intervalDurationInMillis = unit.toMillis(intervalDuration);

        dispatcher.append(new Dispatcher<Object>() {
            private long lastScheduledTime = 0;

            @Override
            public void onNext(final Object param) {
                synchronized (holder) {
                    final long now = N.currentMillis();

                    if (holder.value() == N.NULL_MASK || now - lastScheduledTime > intervalDurationInMillis * INTERVAL_FACTOR) {
                        holder.setValue(param);

                        try {
                            scheduler.schedule(new Runnable() {
                                @Override
                                public void run() {
                                    Object firstParam = null;

                                    synchronized (holder) {
                                        firstParam = holder.value();
                                        holder.setValue(N.NULL_MASK);
                                    }

                                    if (firstParam != N.NULL_MASK && downDispatcher != null) {
                                        downDispatcher.onNext(firstParam);
                                    }
                                }
                            }, intervalDuration, unit);

                            lastScheduledTime = now;
                        } catch (Throwable e) {
                            holder.setValue(N.NULL_MASK);

                            if (downDispatcher != null) {
                                downDispatcher.onError(e);
                            }
                        }
                    }
                }
            }
        });

        return this;
    }

    /**
     * 
     * @param intervalDurationInMillis
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#throttleLast(long,%20java.util.concurrent.TimeUnit)">RxJava#throttleLast</a>
     */
    public Observer<T> throttleLast(final long intervalDurationInMillis) {
        return throttleLast(intervalDurationInMillis, TimeUnit.MILLISECONDS);
    }

    /**
     * 
     * @param intervalDuration
     * @param unit
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#throttleLast(long,%20java.util.concurrent.TimeUnit)">RxJava#throttleLast</a>
     */
    public Observer<T> throttleLast(final long intervalDuration, final TimeUnit unit) {
        N.checkArgument(intervalDuration >= 0, "Delay can't be negative");
        N.requireNonNull(unit, "Time unit can't be null");

        if (intervalDuration == 0) {
            return this;
        }

        final long intervalDurationInMillis = unit.toMillis(intervalDuration);

        dispatcher.append(new Dispatcher<Object>() {
            private long lastScheduledTime = 0;

            @Override
            public void onNext(final Object param) {
                synchronized (holder) {
                    final long now = N.currentMillis();

                    if (holder.value() == N.NULL_MASK || now - lastScheduledTime > intervalDurationInMillis * INTERVAL_FACTOR) {
                        holder.setValue(param);

                        try {
                            scheduler.schedule(new Runnable() {
                                @Override
                                public void run() {
                                    Object lastParam = null;

                                    synchronized (holder) {
                                        lastParam = holder.value();
                                        holder.setValue(N.NULL_MASK);
                                    }

                                    if (lastParam != N.NULL_MASK && downDispatcher != null) {
                                        downDispatcher.onNext(lastParam);
                                    }
                                }
                            }, intervalDuration, unit);

                            lastScheduledTime = now;
                        } catch (Throwable e) {
                            holder.setValue(N.NULL_MASK);

                            if (downDispatcher != null) {
                                downDispatcher.onError(e);
                            }
                        }
                    } else {
                        holder.setValue(param);
                    }
                }
            }
        });

        return this;
    }

    /**
     * 
     * @param delayInMillis
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#delay(long,%20java.util.concurrent.TimeUnit)">RxJava#delay</a>
     */
    public Observer<T> delay(final long delayInMillis) {
        return delay(delayInMillis, TimeUnit.MILLISECONDS);
    }

    /**
     * 
     * @param delayInMillis
     * @param unit
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/2.x/javadoc/io/reactivex/Observable.html#delay(long,%20java.util.concurrent.TimeUnit)">RxJava#delay</a>
     */
    public Observer<T> delay(final long delay, final TimeUnit unit) {
        N.checkArgument(delay >= 0, "Delay can't be negative");
        N.requireNonNull(unit, "Time unit can't be null");

        if (delay == 0) {
            return this;
        }

        dispatcher.append(new Dispatcher<Object>() {
            private final long startTime = N.currentMillis();
            private boolean isDelayed = false;

            @Override
            public void onNext(final Object param) {
                if (isDelayed == false) {
                    N.sleep(unit.toMillis(delay) - (N.currentMillis() - startTime));
                    isDelayed = true;
                }

                if (downDispatcher != null) {
                    downDispatcher.onNext(param);
                }
            }
        });

        return this;
    }

    /**
     * 
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/javadoc/io/reactivex/Observable.html#timeInterval()">RxJava#timeInterval</a>
     */
    public Observer<Timed<T>> timeInterval() {
        dispatcher.append(new Dispatcher<Object>() {
            private long startTime = N.currentMillis();

            @Override
            public synchronized void onNext(final Object param) {
                if (downDispatcher != null) {
                    long now = N.currentMillis();
                    long interval = now - startTime;
                    startTime = now;

                    downDispatcher.onNext(Timed.of(param, interval));
                }
            }
        });

        return (Observer<Timed<T>>) this;
    }

    /**
     * 
     * @return this instance.
     * @see <a href="http://reactivex.io/RxJava/javadoc/io/reactivex/Observable.html#timestamp()">RxJava#timestamp</a>
     */
    public Observer<Timed<T>> timestamp() {
        dispatcher.append(new Dispatcher<Object>() {
            @Override
            public void onNext(final Object param) {
                if (downDispatcher != null) {
                    downDispatcher.onNext(Timed.of(param, N.currentMillis()));
                }
            }
        });

        return (Observer<Timed<T>>) this;
    }

    public Observer<T> skip(final long n) {
        if (n > 0) {
            dispatcher.append(new Dispatcher<Object>() {
                private final AtomicLong counter = new AtomicLong();

                @Override
                public void onNext(final Object param) {
                    if (downDispatcher != null && counter.incrementAndGet() > n) {
                        downDispatcher.onNext(param);
                    }
                }
            });
        }

        return this;
    }

    public Observer<T> limit(final long n) {
        if (n > 0) {
            dispatcher.append(new Dispatcher<Object>() {
                private final AtomicLong counter = new AtomicLong();

                @Override
                public void onNext(final Object param) {
                    if (downDispatcher != null && counter.incrementAndGet() <= n) {
                        downDispatcher.onNext(param);
                    }
                }
            });
        }

        return this;
    }

    public Observer<T> filter(final Predicate<? super T> filter) {
        dispatcher.append(new Dispatcher<Object>() {
            @Override
            public void onNext(final Object param) {
                if (downDispatcher != null && filter.test((T) param) == true) {
                    downDispatcher.onNext(param);
                }
            }
        });

        return this;
    }

    public <U> Observer<U> map(final Function<? super T, U> map) {
        dispatcher.append(new Dispatcher<Object>() {
            @Override
            public void onNext(final Object param) {
                if (downDispatcher != null) {
                    downDispatcher.onNext(map.apply((T) param));
                }
            }
        });

        return (Observer<U>) this;
    }

    public <U> Observer<U> flatMap(final Function<? super T, Collection<U>> map) {
        dispatcher.append(new Dispatcher<Object>() {
            @Override
            public void onNext(final Object param) {
                if (downDispatcher != null) {
                    final Collection<U> c = map.apply((T) param);

                    if (N.notNullOrEmpty(c)) {
                        for (U u : c) {
                            downDispatcher.onNext(u);
                        }
                    }
                }
            }
        });

        return (Observer<U>) this;
    }

    public abstract void observe(final Consumer<? super T> action);

    protected static class Dispatcher<T> {
        protected final Output<Object> holder = Output.of(N.NULL_MASK);
        protected Dispatcher<T> downDispatcher;

        public void onNext(@NonNull final T value) {
            if (downDispatcher != null) {
                downDispatcher.onNext(value);
            }
        }

        /**
         * Signal a Throwable exception.
         * @param error the Throwable to signal, not null
         */
        public void onError(@NonNull final Throwable error) {
            if (downDispatcher != null) {
                downDispatcher.onError(error);
            }
        }

        /**
         * Signal a completion.
         */
        public void onComplete() {
            if (downDispatcher != null) {
                downDispatcher.onComplete();
            }
        }

        public void append(Dispatcher<T> downDispatcher) {
            Dispatcher<T> tmp = this;

            while (tmp.downDispatcher != null) {
                tmp = tmp.downDispatcher;
            }

            tmp.downDispatcher = downDispatcher;
        }
    }

    static final class BlockingQueueObserver<T> extends Observer<T> {
        private final BlockingQueue<T> queue;

        BlockingQueueObserver(final BlockingQueue<T> queue) {
            this.queue = queue;
        }

        @Override
        public void observe(final Consumer<? super T> action) {
            N.requireNonNull(action, "action");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    action.accept((T) param);
                }
            });

            asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    T next = null;
                    try {
                        while ((next = queue.poll(Long.MAX_VALUE, TimeUnit.MILLISECONDS)) != COMPLETE_FLAG) {
                            dispatcher.onNext(next);
                        }
                    } catch (InterruptedException e) {
                        logger.error("Observer is interrupted by exception", e);
                        throw new RuntimeException("");
                    }
                }
            });
        }
    }

    static final class IteratorObserver<T> extends Observer<T> {
        private final Iterator<T> iter;

        IteratorObserver(final Iterator<T> iter) {
            this.iter = iter;
        }

        @Override
        public void observe(final Consumer<? super T> action) {
            N.requireNonNull(action, "action");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    action.accept((T) param);
                }
            });

            asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    while (iter.hasNext()) {
                        dispatcher.onNext(iter.next());
                    }
                }
            });
        }
    }
}