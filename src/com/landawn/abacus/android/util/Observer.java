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

package com.landawn.abacus.android.util;

import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.android.util.AsyncExecutor.UIExecutor;
import com.landawn.abacus.annotation.NonNull;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Output;
import com.landawn.abacus.util.Tuple;
import com.landawn.abacus.util.Tuple.Tuple2;
import com.landawn.abacus.util.Tuple.Tuple4;
import com.landawn.abacus.util.function.Consumer;

import android.text.Editable;
import android.text.TextWatcher;
import android.view.DragEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnDragListener;
import android.view.ViewGroup;
import android.view.ViewGroup.OnHierarchyChangeListener;
import android.widget.SearchView;
import android.widget.SearchView.OnQueryTextListener;
import android.widget.TextView;

/**
 * 
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 * 
 */
public abstract class Observer<T> {
    protected static final double INTERVAL_FACTOR = 3;

    protected static final ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(N.CPU_CORES);

    protected final Dispatcher<Object> dispatcher = new Dispatcher<>();

    protected Observer() {

    }

    public static Observer<View> onClick(final OnClickListener listener) {
        return new OnClickObserver(listener);
    }

    public static Observer<TextView> onTextChanged(OnTextChnagedListener listener) {
        return new OnTextChangedObserver(listener);
    }

    public static Observer<TextView> beforeTextChanged(BeforeTextChnagedListener listener) {
        return new BeforeTextChangedObserver(listener);
    }

    public static Observer<TextView> afterTextChanged(AfterTextChnagedListener listener) {
        return new AfterTextChangedObserver(listener);
    }

    public static Observer<SearchView> onQueryTextChange(OnQueryTextChangeListener listener) {
        return new OnQueryTextChangeObserver(listener);
    }

    public static Observer<SearchView> onQueryTextSubmit(OnQueryTextSubmitListener listener) {
        return new OnQueryTextSubmitObserver(listener);
    }

    public static Observer<ViewGroup> onChildViewAdded(OnChildViewAddedListener listener) {
        return new OnChildViewAddedObserver(listener);
    }

    public static Observer<ViewGroup> onChildViewRemoved(OnChildViewRemovedListener listener) {
        return new OnChildViewRemovedObserver(listener);
    }

    public static Observer<View> onDrag(OnDragListener listener) {
        return new OnDragObserver(listener);
    }

    //    public static Observer<View> onScrollChange(OnScrollChangeListener listener) {
    //        return new OnScrollChangeObserver(listener);
    //    }

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
            @Override
            public void onNext(final Object param) {
                try {
                    scheduler.schedule(new Runnable() {
                        @Override
                        public void run() {
                            if (downDispatcher != null) {
                                downDispatcher.onNext(param);
                            }
                        }
                    }, delay, unit);
                } catch (Throwable e) {
                    if (downDispatcher != null) {
                        downDispatcher.onError(e);
                    }
                }
            }
        });

        return this;
    }

    public void observe(final T target) {
        observe(target, Fu.ON_ERROR_MISSING);
    }

    public void observe(final T target, final Consumer<? super Throwable> onError) {
        observe(target, onError, Fu.EMPTY_ACTION);
    }

    public abstract void observe(final T target, final Consumer<? super Throwable> onError, final Runnable onComplete);

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

    static final class OnClickObserver extends Observer<View> implements OnClickListener {
        private final OnClickListener listener;

        OnClickObserver(final OnClickListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(View view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof View) {
                        final View tmp = (View) param;

                        if (Util.isUiThread()) {
                            listener.onClick(tmp);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.onClick(tmp);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.setOnClickListener(this);
        }

        @Override
        public void onClick(final View view) {
            dispatcher.onNext(view);
        }
    }

    static final class OnTextChangedObserver extends Observer<TextView> implements TextWatcher {
        private final OnTextChnagedListener listener;

        OnTextChangedObserver(final OnTextChnagedListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(TextView view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof Tuple4) {
                        @SuppressWarnings("unchecked")
                        final Tuple4<CharSequence, Integer, Integer, Integer> tmp = (Tuple4<CharSequence, Integer, Integer, Integer>) param;

                        if (Util.isUiThread()) {
                            listener.onTextChanged(tmp._1, tmp._2, tmp._3, tmp._4);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.onTextChanged(tmp._1, tmp._2, tmp._3, tmp._4);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.addTextChangedListener(this);
        }

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            // Do nothing
        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            dispatcher.onNext(Tuple.of(s, start, before, count));
        }

        @Override
        public void afterTextChanged(Editable s) {
            // Do nothing
        }
    }

    static final class BeforeTextChangedObserver extends Observer<TextView> implements TextWatcher {
        private final BeforeTextChnagedListener listener;

        BeforeTextChangedObserver(final BeforeTextChnagedListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(TextView view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof Tuple4) {
                        @SuppressWarnings("unchecked")
                        final Tuple4<CharSequence, Integer, Integer, Integer> tmp = (Tuple4<CharSequence, Integer, Integer, Integer>) param;

                        if (Util.isUiThread()) {
                            listener.beforeTextChanged(tmp._1, tmp._2, tmp._3, tmp._4);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.beforeTextChanged(tmp._1, tmp._2, tmp._3, tmp._4);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.addTextChangedListener(this);
        }

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            dispatcher.onNext(Tuple.of(s, start, count, after));
        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            // Do nothing
        }

        @Override
        public void afterTextChanged(Editable s) {
            // Do nothing
        }
    }

    static final class AfterTextChangedObserver extends Observer<TextView> implements TextWatcher {
        private final AfterTextChnagedListener listener;

        AfterTextChangedObserver(final AfterTextChnagedListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(TextView view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof Editable) {
                        @SuppressWarnings("unchecked")
                        final Editable tmp = (Editable) param;

                        if (Util.isUiThread()) {
                            listener.afterTextChanged(tmp);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.afterTextChanged(tmp);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.addTextChangedListener(this);
        }

        @Override
        public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            // Do nothing
        }

        @Override
        public void onTextChanged(CharSequence s, int start, int before, int count) {
            // Do nothing
        }

        @Override
        public void afterTextChanged(Editable s) {
            dispatcher.onNext(s);
        }
    }

    static final class OnQueryTextChangeObserver extends Observer<SearchView> implements OnQueryTextListener {
        private final OnQueryTextChangeListener listener;

        OnQueryTextChangeObserver(final OnQueryTextChangeListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(SearchView view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof String) {
                        @SuppressWarnings("unchecked")
                        final String tmp = (String) param;

                        if (Util.isUiThread()) {
                            listener.onQueryTextChange(tmp);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.onQueryTextChange(tmp);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.setOnQueryTextListener(this);
        }

        @Override
        public boolean onQueryTextChange(String newText) {
            dispatcher.onNext(newText);

            return true;
        }

        @Override
        public boolean onQueryTextSubmit(String arg0) {
            return false;
        }
    }

    static final class OnQueryTextSubmitObserver extends Observer<SearchView> implements OnQueryTextListener {
        private final OnQueryTextSubmitListener listener;

        OnQueryTextSubmitObserver(final OnQueryTextSubmitListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(SearchView view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof String) {
                        @SuppressWarnings("unchecked")
                        final String tmp = (String) param;

                        if (Util.isUiThread()) {
                            listener.onQueryTextSubmit(tmp);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.onQueryTextSubmit(tmp);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.setOnQueryTextListener(this);
        }

        @Override
        public boolean onQueryTextChange(String newText) {
            // Do nothing.

            return false;
        }

        @Override
        public boolean onQueryTextSubmit(String query) {
            dispatcher.onNext(query);

            return true;
        }
    }

    static final class OnDragObserver extends Observer<View> implements OnDragListener {
        private final OnDragListener listener;

        OnDragObserver(final OnDragListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(View view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof Tuple2) {
                        @SuppressWarnings("unchecked")
                        final Tuple2<View, DragEvent> tmp = (Tuple2<View, DragEvent>) param;

                        if (Util.isUiThread()) {
                            listener.onDrag(tmp._1, tmp._2);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.onDrag(tmp._1, tmp._2);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.setOnDragListener(this);
        }

        @Override
        public boolean onDrag(View view, DragEvent dragEvent) {
            dispatcher.onNext(Tuple.of(view, dragEvent));
            return true;
        }
    }

    //    static final class OnScrollChangeObserver extends Observer<View> implements OnScrollChangeListener {
    //        private final OnScrollChangeListener listener;
    //
    //        OnScrollChangeObserver(final OnScrollChangeListener listener) {
    //            this.listener = listener;
    //        }
    //
    //        @Override
    //        public void observe(View view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
    //            N.requireNonNull(view, "view");
    //            N.requireNonNull(onError, "onError");
    //            N.requireNonNull(onComplete, "onComplete");
    //
    //            dispatcher.append(new Dispatcher<Object>() {
    //                @Override
    //                public void onNext(Object param) {
    //                    if (param instanceof Tuple5) {
    //                        @SuppressWarnings("unchecked")
    //                        final Tuple5<View, Integer, Integer, Integer, Integer> tmp = (Tuple5<View, Integer, Integer, Integer, Integer>) param;
    //
    //                        if (Util.isUiThread()) {
    //                            listener.onScrollChange(tmp._1, tmp._2, tmp._3, tmp._4, tmp._5);
    //                        } else {
    //                            UIExecutor.execute(new Runnable() {
    //                                @Override
    //                                public void run() {
    //                                    listener.onScrollChange(tmp._1, tmp._2, tmp._3, tmp._4, tmp._5);
    //                                }
    //                            });
    //                        }
    //                    }
    //                }
    //
    //                @Override
    //                public void onError(final Throwable error) {
    //                    onError.accept(error);
    //                }
    //
    //                @Override
    //                public void onComplete() {
    //                    onComplete.run();
    //                }
    //            });
    //
    //            view.setOnScrollChangeListener(this);
    //        }
    //
    //        @Override
    //        public void onScrollChange(View v, int scrollX, int scrollY, int oldScrollX, int oldScrollY) {
    //            dispatcher.onNext(Tuple.of(v, scrollX, scrollY, oldScrollX, oldScrollY));
    //        }
    //    }

    static final class OnChildViewAddedObserver extends Observer<ViewGroup> implements OnHierarchyChangeListener {
        private final OnChildViewAddedListener listener;

        OnChildViewAddedObserver(final OnChildViewAddedListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(ViewGroup view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof Tuple2) {
                        @SuppressWarnings("unchecked")
                        final Tuple2<View, View> tmp = (Tuple2<View, View>) param;

                        if (Util.isUiThread()) {
                            listener.onChildViewAdded(tmp._1, tmp._2);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.onChildViewAdded(tmp._1, tmp._2);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.setOnHierarchyChangeListener(this);
        }

        @Override
        public void onChildViewAdded(View parent, View child) {
            dispatcher.onNext(Tuple.of(parent, child));
        }

        @Override
        public void onChildViewRemoved(View parent, View child) {
            // Do nothing
        }
    }

    static final class OnChildViewRemovedObserver extends Observer<ViewGroup> implements OnHierarchyChangeListener {
        private final OnChildViewRemovedListener listener;

        OnChildViewRemovedObserver(final OnChildViewRemovedListener listener) {
            this.listener = listener;
        }

        @Override
        public void observe(ViewGroup view, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(view, "view");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new Dispatcher<Object>() {
                @Override
                public void onNext(Object param) {
                    if (param instanceof Tuple2) {
                        @SuppressWarnings("unchecked")
                        final Tuple2<View, View> tmp = (Tuple2<View, View>) param;

                        if (Util.isUiThread()) {
                            listener.onChildViewRemoved(tmp._1, tmp._2);
                        } else {
                            UIExecutor.execute(new Runnable() {
                                @Override
                                public void run() {
                                    listener.onChildViewRemoved(tmp._1, tmp._2);
                                }
                            });
                        }
                    }
                }

                @Override
                public void onError(final Throwable error) {
                    onError.accept(error);
                }

                @Override
                public void onComplete() {
                    onComplete.run();
                }
            });

            view.setOnHierarchyChangeListener(this);
        }

        @Override
        public void onChildViewAdded(View parent, View child) {
            // Do nothing
        }

        @Override
        public void onChildViewRemoved(View parent, View child) {
            dispatcher.onNext(Tuple.of(parent, child));
        }
    }

    public static interface OnTextChnagedListener {
        void onTextChanged(CharSequence s, int start, int before, int count);
    }

    public static interface BeforeTextChnagedListener {
        void beforeTextChanged(CharSequence s, int start, int count, int after);
    }

    public static interface AfterTextChnagedListener {
        void afterTextChanged(Editable s);
    }

    public static interface OnQueryTextChangeListener {
        void onQueryTextChange(String newText);
    }

    public static interface OnQueryTextSubmitListener {
        void onQueryTextSubmit(String query);
    }

    public static interface OnChildViewAddedListener {
        void onChildViewAdded(View parent, View child);
    }

    public static interface OnChildViewRemovedListener {
        void onChildViewRemoved(View parent, View child);
    }
}