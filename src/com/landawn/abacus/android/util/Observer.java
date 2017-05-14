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

import java.util.concurrent.TimeUnit;

import com.landawn.abacus.android.util.AsyncExecutor.UIExecutor;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Tuple;
import com.landawn.abacus.util.Tuple.Tuple2;
import com.landawn.abacus.util.Tuple.Tuple3;
import com.landawn.abacus.util.Tuple.Tuple4;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.TriConsumer;

import android.text.Editable;
import android.text.TextWatcher;
import android.view.DragEvent;
import android.view.KeyEvent;
import android.view.MenuItem;
import android.view.MenuItem.OnMenuItemClickListener;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnDragListener;
import android.view.View.OnHoverListener;
import android.view.View.OnKeyListener;
import android.view.View.OnLongClickListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.view.ViewGroup.OnHierarchyChangeListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.AutoCompleteTextView;
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
public abstract class Observer<T> extends com.landawn.abacus.util.Observer<T> {
    Observer() {

    }

    public static <T extends View, O extends ViewObserver<T, O>> ViewObserver<T, O> of(final T view) {
        return new ViewObserver<>(view);
    }

    public static <T extends ViewGroup, O extends ViewGroupObserver<T, O>> ViewGroupObserver<T, O> of(final T view) {
        return new ViewGroupObserver<>(view);
    }

    public static <T extends TextView, O extends TextViewObserver<T, O>> TextViewObserver<T, O> of(final T view) {
        return new TextViewObserver<>(view);
    }

    public static <T extends SearchView, O extends SearchViewObserver<T, O>> SearchViewObserver<T, O> of(final T view) {
        return new SearchViewObserver<>(view);
    }

    public static <T extends MenuItem, O extends MenuItemObserver<T, O>> MenuItemObserver<T, O> of(final T view) {
        return new MenuItemObserver<>(view);
    }

    public static <T extends AutoCompleteTextView, O extends AutoCompleteTextViewObserver<T, O>> AutoCompleteTextViewObserver<T, O> of(final T view) {
        return new AutoCompleteTextViewObserver<>(view);
    }

    protected static abstract class ObserverBase<T, O extends ObserverBase<T, O>> extends Observer<T> {
        protected ObserverBase() {
        }

        @Override
        public O debounce(final long intervalDurationInMillis) {
            return (O) super.debounce(intervalDurationInMillis);
        }

        @Override
        public O debounce(final long intervalDuration, final TimeUnit unit) {
            return (O) super.debounce(intervalDuration, unit);
        }

        @Override
        public O throttleFirst(final long intervalDurationInMillis) {
            return (O) super.throttleFirst(intervalDurationInMillis);
        }

        @Override
        public O throttleFirst(final long intervalDuration, final TimeUnit unit) {
            return (O) super.throttleFirst(intervalDuration, unit);
        }

        @Override
        public O throttleLast(final long intervalDurationInMillis) {
            return (O) super.throttleLast(intervalDurationInMillis);
        }

        @Override
        public O throttleLast(final long intervalDuration, final TimeUnit unit) {
            return (O) super.throttleLast(intervalDuration, unit);
        }

        @Override
        public O delay(final long delayInMillis) {
            return (O) super.delay(delayInMillis);
        }

        @Override
        public O delay(final long delay, final TimeUnit unit) {
            return (O) super.delay(delay, unit);
        }

        /**
         * @deprecated
         */
        @Override
        @Deprecated
        public void observe(Consumer<? super T> action) {
            throw new UnsupportedOperationException();
        }
    }

    protected static abstract class DispatcherBase<T> extends Dispatcher<T> {
        private final Consumer<? super Throwable> onError;
        private final Runnable onComplete;

        protected DispatcherBase(final Consumer<? super Throwable> onError, final Runnable onComplete) {
            this.onError = onError;
            this.onComplete = onComplete;
        }

        @Override
        public void onError(final Throwable error) {
            onError.accept(error);
        }

        @Override
        public void onComplete() {
            onComplete.run();
        }
    }

    public static class ViewObserver<T extends View, O extends ViewObserver<T, O>> extends ObserverBase<T, O> {
        final T view;

        ViewObserver(final T view) {
            this.view = view;
        }

        public void onClick(final Consumer<? super View> onNext) {
            onClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onClick(final Consumer<? super View> onNext, final Consumer<? super Throwable> onError) {
            onClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onClick(final Consumer<? super View> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final View tmp = (View) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View view) {
                    dispatcher.onNext(view);
                };
            });
        }

        public void onLongClick(final Consumer<? super View> onNext) {
            onLongClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onLongClick(final Consumer<? super View> onNext, final Consumer<? super Throwable> onError) {
            onLongClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onLongClick(final Consumer<? super View> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final View tmp = (View) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.setOnLongClickListener(new OnLongClickListener() {
                @Override
                public boolean onLongClick(View view) {
                    dispatcher.onNext(view);
                    return true;
                };
            });
        }

        public void onDrag(final BiConsumer<? super View, ? super DragEvent> onNext) {
            onDrag(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onDrag(final BiConsumer<? super View, ? super DragEvent> onNext, final Consumer<? super Throwable> onError) {
            onDrag(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onDrag(final BiConsumer<? super View, ? super DragEvent> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<View, DragEvent> tmp = (Tuple2<View, DragEvent>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            view.setOnDragListener(new OnDragListener() {
                @Override
                public boolean onDrag(View view, DragEvent dragEvent) {
                    dispatcher.onNext(Tuple.of(view, dragEvent));
                    return true;
                }
            });
        }

        public void onTouch(final BiConsumer<? super View, ? super MotionEvent> onNext) {
            onTouch(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onTouch(final BiConsumer<? super View, ? super MotionEvent> onNext, final Consumer<? super Throwable> onError) {
            onTouch(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onTouch(final BiConsumer<? super View, ? super MotionEvent> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<View, MotionEvent> tmp = (Tuple2<View, MotionEvent>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            view.setOnTouchListener(new OnTouchListener() {
                @Override
                public boolean onTouch(View view, MotionEvent dragEvent) {
                    dispatcher.onNext(Tuple.of(view, dragEvent));
                    return true;
                }
            });
        }

        public void onHover(final BiConsumer<? super View, ? super MotionEvent> onNext) {
            onHover(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onHover(final BiConsumer<? super View, ? super MotionEvent> onNext, final Consumer<? super Throwable> onError) {
            onHover(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onHover(final BiConsumer<? super View, ? super MotionEvent> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<View, MotionEvent> tmp = (Tuple2<View, MotionEvent>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            view.setOnHoverListener(new OnHoverListener() {
                @Override
                public boolean onHover(View view, MotionEvent dragEvent) {
                    dispatcher.onNext(Tuple.of(view, dragEvent));
                    return true;
                }
            });
        }

        public void onKey(final TriConsumer<? super View, ? super Integer, ? super KeyEvent> onNext) {
            onKey(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onKey(final TriConsumer<? super View, ? super Integer, ? super KeyEvent> onNext, final Consumer<? super Throwable> onError) {
            onKey(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onKey(final TriConsumer<? super View, ? super Integer, ? super KeyEvent> onNext, final Consumer<? super Throwable> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple3<View, Integer, KeyEvent> tmp = (Tuple3<View, Integer, KeyEvent>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp._1, tmp._2, tmp._3);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp._1, tmp._2, tmp._3);
                            }
                        });
                    }
                }
            });

            view.setOnKeyListener(new OnKeyListener() {
                @Override
                public boolean onKey(View view, int keyCode, KeyEvent event) {
                    dispatcher.onNext(Tuple.of(view, keyCode, event));
                    return true;
                }
            });
        }
    }

    public static class ViewGroupObserver<T extends ViewGroup, O extends ViewGroupObserver<T, O>> extends ViewObserver<T, ViewGroupObserver<T, O>> {

        ViewGroupObserver(final T view) {
            super(view);
        }

        public void onChildViewAdded(final BiConsumer<? super View, ? super View> onNext) {
            onChildViewAdded(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onChildViewAdded(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Throwable> onError) {
            onChildViewAdded(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onChildViewAdded(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Throwable> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple2<View, View> tmp = (Tuple2<View, View>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            view.setOnHierarchyChangeListener(new OnHierarchyChangeListener() {
                @Override
                public void onChildViewAdded(View parent, View child) {
                    dispatcher.onNext(Tuple.of(parent, child));
                }

                @Override
                public void onChildViewRemoved(View parent, View child) {
                    // Do nothing
                }
            });
        }

        public void onChildViewRemoved(final BiConsumer<? super View, ? super View> onNext) {
            onChildViewRemoved(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onChildViewRemoved(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Throwable> onError) {
            onChildViewRemoved(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onChildViewRemoved(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Throwable> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple2<View, View> tmp = (Tuple2<View, View>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            view.setOnHierarchyChangeListener(new OnHierarchyChangeListener() {
                @Override
                public void onChildViewAdded(View parent, View child) {
                    // Do nothing
                }

                @Override
                public void onChildViewRemoved(View parent, View child) {
                    dispatcher.onNext(Tuple.of(parent, child));
                }
            });
        }
    }

    public static class TextViewObserver<T extends TextView, O extends TextViewObserver<T, O>> extends ViewObserver<T, O> {

        TextViewObserver(final T view) {
            super(view);
        }

        public void onTextChanged(final Consumer<? super String> onNext) {
            onTextChanged(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onTextChanged(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError) {
            onTextChanged(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onTextChanged(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                    // Do nothing
                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {
                    dispatcher.onNext(s.toString());
                }

                @Override
                public void afterTextChanged(Editable s) {
                    // Do nothing
                }
            });
        }

        public void onTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext) {
            onTextChanged2(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext, final Consumer<? super Throwable> onError) {
            onTextChanged2(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext, final Consumer<? super Throwable> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple4<CharSequence, Integer, Integer, Integer> tmp = (Tuple4<CharSequence, Integer, Integer, Integer>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.addTextChangedListener(new TextWatcher() {
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
            });
        }

        public void beforeTextChanged(final Consumer<? super String> onNext) {
            beforeTextChanged(onNext, Fu.ON_ERROR_MISSING);
        }

        public void beforeTextChanged(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError) {
            beforeTextChanged(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void beforeTextChanged(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int before, int count) {
                    dispatcher.onNext(s.toString());
                }

                @Override
                public void onTextChanged(CharSequence s, int start, int count, int after) {
                    // Do nothing
                }

                @Override
                public void afterTextChanged(Editable s) {
                    // Do nothing
                }
            });
        }

        public void beforeTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext) {
            beforeTextChanged2(onNext, Fu.ON_ERROR_MISSING);
        }

        public void beforeTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext,
                final Consumer<? super Throwable> onError) {
            beforeTextChanged2(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void beforeTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext,
                final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple4<CharSequence, Integer, Integer, Integer> tmp = (Tuple4<CharSequence, Integer, Integer, Integer>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int before, int count) {
                    dispatcher.onNext(Tuple.of(s, start, before, count));
                }

                @Override
                public void onTextChanged(CharSequence s, int start, int count, int after) {
                    // Do nothing
                }

                @Override
                public void afterTextChanged(Editable s) {
                    // Do nothing
                }
            });
        }

        public void afterTextChanged(final Consumer<? super String> onNext) {
            afterTextChanged(onNext, Fu.ON_ERROR_MISSING);
        }

        public void afterTextChanged(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError) {
            afterTextChanged(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void afterTextChanged(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int after, int count) {
                    // Do nothing
                }

                @Override
                public void onTextChanged(CharSequence s, int start, int count, int after) {
                    // Do nothing
                }

                @Override
                public void afterTextChanged(Editable s) {
                    dispatcher.onNext(s.toString());
                }
            });
        }

        public void afterTextChanged2(final Consumer<? super Editable> onNext) {
            afterTextChanged2(onNext, Fu.ON_ERROR_MISSING);
        }

        public void afterTextChanged2(final Consumer<? super Editable> onNext, final Consumer<? super Throwable> onError) {
            afterTextChanged2(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void afterTextChanged2(final Consumer<? super Editable> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Editable tmp = (Editable) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.addTextChangedListener(new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int after, int count) {
                    // Do nothing
                }

                @Override
                public void onTextChanged(CharSequence s, int start, int count, int after) {
                    // Do nothing
                }

                @Override
                public void afterTextChanged(Editable s) {
                    dispatcher.onNext(s);
                }
            });
        }

    }

    public static class SearchViewObserver<T extends SearchView, O extends SearchViewObserver<T, O>> extends ViewGroupObserver<T, O> {

        SearchViewObserver(final T view) {
            super(view);
        }

        public void onQueryTextChange(final Consumer<? super String> onNext) {
            onQueryTextChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onQueryTextChange(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError) {
            onQueryTextChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onQueryTextChange(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.setOnQueryTextListener(new OnQueryTextListener() {
                @Override
                public boolean onQueryTextChange(String newText) {
                    dispatcher.onNext(newText);
                    return true;
                }

                @Override
                public boolean onQueryTextSubmit(String query) {
                    // Do nothing.
                    return false;
                }
            });
        }

        public void onQueryTextSubmit(final Consumer<? super String> onNext) {
            onQueryTextSubmit(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onQueryTextSubmit(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError) {
            onQueryTextSubmit(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onQueryTextSubmit(final Consumer<? super String> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.setOnQueryTextListener(new OnQueryTextListener() {
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
            });
        }
    }

    public static class MenuItemObserver<T extends MenuItem, O extends MenuItemObserver<T, O>> extends ObserverBase<T, O> {
        final MenuItem view;

        MenuItemObserver(final MenuItem view) {
            this.view = view;
        }

        public void onMenuItemClick(final Consumer<? super MenuItem> onNext) {
            onMenuItemClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onMenuItemClick(final Consumer<? super MenuItem> onNext, final Consumer<? super Throwable> onError) {
            onMenuItemClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onMenuItemClick(final Consumer<? super MenuItem> onNext, final Consumer<? super Throwable> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final MenuItem tmp = (MenuItem) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.setOnMenuItemClickListener(new OnMenuItemClickListener() {
                @Override
                public boolean onMenuItemClick(MenuItem item) {
                    dispatcher.onNext(item);
                    return true;
                };
            });
        }
    }

    public static class AutoCompleteTextViewObserver<T extends AutoCompleteTextView, O extends AutoCompleteTextViewObserver<T, O>>
            extends TextViewObserver<T, O> {

        AutoCompleteTextViewObserver(final T view) {
            super(view);
        }

        public void onItemClick(final Consumer<? super Tuple4<AdapterView<?>, View, Integer, Integer>> onNext) {
            onItemClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public void onItemClick(final Consumer<? super Tuple4<AdapterView<?>, View, Integer, Integer>> onNext, final Consumer<? super Throwable> onError) {
            onItemClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public void onItemClick(final Consumer<? super Tuple4<AdapterView<?>, View, Integer, Integer>> onNext, final Consumer<? super Throwable> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple4<AdapterView<?>, View, Integer, Integer> tmp = (Tuple4<AdapterView<?>, View, Integer, Integer>) param;

                    if (Util.isUiThread()) {
                        onNext.accept(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.accept(tmp);
                            }
                        });
                    }
                }
            });

            view.setOnItemClickListener(new OnItemClickListener() {
                @Override
                public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                    dispatcher.onNext(Tuple.of(parent, view, position, id));
                }
            });
        }
    }
}