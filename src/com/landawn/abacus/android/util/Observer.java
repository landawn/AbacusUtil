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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

import com.landawn.abacus.android.util.Async.UIExecutor;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Timed;
import com.landawn.abacus.util.Tuple;
import com.landawn.abacus.util.Tuple.Tuple2;
import com.landawn.abacus.util.Tuple.Tuple3;
import com.landawn.abacus.util.Tuple.Tuple4;
import com.landawn.abacus.util.Tuple.Tuple5;
import com.landawn.abacus.util.Tuple.Tuple9;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;

import android.text.Editable;
import android.text.TextWatcher;
import android.view.DragEvent;
import android.view.KeyEvent;
import android.view.MenuItem;
import android.view.MenuItem.OnMenuItemClickListener;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnAttachStateChangeListener;
import android.view.View.OnClickListener;
import android.view.View.OnDragListener;
import android.view.View.OnFocusChangeListener;
import android.view.View.OnHoverListener;
import android.view.View.OnKeyListener;
import android.view.View.OnLayoutChangeListener;
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
        super();
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

    public static <T extends AutoCompleteTextView, O extends AutoCompleteTextViewObserver<T, O>> AutoCompleteTextViewObserver<T, O> of(final T view) {
        return new AutoCompleteTextViewObserver<>(view);
    }

    public static <T extends MenuItem, O extends MenuItemObserver<T, O>> MenuItemObserver<T, O> of(final T menuItem) {
        return new MenuItemObserver<>(menuItem);
    }

    protected static abstract class UIObserverBase<T, O extends UIObserverBase<T, O>> extends Observer<T> implements Disposable {
        final List<Runnable> disposeActions = new ArrayList<>();
        boolean isDisposed = false;

        protected UIObserverBase() {
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
         * 
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<Timed<T>> timeInterval() {
            throw new UnsupportedOperationException();
        }

        /**
         * 
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<Timed<T>> timestamp() {
            throw new UnsupportedOperationException();
        }

        @Override
        public O skip(final long n) {
            return (O) super.skip(n);

        }

        @Override
        public O limit(final long n) {
            return (O) super.limit(n);

        }

        /*
         * Is it possible to cause memory leak by caching the previous values?
         */
        /**
         * 
         * @return
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<T> distinct() {
            throw new UnsupportedOperationException();
        }

        /*
         * Is it possible to cause memory leak by caching the previous values?
         */
        /**
         * 
         * @param keyExtractor
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<T> distinctBy(final Function<? super T, ?> keyExtractor) {
            throw new UnsupportedOperationException();
        }

        @Override
        public O filter(final Predicate<? super T> filter) {
            return (O) super.filter(filter);
        }

        /**
         * @param map
         * @deprecated
         */
        @Deprecated
        @Override
        public <U> Observer<U> map(final Function<? super T, U> map) {
            throw new UnsupportedOperationException();
        }

        /**
         * @param map
         * @deprecated
         */
        @Deprecated
        @Override
        public <U> Observer<U> flatMap(final Function<? super T, Collection<U>> map) {
            throw new UnsupportedOperationException();
        }

        /**
         * @param timespan
         * @param unit
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<List<T>> buffer(final long timespan, final TimeUnit unit) {
            throw new UnsupportedOperationException();
        }

        /**
         * @param timespan
         * @param unit
         * @param count
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<List<T>> buffer(final long timespan, final TimeUnit unit, final int count) {
            throw new UnsupportedOperationException();
        }

        /**
         * @param timespan
         * @param timeskip
         * @param unit
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<List<T>> buffer(final long timespan, final long timeskip, final TimeUnit unit) {
            throw new UnsupportedOperationException();
        }

        /**
         * @param timespan
         * @param timeskip
         * @param unit
         * @param count
         * @deprecated
         */
        @Deprecated
        @Override
        public Observer<List<T>> buffer(final long timespan, final long timeskip, final TimeUnit unit, final int count) {
            throw new UnsupportedOperationException();
        }

        /**
         * @param action
         * 
         * @deprecated
         */
        @Deprecated
        @Override
        public void observe(Consumer<? super T> action) {
            throw new UnsupportedOperationException();
        }

        /**
         * 
         * @deprecated
         */
        @Deprecated
        @Override
        public void observe(final Consumer<? super T> action, final Consumer<? super Exception> onError) {
            throw new UnsupportedOperationException();
        }

        /**
         * 
         * @deprecated
         */
        @Deprecated
        @Override
        public void observe(final Consumer<? super T> action, final Consumer<? super Exception> onError, final Runnable onComplete) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void dispose() {
            if (isDisposed() == false) {
                isDisposed = true;
                if (N.notNullOrEmpty(disposeActions)) {
                    for (Runnable action : disposeActions) {
                        action.run();
                    }
                }
            }
        }

        @Override
        public boolean isDisposed() {
            return isDisposed;
        }
    }

    public static class ViewObserver<T extends View, O extends ViewObserver<T, O>> extends UIObserverBase<T, O> {
        final T _view;

        ViewObserver(final T view) {
            this._view = view;
        }

        public Disposable onClick(final OnClickListener onNext) {
            return onClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onClick(final OnClickListener onNext, final Consumer<? super Exception> onError) {
            return onClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onClick(final OnClickListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final View tmp = (View) param;

                    if (Fu.isUiThread()) {
                        onNext.onClick(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onClick(tmp);
                            }
                        });
                    }
                }
            });

            _view.setOnClickListener(new OnClickListener() {
                @Override
                public void onClick(View view) {
                    dispatcher.onNext(view);
                };
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnClickListener(null);
                }
            });

            return this;
        }

        public Disposable onLongClick(final OnLongClickListener onNext) {
            return onLongClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onLongClick(final OnLongClickListener onNext, final Consumer<? super Exception> onError) {
            return onLongClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onLongClick(final OnLongClickListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final View tmp = (View) param;

                    if (Fu.isUiThread()) {
                        onNext.onLongClick(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onLongClick(tmp);
                            }
                        });
                    }
                }
            });

            _view.setOnLongClickListener(new OnLongClickListener() {
                @Override
                public boolean onLongClick(View view) {
                    dispatcher.onNext(view);
                    return true;
                };
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnLongClickListener(null);
                }
            });

            return this;
        }

        public Disposable onDrag(final OnDragListener onNext) {
            return onDrag(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onDrag(final OnDragListener onNext, final Consumer<? super Exception> onError) {
            return onDrag(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onDrag(final OnDragListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<View, DragEvent> tmp = (Tuple2<View, DragEvent>) param;

                    if (Fu.isUiThread()) {
                        onNext.onDrag(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onDrag(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            _view.setOnDragListener(new OnDragListener() {
                @Override
                public boolean onDrag(View view, DragEvent dragEvent) {
                    dispatcher.onNext(Tuple.of(view, dragEvent));
                    return true;
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnDragListener(null);
                }
            });

            return this;
        }

        public Disposable onTouch(final OnTouchListener onNext) {
            return onTouch(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onTouch(final OnTouchListener onNext, final Consumer<? super Exception> onError) {
            return onTouch(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onTouch(final OnTouchListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<View, MotionEvent> tmp = (Tuple2<View, MotionEvent>) param;

                    if (Fu.isUiThread()) {
                        onNext.onTouch(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onTouch(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            _view.setOnTouchListener(new OnTouchListener() {
                @Override
                public boolean onTouch(View view, MotionEvent dragEvent) {
                    dispatcher.onNext(Tuple.of(view, dragEvent));
                    return true;
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnTouchListener(null);
                }
            });

            return this;
        }

        public Disposable onFocusChange(final OnFocusChangeListener onNext) {
            return onFocusChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onFocusChange(final OnFocusChangeListener onNext, final Consumer<? super Exception> onError) {
            return onFocusChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onFocusChange(final OnFocusChangeListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<View, Boolean> tmp = (Tuple2<View, Boolean>) param;

                    if (Fu.isUiThread()) {
                        onNext.onFocusChange(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onFocusChange(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            _view.setOnFocusChangeListener(new OnFocusChangeListener() {
                @Override
                public void onFocusChange(View v, boolean hasFocus) {
                    dispatcher.onNext(Tuple.of(v, hasFocus));
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnFocusChangeListener(null);
                }
            });

            return this;
        }

        public Disposable onHover(final OnHoverListener onNext) {
            return onHover(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onHover(final OnHoverListener onNext, final Consumer<? super Exception> onError) {
            return onHover(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onHover(final OnHoverListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<View, MotionEvent> tmp = (Tuple2<View, MotionEvent>) param;

                    if (Fu.isUiThread()) {
                        onNext.onHover(tmp._1, tmp._2);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onHover(tmp._1, tmp._2);
                            }
                        });
                    }
                }
            });

            _view.setOnHoverListener(new OnHoverListener() {
                @Override
                public boolean onHover(View view, MotionEvent dragEvent) {
                    dispatcher.onNext(Tuple.of(view, dragEvent));
                    return true;
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnHoverListener(null);
                }
            });

            return this;
        }

        public Disposable onKey(final OnKeyListener onNext) {
            return onKey(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onKey(final OnKeyListener onNext, final Consumer<? super Exception> onError) {
            return onKey(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onKey(final OnKeyListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple3<View, Integer, KeyEvent> tmp = (Tuple3<View, Integer, KeyEvent>) param;

                    if (Fu.isUiThread()) {
                        onNext.onKey(tmp._1, tmp._2, tmp._3);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onKey(tmp._1, tmp._2, tmp._3);
                            }
                        });
                    }
                }
            });

            _view.setOnKeyListener(new OnKeyListener() {
                @Override
                public boolean onKey(View view, int keyCode, KeyEvent event) {
                    dispatcher.onNext(Tuple.of(view, keyCode, event));
                    return true;
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnKeyListener(null);
                }
            });

            return this;
        }

        public Disposable onViewAttachedToWindow(final Consumer<? super View> onNext) {
            return onViewAttachedToWindow(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onViewAttachedToWindow(final Consumer<? super View> onNext, final Consumer<? super Exception> onError) {
            return onViewAttachedToWindow(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onViewAttachedToWindow(final Consumer<? super View> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final View tmp = (View) param;

                    if (Fu.isUiThread()) {
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

            final OnAttachStateChangeListener onAttachStateChangeListener = new OnAttachStateChangeListener() {
                @Override
                public void onViewAttachedToWindow(View view) {
                    dispatcher.onNext(view);
                }

                @Override
                public void onViewDetachedFromWindow(View view) {
                    // Do nothing
                };
            };

            _view.addOnAttachStateChangeListener(onAttachStateChangeListener);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeOnAttachStateChangeListener(onAttachStateChangeListener);
                }
            });

            return this;
        }

        public Disposable onViewDetachedFromWindow(final Consumer<? super View> onNext) {
            return onViewDetachedFromWindow(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onViewDetachedFromWindow(final Consumer<? super View> onNext, final Consumer<? super Exception> onError) {
            return onViewDetachedFromWindow(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onViewDetachedFromWindow(final Consumer<? super View> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final View tmp = (View) param;

                    if (Fu.isUiThread()) {
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

            final OnAttachStateChangeListener onAttachStateChangeListener = new OnAttachStateChangeListener() {
                @Override
                public void onViewAttachedToWindow(View view) {
                    // Do nothing
                }

                @Override
                public void onViewDetachedFromWindow(View view) {
                    dispatcher.onNext(view);
                };
            };

            _view.addOnAttachStateChangeListener(onAttachStateChangeListener);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeOnAttachStateChangeListener(onAttachStateChangeListener);
                }
            });

            return this;
        }

        public Disposable onAttachStateChange(final OnAttachStateChangeListener onNext) {
            return onAttachStateChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onAttachStateChange(final OnAttachStateChangeListener onNext, final Consumer<? super Exception> onError) {
            return onAttachStateChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onAttachStateChange(final OnAttachStateChangeListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple2<Integer, View> tmp = (Tuple2<Integer, View>) param;

                    if (Fu.isUiThread()) {
                        if (tmp._1 == 0) {
                            onNext.onViewAttachedToWindow(tmp._2);
                        } else {
                            onNext.onViewDetachedFromWindow(tmp._2);
                        }
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                if (tmp._1 == 0) {
                                    onNext.onViewAttachedToWindow(tmp._2);
                                } else {
                                    onNext.onViewDetachedFromWindow(tmp._2);
                                }
                            }
                        });
                    }
                }
            });

            final OnAttachStateChangeListener onAttachStateChangeListener = new OnAttachStateChangeListener() {
                @Override
                public void onViewAttachedToWindow(View view) {
                    dispatcher.onNext(Tuple.of(0, view));
                }

                @Override
                public void onViewDetachedFromWindow(View view) {
                    dispatcher.onNext(Tuple.of(1, view));
                };
            };

            _view.addOnAttachStateChangeListener(onAttachStateChangeListener);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeOnAttachStateChangeListener(onAttachStateChangeListener);
                }
            });

            return this;
        }

        public Disposable onLayoutChange(final OnLayoutChangeListener onNext) {
            return onLayoutChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onLayoutChange(final OnLayoutChangeListener onNext, final Consumer<? super Exception> onError) {
            return onLayoutChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onLayoutChange(final OnLayoutChangeListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple9<View, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer> tmp = (Tuple9<View, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer>) param;

                    if (Fu.isUiThread()) {
                        onNext.onLayoutChange(tmp._1, tmp._2, tmp._3, tmp._4, tmp._5, tmp._6, tmp._7, tmp._8, tmp._9);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onLayoutChange(tmp._1, tmp._2, tmp._3, tmp._4, tmp._5, tmp._6, tmp._7, tmp._8, tmp._9);
                            }
                        });
                    }
                }
            });

            final OnLayoutChangeListener onAttachStateChangeListener = new OnLayoutChangeListener() {
                @Override
                public void onLayoutChange(View v, int left, int top, int right, int bottom, int oldLeft, int oldTop, int oldRight, int oldBottom) {
                    dispatcher.onNext(Tuple.of(v, left, top, right, bottom, oldLeft, oldTop, oldRight, oldBottom));
                };
            };

            _view.addOnLayoutChangeListener(onAttachStateChangeListener);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeOnLayoutChangeListener(onAttachStateChangeListener);
                }
            });

            return this;
        }

        public Disposable onLayoutChange(final Consumer<? super Tuple9<View, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer>> onNext) {
            return onLayoutChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onLayoutChange(final Consumer<? super Tuple9<View, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError) {
            return onLayoutChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onLayoutChange(final Consumer<? super Tuple9<View, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple9<View, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer> tmp = (Tuple9<View, Integer, Integer, Integer, Integer, Integer, Integer, Integer, Integer>) param;

                    if (Fu.isUiThread()) {
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

            final OnLayoutChangeListener onAttachStateChangeListener = new OnLayoutChangeListener() {
                @Override
                public void onLayoutChange(View v, int left, int top, int right, int bottom, int oldLeft, int oldTop, int oldRight, int oldBottom) {
                    dispatcher.onNext(Tuple.of(v, left, top, right, bottom, oldLeft, oldTop, oldRight, oldBottom));
                };
            };

            _view.addOnLayoutChangeListener(onAttachStateChangeListener);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeOnLayoutChangeListener(onAttachStateChangeListener);
                }
            });

            return this;
        }
    }

    public static class ViewGroupObserver<T extends ViewGroup, O extends ViewGroupObserver<T, O>> extends ViewObserver<T, ViewGroupObserver<T, O>> {

        ViewGroupObserver(final T view) {
            super(view);
        }

        public Disposable onChildViewAdded(final BiConsumer<? super View, ? super View> onNext) {
            return onChildViewAdded(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onChildViewAdded(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Exception> onError) {
            return onChildViewAdded(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onChildViewAdded(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Exception> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple2<View, View> tmp = (Tuple2<View, View>) param;

                    if (Fu.isUiThread()) {
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

            _view.setOnHierarchyChangeListener(new OnHierarchyChangeListener() {
                @Override
                public void onChildViewAdded(View parent, View child) {
                    dispatcher.onNext(Tuple.of(parent, child));
                }

                @Override
                public void onChildViewRemoved(View parent, View child) {
                    // Do nothing
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnHierarchyChangeListener(null);
                }
            });

            return this;
        }

        public Disposable onChildViewRemoved(final BiConsumer<? super View, ? super View> onNext) {
            return onChildViewRemoved(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onChildViewRemoved(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Exception> onError) {
            return onChildViewRemoved(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onChildViewRemoved(final BiConsumer<? super View, ? super View> onNext, final Consumer<? super Exception> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple2<View, View> tmp = (Tuple2<View, View>) param;

                    if (Fu.isUiThread()) {
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

            _view.setOnHierarchyChangeListener(new OnHierarchyChangeListener() {
                @Override
                public void onChildViewAdded(View parent, View child) {
                    // Do nothing
                }

                @Override
                public void onChildViewRemoved(View parent, View child) {
                    dispatcher.onNext(Tuple.of(parent, child));
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnHierarchyChangeListener(null);
                }
            });

            return this;
        }

        public Disposable onHierarchyChange(final OnHierarchyChangeListener onNext) {
            return onHierarchyChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onHierarchyChange(final OnHierarchyChangeListener onNext, final Consumer<? super Exception> onError) {
            return onHierarchyChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onHierarchyChange(final OnHierarchyChangeListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple3<Integer, View, View> tmp = (Tuple3<Integer, View, View>) param;

                    if (Fu.isUiThread()) {
                        if (tmp._1 == 0) {
                            onNext.onChildViewAdded(tmp._2, tmp._3);
                        } else {
                            onNext.onChildViewRemoved(tmp._2, tmp._3);
                        }
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                if (tmp._1 == 0) {
                                    onNext.onChildViewAdded(tmp._2, tmp._3);
                                } else {
                                    onNext.onChildViewRemoved(tmp._2, tmp._3);
                                }
                            }
                        });
                    }
                }
            });

            _view.setOnHierarchyChangeListener(new OnHierarchyChangeListener() {
                @Override
                public void onChildViewAdded(View parent, View child) {
                    dispatcher.onNext(Tuple.of(0, parent, child));
                }

                @Override
                public void onChildViewRemoved(View parent, View child) {
                    dispatcher.onNext(Tuple.of(1, parent, child));
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnHierarchyChangeListener(null);
                }
            });

            return this;
        }
    }

    public static class TextViewObserver<T extends TextView, O extends TextViewObserver<T, O>> extends ViewObserver<T, O> {

        TextViewObserver(final T view) {
            super(view);
        }

        public Disposable onTextChanged(final Consumer<? super String> onNext) {
            return onTextChanged(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onTextChanged(final Consumer<? super String> onNext, final Consumer<? super Exception> onError) {
            return onTextChanged(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onTextChanged(final Consumer<? super String> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Fu.isUiThread()) {
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

            final TextWatcher textWatcher = new TextWatcher() {
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
            };

            _view.addTextChangedListener(textWatcher);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeTextChangedListener(textWatcher);
                }
            });

            return this;
        }

        public Disposable onTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext) {
            return onTextChanged2(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError) {
            return onTextChanged2(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple4<CharSequence, Integer, Integer, Integer> tmp = (Tuple4<CharSequence, Integer, Integer, Integer>) param;

                    if (Fu.isUiThread()) {
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

            final TextWatcher textWatcher = new TextWatcher() {
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
            };

            _view.addTextChangedListener(textWatcher);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeTextChangedListener(textWatcher);
                }
            });

            return this;
        }

        public Disposable beforeTextChanged(final Consumer<? super String> onNext) {
            return beforeTextChanged(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable beforeTextChanged(final Consumer<? super String> onNext, final Consumer<? super Exception> onError) {
            return beforeTextChanged(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable beforeTextChanged(final Consumer<? super String> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Fu.isUiThread()) {
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

            final TextWatcher textWatcher = new TextWatcher() {
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
            };

            _view.addTextChangedListener(textWatcher);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeTextChangedListener(textWatcher);
                }
            });

            return this;
        }

        public Disposable beforeTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext) {
            return beforeTextChanged2(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable beforeTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError) {
            return beforeTextChanged2(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable beforeTextChanged2(final Consumer<? super Tuple4<CharSequence, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple4<CharSequence, Integer, Integer, Integer> tmp = (Tuple4<CharSequence, Integer, Integer, Integer>) param;

                    if (Fu.isUiThread()) {
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

            final TextWatcher textWatcher = new TextWatcher() {
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
            };

            _view.addTextChangedListener(textWatcher);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeTextChangedListener(textWatcher);
                }
            });

            return this;
        }

        public Disposable afterTextChanged(final Consumer<? super String> onNext) {
            return afterTextChanged(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable afterTextChanged(final Consumer<? super String> onNext, final Consumer<? super Exception> onError) {
            return afterTextChanged(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable afterTextChanged(final Consumer<? super String> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Fu.isUiThread()) {
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

            final TextWatcher textWatcher = new TextWatcher() {
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
            };

            _view.addTextChangedListener(textWatcher);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeTextChangedListener(textWatcher);
                }
            });

            return this;
        }

        public Disposable afterTextChanged2(final Consumer<? super Editable> onNext) {
            return afterTextChanged2(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable afterTextChanged2(final Consumer<? super Editable> onNext, final Consumer<? super Exception> onError) {
            return afterTextChanged2(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable afterTextChanged2(final Consumer<? super Editable> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Editable tmp = (Editable) param;

                    if (Fu.isUiThread()) {
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

            final TextWatcher textWatcher = new TextWatcher() {
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
            };

            _view.addTextChangedListener(textWatcher);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeTextChangedListener(textWatcher);
                }
            });

            return this;
        }

        public Disposable onTextChanged(final TextWatcher onNext) {
            return onTextChanged(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onTextChanged(final TextWatcher onNext, final Consumer<? super Exception> onError) {
            return onTextChanged(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onTextChanged(final TextWatcher onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(final Object param) {
                    if (Fu.isUiThread()) {
                        if (param instanceof Tuple5) {
                            final Tuple5<Integer, CharSequence, Integer, Integer, Integer> tmp = (Tuple5<Integer, CharSequence, Integer, Integer, Integer>) param;

                            if (tmp._1 == 0) {
                                onNext.beforeTextChanged(tmp._2, tmp._3, tmp._4, tmp._5);
                            } else {
                                onNext.onTextChanged(tmp._2, tmp._3, tmp._4, tmp._5);
                            }
                        } else {
                            final Tuple2<Integer, Editable> tmp = (Tuple2<Integer, Editable>) param;

                            onNext.afterTextChanged(tmp._2);
                        }
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                if (param instanceof Tuple5) {
                                    final Tuple5<Integer, CharSequence, Integer, Integer, Integer> tmp = (Tuple5<Integer, CharSequence, Integer, Integer, Integer>) param;

                                    if (tmp._1 == 0) {
                                        onNext.beforeTextChanged(tmp._2, tmp._3, tmp._4, tmp._5);
                                    } else {
                                        onNext.onTextChanged(tmp._2, tmp._3, tmp._4, tmp._5);
                                    }
                                } else {
                                    final Tuple2<Integer, Editable> tmp = (Tuple2<Integer, Editable>) param;

                                    onNext.afterTextChanged(tmp._2);
                                }
                            }
                        });
                    }
                }
            });

            final TextWatcher textWatcher = new TextWatcher() {
                @Override
                public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                    dispatcher.onNext(Tuple.of(0, s, start, count, after));
                }

                @Override
                public void onTextChanged(CharSequence s, int start, int before, int count) {
                    dispatcher.onNext(Tuple.of(1, s, start, before, count));
                }

                @Override
                public void afterTextChanged(Editable s) {
                    dispatcher.onNext(Tuple.of(2, s));
                }
            };

            _view.addTextChangedListener(textWatcher);

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.removeTextChangedListener(textWatcher);
                }
            });

            return this;
        }
    }

    public static class SearchViewObserver<T extends SearchView, O extends SearchViewObserver<T, O>> extends ViewGroupObserver<T, O> {

        SearchViewObserver(final T view) {
            super(view);
        }

        public Disposable onQueryTextChange(final Consumer<? super String> onNext) {
            return onQueryTextChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onQueryTextChange(final Consumer<? super String> onNext, final Consumer<? super Exception> onError) {
            return onQueryTextChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onQueryTextChange(final Consumer<? super String> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Fu.isUiThread()) {
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

            _view.setOnQueryTextListener(new OnQueryTextListener() {
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

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnQueryTextListener(null);
                }
            });

            return this;
        }

        public Disposable onQueryTextSubmit(final Consumer<? super String> onNext) {
            return onQueryTextSubmit(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onQueryTextSubmit(final Consumer<? super String> onNext, final Consumer<? super Exception> onError) {
            return onQueryTextSubmit(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onQueryTextSubmit(final Consumer<? super String> onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final String tmp = (String) param;

                    if (Fu.isUiThread()) {
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

            _view.setOnQueryTextListener(new OnQueryTextListener() {
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

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnQueryTextListener(null);
                }
            });

            return this;
        }

        public Disposable onQueryText(final OnQueryTextListener onNext) {
            return onQueryText(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onQueryText(final OnQueryTextListener onNext, final Consumer<? super Exception> onError) {
            return onQueryText(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onQueryText(final OnQueryTextListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    @SuppressWarnings("unchecked")
                    final Tuple2<Integer, String> tmp = (Tuple2<Integer, String>) param;

                    if (Fu.isUiThread()) {
                        if (tmp._1 == 0) {
                            onNext.onQueryTextChange(tmp._2);
                        } else {
                            onNext.onQueryTextSubmit(tmp._2);
                        }
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                if (tmp._1 == 0) {
                                    onNext.onQueryTextChange(tmp._2);
                                } else {
                                    onNext.onQueryTextSubmit(tmp._2);
                                }
                            }
                        });
                    }
                }
            });

            _view.setOnQueryTextListener(new OnQueryTextListener() {
                @Override
                public boolean onQueryTextChange(String newText) {
                    dispatcher.onNext(Tuple.of(0, newText));
                    return true;
                }

                @Override
                public boolean onQueryTextSubmit(String query) {
                    dispatcher.onNext(Tuple.of(1, query));
                    return true;
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnQueryTextListener(null);
                }
            });

            return this;
        }
    }

    public static class AutoCompleteTextViewObserver<T extends AutoCompleteTextView, O extends AutoCompleteTextViewObserver<T, O>>
            extends TextViewObserver<T, O> {

        AutoCompleteTextViewObserver(final T view) {
            super(view);
        }

        public Disposable onItemClick(final OnItemClickListener onNext) {
            return onItemClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onItemClick(final OnItemClickListener onNext, final Consumer<? super Exception> onError) {
            return onItemClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onItemClick(final OnItemClickListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple4<AdapterView<?>, View, Integer, Integer> tmp = (Tuple4<AdapterView<?>, View, Integer, Integer>) param;

                    if (Fu.isUiThread()) {
                        onNext.onItemClick(tmp._1, tmp._2, tmp._3, tmp._4);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onItemClick(tmp._1, tmp._2, tmp._3, tmp._4);
                            }
                        });
                    }
                }
            });

            _view.setOnItemClickListener(new OnItemClickListener() {
                @Override
                public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                    dispatcher.onNext(Tuple.of(parent, view, position, id));
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnItemClickListener(null);
                }
            });

            return this;
        }

        public Disposable onItemClick(final Consumer<? super Tuple4<AdapterView<?>, View, Integer, Integer>> onNext) {
            return onItemClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onItemClick(final Consumer<? super Tuple4<AdapterView<?>, View, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError) {
            return onItemClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onItemClick(final Consumer<? super Tuple4<AdapterView<?>, View, Integer, Integer>> onNext, final Consumer<? super Exception> onError,
                final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple4<AdapterView<?>, View, Integer, Integer> tmp = (Tuple4<AdapterView<?>, View, Integer, Integer>) param;

                    if (Fu.isUiThread()) {
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

            _view.setOnItemClickListener(new OnItemClickListener() {
                @Override
                public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                    dispatcher.onNext(Tuple.of(parent, view, position, id));
                }
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnItemClickListener(null);
                }
            });

            return this;
        }
    }

    public static class MenuItemObserver<T extends MenuItem, O extends MenuItemObserver<T, O>> extends UIObserverBase<T, O> {
        final MenuItem _menuItem;

        MenuItemObserver(final MenuItem menuItem) {
            this._menuItem = menuItem;
        }

        public Disposable onMenuItemClick(final OnMenuItemClickListener onNext) {
            return onMenuItemClick(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onMenuItemClick(final OnMenuItemClickListener onNext, final Consumer<? super Exception> onError) {
            return onMenuItemClick(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onMenuItemClick(final OnMenuItemClickListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final MenuItem tmp = (MenuItem) param;

                    if (Fu.isUiThread()) {
                        onNext.onMenuItemClick(tmp);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onMenuItemClick(tmp);
                            }
                        });
                    }
                }
            });

            _menuItem.setOnMenuItemClickListener(new OnMenuItemClickListener() {
                @Override
                public boolean onMenuItemClick(MenuItem item) {
                    dispatcher.onNext(item);
                    return true;
                };
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _menuItem.setOnMenuItemClickListener(null);
                }
            });

            return this;
        }
    }
}