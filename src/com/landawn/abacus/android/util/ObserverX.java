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

import com.landawn.abacus.android.util.Async.UIExecutor;
import com.landawn.abacus.android.util.Observer.ViewObserver;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Tuple;
import com.landawn.abacus.util.Tuple.Tuple5;
import com.landawn.abacus.util.function.Consumer;

import android.view.View;
import android.view.View.OnScrollChangeListener;

/**
 * 
 * 
 * @since 0.9
 * 
 * @author Haiyang Li
 * 
 */
public abstract class ObserverX<T> {
    ObserverX() {

    }

    public static <T extends View, O extends ViewObserverX<T, O>> ViewObserverX<T, O> of(final T view) {
        return new ViewObserverX<>(view);
    }

    public static class ViewObserverX<T extends View, O extends ViewObserverX<T, O>> extends ViewObserver<T, ViewObserverX<T, O>> {
        ViewObserverX(T view) {
            super(view);
        }

        public Disposable onScrollChange(final OnScrollChangeListener onNext) {
            return onScrollChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onScrollChange(final OnScrollChangeListener onNext, final Consumer<? super Exception> onError) {
            return onScrollChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onScrollChange(final OnScrollChangeListener onNext, final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple5<View, Integer, Integer, Integer, Integer> tmp = (Tuple5<View, Integer, Integer, Integer, Integer>) param;

                    if (Fu.isUiThread()) {
                        onNext.onScrollChange(tmp._1, tmp._2, tmp._3, tmp._4, tmp._5);
                    } else {
                        UIExecutor.execute(new Runnable() {
                            @Override
                            public void run() {
                                onNext.onScrollChange(tmp._1, tmp._2, tmp._3, tmp._4, tmp._5);
                            }
                        });
                    }
                }
            });

            _view.setOnScrollChangeListener(new OnScrollChangeListener() {
                @Override
                public void onScrollChange(View v, int scrollX, int scrollY, int oldScrollX, int oldScrollY) {
                    dispatcher.onNext(Tuple.of(v, scrollX, scrollY, oldScrollX, oldScrollY));
                };
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnScrollChangeListener(null);
                }
            });

            return this;
        }

        public Disposable onScrollChange(final Consumer<? super Tuple5<View, Integer, Integer, Integer, Integer>> onNext) {
            return onScrollChange(onNext, Fu.ON_ERROR_MISSING);
        }

        public Disposable onScrollChange(final Consumer<? super Tuple5<View, Integer, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError) {
            return onScrollChange(onNext, onError, Fu.EMPTY_ACTION);
        }

        public Disposable onScrollChange(final Consumer<? super Tuple5<View, Integer, Integer, Integer, Integer>> onNext,
                final Consumer<? super Exception> onError, final Runnable onComplete) {
            N.requireNonNull(onNext, "onNext");
            N.requireNonNull(onError, "onError");
            N.requireNonNull(onComplete, "onComplete");

            dispatcher.append(new DispatcherBase<Object>(onError, onComplete) {
                @Override
                public void onNext(Object param) {
                    final Tuple5<View, Integer, Integer, Integer, Integer> tmp = (Tuple5<View, Integer, Integer, Integer, Integer>) param;

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

            _view.setOnScrollChangeListener(new OnScrollChangeListener() {
                @Override
                public void onScrollChange(View v, int scrollX, int scrollY, int oldScrollX, int oldScrollY) {
                    dispatcher.onNext(Tuple.of(v, scrollX, scrollY, oldScrollX, oldScrollY));
                };
            });

            disposeActions.add(new Runnable() {
                @Override
                public void run() {
                    _view.setOnScrollChangeListener(null);
                }
            });

            return this;
        }
    }
}