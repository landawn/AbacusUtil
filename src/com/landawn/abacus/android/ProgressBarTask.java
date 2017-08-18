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

package com.landawn.abacus.android;

import java.util.concurrent.Callable;

import com.landawn.abacus.android.util.Async.UIExecutor;
import com.landawn.abacus.android.util.CompletableFuture;
import com.landawn.abacus.util.Multiset;

import android.app.Activity;
import android.app.Dialog;
import android.view.Gravity;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;
import android.widget.ProgressBar;

/**
 * Designed to show progress bar easily for network or other heavy operation:
 * <pre>
 * <code>
        final ProgressBarTask progressBarTask = ProgressBarTask.display(this, 1000, 0xFFE0E0E0);
        final LoginRequest request = ...;

        AsyncExecutor.executeInParallel(() -> accountService.login(request))
            .callbackOnUiThread((e, resp) -> {
                displayProgressBarTask.finish();

                if (resp != null && resp.getRespCode() == ResponseCode.OK) {
                    // TODO ...
                } else {// TODO ...
                }
            });
 * </code>
 * </pre>
 * 
 * @since 0.8
 * 
 * @author haiyangl
 */
public class ProgressBarTask {
    private static final Multiset<ViewGroup> activeProgressBarSet = new Multiset<>();
    private static volatile int maxProgressBarTask = Integer.MAX_VALUE;
    private static volatile int maxProgressBarTaskPerView = Integer.MAX_VALUE;

    protected final CompletableFuture<ProgressBar> future;
    protected ProgressBar progressBar;

    public ProgressBarTask(final ViewGroup root, final long delay, final int circleColor) {
        future = UIExecutor.execute(new Callable<ProgressBar>() {
            @Override
            public ProgressBar call() {
                synchronized (ProgressBarTask.this) {
                    return ProgressBarTask.this.progressBar = (future.isCancelled() || activeProgressBarTaskCount() >= maxProgressBarTask
                            || activeProgressBarTaskCount(root) >= maxProgressBarTaskPerView) ? null : createProgressBar(root, circleColor);
                }
            }
        }, delay);
    }

    public static void setMaxProgressBarTask(int maxProgressBarTask) {
        ProgressBarTask.maxProgressBarTask = maxProgressBarTask;
    }

    public static void setMaxProgressBarTaskPerView(int maxProgressBarTaskPerView) {
        ProgressBarTask.maxProgressBarTaskPerView = maxProgressBarTaskPerView;
    }

    public static ProgressBarTask display(final Activity activity) {
        return display(activity, 0);
    }

    public static ProgressBarTask display(final Activity activity, final long delay) {
        return display(activity, delay, Integer.MIN_VALUE);
    }

    public static ProgressBarTask display(final Activity activity, final long delay, final int circleColor) {
        return display((ViewGroup) activity.getWindow().getDecorView(), delay, circleColor);
    }

    public static ProgressBarTask display(final Dialog dialog) {
        return display(dialog, 0);
    }

    public static ProgressBarTask display(final Dialog dialog, final long delay) {
        return display(dialog, delay, Integer.MIN_VALUE);
    }

    public static ProgressBarTask display(final Dialog dialog, final long delay, final int circleColor) {
        return display((ViewGroup) dialog.getWindow().getDecorView(), delay, circleColor);
    }

    public static ProgressBarTask display(final ViewGroup root) {
        return display(root, 0);
    }

    public static ProgressBarTask display(final ViewGroup root, final long delay) {
        return display(root, delay, Integer.MIN_VALUE);
    }

    public static ProgressBarTask display(final ViewGroup root, final long delay, final int circleColor) {
        return new ProgressBarTask(root, delay, circleColor);
    }

    static int activeProgressBarTaskCount() {
        synchronized (activeProgressBarSet) {
            return (int) activeProgressBarSet.sumOfOccurrences();
        }
    }

    static int activeProgressBarTaskCount(Activity activity) {
        synchronized (activeProgressBarSet) {
            return activeProgressBarSet.get(activity.getWindow().getDecorView());
        }
    }

    static int activeProgressBarTaskCount(Dialog dialog) {
        synchronized (activeProgressBarSet) {
            return activeProgressBarSet.get(dialog.getWindow().getDecorView());
        }
    }

    static int activeProgressBarTaskCount(ViewGroup root) {
        synchronized (activeProgressBarSet) {
            return activeProgressBarSet.get(root);
        }
    }

    public void finish() {
        synchronized (this) {
            try {
                future.cancel(true);
            } catch (Throwable e) {
                // ignore.
            } finally {
                if (progressBar != null) {
                    ViewParent parent = progressBar.getParent();

                    if (parent instanceof ViewGroup) {
                        final ViewGroup root = (ViewGroup) parent;
                        root.removeView(progressBar);

                        synchronized (activeProgressBarSet) {
                            activeProgressBarSet.remove(root);
                        }
                    }
                }
            }
        }
    }

    protected ProgressBar createProgressBar(final ViewGroup root, final int circleColor) {
        // Create layout params expecting to be added to a frame layout.
        final FrameLayout.LayoutParams lp = new FrameLayout.LayoutParams(0, 0);
        lp.width = lp.height = FrameLayout.LayoutParams.WRAP_CONTENT;
        lp.gravity = Gravity.CENTER;

        // Create a progress bar to be added to the window.
        final ProgressBar progressBar = new ProgressBar(root.getContext());
        progressBar.setIndeterminate(true);
        progressBar.setLayoutParams(lp);

        if (circleColor != Integer.MIN_VALUE) {
            progressBar.getIndeterminateDrawable().setColorFilter(circleColor, android.graphics.PorterDuff.Mode.SRC_ATOP);
        }

        root.addView(progressBar);

        synchronized (activeProgressBarSet) {
            activeProgressBarSet.add(root);
        }

        return progressBar;
    }
}
