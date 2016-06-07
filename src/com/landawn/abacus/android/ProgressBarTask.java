package com.landawn.abacus.android;

import java.util.concurrent.Callable;

import com.landawn.abacus.android.util.AsyncExecutor;
import com.landawn.abacus.android.util.CompletableFuture;

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
 * @author haiyangl
 *
 */
public class ProgressBarTask {
    protected final CompletableFuture<ProgressBar> future;
    protected ProgressBar progressBar;

    public ProgressBarTask(final ViewGroup root, final long delay, final int circleColor) {
        future = AsyncExecutor.executeOnUiThread(new Callable<ProgressBar>() {
            @Override
            public ProgressBar call() {
                synchronized (ProgressBarTask.this) {
                    return ProgressBarTask.this.progressBar = future.isCancelled() ? null : createProgressBar(root, circleColor);
                }
            }
        }, delay);
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
                        ((ViewGroup) parent).removeView(progressBar);
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

        return progressBar;
    }
}
