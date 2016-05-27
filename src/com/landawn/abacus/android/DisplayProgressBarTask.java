package com.landawn.abacus.android;

import java.util.concurrent.Callable;

import com.landawn.abacus.android.util.AsyncExecutor;
import com.landawn.abacus.android.util.CompletableFuture;

import android.app.Activity;
import android.view.Gravity;
import android.view.ViewGroup;
import android.view.ViewParent;
import android.widget.FrameLayout;
import android.widget.ProgressBar;

/**
 * Designed to show progress bar easily for network or other heavy operation:
 * <pre>
 * <code>
        final DisplayProgressBarTask displayProgressBarTask = DisplayProgressBarTask.start(this, 1000, 0xFFE0E0E0);
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
public class DisplayProgressBarTask {
    protected final CompletableFuture<ProgressBar> future;
    protected ProgressBar progressBar;

    public DisplayProgressBarTask(final Activity activity, final long delay, final int circleColor) {
        future = AsyncExecutor.executeOnUiThread(new Callable<ProgressBar>() {
            @Override
            public ProgressBar call() {
                synchronized (DisplayProgressBarTask.this) {
                    return DisplayProgressBarTask.this.progressBar = future.isCancelled() ? null : createProgressBar(activity, circleColor);
                }
            }
        }, delay);
    }

    public static DisplayProgressBarTask start(final Activity activity) {
        return start(activity, 0);
    }

    public static DisplayProgressBarTask start(final Activity activity, final long delay) {
        return new DisplayProgressBarTask(activity, delay, Integer.MIN_VALUE);
    }

    public static DisplayProgressBarTask start(final Activity activity, final long delay, final int circleColor) {
        return new DisplayProgressBarTask(activity, delay, circleColor);
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

    protected ProgressBar createProgressBar(final Activity activity, final int circleColor) {
        // Create layout params expecting to be added to a frame layout.
        final FrameLayout.LayoutParams lp = new FrameLayout.LayoutParams(0, 0);
        lp.width = lp.height = FrameLayout.LayoutParams.WRAP_CONTENT;
        lp.gravity = Gravity.CENTER;

        // Create a progress bar to be added to the window.
        final ProgressBar progressBar = new ProgressBar(activity);
        progressBar.setIndeterminate(true);
        progressBar.setLayoutParams(lp);

        if (circleColor != Integer.MIN_VALUE) {
            progressBar.getIndeterminateDrawable().setColorFilter(circleColor, android.graphics.PorterDuff.Mode.SRC_ATOP);
        }

        // Get the root view to attach the progress bar to.
        ViewGroup decorView = (ViewGroup) activity.getWindow().getDecorView();
        decorView.addView(progressBar);
        return progressBar;
    }
}
