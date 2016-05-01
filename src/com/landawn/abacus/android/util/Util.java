package com.landawn.abacus.android.util;

import java.io.Closeable;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.IOUtil;

import android.annotation.TargetApi;
import android.app.Activity;
import android.app.ActivityManager;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.graphics.Bitmap;
import android.os.Build;
import android.os.Looper;
import android.os.StatFs;
import android.util.DisplayMetrics;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

/**
 * Always remember to initialize {@code Util} class by calling method {@code init(Context context)} when the application is started.
 *
 */
public final class Util {
    static final Logger logger = LoggerFactory.getLogger(Util.class);

    private static final int CPU_COUNT = Runtime.getRuntime().availableProcessors();
    private static volatile int MAX_APP_MEMORY;

    private Util() {
        // singleton.
    }

    private static Context context;

    public static void init(Context context) {
        Util.context = context;
    }

    public static Context context() {
        return context;
    }

    public static int cupCount() {
        return CPU_COUNT;
    }

    /**
     * Copied from Picasso: http://square.github.io/picasso Copyright (C) 2013 Square, Inc.
     * 
     * @param context
     * @return approximate per-application memory in megabytes.
     */
    public static int maxMemoryPerApp() {
        int maxAppMemory = MAX_APP_MEMORY;

        if (maxAppMemory == 0) {
            final ActivityManager am = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
            boolean isLargeHeap = (context.getApplicationInfo().flags & ApplicationInfo.FLAG_LARGE_HEAP) != 0;
            int memoryClass = am.getMemoryClass();

            if (isLargeHeap && Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
                memoryClass = am.getLargeMemoryClass();
            }

            maxAppMemory = memoryClass;
            MAX_APP_MEMORY = maxAppMemory;
        }

        return maxAppMemory;
    }

    /**
     * @param dir
     * @return available size in megabytes.
     */
    public static int freeDiskSpace() {
        final StatFs stat = new StatFs(context.getFilesDir().getPath());
        final long bytesAvailable = stat.getBlockSizeLong() * stat.getAvailableBlocksLong();
        return (int) (bytesAvailable / (1024 * 1024));
    }

    /**
     * @param dir
     * @return available size in megabytes.
     */
    public static int freeDiskSpace(File dir) {
        final StatFs stat = new StatFs(dir.getPath());
        final long bytesAvailable = stat.getBlockSizeLong() * stat.getAvailableBlocksLong();
        return (int) (bytesAvailable / (1024 * 1024));
    }

    /**
     * 
     * @return
     * @see android.os.Build.BRAND
     */
    public static String deviceBrand() {
        return Build.BRAND;
    }

    /**
     * 
     * @return
     * @see android.os.Build.MODEL
     */
    public static String deviceModel() {
        return Build.MODEL;
    }

    /**
     * 
     * @return
     * @see android.os.Build.MANUFACTURER
     */
    public static String deviceManufacturer() {
        return Build.MANUFACTURER;
    }

    /**
     * 
     * @return     * 
     * @see android.os.Build.SERIAL
     */
    public static String deviceSerialNumber() {
        return Build.SERIAL;
    }

    /**
     * 
     * @return
     * @see android.os.Build.VERSION.RELEASE
     */
    public static String releaseVersion() {
        return Build.VERSION.RELEASE;
    }

    /**
     * 
     * @return
     * @see android.os.Build.VERSION.BASE_OS
     */
    public static String baseOS() {
        return Build.VERSION.BASE_OS;
    }

    public static DisplayMetrics getDisplayMetrics() {
        return context.getResources().getDisplayMetrics();
    }

    public static boolean isUiThread() {
        return Looper.myLooper() == Looper.getMainLooper();
    }

    public static boolean isUiThread(Thread thread) {
        return thread == Looper.getMainLooper().getThread();
    }

    /**
     * Execute the action right now if current thread is a background thread or execute it asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} if current thread is UI thread.
     * 
     * @param action
     */
    public static void runInBackground(final Runnable action) {
        if (isUiThread()) {
            AsyncExecutor.execute(action);
        } else {
            action.run();
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} if current thread is UI thread.
     * 
     * @param actions
     */
    @Beta
    static void runInBackground(final List<? extends Runnable> actions) {
        for (Runnable action : actions) {
            runInBackground(action);
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} if current thread is UI thread.
     * 
     * @param action
     * @return
     */
    public static <T> void runInBackground(final Callable<T> action) {
        if (isUiThread()) {
            AsyncExecutor.execute(action);
        } else {
            try {
                action.call();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} if current thread is UI thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> void runInBackground(final Collection<? extends Callable<T>> actions) {
        for (Callable<T> action : actions) {
            runInBackground(action);
        }
    }

    /**
     * Execute the action right now if current thread is a background thread or execute it asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} if current thread is UI thread.
     * 
     * @param action
     */
    public static void runInParallel(final Runnable action) {
        if (isUiThread()) {
            AsyncExecutor.executeInParallel(action);
        } else {
            action.run();
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} if current thread is UI thread.
     * 
     * @param actions
     */
    @Beta
    static void runInParallel(final List<? extends Runnable> actions) {
        for (Runnable action : actions) {
            runInParallel(action);
        }
    }

    /**
     * Execute the action right now if current thread is a background thread or execute it asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} if current thread is UI thread.
     * 
     * @param action
     * @return
     */
    public static <T> void runInParallel(final Callable<T> action) {
        if (isUiThread()) {
            AsyncExecutor.executeInParallel(action);
        } else {
            try {
                action.call();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} if current thread is UI thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> void runInParallel(final Collection<? extends Callable<T>> actions) {
        for (Callable<T> action : actions) {
            runInParallel(action);
        }
    }

    /**
     * Execute the action right now if current thread is UI thread or execute it asynchronously if current thread is a background thread.
     * 
     * @param action
     */
    public static void runOnUiThread(final Runnable action) {
        if (isUiThread()) {
            action.run();
        } else {
            AsyncExecutor.executeOnUiThread(action);
        }
    }

    /**
     * Execute the actions right now if current thread is UI thread or execute them asynchronously if current thread is a background thread.
     * 
     * @param actions
     */
    @Beta
    static void runOnUiThread(final List<? extends Runnable> actions) {
        for (Runnable action : actions) {
            runOnUiThread(action);
        }
    }

    /**
     * Execute the action right now if current thread is UI thread or execute it asynchronously if current thread is a background thread.
     * 
     * @param action
     * @return
     */
    public static <T> void runOnUiThread(final Callable<T> action) {
        if (isUiThread()) {
            try {
                action.call();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        } else {
            AsyncExecutor.executeOnUiThread(action);
        }
    }

    /**
     * Execute the actions right now if current thread is UI thread or execute them asynchronously if current thread is a background thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> void runOnUiThread(final Collection<? extends Callable<T>> actions) {
        for (Callable<T> action : actions) {
            runOnUiThread(action);
        }
    }

    /**
     * Execute the action right now if current thread is a background thread or execute it asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param action
     */
    public static void callInBackground(final Runnable action) {
        if (isUiThread()) {
            try {
                AsyncExecutor.execute(action).get();
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        } else {
            action.run();
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param actions
     */
    @Beta
    static void callInBackground(final List<? extends Runnable> actions) {
        for (Runnable action : actions) {
            callInBackground(action);
        }
    }

    /**
     * Execute the action right now if current thread is a background thread or execute it asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param action
     * @return
     */
    public static <T> T callInBackground(final Callable<T> action) {
        if (isUiThread()) {
            try {
                return AsyncExecutor.execute(action).get();
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        } else {
            try {
                return action.call();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#SERIAL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> List<T> callInBackground(final Collection<? extends Callable<T>> actions) {
        final List<T> result = new ArrayList<T>(actions.size());

        for (Callable<T> action : actions) {
            result.add(callInBackground(action));
        }

        return result;
    }

    /**
     * Execute the action right now if current thread is a background thread or execute it asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param action
     */
    public static void callInParallel(final Runnable action) {
        if (isUiThread()) {
            try {
                AsyncExecutor.executeInParallel(action).get();
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        } else {
            action.run();
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param actions
     */
    @Beta
    static void callInParallel(final List<? extends Runnable> actions) {
        for (Runnable action : actions) {
            callInParallel(action);
        }
    }

    /**
     * Execute the action right now if current thread is a background thread or execute it asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param action
     * @return
     */
    public static <T> T callInParallel(final Callable<T> action) {
        if (isUiThread()) {
            try {
                return AsyncExecutor.executeInParallel(action).get();
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        } else {
            try {
                return action.call();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Execute the actions right now if current thread is a background thread or execute them asynchronously with @{code android.os.AsyncTask#THREAD_POOL_EXECUTOR} and waiting for result before it returns if current thread is UI thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> List<T> callInParallel(final Collection<? extends Callable<T>> actions) {
        final List<T> result = new ArrayList<T>(actions.size());

        for (Callable<T> action : actions) {
            result.add(callInParallel(action));
        }

        return result;
    }

    /**
     * Execute the action right now if current thread is UI thread or execute it asynchronously and waiting for result before it returns if current thread is a background thread.
     * 
     * @param action
     */
    public static void callOnUiThread(final Runnable action) {
        if (isUiThread()) {
            action.run();
        } else {
            try {
                AsyncExecutor.executeOnUiThread(action).get();
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Execute the actions right now if current thread is UI thread or execute them asynchronously and waiting for result before it returns if current thread is a background thread.
     * 
     * @param actions
     */
    @Beta
    static void callOnUiThread(final List<? extends Runnable> actions) {
        for (Runnable action : actions) {
            callOnUiThread(action);
        }
    }

    /**
     * Execute the action right now if current thread is UI thread or execute it asynchronously and waiting for result before it returns if current thread is a background thread.
     * 
     * @param action
     * @return
     */
    public static <T> T callOnUiThread(final Callable<T> action) {
        if (isUiThread()) {
            try {
                return action.call();
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        } else {
            try {
                return AsyncExecutor.executeOnUiThread(action).get();
            } catch (InterruptedException | ExecutionException e) {
                throw new RuntimeException(e);
            }
        }
    }

    /**
     * Execute the actions right now if current thread is UI thread or execute them asynchronously and waiting for result before it returns if current thread is a background thread.
     * 
     * @param actions
     * @return
     */
    @Beta
    static <T> List<T> callOnUiThread(final Collection<? extends Callable<T>> actions) {
        final List<T> result = new ArrayList<T>(actions.size());

        for (Callable<T> action : actions) {
            result.add(callOnUiThread(action));
        }

        return result;
    }

    public static <T extends View> T getViewById(View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends View> T getViewById(Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static <T extends View> T getViewById(Class<T> cls, View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends View> T getViewById(Class<T> cls, Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static TextView getTextViewById(View root, int id) {
        return (TextView) root.findViewById(id);
    }

    public static TextView getTextViewById(Activity activity, int id) {
        return (TextView) activity.findViewById(id);
    }

    public static EditText getEditTextById(View root, int id) {
        return (EditText) root.findViewById(id);
    }

    public static EditText getEditTextById(Activity activity, int id) {
        return (EditText) activity.findViewById(id);
    }

    public static ImageView getImageViewById(View root, int id) {
        return (ImageView) root.findViewById(id);
    }

    public static ImageView getImageViewById(Activity activity, int id) {
        return (ImageView) activity.findViewById(id);
    }

    public static Button getButtonById(View root, int id) {
        return (Button) root.findViewById(id);
    }

    public static Button getButtonById(Activity activity, int id) {
        return (Button) activity.findViewById(id);
    }

    public static void showToast(final CharSequence text) {
        showToast(context, text);
    }

    public static void showToast(final CharSequence text, final int duration) {
        showToast(context, text, duration);
    }

    public static void showToast(final Context context, final CharSequence text) {
        Toast.makeText(context, text, Toast.LENGTH_SHORT).show();
    }

    public static void showToast(final Context context, final CharSequence text, final int duration) {
        Toast.makeText(context, text, duration).show();
    }

    public static void close(Closeable closeable) {
        IOUtil.close(closeable);
    }

    public static void closeQuietly(Closeable closeable) {
        IOUtil.closeQuietly(closeable);
    }

    /**
     * Copied from Picasso: http://square.github.io/picasso Copyright (C) 2013 Square, Inc.
     * 
     * @param bitmap
     * @return
     */
    public static int getBitmapBytes(Bitmap bitmap) {
        int result;

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB_MR1) {
            result = BitmapHoneycombMR1.getByteCount(bitmap);
        } else {
            result = bitmap.getRowBytes() * bitmap.getHeight();
        }

        if (result < 0) {
            throw new IllegalStateException("Negative size: " + bitmap);
        }

        return result;
    }

    /**
     * Copied from Picasso: http://square.github.io/picasso Copyright (C) 2013 Square, Inc.
     * 
     */
    @TargetApi(Build.VERSION_CODES.HONEYCOMB_MR1)
    private static class BitmapHoneycombMR1 {
        static int getByteCount(Bitmap bitmap) {
            return bitmap.getByteCount();
        }
    }

}
