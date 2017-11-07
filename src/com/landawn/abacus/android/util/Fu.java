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

package com.landawn.abacus.android.util;

import java.io.Closeable;
import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.android.util.Async.UIExecutor;
import com.landawn.abacus.android.util.SQLiteExecutor.Type;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.IOUtil;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NamingPolicy;
import com.landawn.abacus.util.function.Consumer;

import android.annotation.TargetApi;
import android.app.Activity;
import android.app.ActivityManager;
import android.app.Dialog;
import android.app.KeyguardManager;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.content.Context;
import android.content.pm.ActivityInfo;
import android.content.pm.ApplicationInfo;
import android.content.res.Configuration;
import android.database.Cursor;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Build;
import android.os.CancellationSignal;
import android.os.Looper;
import android.os.StatFs;
import android.util.DisplayMetrics;
import android.view.Surface;
import android.view.View;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

/**
 * Always remember to initialize {@code Fu} class by calling method {@code init(Context context)} when the application is started.
 *
 * More:
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="https://github.com/wasabeef/awesome-android-ui">awesome-android-ui</a>
 * @see <a href="https://github.com/wasabeef/awesome-android-libraries">awesome-android-libraries</a>
 * @see <a href="https://github.com/Blankj/AndroidUtilCode">AndroidUtilCode</a>
 * @see <a href="https://github.com/jingle1267/android-utils">android-utils</a>
 * @see <a href="https://github.com/wyouflf/xUtils3">xUtils3</a>
 */
public class Fu {
    static final Logger logger = LoggerFactory.getLogger(Fu.class);
    private static final int CPU_COUNT = Runtime.getRuntime().availableProcessors();

    protected static volatile Context context;
    private static volatile int MAX_APP_MEMORY;

    public static final Runnable EMPTY_ACTION = new Runnable() {
        @Override
        public void run() {
            // Do nothing;            
        }
    };

    public static final Consumer<Exception> ON_ERROR_MISSING = new Consumer<Exception>() {
        @Override
        public void accept(Exception t) {
            throw new RuntimeException(t);
        }
    };

    private Fu() {
        // singleton.
    }

    public static synchronized void init(Context context) {
        if (Fu.context != null) {
            return;
        }

        Fu.context = context;

        final ActivityManager am = (ActivityManager) context.getSystemService(Context.ACTIVITY_SERVICE);
        boolean isLargeHeap = (context.getApplicationInfo().flags & ApplicationInfo.FLAG_LARGE_HEAP) != 0;
        int memoryClass = am.getMemoryClass();

        if (isLargeHeap && Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
            memoryClass = am.getLargeMemoryClass();
        }

        MAX_APP_MEMORY = memoryClass;
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
        return MAX_APP_MEMORY;
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

    //    /**
    //     * 
    //     * @return
    //     * @see android.os.Build.VERSION.RELEASE
    //     */
    //    public static String releaseVersion() {
    //        return Build.VERSION.RELEASE;
    //    }
    //
    //    /**
    //     * 
    //     * @return
    //     * @see android.os.Build.VERSION.BASE_OS
    //     */
    //    public static String baseOS() {
    //        return Build.VERSION.BASE_OS;
    //    }

    /**
     * 
     * @param targetClass
     * @param cursor
     * @return
     * 
     * @see SQLiteExecutor#extractData(Class, Cursor)
     */
    public static DataSet extractData(final Class<?> targetClass, final Cursor cursor) {
        return SQLiteExecutor.extractData(targetClass, cursor);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter methods.
     * @param cursor
     * @param offset
     * @param count
     * @return
     * 
     * @see SQLiteExecutor#extractData(Class, Cursor, int, int)
     */
    public static DataSet extractData(final Class<?> targetClass, final Cursor cursor, final int offset, final int count) {
        return SQLiteExecutor.extractData(targetClass, cursor, offset, count);
    }

    /**
     * 
     * @param cursor
     * @param selectColumnTypes
     * @return
     * 
     * 
     * @see SQLiteExecutor#extractData(Cursor, Class[])
     */
    @SuppressWarnings("rawtypes")
    public static DataSet extractData(final Cursor cursor, final Class[] selectColumnTypes) {
        return SQLiteExecutor.extractData(cursor, selectColumnTypes);
    }

    /**
     * 
     * @param cursor
     * @param selectColumnTypes
     * @param offset
     * @param count
     * @return
     * 
     * @see SQLiteExecutor#extractData(Cursor, Class[], int, int)
     */
    @SuppressWarnings("rawtypes")
    public static DataSet extractData(final Cursor cursor, final Class[] selectColumnTypes, final int offset, final int count) {
        return SQLiteExecutor.extractData(cursor, selectColumnTypes, offset, count);
    }

    /**
     * 
     * @param cursor
     * @param selectColumnTypes
     * @return
     * 
     * @see SQLiteExecutor#extractData(Cursor, Collection)
     */
    @SuppressWarnings("rawtypes")
    public static DataSet extractData(final Cursor cursor, final Collection<Class> selectColumnTypes) {
        return SQLiteExecutor.extractData(cursor, selectColumnTypes);
    }

    /**
     * 
     * @param cursor
     * @param selectColumnTypes
     * @param offset
     * @param count
     * @return
     * 
     * @see SQLiteExecutor#extractData(Cursor, Collection, int, int)
     */
    @SuppressWarnings("rawtypes")
    public static DataSet extractData(final Cursor cursor, final Collection<Class> selectColumnTypes, final int offset, final int count) {
        return SQLiteExecutor.extractData(cursor, selectColumnTypes, offset, count);
    }

    /**
     * Returns values from all rows associated with the specified <code>targetClass</code> if the specified <code>targetClass</code> is an entity class, otherwise, only returns values from first column.
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @return
     * 
     * @see SQLiteExecutor#toList(Class, Cursor)
     */
    public static <T> List<T> toList(final Class<T> targetClass, final Cursor cursor) {
        return SQLiteExecutor.toList(targetClass, cursor);
    }

    /**
     * Returns values from all rows associated with the specified <code>targetClass</code> if the specified <code>targetClass</code> is an entity class, otherwise, only returns values from first column.
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @param offset
     * @param count
     * @return
     * 
     * @see SQLiteExecutor#toList(Class, Cursor, int, int)
     */
    public static <T> List<T> toList(final Class<T> targetClass, final Cursor cursor, final int offset, final int count) {
        return SQLiteExecutor.toList(targetClass, cursor, offset, count);
    }

    /**
     * Returns the values from the specified <code>column</code>. 
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @param columnIndex
     * @return
     * 
     * @see SQLiteExecutor#toList(Class, Cursor, int)
     */
    public static <T> List<T> toList(Class<T> targetClass, Cursor cursor, int columnIndex) {
        return SQLiteExecutor.toList(targetClass, cursor, columnIndex);
    }

    /**
     * Returns the values from the specified <code>column</code>. 
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @param columnIndex
     * @param offset
     * @param count
     * @return
     * 
     * @see SQLiteExecutor#toList(Class, Cursor, int, int, int)
     */
    public static <T> List<T> toList(final Class<T> targetClass, final Cursor cursor, final int columnIndex, final int offset, final int count) {
        return SQLiteExecutor.toList(targetClass, cursor, columnIndex, offset, count);
    }

    /**
     * 
     * @param targetClass entity class with getter/setter methods.
     * @param cursor
     * @return
     * 
     * @see SQLiteExecutor#toEntity(Class, Cursor)
     */
    public static <T> T toEntity(Class<T> targetClass, Cursor cursor) {
        return SQLiteExecutor.toEntity(targetClass, cursor);
    }

    /**
     * 
     * @param targetClass entity class with getter/setter methods.
     * @param cursor
     * @param rowNum
     * @return
     * 
     * @see SQLiteExecutor#toEntity(Class, Cursor, int)
     */
    public static <T> T toEntity(Class<T> targetClass, Cursor cursor, int rowNum) {
        return SQLiteExecutor.toEntity(targetClass, cursor, rowNum);
    }

    /**
     * 
     * @param targetClass
     * @param contentValues
     * @return
     * 
     * @see SQLiteExecutor#toEntity(Class, ContentValues)
     */
    public static <T> T toEntity(final Class<T> targetClass, final ContentValues contentValues) {
        return SQLiteExecutor.toEntity(targetClass, contentValues);
    }

    /**
     * 
     * @param targetClass an Map class or Entity class with getter/setter methods.
     * @param contentValues
     * @param namingPolicy
     * @return
     * 
     * @see SQLiteExecutor#toEntity(Class, ContentValues, NamingPolicy)
     */
    public static <T> T toEntity(final Class<T> targetClass, final ContentValues contentValues, final NamingPolicy namingPolicy) {
        return SQLiteExecutor.toEntity(targetClass, contentValues, namingPolicy);
    }

    /**
     * 
     * @param obj
     * @return
     * 
     * @see SQLiteExecutor#toContentValues(Object)
     */
    public static ContentValues toContentValues(final Object obj) {
        return SQLiteExecutor.toContentValues(obj, null);
    }

    /**
     * 
     * @param obj
     * @param ignoredPropNames
     * @return
     * 
     * @see SQLiteExecutor#toContentValues(Object)
     */
    public static ContentValues toContentValues(final Object obj, final Collection<String> ignoredPropNames) {
        return SQLiteExecutor.toContentValues(obj, ignoredPropNames);
    }

    /**
     * 
     * @param obj an instance of Map or Entity.
     * @param namingPolicy
     * @return
     * 
     * @see SQLiteExecutor#toContentValues(Object, NamingPolicy)
     */
    public static ContentValues toContentValues(final Object obj, final NamingPolicy namingPolicy) {
        return SQLiteExecutor.toContentValues(obj, null, namingPolicy);
    }

    /**
     * 
     * @param obj an instance of Map or Entity.
     * @param ignoredPropNames
     * @param namingPolicy
     * @return
     * 
     * @see SQLiteExecutor#toContentValues(Object, NamingPolicy)
     */
    public static ContentValues toContentValues(final Object obj, final Collection<String> ignoredPropNames, final NamingPolicy namingPolicy) {
        return SQLiteExecutor.toContentValues(obj, ignoredPropNames, namingPolicy);
    }

    public static final ContentValues asContentValues(String key1, Object value1) {
        final ContentValues result = new ContentValues();

        if (value1 == null) {
            result.putNull(key1);
        } else {
            Type.valueOf(value1.getClass()).set(result, key1, value1);
        }

        return result;
    }

    public static final ContentValues asContentValues(String key1, Object value1, String key2, Object value2) {
        final ContentValues result = new ContentValues();

        if (value1 == null) {
            result.putNull(key1);
        } else {
            Type.valueOf(value1.getClass()).set(result, key1, value1);
        }

        if (value2 == null) {
            result.putNull(key2);
        } else {
            Type.valueOf(value2.getClass()).set(result, key2, value2);
        }

        return result;
    }

    public static final ContentValues asContentValues(String key1, Object value1, String key2, Object value2, String key3, Object value3) {
        final ContentValues result = new ContentValues();

        if (value1 == null) {
            result.putNull(key1);
        } else {
            Type.valueOf(value1.getClass()).set(result, key1, value1);
        }

        if (value2 == null) {
            result.putNull(key2);
        } else {
            Type.valueOf(value2.getClass()).set(result, key2, value2);
        }

        if (value3 == null) {
            result.putNull(key3);
        } else {
            Type.valueOf(value3.getClass()).set(result, key3, value3);
        }

        return result;
    }

    @SafeVarargs
    public static final ContentValues asContentValues(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new ContentValues();
        }

        final ContentValues result = new ContentValues();

        if ((a.length % 2) != 0) {
            throw new IllegalArgumentException(
                    "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
        }

        @SuppressWarnings("rawtypes")
        Type type = null;
        String key = null;
        Object value = null;

        for (int i = 0, len = a.length; i < len; i++) {
            key = (String) a[i];
            value = a[++i];

            if (a[i] == null) {
                result.putNull(key);
            } else {
                type = Type.valueOf(value.getClass());
                type.set(result, key, value);
            }
        }

        return result;
    }

    @SuppressWarnings("rawtypes")
    public static Map<String, Class> asColumnTypes(String c1, Class t1) {
        return N.asMap(c1, t1);
    }

    @SuppressWarnings("rawtypes")
    public static Map<String, Class> asColumnTypes(String c1, Class t1, String c2, Class t2) {
        return N.asMap(c1, t1, c2, t2);
    }

    @SuppressWarnings("rawtypes")
    public static Map<String, Class> asColumnTypes(String c1, Class t1, String c2, Class t2, String c3, Class t3) {
        return N.asMap(c1, t1, c2, t2, c3, t3);
    }

    @SuppressWarnings("rawtypes")
    @SafeVarargs
    public static Map<String, Class> asColumnTypes(final Object... a) {
        return N.asMap(a);
    }

    public static ContentResolver getContentResolver() {
        return context.getContentResolver();
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param targetClass
     * @param uri
     * @param projection
     * @return
     */
    public static <T> List<T> query(Class<T> targetClass, Uri uri, String projection) {
        return query(targetClass, uri, projection, null, null, null);
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param targetClass
     * @param uri
     * @param projection
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * @return
     */
    public static <T> List<T> query(Class<T> targetClass, Uri uri, String projection, String selection, String[] selectionArgs, String sortOrder) {
        return query(targetClass, uri, projection, selection, selectionArgs, sortOrder, null);
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param targetClass entity class or specific column type.
     * @param uri
     * @param projection
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * @param cancellationSignal
     * @return
     */
    public static <T> List<T> query(Class<T> targetClass, final Uri uri, String projection, String selection, String[] selectionArgs, String sortOrder,
            CancellationSignal cancellationSignal) {
        final Cursor cursor = getContentResolver().query(uri, N.asArray(projection), selection, selectionArgs, sortOrder, cancellationSignal);

        try {
            return toList(targetClass, cursor);
        } finally {
            closeQuietly(cursor);
        }
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param targetClass entity class with getter/setting method.
     * @param uri
     * @param projection
     * @return
     */
    public static <T> List<T> query(Class<T> targetClass, final Uri uri, String[] projection) {
        return query(targetClass, uri, projection, null, null, null);
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param targetClass entity class with getter/setting method.
     * @param uri
     * @param projection
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * @return
     */
    public static <T> List<T> query(Class<T> targetClass, final Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        return query(targetClass, uri, projection, selection, selectionArgs, sortOrder, null);
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param targetClass entity class with getter/setting method.
     * @param uri
     * @param projection
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * @param cancellationSignal
     * @return
     */
    public static <T> List<T> query(Class<T> targetClass, final Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder,
            CancellationSignal cancellationSignal) {
        final Cursor cursor = getContentResolver().query(uri, projection, selection, selectionArgs, sortOrder);

        try {
            return toList(targetClass, cursor);
        } finally {
            closeQuietly(cursor);
        }
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param uri
     * @param projectionTypeMap
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static List<Map<String, Object>> query(final Uri uri, Map<String, Class> projectionTypeMap) {
        return query(uri, projectionTypeMap, null, null, null);
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param uri
     * @param projectionTypeMap
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static List<Map<String, Object>> query(final Uri uri, Map<String, Class> projectionTypeMap, String selection, String[] selectionArgs,
            String sortOrder) {
        return query(uri, projectionTypeMap, selection, selectionArgs, sortOrder, null);
    }

    /**
     * Query by context.getContentResolver().
     * 
     * @param uri
     * @param projectionTypeMap
     * @param selection
     * @param selectionArgs
     * @param sortOrder
     * @param cancellationSignal
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static List<Map<String, Object>> query(final Uri uri, Map<String, Class> projectionTypeMap, String selection, String[] selectionArgs,
            String sortOrder, CancellationSignal cancellationSignal) {
        final Cursor cursor = getContentResolver().query(uri, projectionTypeMap.keySet().toArray(new String[projectionTypeMap.size()]), selection,
                selectionArgs, sortOrder);

        try {
            return (List) extractData(cursor, projectionTypeMap.values()).toList(Map.class);
        } finally {
            closeQuietly(cursor);
        }
    }

    public static boolean isUiThread() {
        return Looper.myLooper() == Looper.getMainLooper();
    }

    public static boolean isUiThread(Thread thread) {
        return thread == Looper.getMainLooper().getThread();
    }

    public static void runOnUiThread(Runnable action) {
        if (isUiThread()) {
            action.run();
        } else {
            UIExecutor.execute(action);
        }
    }

    //    public static <T> T callOnUiThread(Callable<? extends T> action) throws Exception {
    //        if (isUiThread()) {
    //            return action.call();
    //        } else {
    //            return UIExecutor.execute(action).get();
    //        }
    //    }

    public static <T extends View> T getViewById(View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends View> T getViewById(Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static <T extends View> T getViewById(Dialog dialog, int id) {
        return (T) dialog.findViewById(id);
    }

    public static <T extends View> T getViewById(Class<T> cls, View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends View> T getViewById(Class<T> cls, Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static <T extends View> T getViewById(Class<T> cls, Dialog dialog, int id) {
        return (T) dialog.findViewById(id);
    }

    public static <T extends TextView> T getTextViewById(View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends TextView> T getTextViewById(Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static <T extends TextView> T getTextViewById(Dialog dialog, int id) {
        return (T) dialog.findViewById(id);
    }

    public static <T extends EditText> T getEditTextById(View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends EditText> T getEditTextById(Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static <T extends EditText> T getEditTextById(Dialog dialog, int id) {
        return (T) dialog.findViewById(id);
    }

    public static <T extends ImageView> T getImageViewById(View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends ImageView> T getImageViewById(Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static <T extends ImageView> T getImageViewById(Dialog dialog, int id) {
        return (T) dialog.findViewById(id);
    }

    public static <T extends Button> T getButtonById(View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends Button> T getButtonById(Activity activity, int id) {
        return (T) activity.findViewById(id);
    }

    public static <T extends Button> T getButtonById(Dialog dialog, int id) {
        return (T) dialog.findViewById(id);
    }

    public static String getViewTextById(View root, int id) {
        return getTextViewById(root, id).getText().toString().trim();
    }

    public static String getViewTextById(Activity activity, int id) {
        return getTextViewById(activity, id).getText().toString().trim();
    }

    public static String getViewTextById(Dialog dialog, int id) {
        return getTextViewById(dialog, id).getText().toString().trim();
    }

    /**
     * Sometimes {@code view.requestFocus()}, or {@code dialog.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE)} (the view is in a dialog) is required to show virtual keyboard.
     * 
     * @param view
     */
    public static void showVirtualKeyboard(View view) {
        final Context context = view.getContext();
        final InputMethodManager imm = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.showSoftInput(view, InputMethodManager.SHOW_IMPLICIT);
    }

    public static void hideVirtualKeyboard(View view) {
        final Context context = view.getContext();
        final InputMethodManager imm = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
        imm.hideSoftInputFromWindow(view.getWindowToken(), 0);
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

    public static int dp2px(float dpValue) {
        final float scale = getDisplayMetrics().density;
        return (int) (dpValue * scale + 0.5f);
    }

    public static int px2dp(float pxValue) {
        final float scale = getDisplayMetrics().density;
        return (int) (pxValue / scale + 0.5f);
    }

    public static int sp2px(float spValue) {
        final float fontScale = getDisplayMetrics().scaledDensity;
        return (int) (spValue * fontScale + 0.5f);
    }

    public static int px2sp(float pxValue) {
        final float fontScale = getDisplayMetrics().scaledDensity;
        return (int) (pxValue / fontScale + 0.5f);
    }

    public static DisplayMetrics getDisplayMetrics() {
        return context.getResources().getDisplayMetrics();
    }

    public static int getScreenWidth() {
        // cache the result?
        final WindowManager windowManager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        final DisplayMetrics dm = new DisplayMetrics();
        windowManager.getDefaultDisplay().getMetrics(dm);
        return dm.widthPixels;
    }

    public static int getScreenHeight() {
        // cache the result?
        final WindowManager windowManager = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
        final DisplayMetrics dm = new DisplayMetrics();
        windowManager.getDefaultDisplay().getMetrics(dm);
        return dm.heightPixels;
    }

    public static boolean isLandscape() {
        return context.getResources().getConfiguration().orientation == Configuration.ORIENTATION_LANDSCAPE;
    }

    public static boolean isPortrait() {
        return context.getResources().getConfiguration().orientation == Configuration.ORIENTATION_PORTRAIT;
    }

    public static void setLandscape(Activity activity) {
        activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
    }

    public static void setPortrait(Activity activity) {
        activity.setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
    }

    public static int getScreenRotation(Activity activity) {
        switch (activity.getWindowManager().getDefaultDisplay().getRotation()) {
            default:
            case Surface.ROTATION_0:
                return 0;
            case Surface.ROTATION_90:
                return 90;
            case Surface.ROTATION_180:
                return 180;
            case Surface.ROTATION_270:
                return 270;
        }
    }

    public static boolean isScreenLocked() {
        final KeyguardManager km = (KeyguardManager) context.getSystemService(Context.KEYGUARD_SERVICE);
        return km.inKeyguardRestrictedInputMode();
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
     * @return the size of the specified bitmap in bytes.
     */
    public static int getByteCount(Bitmap bitmap) {
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

    public static final class Fragments {
        private Fragments() {
            // singleton.
        }
    }

    public static final class O extends Fu {

        private O() {
            // singleton.
        }
    }
}
