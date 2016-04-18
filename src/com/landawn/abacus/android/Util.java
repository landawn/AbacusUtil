package com.landawn.abacus.android;

import android.app.Activity;
import android.view.View;

public final class Util {
    private Util() {
        // singleton.
    }

    public static <T extends View> T getViewById(View root, int id) {
        return (T) root.findViewById(id);
    }

    public static <T extends View> T getViewById(Activity activity, int id) {
        return (T) activity.findViewById(id);
    }
}
