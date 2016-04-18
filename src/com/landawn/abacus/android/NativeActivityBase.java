package com.landawn.abacus.android;

import android.app.NativeActivity;
import android.view.View;

public abstract class NativeActivityBase extends NativeActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

}
