package com.landawn.abacus.android;

import android.app.LauncherActivity;
import android.view.View;

public abstract class LauncherActivityBase extends LauncherActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

}
