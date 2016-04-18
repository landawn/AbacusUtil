package com.landawn.abacus.android;

import android.preference.PreferenceActivity;
import android.view.View;

public abstract class PreferenceActivityBase extends PreferenceActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

}
