package com.landawn.abacus.android;

import android.app.AliasActivity;
import android.view.View;

public abstract class AliasActivityBase extends AliasActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

}
