package com.landawn.abacus.android;

import android.app.ListActivity;
import android.view.View;

public abstract class ListActivityBase extends ListActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

}
