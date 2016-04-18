package com.landawn.abacus.android;

import android.app.ExpandableListActivity;
import android.view.View;

public abstract class ExpandableListActivityBase extends ExpandableListActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

}
