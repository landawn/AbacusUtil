package com.landawn.abacus.android;

import android.app.AliasActivity;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

public abstract class AliasActivityBase extends AliasActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

    public <T extends View> T getViewById(Class<T> cls, int id) {
        return (T) this.findViewById(id);
    }

    public TextView getTextViewById(int id) {
        return (TextView) this.findViewById(id);
    }

    public EditText getEditTextById(int id) {
        return (EditText) this.findViewById(id);
    }

    public ImageView getImageViewById(int id) {
        return (ImageView) this.findViewById(id);
    }

    public Button getButtonById(int id) {
        return (Button) this.findViewById(id);
    }

}
