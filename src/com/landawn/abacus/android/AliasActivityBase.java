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

    public <T extends TextView> T getTextViewById(int id) {
        return (T) this.findViewById(id);
    }

    public <T extends EditText> T getEditTextById(int id) {
        return (T) this.findViewById(id);
    }

    public <T extends ImageView> T getImageViewById(int id) {
        return (T) this.findViewById(id);
    }

    public <T extends Button> T getButtonById(int id) {
        return (T) this.findViewById(id);
    }

    public String getViewTextById(int id) {
        return this.getTextViewById(id).getText().toString().trim();
    }
}
