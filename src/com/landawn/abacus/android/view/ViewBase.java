package com.landawn.abacus.android.view;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;

public abstract class ViewBase extends View {

    public ViewBase(Context context) {
        super(context);
    }

    public ViewBase(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public ViewBase(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public ViewBase(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }
}
