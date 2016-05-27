package com.landawn.abacus.android.widget;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.ImageView;

public abstract class ImageViewBase extends ImageView {

    public ImageViewBase(Context context) {
        super(context);
    }

    public ImageViewBase(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public ImageViewBase(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    public ImageViewBase(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
    }
}
