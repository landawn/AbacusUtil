/*
 * Copyright (C) 2016 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.landawn.abacus.android;

import android.app.AliasActivity;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
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
