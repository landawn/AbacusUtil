package com.landawn.abacus.android;

import android.accounts.AccountAuthenticatorActivity;
import android.view.View;

public abstract class AccountAuthenticatorActivityBase extends AccountAuthenticatorActivity {

    public <T extends View> T getViewById(int id) {
        return (T) this.findViewById(id);
    }

}
