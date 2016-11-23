package com.landawn.abacus.util.function;

import java.security.SecureRandom;
import java.util.Random;

class Util {
    private Util() {
        // singleton.
    }

    static final Random RAND = new SecureRandom();

    static final int CHAR_MOD = Character.MAX_VALUE + 1;
}
