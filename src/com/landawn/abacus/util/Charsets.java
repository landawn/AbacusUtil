/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util;

import java.nio.charset.Charset;
import java.util.Map;

/**
 * 
 * @since 0.8
 * 
 */
public final class Charsets {
    public static final Charset US_ASCII = Charset.forName("US-ASCII");
    public static final Charset ISO_8859_1 = Charset.forName("ISO-8859-1");
    public static final Charset UTF_8 = Charset.forName("UTF-8");
    public static final Charset UTF_16BE = Charset.forName("UTF-16BE");
    public static final Charset UTF_16LE = Charset.forName("UTF-16LE");
    public static final Charset UTF_16 = Charset.forName("UTF-16");

    /**
     * Returns the default charset of this Java virtual machine.
     */
    public static final Charset DEFAULT = Charset.defaultCharset();

    private static final Map<String, Charset> charsetPool = new ObjectPool<String, Charset>(128);
    static {
        charsetPool.put(US_ASCII.name(), US_ASCII);
        charsetPool.put(ISO_8859_1.name(), ISO_8859_1);
        charsetPool.put(UTF_8.name(), UTF_8);
        charsetPool.put(UTF_16BE.name(), UTF_16BE);
        charsetPool.put(UTF_16LE.name(), UTF_16LE);
        charsetPool.put(UTF_16.name(), UTF_16);
    }

    private Charsets() {
        // singleton.
    }

    public static Charset get(String charsetName) {
        Charset charset = charsetPool.get(charsetName);

        if (charset == null) {
            charset = Charset.forName(charsetName);
            charsetPool.put(charsetName, charset);
        }

        return charset;
    }
}
