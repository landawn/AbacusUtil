/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util;

import java.io.File;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface FileFilter {
    boolean accept(File parentDir, File file);
}
