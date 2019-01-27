/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.dataSource;

import java.util.List;
import java.util.Map;

import com.landawn.abacus.SliceSelector;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class NonSliceSelector implements SliceSelector {
    @Override
    public String select(String entityName, String sql, Object[] parameters, Map<String, Object> options) {
        return sql;
    }

    @Override
    public String select(String entityName, String sql, List<?> parameters, Map<String, Object> options) {
        return sql;
    }
}
