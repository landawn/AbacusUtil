/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.dataSource;

import java.util.List;
import java.util.Map;

import com.landawn.abacus.DataSource;
import com.landawn.abacus.DataSourceManager;
import com.landawn.abacus.DataSourceSelector;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class SimpleSourceSelector implements DataSourceSelector {
    private static final String QUERY_WITH_DATA_SOURCE = "queryWithDataSource";

    @Override
    public DataSource select(DataSourceManager dataSourceManager, String entityName, String sql, Object[] parameters, Map<String, Object> options) {
        return getDataSource(dataSourceManager, options);
    }

    @Override
    public DataSource select(DataSourceManager dataSourceManager, String entityName, String sql, List<?> parameters, Map<String, Object> options) {
        return getDataSource(dataSourceManager, options);
    }

    private DataSource getDataSource(DataSourceManager dataSourceManager, Map<String, Object> options) {
        String dataSourceName = (options == null) ? null : (String) options.get(QUERY_WITH_DATA_SOURCE);

        DataSource ds = (dataSourceName == null) ? dataSourceManager.getPrimaryDataSource() : dataSourceManager.getActiveDataSources().get(dataSourceName);

        if (ds == null) {
            throw new IllegalArgumentException("No data source is available with name: " + dataSourceName);
        }

        return ds;
    }
}
