/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.dataSource;

import java.sql.Connection;
import java.util.Map;
import java.util.Properties;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface ConnectionManager {

    Map<String, String> getProperties();

    Properties getConnectionProperties();

    void updateLastSQLExecutionFailureTime();

    int getMaxActive();

    int getNumActive();

    Connection getConnection();

    void closeConnection(Connection conn);

    void detroyConnection(Connection conn);

    void close();
}
