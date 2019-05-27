/*
 * Copyright (c) 2018, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.landawn.abacus.spring;

import java.sql.Connection;

import org.springframework.jdbc.datasource.DataSourceUtils;

import com.landawn.abacus.DataSource;
import com.landawn.abacus.IsolationLevel;
import com.landawn.abacus.util.AsyncExecutor;
import com.landawn.abacus.util.NamingPolicy;
import com.landawn.abacus.util.SQLMapper;
import com.landawn.abacus.util.SQLTransaction;

/**
 * Integrated with Spring JDBC/Transaction.
 * 
 * @author haiyangl
 *
 */
public class SQLExecutor extends com.landawn.abacus.util.SQLExecutor {

    public SQLExecutor(final javax.sql.DataSource dataSource) {
        super(dataSource);
    }

    public SQLExecutor(final javax.sql.DataSource dataSource, final JdbcSettings jdbcSettings) {
        super(dataSource, jdbcSettings);
    }

    public SQLExecutor(final javax.sql.DataSource dataSource, final JdbcSettings jdbcSettings, final SQLMapper sqlMapper) {
        super(dataSource, jdbcSettings, sqlMapper);
    }

    public SQLExecutor(final javax.sql.DataSource dataSource, final JdbcSettings jdbcSettings, final SQLMapper sqlMapper, final NamingPolicy namingPolicy) {
        super(dataSource, jdbcSettings, sqlMapper, namingPolicy);
    }

    public SQLExecutor(final javax.sql.DataSource dataSource, final JdbcSettings jdbcSettings, final SQLMapper sqlMapper, final NamingPolicy namingPolicy,
            final AsyncExecutor asyncExecutor) {
        super(dataSource, jdbcSettings, sqlMapper, namingPolicy, asyncExecutor);
    }

    @Override
    protected Connection getConnection(final DataSource ds) {
        return DataSourceUtils.getConnection(ds);
    }

    @Override
    protected void closeQuietly(final Connection conn, final DataSource ds) {
        DataSourceUtils.releaseConnection(conn, ds);
    }

    /** 
     * The connection opened in the transaction will be automatically closed after the transaction is committed or rolled back.
     * DON'T close it again by calling the close method.
     * 
     * @return
     * @deprecated
     */
    @Deprecated
    @Override
    public SQLTransaction beginTransaction() {
        return super.beginTransaction();
    }

    /** 
     * The connection opened in the transaction will be automatically closed after the transaction is committed or rolled back.
     * DON'T close it again by calling the close method.
     * 
     * @param isolationLevel
     * @return
     * @deprecated
     */
    @Deprecated
    @Override
    public SQLTransaction beginTransaction(final IsolationLevel isolationLevel) {
        return super.beginTransaction(isolationLevel);
    }

    /** 
     * The connection opened in the transaction will be automatically closed after the transaction is committed or rolled back.
     * DON'T close it again by calling the close method.
     * 
     * @param forUpdateOnly
     * @return
     * @deprecated
     */
    @Deprecated
    @Override
    public SQLTransaction beginTransaction(final boolean forUpdateOnly) {
        return super.beginTransaction(forUpdateOnly);
    }

    /**
     * The connection opened in the transaction will be automatically closed after the transaction is committed or rolled back.
     * DON'T close it again by calling the close method.
     * 
     * @param isolationLevel
     * @param forUpdateOnly
     * @return
     * @deprecated
     */
    @Deprecated
    @Override
    public SQLTransaction beginTransaction(final IsolationLevel isolationLevel, final boolean forUpdateOnly) {
        return super.beginTransaction(isolationLevel, forUpdateOnly);
    }

    /**
     * The connection opened in the transaction will be automatically closed after the transaction is committed or rolled back.
     * DON'T close it again by calling the close method.
     * 
     * @param isolationLevel
     * @param forUpdateOnly
     * @param jdbcSettings
     * @return
     * @deprecated
     */
    @Deprecated
    @Override
    public SQLTransaction beginTransaction(final IsolationLevel isolationLevel, final boolean forUpdateOnly, final JdbcSettings jdbcSettings) {
        return super.beginTransaction(isolationLevel, forUpdateOnly, jdbcSettings);
    }
}
