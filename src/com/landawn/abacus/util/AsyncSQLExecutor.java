/*
 * Copyright (C) 2015 HaiYang Li
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

package com.landawn.abacus.util;

import java.math.BigDecimal;
import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.concurrent.Callable;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.SQLExecutor.JdbcSettings;
import com.landawn.abacus.util.SQLExecutor.ResultExtractor;
import com.landawn.abacus.util.SQLExecutor.StatementSetter;
import com.landawn.abacus.util.stream.Stream;

/**
 * Asynchronous <code>SQLExecutor</code>.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class AsyncSQLExecutor {
    protected static final Logger logger = LoggerFactory.getLogger(AsyncSQLExecutor.class);

    private final SQLExecutor sqlExecutor;
    private final AsyncExecutor asyncExecutor;

    AsyncSQLExecutor(final SQLExecutor sqlExecutor, final AsyncExecutor asyncExecutor) {
        this.sqlExecutor = sqlExecutor;
        this.asyncExecutor = asyncExecutor;
    }

    public SQLExecutor sync() {
        return sqlExecutor;
    }

    AsyncExecutor asyncExecutor() {
        return asyncExecutor;
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> insert(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> insert(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(sql, statementSetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> insert(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> insert(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> insert(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(conn, sql, statementSetter, parameters);
            }
        });
    }

    /**
     * @see SQLExecutor#insert(Connection, String, StatementSetter, JdbcSettings, Object...)
     */
    @SafeVarargs
    public final <T> ContinuableFuture<T> insert(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<List<T>> batchInsert(final String sql, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, parametersList);
            }
        });
    }

    <T> ContinuableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, parametersList);
            }
        });
    }

    <T> ContinuableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    <T> ContinuableFuture<List<T>> batchInsert(final Connection conn, final String sql, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, parametersList);
            }
        });
    }

    <T> ContinuableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, parametersList);
            }
        });
    }

    <T> ContinuableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> batchInsert(final String sql, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, parametersList);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, parametersList);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> batchInsert(final Connection conn, final String sql, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, parametersList);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter,
            final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, parametersList);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> update(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> update(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(sql, statementSetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> update(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> update(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> update(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(conn, sql, statementSetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> update(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<Integer> batchUpdate(final String sql, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, parametersList);
            }
        });
    }

    ContinuableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, parametersList);
            }
        });
    }

    ContinuableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    ContinuableFuture<Integer> batchUpdate(final Connection conn, final String sql, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, parametersList);
            }
        });
    }

    ContinuableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter, final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, parametersList);
            }
        });
    }

    ContinuableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    public ContinuableFuture<Integer> batchUpdate(final String sql, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, parametersList);
            }
        });
    }

    public ContinuableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, parametersList);
            }
        });
    }

    public ContinuableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    public ContinuableFuture<Integer> batchUpdate(final Connection conn, final String sql, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, parametersList);
            }
        });
    }

    public ContinuableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter,
            final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, parametersList);
            }
        });
    }

    public ContinuableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final List<?> parametersList) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, jdbcSettings, parametersList);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Boolean> exists(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return sqlExecutor.exists(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Boolean> exists(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return sqlExecutor.exists(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> count(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.count(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Integer> count(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.count(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> get(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> get(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> get(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> get(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.gett(targetClass, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.gett(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.gett(targetClass, conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final Connection conn, final String sql,
            final StatementSetter statementSetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.gett(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    //
    //    public ContinuableFuture<Map<String, Object>> queryForMap(final String sql, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(sql, parameters);
    //            }
    //        });
    //    }
    //
    //    public ContinuableFuture<Map<String, Object>> queryForMap(final String sql, final StatementSetter statementSetter, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(sql, statementSetter, parameters);
    //            }
    //        });
    //    }
    //
    //    public ContinuableFuture<Map<String, Object>> queryForMap(final Connection conn, final String sql, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(conn, sql, parameters);
    //            }
    //        });
    //    }
    //
    //    public ContinuableFuture<Map<String, Object>> queryForMap(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(conn, sql, statementSetter, parameters);
    //            }
    //        });
    //    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(targetClass, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(targetClass, conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final Connection conn, final String sql,
            final StatementSetter statementSetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final String sql, final Try.Function<ResultSet, T, SQLException> recordGetter,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(sql, recordGetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final String sql, final StatementSetter statementSetter,
            final Try.Function<ResultSet, T, SQLException> recordGetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(sql, statementSetter, recordGetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final Connection conn, final String sql,
            final Try.Function<ResultSet, T, SQLException> recordGetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(conn, sql, recordGetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Optional<T>> findFirst(final Connection conn, final String sql, final StatementSetter statementSetter,
            final Try.Function<ResultSet, T, SQLException> recordGetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.findFirst(conn, sql, statementSetter, recordGetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final String sql, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sql, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final String sql, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sql, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sqls, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sqls, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> list(final String sql, final Try.BiFunction<ResultSet, List<String>, T, SQLException> recordGetter,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.list(sql, recordGetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> list(final String sql, final StatementSetter statementSetter,
            final Try.BiFunction<ResultSet, List<String>, T, SQLException> recordGetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.list(sql, statementSetter, recordGetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> list(final Connection conn, final String sql,
            final Try.BiFunction<ResultSet, List<String>, T, SQLException> recordGetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.list(conn, sql, recordGetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<List<T>> list(final Connection conn, final String sql, final StatementSetter statementSetter,
            final Try.BiFunction<ResultSet, List<String>, T, SQLException> recordGetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.list(conn, sql, statementSetter, recordGetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalBoolean> queryForBoolean(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalBoolean>() {
            @Override
            public OptionalBoolean call() throws Exception {
                return sqlExecutor.queryForBoolean(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalChar> queryForChar(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalChar>() {
            @Override
            public OptionalChar call() throws Exception {
                return sqlExecutor.queryForChar(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalByte> queryForByte(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalByte>() {
            @Override
            public OptionalByte call() throws Exception {
                return sqlExecutor.queryForByte(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalShort> queryForShort(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalShort>() {
            @Override
            public OptionalShort call() throws Exception {
                return sqlExecutor.queryForShort(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalInt> queryForInt(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalInt>() {
            @Override
            public OptionalInt call() throws Exception {
                return sqlExecutor.queryForInt(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalLong> queryForLong(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalLong>() {
            @Override
            public OptionalLong call() throws Exception {
                return sqlExecutor.queryForLong(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalFloat> queryForFloat(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalFloat>() {
            @Override
            public OptionalFloat call() throws Exception {
                return sqlExecutor.queryForFloat(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<OptionalDouble> queryForDouble(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalDouble>() {
            @Override
            public OptionalDouble call() throws Exception {
                return sqlExecutor.queryForDouble(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Nullable<BigDecimal>> queryForBigDecimal(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<BigDecimal>>() {
            @Override
            public Nullable<BigDecimal> call() throws Exception {
                return sqlExecutor.queryForBigDecimal(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Nullable<String>> queryForString(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<String>>() {
            @Override
            public Nullable<String> call() throws Exception {
                return sqlExecutor.queryForString(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Nullable<java.sql.Date>> queryForDate(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<java.sql.Date>>() {
            @Override
            public Nullable<java.sql.Date> call() throws Exception {
                return sqlExecutor.queryForDate(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Nullable<java.sql.Time>> queryForTime(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<java.sql.Time>>() {
            @Override
            public Nullable<java.sql.Time> call() throws Exception {
                return sqlExecutor.queryForTime(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Nullable<java.sql.Timestamp>> queryForTimestamp(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<java.sql.Timestamp>>() {
            @Override
            public Nullable<java.sql.Timestamp> call() throws Exception {
                return sqlExecutor.queryForTimestamp(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final Connection conn, final String sql,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final Connection conn, final String sql,
            final StatementSetter statementSetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> query(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> query(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(sql, statementSetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> query(final String sql, final StatementSetter statementSetter, final ResultExtractor<T> resultExtractor,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(sql, statementSetter, resultExtractor, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> query(final String sql, final StatementSetter statementSetter, final ResultExtractor<T> resultExtractor,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(sql, statementSetter, resultExtractor, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> query(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> query(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(conn, sql, statementSetter, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> query(final Connection conn, final String sql, final StatementSetter statementSetter,
            final ResultExtractor<T> resultExtractor, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(conn, sql, statementSetter, resultExtractor, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<T> query(final Connection conn, final String sql, final StatementSetter statementSetter,
            final ResultExtractor<T> resultExtractor, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(conn, sql, statementSetter, resultExtractor, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> queryAll(final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sql, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> queryAll(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<DataSet> queryAll(final Connection conn, final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sql, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<DataSet> queryAll(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> queryAll(final List<String> sqls, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sqls, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<DataSet> queryAll(final List<String> sqls, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<DataSet> queryAll(final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sqls, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<DataSet> queryAll(final Connection conn, final List<String> sqls, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Try<Stream<Object[]>>> stream(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Try<Stream<Object[]>>> stream(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(sql, statementSetter, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code>.
     * 
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final ContinuableFuture<Try<Stream<Object[]>>> stream(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<Try<Stream<Object[]>>> stream(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(conn, sql, parameters);
            }
        });
    }

    ContinuableFuture<Try<Stream<Object[]>>> stream(final Connection conn, final String sql, final StatementSetter statementSetter,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(conn, sql, statementSetter, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code>.
     * 
     * @param conn
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    ContinuableFuture<Try<Stream<Object[]>>> stream(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Try<Stream<Object[]>>> streamAll(final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(sql, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code> list.
     * 
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final ContinuableFuture<Try<Stream<Object[]>>> streamAll(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(conn, sql, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Returns the merged ResultSet acquired by querying with the specified sql in parallel. Mostly it's designed
     * for partition to query the partitioning table in more than one databases.
     *
     * @param conn
     * @param sql
     * @param statementSetter
     * @param jdbcSettings set multiple data sources by method: <code>setQueryWithDataSources</code>
     * @param parameters
     * @return
     */
    ContinuableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Try<Stream<Object[]>>> streamAll(final List<String> sqls, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(sqls, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code> list.
     * 
     * @param sqls
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final ContinuableFuture<Try<Stream<Object[]>>> streamAll(final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    ContinuableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(conn, sqls, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Returns the merged ResultSet acquired by querying with the specified sql list in parallel. Mostly it's designed
     * for partition to query multiple partitioning tables in one or more databases.
     *
     * @param conn
     * @param sqls
     * @param statementSetter
     * @param jdbcSettings set multiple data sources by method: <code>setQueryWithDataSources</code>
     * @param parameters
     * @return
     */
    ContinuableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, sql, statementSetter, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code>.
     * 
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final <T> ContinuableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, conn, sql, parameters);
            }
        });
    }

    <T> ContinuableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, conn, sql, statementSetter, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code>.
     * 
     * @param conn
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    <T> ContinuableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final String sql, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, sql, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code> list.
     * 
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final String sql, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, conn, sql, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Returns the merged ResultSet acquired by querying with the specified sql in parallel. Mostly it's designed
     * for partition to query the partitioning table in more than one databases.
     *
     * @param conn
     * @param sql
     * @param statementSetter
     * @param jdbcSettings set multiple data sources by method: <code>setQueryWithDataSources</code>
     * @param parameters
     * @return
     */
    <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, sqls, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>Stream</code> to close the underlying <code>ResultSet</code> list.
     * 
     * @param sqls
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, conn, sqls, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Returns the merged ResultSet acquired by querying with the specified sql list in parallel. Mostly it's designed
     * for partition to query multiple partitioning tables in one or more databases.
     *
     * @param conn
     * @param sqls
     * @param statementSetter
     * @param jdbcSettings set multiple data sources by method: <code>setQueryWithDataSources</code>
     * @param parameters
     * @return
     */
    <T> ContinuableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final List<String> sqls,
            final StatementSetter statementSetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Void> execute(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                sqlExecutor.execute(sql, parameters);
                return null;
            }
        });
    }

    @SafeVarargs
    public final ContinuableFuture<Void> execute(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                sqlExecutor.execute(conn, sql, parameters);
                return null;
            }
        });
    }
}
