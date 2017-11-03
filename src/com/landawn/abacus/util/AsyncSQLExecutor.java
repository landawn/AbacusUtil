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

import java.sql.Connection;
import java.util.List;
import java.util.concurrent.Callable;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.util.SQLExecutor.JdbcSettings;
import com.landawn.abacus.util.SQLExecutor.ResultSetExtractor;
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
    public final <T> CompletableFuture<T> insert(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<T> insert(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(sql, statementSetter, parameters);
            }
        });
    }

    public <T> CompletableFuture<T> insert(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<T> insert(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<T> insert(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
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
    public <T> CompletableFuture<T> insert(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.insert(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> batchInsert(final String sql, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, batchParameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, batchParameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> batchInsert(final Connection conn, final String sql, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, batchParameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, batchParameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> batchInsert(final String sql, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, batchParameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, batchParameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> batchInsert(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> batchInsert(final Connection conn, final String sql, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, batchParameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter,
            final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, batchParameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> batchInsert(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.batchInsert(conn, sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Integer> update(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Integer> update(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(sql, statementSetter, parameters);
            }
        });
    }

    public CompletableFuture<Integer> update(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Integer> update(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Integer> update(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(conn, sql, statementSetter, parameters);
            }
        });
    }

    public CompletableFuture<Integer> update(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.update(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<Integer> batchUpdate(final String sql, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, batchParameters);
            }
        });
    }

    CompletableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, batchParameters);
            }
        });
    }

    CompletableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    CompletableFuture<Integer> batchUpdate(final Connection conn, final String sql, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, batchParameters);
            }
        });
    }

    CompletableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter, final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, batchParameters);
            }
        });
    }

    CompletableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object[] batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    public CompletableFuture<Integer> batchUpdate(final String sql, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, batchParameters);
            }
        });
    }

    public CompletableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, batchParameters);
            }
        });
    }

    public CompletableFuture<Integer> batchUpdate(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    public CompletableFuture<Integer> batchUpdate(final Connection conn, final String sql, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, batchParameters);
            }
        });
    }

    public CompletableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter,
            final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, batchParameters);
            }
        });
    }

    public CompletableFuture<Integer> batchUpdate(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final List<?> batchParameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.batchUpdate(conn, sql, statementSetter, jdbcSettings, batchParameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Boolean> exists(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return sqlExecutor.exists(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Boolean> exists(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return sqlExecutor.exists(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Integer> count(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.count(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Integer> count(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return sqlExecutor.count(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<T> get(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<T> get(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, conn, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.get(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, conn, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.find(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sql, jdbcSettings, parameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final String sql, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sql, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sqls, jdbcSettings, parameters);
            }
        });
    }

    public <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sqls, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<List<T>> findAll(final Class<T> targetClass, final Connection conn, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return sqlExecutor.findAll(targetClass, conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalBoolean> queryForBoolean(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalBoolean>() {
            @Override
            public OptionalBoolean call() throws Exception {
                return sqlExecutor.queryForBoolean(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalChar> queryForChar(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalChar>() {
            @Override
            public OptionalChar call() throws Exception {
                return sqlExecutor.queryForChar(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalByte> queryForByte(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalByte>() {
            @Override
            public OptionalByte call() throws Exception {
                return sqlExecutor.queryForByte(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalShort> queryForShort(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalShort>() {
            @Override
            public OptionalShort call() throws Exception {
                return sqlExecutor.queryForShort(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalInt> queryForInt(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalInt>() {
            @Override
            public OptionalInt call() throws Exception {
                return sqlExecutor.queryForInt(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalLong> queryForLong(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalLong>() {
            @Override
            public OptionalLong call() throws Exception {
                return sqlExecutor.queryForLong(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalFloat> queryForFloat(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalFloat>() {
            @Override
            public OptionalFloat call() throws Exception {
                return sqlExecutor.queryForFloat(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<OptionalDouble> queryForDouble(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<OptionalDouble>() {
            @Override
            public OptionalDouble call() throws Exception {
                return sqlExecutor.queryForDouble(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Nullable<String>> queryForString(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<String>>() {
            @Override
            public Nullable<String> call() throws Exception {
                return sqlExecutor.queryForString(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    public <T> CompletableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final Connection conn, final String sql,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, conn, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final Connection conn, final String sql,
            final StatementSetter statementSetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return sqlExecutor.queryForSingleResult(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    //
    //    public CompletableFuture<Map<String, Object>> queryForMap(final String sql, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(sql, parameters);
    //            }
    //        });
    //    }
    //
    //    public CompletableFuture<Map<String, Object>> queryForMap(final String sql, final StatementSetter statementSetter, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(sql, statementSetter, parameters);
    //            }
    //        });
    //    }
    //
    //    public CompletableFuture<Map<String, Object>> queryForMap(final Connection conn, final String sql, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(conn, sql, parameters);
    //            }
    //        });
    //    }
    //
    //    public CompletableFuture<Map<String, Object>> queryForMap(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
    //            @Override
    //            public Map<String, Object> call() throws Exception {
    //                return sqlExecutor.queryForMap(conn, sql, statementSetter, parameters);
    //            }
    //        });
    //    }
    //
    @SafeVarargs
    public final <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.queryForEntity(targetClass, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.queryForEntity(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.queryForEntity(targetClass, conn, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final Connection conn, final String sql,
            final StatementSetter statementSetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return sqlExecutor.queryForEntity(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> query(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> query(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(sql, statementSetter, parameters);
            }
        });
    }

    public <T> CompletableFuture<T> query(final String sql, final StatementSetter statementSetter, final ResultSetExtractor<T> resultSetExtractor,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(sql, statementSetter, resultSetExtractor, parameters);
            }
        });
    }

    public <T> CompletableFuture<T> query(final String sql, final StatementSetter statementSetter, final ResultSetExtractor<T> resultSetExtractor,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(sql, statementSetter, resultSetExtractor, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> query(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(conn, sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> query(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.query(conn, sql, statementSetter, parameters);
            }
        });
    }

    public <T> CompletableFuture<T> query(final Connection conn, final String sql, final StatementSetter statementSetter,
            final ResultSetExtractor<T> resultSetExtractor, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(conn, sql, statementSetter, resultSetExtractor, parameters);
            }
        });
    }

    public <T> CompletableFuture<T> query(final Connection conn, final String sql, final StatementSetter statementSetter,
            final ResultSetExtractor<T> resultSetExtractor, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return sqlExecutor.query(conn, sql, statementSetter, resultSetExtractor, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> queryAll(final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sql, jdbcSettings, parameters);
            }
        });
    }

    public CompletableFuture<DataSet> queryAll(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<DataSet> queryAll(final Connection conn, final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sql, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<DataSet> queryAll(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> queryAll(final List<String> sqls, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sqls, jdbcSettings, parameters);
            }
        });
    }

    public CompletableFuture<DataSet> queryAll(final List<String> sqls, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<DataSet> queryAll(final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sqls, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<DataSet> queryAll(final Connection conn, final List<String> sqls, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return sqlExecutor.queryAll(conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<RowIterator> iterate(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<RowIterator>() {
            @Override
            public RowIterator call() throws Exception {
                return sqlExecutor.iterate(sql, parameters);
            }
        });
    }

    CompletableFuture<RowIterator> iterate(final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<RowIterator>() {
            @Override
            public RowIterator call() throws Exception {
                return sqlExecutor.iterate(sql, statementSetter, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>RowIterator</code> to close the underlying <code>ResultSet</code>.
     * 
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    CompletableFuture<RowIterator> iterate(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<RowIterator>() {
            @Override
            public RowIterator call() throws Exception {
                return sqlExecutor.iterate(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<RowIterator> iterate(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<RowIterator>() {
            @Override
            public RowIterator call() throws Exception {
                return sqlExecutor.iterate(conn, sql, parameters);
            }
        });
    }

    CompletableFuture<RowIterator> iterate(final Connection conn, final String sql, final StatementSetter statementSetter, final Object... parameters) {
        return asyncExecutor.execute(new Callable<RowIterator>() {
            @Override
            public RowIterator call() throws Exception {
                return sqlExecutor.iterate(conn, sql, statementSetter, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>RowIterator</code> to close the underlying <code>ResultSet</code>.
     * 
     * @param conn
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    CompletableFuture<RowIterator> iterate(final Connection conn, final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<RowIterator>() {
            @Override
            public RowIterator call() throws Exception {
                return sqlExecutor.iterate(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<List<RowIterator>> iterateAll(final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(sql, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>RowIterator</code> list to close the underlying <code>ResultSet</code> list.
     * 
     * @param sql
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    CompletableFuture<List<RowIterator>> iterateAll(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<List<RowIterator>> iterateAll(final Connection conn, final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(conn, sql, jdbcSettings, parameters);
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
    CompletableFuture<List<RowIterator>> iterateAll(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<List<RowIterator>> iterateAll(final List<String> sqls, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(sqls, jdbcSettings, parameters);
            }
        });
    }

    /**
     * Remember to close the returned <code>RowIterator</code> list to close the underlying <code>ResultSet</code> list.
     * 
     * @param sqls
     * @param statementSetter
     * @param jdbcSettings
     * @param parameters
     * @return
     */
    CompletableFuture<List<RowIterator>> iterateAll(final List<String> sqls, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<List<RowIterator>> iterateAll(final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(conn, sqls, jdbcSettings, parameters);
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
    CompletableFuture<List<RowIterator>> iterateAll(final Connection conn, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<RowIterator>>() {
            @Override
            public List<RowIterator> call() throws Exception {
                return sqlExecutor.iterateAll(conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Try<Stream<Object[]>>> stream(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(sql, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Try<Stream<Object[]>>> stream(final String sql, final StatementSetter statementSetter, final Object... parameters) {
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
    public CompletableFuture<Try<Stream<Object[]>>> stream(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<Try<Stream<Object[]>>> stream(final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(conn, sql, parameters);
            }
        });
    }

    CompletableFuture<Try<Stream<Object[]>>> stream(final Connection conn, final String sql, final StatementSetter statementSetter,
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
    CompletableFuture<Try<Stream<Object[]>>> stream(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.stream(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Try<Stream<Object[]>>> streamAll(final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
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
    public CompletableFuture<Try<Stream<Object[]>>> streamAll(final String sql, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final String sql, final JdbcSettings jdbcSettings, final Object... parameters) {
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
    CompletableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Try<Stream<Object[]>>> streamAll(final List<String> sqls, final JdbcSettings jdbcSettings, final Object... parameters) {
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
    public CompletableFuture<Try<Stream<Object[]>>> streamAll(final List<String> sqls, final StatementSetter statementSetter, final JdbcSettings jdbcSettings,
            final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    CompletableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings,
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
    CompletableFuture<Try<Stream<Object[]>>> streamAll(final Connection conn, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<Object[]>>>() {
            @Override
            public Try<Stream<Object[]>> call() throws Exception {
                return sqlExecutor.streamAll(conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, sql, parameters);
            }
        });
    }

    public <T> CompletableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
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
    public <T> CompletableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final Connection conn, final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, conn, sql, parameters);
            }
        });
    }

    <T> CompletableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
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
    <T> CompletableFuture<Try<Stream<T>>> stream(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.stream(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    public <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final String sql, final JdbcSettings jdbcSettings,
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
    public <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final String sql, final JdbcSettings jdbcSettings,
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
    <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final String sql, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, conn, sql, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    public <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final List<String> sqls, final JdbcSettings jdbcSettings,
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
    public <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final List<String> sqls, final StatementSetter statementSetter,
            final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final List<String> sqls, final JdbcSettings jdbcSettings,
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
    <T> CompletableFuture<Try<Stream<T>>> streamAll(final Class<T> targetClass, final Connection conn, final List<String> sqls,
            final StatementSetter statementSetter, final JdbcSettings jdbcSettings, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Try<Stream<T>>>() {
            @Override
            public Try<Stream<T>> call() throws Exception {
                return sqlExecutor.streamAll(targetClass, conn, sqls, statementSetter, jdbcSettings, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Void> execute(final String sql, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                sqlExecutor.execute(sql, parameters);
                return null;
            }
        });
    }

    //    public CompletableFuture<Integer> executeUpdate(final String sql, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<Integer>() {
    //            @Override
    //            public Integer call() throws Exception {
    //                return sqlExecutor.executeUpdate(sql, parameters);
    //            }
    //        });
    //    }
    //
    //    /**
    //     * Don't forget to close the returned ResultSet.
    //     * 
    //     * @param sql
    //     * @param parameters
    //     * @return
    //     */
    //    public CompletableFuture<ResultSet> executeQuery(final String sql, final Object... parameters) {
    //        return asyncExecutor.execute(new Callable<ResultSet>() {
    //            @Override
    //            public ResultSet call() throws Exception {
    //                return sqlExecutor.executeQuery(sql, parameters);
    //            }
    //        });
    //    }
}
