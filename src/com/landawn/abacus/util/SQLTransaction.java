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
import java.sql.SQLException;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import com.landawn.abacus.IsolationLevel;
import com.landawn.abacus.Transaction;
import com.landawn.abacus.exception.UncheckedSQLException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;

/**
 * DO NOT CLOSE the connection manually. It will be automatically closed after the transaction is committed or rolled back.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class SQLTransaction implements Transaction {
    private static final Logger logger = LoggerFactory.getLogger(SQLTransaction.class);

    private final SQLExecutor sqlExecutor;
    private final String id;
    private Connection conn;
    private final int originalIsolationLevel;
    private final boolean originalAutoCommit;
    private Status status;

    private final AtomicInteger refCount = new AtomicInteger();
    private final Stack<IsolationLevel> isolationLevelStack = new Stack<>();
    private final Stack<Boolean> isForUpdateOnlyStack = new Stack<>();
    private IsolationLevel isolationLevel;
    private boolean isForUpdateOnly;

    SQLTransaction(SQLExecutor sqlExecutor, IsolationLevel isolationLevel) {
        this.sqlExecutor = sqlExecutor;
        this.id = SQLExecutor.getTransactionThreadId() + "_" + System.currentTimeMillis();
        this.isolationLevel = isolationLevel;
        this.conn = sqlExecutor.getConnection();
        status = Status.ACTIVE;

        try {
            originalIsolationLevel = conn.getTransactionIsolation();
            originalAutoCommit = conn.getAutoCommit();

            conn.setAutoCommit(false);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    @Override
    public String id() {
        return id;
    }

    /**
     * DO NOT CLOSE the connection manually. It will be automatically closed after the transaction is committed or rolled back.
     * 
     * @return
     */
    public Connection connection() {
        return conn;
    }

    @Override
    public IsolationLevel isolationLevel() {
        return isolationLevel;
    }

    @Override
    public Status status() {
        return status;
    }

    @Override
    public boolean isActive() {
        return status == Status.ACTIVE;
    }

    @Override
    public void commit() throws UncheckedSQLException {
        if (decrementAndGetRef() > 0) {
            return;
        }

        if (status.equals(Status.MARKED_ROLLBACK)) {
            logger.warn("Transaction(id={}) be rolled back because it's marked for roll back only", id);

            executeRollback();
        }

        if (!status.equals(Status.ACTIVE)) {
            throw new IllegalStateException("Transaction(id=" + id + ") is already: " + status);
        }

        logger.info("Committing transaction(id={})", id);

        status = Status.FAILED_COMMIT;

        try {
            conn.commit();

            status = Status.COMMITTED;
        } catch (SQLException e) {
            throw new UncheckedSQLException("Failed to commit transaction(id=" + id + ")", e);
        } finally {
            if (status == Status.COMMITTED) {
                logger.info("Transaction(id={}) is committed sucessfully", id);

                resetAndCloseConnection();
            } else {
                logger.warn("Failed to commit transaction(id={}). It will be automatically rolled back ", id);

                executeRollback();
            }
        }
    }

    @Override
    public void rollback() throws UncheckedSQLException {
        if (decrementAndGetRef() > 0) {
            status = Status.MARKED_ROLLBACK;
            return;
        }

        if (!(status.equals(Status.ACTIVE) || status.equals(Status.MARKED_ROLLBACK) || status == Status.FAILED_COMMIT)) {
            throw new IllegalStateException("Transaction(id=" + id + ") is already: " + status);
        }

        executeRollback();
    }

    private void executeRollback() throws UncheckedSQLException {
        logger.warn("Rolling back transaction(id={})" + id);

        status = Status.FAILED_ROLLBACK;

        try {
            conn.rollback();

            status = Status.ROLLED_BACK;
        } catch (SQLException e) {
            throw new UncheckedSQLException("Failed to roll back transaction(id=" + id + ")", e);
        } finally {
            if (status == Status.ROLLED_BACK) {
                logger.warn("Transaction(id={}) is rolled back successfully", id);
            } else {
                logger.warn("Failed to roll back transaction(id={})", id);
            }

            resetAndCloseConnection();
        }
    }

    /**
     * Associate current thread with this transaction.
     * 
     */
    public void attach() {
        attach(SQLExecutor.getTransactionThreadId());
    }

    /**
     * Associate the specified thread with this transaction.
     * 
     * @param threadId
     */
    private void attach(String ttid) {
        if (!status.equals(Status.ACTIVE)) {
            throw new IllegalStateException("Transaction(id=" + id + ") is already: " + status);
        }

        logger.info("Attaching transaction(id={}) to thread={}", id, ttid);

        sqlExecutor.threadTransactionMap.put(ttid, this);
    }

    /**
     * Remove this transaction from current thread.
     * 
     */
    public void detach() {
        detach(SQLExecutor.getTransactionThreadId());
    }

    /**
     * Remove this transaction from the specified thread.
     * 
     * @param threadId
     */
    private void detach(String ttid) {
        logger.info("Detaching transaction(id={}) from thread={}", id, ttid);

        sqlExecutor.threadTransactionMap.remove(ttid);
    }

    private void resetAndCloseConnection() {
        if (conn == null) {
            return;
        }

        try {
            conn.setAutoCommit(originalAutoCommit);
            conn.setTransactionIsolation(originalIsolationLevel);
        } catch (SQLException e) {
            logger.error("Failed to reset connection in transaction(id=" + id + ")", e);
        } finally {
            JdbcUtil.closeQuietly(conn);
            conn = null;
        }
    }

    synchronized int incrementAndGetRef(final IsolationLevel isolationLevel, final boolean forUpdateOnly) {
        if (!status.equals(Status.ACTIVE)) {
            throw new IllegalStateException("Transaction(id=" + id + ") is already: " + status);
        }

        if (conn != null) {
            try {
                conn.setTransactionIsolation(isolationLevel.intValue());
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }

        if (refCount.get() > 0) {
            this.isolationLevelStack.push(this.isolationLevel);
            this.isForUpdateOnlyStack.push(this.isForUpdateOnly);
        }

        this.isolationLevel = isolationLevel;
        this.isForUpdateOnly = forUpdateOnly;

        return refCount.incrementAndGet();
    }

    synchronized int decrementAndGetRef() throws UncheckedSQLException {
        final int res = refCount.decrementAndGet();

        if (res == 0) {
            sqlExecutor.threadTransactionMap.remove(SQLExecutor.getTransactionThreadId());

            logger.info("Finishing transaction(id={})", id);

            logger.debug("Remaining active transactions: {}" + sqlExecutor.threadTransactionMap.values());
        } else {
            this.isolationLevel = isolationLevelStack.pop();
            this.isForUpdateOnly = isForUpdateOnlyStack.pop();

            if (conn != null) {
                try {
                    conn.setTransactionIsolation(isolationLevel.intValue());
                } catch (SQLException e) {
                    throw new UncheckedSQLException(e);
                }
            }

        }

        return res;
    }

    boolean isForUpdateOnly() {
        return isForUpdateOnly;
    }

    @Override
    public int hashCode() {
        return id.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof SQLTransaction && id.equals(((SQLTransaction) obj).id);
    }

    @Override
    public String toString() {
        return id;
    }
}
