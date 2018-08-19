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

    private final String id;
    private Connection conn;
    private final int originalIsolationLevel;
    private final boolean originalAutoCommit;
    private Status status;
    private boolean isRollbackOnly = false;

    private final AtomicInteger refCount = new AtomicInteger();
    private final Stack<IsolationLevel> isolationLevelStack = new Stack<>();
    private final Stack<Boolean> isForUpdateOnlyStack = new Stack<>();
    private IsolationLevel isolationLevel;
    private boolean isForUpdateOnly;
    private SQLExecutor sqlExecutor;

    public SQLTransaction(SQLExecutor sqlExecutor, IsolationLevel isolationLevel) {
        this.sqlExecutor = sqlExecutor;
        this.id = N.uuid();
        this.isolationLevel = isolationLevel;
        this.conn = sqlExecutor.getConnection();
        status = Status.ACTIVE;

        try {
            originalIsolationLevel = conn.getTransactionIsolation();
            originalAutoCommit = conn.getAutoCommit();
            conn.setAutoCommit(false);

            if ((isolationLevel == null) || (isolationLevel == IsolationLevel.DEFAULT)) {
                // ignore. by default
            } else {
                conn.setTransactionIsolation(isolationLevel.intValue());
            }
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    @Override
    public String id() {
        return id;
    }

    @Override
    public IsolationLevel isolationLevel() {
        return isolationLevel;
    }

    @Override
    public Status status() {
        return status;
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
    public void commit() throws UncheckedSQLException {
        if (decrementAndGetRef() > 0) {
            return;
        }

        if (!status.equals(Status.ACTIVE)) {
            throw new IllegalStateException("transaction is already " + status);
        }

        if (isRollbackOnly) {
            try {
                throw new IllegalStateException("transaction will be rolled back because it's for roll back only");
            } finally {
                rollback();
            }
        } else {
            if (logger.isInfoEnabled()) {
                logger.info("Committing transaction with id: " + id);
            }

            status = Status.COMMIT_FAILED;

            try {
                conn.commit();

                status = Status.COMMITTED;
            } catch (SQLException e) {
                boolean rollback = false;

                try {
                    rollback();

                    rollback = true;
                } catch (Throwable e2) {
                    // ignore;
                    logger.error("Failed to rollback after error happened during committing", e2);
                }

                throw new UncheckedSQLException(
                        "Failed to commit transaction with id: " + id + ". and " + (rollback ? "rollback sucessfully" : "failed to rollback"), e);
            } finally {
                closeConnection();
            }
        }
    }

    @Override
    public void rollback() throws UncheckedSQLException {
        if (decrementAndGetRef() > 0) {
            isRollbackOnly = true;
            return;
        }

        if (!(status.equals(Status.ACTIVE) || status == Status.COMMIT_FAILED)) {
            throw new IllegalStateException("transaction is already " + status);
        }

        if (logger.isInfoEnabled()) {
            logger.info("Rolling back transaction with id: " + id);
        }

        status = Status.ROLLBACK_FAILED;

        try {
            conn.rollback();

            status = Status.ROLLBACKED;
        } catch (SQLException e) {
            throw new UncheckedSQLException("Failed to rollback transaction with id: " + id, e);
        } finally {
            closeConnection();
        }
    }

    /**
     * Associate current thread with this transaction.
     * 
     */
    public void attach() {
        attach(Thread.currentThread().getId());
    }

    /**
     * Associate the specified thread with this transaction.
     * 
     * @param threadId
     */
    public void attach(long threadId) {
        sqlExecutor.threadIdTransactionMap.put(threadId, this);

        if (logger.isInfoEnabled()) {
            logger.info("Active transactions: " + sqlExecutor.threadIdTransactionMap);
        }
    }

    /**
     * Remove this transaction from current thread.
     * 
     */
    public void detach() {
        detach(Thread.currentThread().getId());
    }

    /**
     * Remove this transaction from the specified thread.
     * 
     * @param threadId
     */
    public void detach(long threadId) {
        sqlExecutor.threadIdTransactionMap.remove(threadId);

        if (logger.isInfoEnabled()) {
            logger.info("Active transactions: " + sqlExecutor.threadIdTransactionMap);
        }
    }

    private void closeConnection() {
        if (conn == null) {
            return;
        }

        try {
            conn.setAutoCommit(originalAutoCommit);
            conn.setTransactionIsolation(originalIsolationLevel);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            try {
                conn.close();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }

            conn = null;
        }
    }

    synchronized int getAndIncrementRef(final IsolationLevel isolationLevel, final boolean forUpdateOnly) {
        if (conn != null) {
            try {
                conn.setTransactionIsolation(isolationLevel.intValue());
            } catch (SQLException e) {
                new UncheckedSQLException(e);
            }
        }

        this.isolationLevel = isolationLevel;
        this.isForUpdateOnly = forUpdateOnly;
        this.isolationLevelStack.push(isolationLevel);
        this.isForUpdateOnlyStack.push(forUpdateOnly);
        return refCount.getAndIncrement();
    }

    synchronized int decrementAndGetRef() throws UncheckedSQLException {
        final int res = refCount.decrementAndGet();

        if (res == 0) {
            sqlExecutor.threadIdTransactionMap.remove(Thread.currentThread().getId());

            if (logger.isInfoEnabled()) {
                logger.info("Closing transaction: " + id);
                logger.info("Active transactions: " + sqlExecutor.threadIdTransactionMap);
            }
        } else {
            final IsolationLevel tmp = isolationLevelStack.pop();

            if (conn != null) {
                try {
                    conn.setTransactionIsolation(tmp.intValue());
                } catch (SQLException e) {
                    new UncheckedSQLException(e);
                }
            }

            this.isolationLevel = tmp;
            this.isForUpdateOnly = isForUpdateOnlyStack.pop();
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
