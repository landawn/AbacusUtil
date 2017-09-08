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
    private final IsolationLevel isolationLevel;
    private final int originalIsolationLevel;
    private Status status;

    public SQLTransaction(Connection conn, IsolationLevel isolationLevel) {
        this.id = N.uuid();
        this.isolationLevel = isolationLevel;
        this.conn = conn;
        status = Status.ACTIVE;

        try {
            originalIsolationLevel = conn.getTransactionIsolation();
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
        if (!status.equals(Status.ACTIVE)) {
            throw new IllegalStateException("transaction is already " + status);
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

            throw new UncheckedSQLException("Failed to commit transaction with id: " + id + ". and " + (rollback ? "rollback sucessfully" : "failed to rollback"),
                    e);
        } finally {
            closeConnection();
        }
    }

    @Override
    public void rollback() throws UncheckedSQLException {
        if (!(status.equals(Status.ACTIVE) || status == Status.COMMIT_FAILED)) {
            throw new IllegalStateException("transaction is already " + status);
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

    private void closeConnection() {
        try {
            conn.setAutoCommit(true);
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
}
