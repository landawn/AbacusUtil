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

import java.io.Closeable;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

import com.landawn.abacus.exception.UncheckedSQLException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class RowIterator implements Iterator<Object[]>, Closeable {
    private static final Logger logger = LoggerFactory.getLogger(RowIterator.class);

    private final ResultSet rs;
    private final long count;
    private final ResultSetMetaData metaData;
    private final int columnCount;
    private volatile List<String> columnLabelList = null;
    private boolean hasNext = false;
    private long cnt = 0;
    private final boolean closeStatement;
    private final boolean closeConnection;
    private volatile boolean isClosed = false;

    public RowIterator(final ResultSet rs, final boolean closeStatement, final boolean closeConnection) throws IllegalArgumentException, UncheckedSQLException {
        this(rs, 0, Long.MAX_VALUE, closeStatement, closeConnection);
    }

    public RowIterator(final ResultSet rs, long offset, long count, final boolean closeStatement, final boolean closeConnection)
            throws IllegalArgumentException, UncheckedSQLException {
        if (offset < 0 || count < 0) {
            throw new IllegalArgumentException("'offset' and 'count' can't be negative");
        }

        if (rs == null) {
            throw new IllegalArgumentException("ResultSet must not be null");
        }

        this.rs = rs;
        this.count = count;

        try {
            this.metaData = rs.getMetaData();
            this.columnCount = metaData.getColumnCount();
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }

        JdbcUtil.skip(rs, offset);

        this.closeStatement = closeStatement;
        this.closeConnection = closeConnection;
    }

    ResultSet resultSet() {
        return rs;
    }

    public static RowIterator of(final ResultSet rs, final boolean closeStatement, final boolean closeConnection) throws UncheckedSQLException {
        return new RowIterator(rs, closeStatement, closeConnection);
    }

    public static RowIterator of(final ResultSet rs, final long offset, final long count, final boolean closeStatement, final boolean closeConnection)
            throws UncheckedSQLException {
        return new RowIterator(rs, offset, count, closeStatement, closeConnection);
    }

    public static List<RowIterator> of(final Collection<ResultSet> c, final boolean closeStatement, final boolean closeConnection)
            throws UncheckedSQLException {
        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        final List<RowIterator> result = new ArrayList<>(c.size());

        for (ResultSet e : c) {
            result.add(new RowIterator(e, closeStatement, closeConnection));
        }

        return result;
    }

    public int getColumnCount() {
        return columnCount;
    }

    public List<String> getColumnLabelList() throws UncheckedSQLException {
        if (columnLabelList == null) {
            columnLabelList = ImmutableList.of(JdbcUtil.getColumnLabelList(rs));
        }

        return columnLabelList;
    }

    @Override
    public boolean hasNext() throws UncheckedSQLException {
        if (hasNext == false) {
            try {
                hasNext = cnt < count && rs.next();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }

        return hasNext;
    }

    public boolean moveToNext() throws UncheckedSQLException {
        if (hasNext) {
            cnt++;
            hasNext = false;
            return true;
        } else {
            try {
                if (cnt < count && rs.next()) {
                    cnt++;
                    return true;
                }
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }

        return false;
    }

    public void skip(final int n) throws UncheckedSQLException {
        skip((long) n);
    }

    public void skip(final long n) throws UncheckedSQLException {
        if (n <= 0) {
            return;
        }

        final long m = Math.min(hasNext ? n - 1 : n, count - cnt);
        JdbcUtil.skip(rs, m);
        cnt += Math.min(n, count - cnt);
        hasNext = false;
    }

    @Override
    public Object[] next() throws UncheckedSQLException {
        if (!hasNext()) {
            throw new NoSuchElementException("No more rows");
        }

        final Object[] row = new Object[columnCount];

        try {
            for (int i = 0; i < columnCount; i++) {
                row[i] = rs.getObject(i + 1);
            }
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }

        hasNext = false;
        cnt++;

        return row;
    }

    @Override
    public synchronized void close() throws UncheckedSQLException {
        if (isClosed) {
            return;
        }

        isClosed = true;

        Connection conn = null;
        Statement stmt = null;

        try {
            stmt = rs.getStatement();
            conn = stmt.getConnection();
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(rs, stmt, conn);
        }
    }

    /**
     * Unsupported.
     *
     * @throws UnsupportedOperationException always
     * @Deprecated UnsupportedOperationException
     */
    @Override
    @Deprecated
    public void remove() {
        throw new UnsupportedOperationException("Remove unsupported on LineIterator");
    }

    private void closeQuietly(final ResultSet rs, final Statement stmt, final Connection conn) {
        if (rs != null) {
            try {
                rs.close();
            } catch (Throwable e) {
                logger.error("Failed to close ResultSet", e);
            }
        }

        if (stmt != null && closeStatement) {
            if (stmt instanceof PreparedStatement) {
                try {
                    ((PreparedStatement) stmt).clearParameters();
                } catch (Throwable e) {
                    logger.error("Failed to clear parameters", e);
                }
            }

            try {
                stmt.close();
            } catch (Throwable e) {
                logger.error("Failed to close Statement", e);
            }
        }

        if (conn != null && closeConnection) {
            try {
                conn.close();
            } catch (Throwable e) {
                logger.error("Failed to close Connection", e);
            }
        }
    }
}
