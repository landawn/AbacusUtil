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
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.NoSuchElementException;

import com.landawn.abacus.exception.UncheckedSQLException;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class RowIterator extends ImmutableIterator<Object[]> implements Closeable {
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

        if (closeConnection && closeStatement == false) {
            throw new IllegalArgumentException("'closeStatement' can't be false while 'closeConnection' is true");
        }

        this.rs = rs;
        this.count = count;

        try {
            this.metaData = rs.getMetaData();
            this.columnCount = metaData.getColumnCount();

            JdbcUtil.skip(rs, offset);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }

        this.closeStatement = closeStatement;
        this.closeConnection = closeConnection;
    }

    ResultSet resultSet() {
        return rs;
    }

    public int columnCount() {
        return columnCount;
    }

    public List<String> columnLabels() throws UncheckedSQLException {
        if (columnLabelList == null) {
            try {
                columnLabelList = ImmutableList.of(JdbcUtil.getColumnLabelList(rs));
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
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

    public void skip(final int n) throws UncheckedSQLException {
        skip((long) n);
    }

    public void skip(final long n) throws UncheckedSQLException {
        if (n <= 0) {
            return;
        }

        final long m = Math.min(hasNext ? n - 1 : n, count - cnt);

        try {
            JdbcUtil.skip(rs, m);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }

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

    public <E extends Exception> void foreachRemaining(Try.Consumer<? super Object[], E> action) throws E {
        N.checkArgNotNull(action);

        while (hasNext()) {
            action.accept(next());
        }
    }

    @Override
    public synchronized void close() throws UncheckedSQLException {
        if (isClosed) {
            return;
        }

        isClosed = true;

        JdbcUtil.closeQuietly(rs, closeStatement, closeConnection);
    }
}
