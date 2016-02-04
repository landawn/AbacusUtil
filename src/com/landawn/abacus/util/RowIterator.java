/*
 * Copyright (c) 2015, Haiyang Li. All rights reserved.
 */

package com.landawn.abacus.util;

import java.io.Closeable;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Iterator;
import java.util.NoSuchElementException;

import com.landawn.abacus.exception.AbacusSQLException;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class RowIterator implements Iterator<Object[]>, Closeable {
    private final ResultSet rs;
    private final ResultSetMetaData metaData;
    private final int columnCount;
    private boolean hasNext = false;

    public RowIterator(final ResultSet rs) throws IllegalArgumentException {
        if (rs == null) {
            throw new IllegalArgumentException("ResultSet must not be null");
        }

        this.rs = rs;

        try {
            this.metaData = rs.getMetaData();
            this.columnCount = metaData.getColumnCount();
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        }
    }

    ResultSet resultSet() {
        return rs;
    }

    @Override
    public boolean hasNext() {
        if (hasNext == false) {
            try {
                hasNext = rs.next();
            } catch (SQLException e) {
                throw new AbacusSQLException(e);
            }
        }
        return hasNext;
    }

    public boolean moveToNext() {
        final boolean res = hasNext();
        hasNext = false;
        return res;
    }

    @Override
    public Object[] next() {
        if (!hasNext()) {
            throw new NoSuchElementException("No more rows");
        }

        final Object[] row = new Object[columnCount];

        try {
            for (int i = 0; i < columnCount; i++) {
                row[i] = rs.getObject(i + 1);
            }
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        }

        hasNext = false;

        return row;
    }

    @Override
    public void close() {
        JdbcUtil.close(rs);
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException("Remove unsupported on LineIterator");
    }
}
