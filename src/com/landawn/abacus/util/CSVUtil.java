/*
 * Copyright (c) 2015, Haiyang Li.
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

package com.landawn.abacus.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.Writer;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.core.RowDataSet;
import com.landawn.abacus.dataChannel.DataChannel;
import com.landawn.abacus.dataChannel.ResultSetChannel;
import com.landawn.abacus.dataChannel.StatementDataChannel;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.AbacusIOException;
import com.landawn.abacus.exception.AbacusSQLException;
import com.landawn.abacus.parser.JSONParser;
import com.landawn.abacus.parser.JSONSerializationConfig;
import com.landawn.abacus.parser.JSONSerializationConfig.JSC;
import com.landawn.abacus.parser.ParserFactory;
import com.landawn.abacus.parser.ParserUtil;
import com.landawn.abacus.parser.ParserUtil.EntityInfo;
import com.landawn.abacus.parser.ParserUtil.PropInfo;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.function.Predicate;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 *
 */
public final class CSVUtil {
    private static final JSONParser jsonParser = ParserFactory.createJSONParser();

    public static DataSet loadCSV(final File csvFile) {
        return loadCSV(csvFile, null);
    }

    public static DataSet loadCSV(final File csvFile, final Collection<String> selectColumnNames) {
        return loadCSV(csvFile, selectColumnNames, 0, Long.MAX_VALUE);
    }

    public static DataSet loadCSV(final File csvFile, final Collection<String> selectColumnNames, final long offset, final long count) {
        return loadCSV(csvFile, selectColumnNames, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param csvFile
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    public static DataSet loadCSV(final File csvFile, final Collection<String> selectColumnNames, final long offset, final long count,
            final Predicate<String[]> filter) {
        InputStream csvInputStream = null;

        try {
            csvInputStream = new FileInputStream(csvFile);

            return loadCSV(csvInputStream, selectColumnNames, offset, count, filter);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.closeQuietly(csvInputStream);
        }
    }

    public static DataSet loadCSV(final InputStream csvInputStream) {
        return loadCSV(csvInputStream, null);
    }

    public static DataSet loadCSV(final InputStream csvInputStream, final Collection<String> selectColumnNames) {
        return loadCSV(csvInputStream, selectColumnNames, 0, Long.MAX_VALUE);
    }

    public static DataSet loadCSV(final InputStream csvInputStream, final Collection<String> selectColumnNames, final long offset, final long count) {
        return loadCSV(csvInputStream, selectColumnNames, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param csvInputStream
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    public static DataSet loadCSV(final InputStream csvInputStream, final Collection<String> selectColumnNames, final long offset, final long count,
            final Predicate<String[]> filter) {
        final Reader csvReader = new InputStreamReader(csvInputStream);

        return loadCSV(csvReader, selectColumnNames, offset, count, filter);
    }

    public static DataSet loadCSV(final Reader csvReader) {
        return loadCSV(csvReader, null);
    }

    public static DataSet loadCSV(final Reader csvReader, final Collection<String> selectColumnNames) {
        return loadCSV(csvReader, selectColumnNames, 0, Long.MAX_VALUE);
    }

    public static DataSet loadCSV(final Reader csvReader, final Collection<String> selectColumnNames, long offset, long count) {
        return loadCSV(csvReader, selectColumnNames, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param csvReader
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    public static DataSet loadCSV(final Reader csvReader, final Collection<String> selectColumnNames, long offset, long count,
            final Predicate<String[]> filter) {
        final BufferedReader br = csvReader instanceof BufferedReader ? (BufferedReader) csvReader : ObjectFactory.createBufferedReader(csvReader);

        try {
            List<String> tmp = N.newArrayList();
            String line = br.readLine();
            jsonParser.readString(tmp, line);
            final String[] titles = tmp.toArray(new String[tmp.size()]);

            final int columnCount = titles.length;
            final Type<?>[] columnTypes = new Type<?>[columnCount];
            final List<String> columnNameList = N.newArrayList(selectColumnNames == null ? columnCount : selectColumnNames.size());
            final List<List<Object>> columnList = N.newArrayList(selectColumnNames == null ? columnCount : selectColumnNames.size());
            final Set<String> selectPropNameSet = selectColumnNames == null ? null : N.newHashSet(selectColumnNames);

            for (int i = 0; i < columnCount; i++) {
                if (selectPropNameSet == null || selectPropNameSet.remove(titles[i])) {
                    columnNameList.add(titles[i]);
                    columnList.add(N.newArrayList());
                    columnTypes[i] = N.getType(String.class);
                }
            }

            if (selectPropNameSet != null && selectPropNameSet.size() > 0) {
                throw new AbacusException(selectPropNameSet + " are not included in titles: " + N.toString(titles));
            }

            final String[] strs = new String[titles.length];

            while (offset-- > 0 && br.readLine() != null) {
            }

            while (count > 0 && (line = br.readLine()) != null) {
                jsonParser.readString(strs, line);

                if (filter != null && filter.test(strs) == false) {
                    continue;
                }

                for (int i = 0, columnIndex = 0; i < columnCount; i++) {
                    if (columnTypes[i] != null) {
                        columnList.get(columnIndex++).add(strs[i]);
                    }
                }

                count--;
            }

            return new RowDataSet(columnNameList, columnList);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            if (br != csvReader) {
                ObjectFactory.recycle(br);
            }
        }
    }

    public static DataSet loadCSV(final Class<?> entityClass, final File csvFile) {
        return loadCSV(entityClass, csvFile, null);
    }

    public static DataSet loadCSV(final Class<?> entityClass, final File csvFile, final Collection<String> selectColumnNames) {
        return loadCSV(entityClass, csvFile, selectColumnNames, 0, Long.MAX_VALUE);
    }

    public static DataSet loadCSV(final Class<?> entityClass, final File csvFile, final Collection<String> selectColumnNames, final long offset,
            final long count) {
        return loadCSV(entityClass, csvFile, selectColumnNames, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param entityClass
     * @param csvFile
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    public static DataSet loadCSV(final Class<?> entityClass, final File csvFile, final Collection<String> selectColumnNames, final long offset,
            final long count, final Predicate<String[]> filter) {
        InputStream csvInputStream = null;

        try {
            csvInputStream = new FileInputStream(csvFile);

            return loadCSV(entityClass, csvInputStream, selectColumnNames, offset, count, filter);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.closeQuietly(csvInputStream);
        }
    }

    public static DataSet loadCSV(final Class<?> entityClass, final InputStream csvInputStream) {
        return loadCSV(entityClass, csvInputStream, null);
    }

    public static DataSet loadCSV(final Class<?> entityClass, final InputStream csvInputStream, final Collection<String> selectColumnNames) {
        return loadCSV(entityClass, csvInputStream, selectColumnNames, 0, Long.MAX_VALUE);
    }

    public static DataSet loadCSV(final Class<?> entityClass, final InputStream csvInputStream, final Collection<String> selectColumnNames, final long offset,
            final long count) {
        return loadCSV(entityClass, csvInputStream, selectColumnNames, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param entityClass
     * @param csvInputStream
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    public static DataSet loadCSV(final Class<?> entityClass, final InputStream csvInputStream, final Collection<String> selectColumnNames, final long offset,
            final long count, final Predicate<String[]> filter) {
        final Reader csvReader = new InputStreamReader(csvInputStream);
        return loadCSV(entityClass, csvReader, selectColumnNames, offset, count, filter);
    }

    public static DataSet loadCSV(final Class<?> entityClass, final Reader csvReader) {
        return loadCSV(entityClass, csvReader, null);
    }

    public static DataSet loadCSV(final Class<?> entityClass, final Reader csvReader, final Collection<String> selectColumnNames) {
        return loadCSV(entityClass, csvReader, selectColumnNames, 0, Long.MAX_VALUE);
    }

    public static DataSet loadCSV(final Class<?> entityClass, final Reader csvReader, final Collection<String> selectColumnNames, long offset, long count) {
        return loadCSV(entityClass, csvReader, selectColumnNames, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param entityClass
     * @param csvReader
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    public static DataSet loadCSV(final Class<?> entityClass, final Reader csvReader, final Collection<String> selectColumnNames, long offset, long count,
            final Predicate<String[]> filter) {
        final BufferedReader br = csvReader instanceof BufferedReader ? (BufferedReader) csvReader : ObjectFactory.createBufferedReader(csvReader);
        final EntityInfo entityInfo = ParserUtil.getEntityInfo(entityClass);

        try {
            List<String> tmp = N.newArrayList();
            String line = br.readLine();
            jsonParser.readString(tmp, line);
            final String[] titles = tmp.toArray(new String[tmp.size()]);

            final int columnCount = titles.length;
            final Type<?>[] columnTypes = new Type<?>[columnCount];
            final List<String> columnNameList = N.newArrayList(selectColumnNames == null ? columnCount : selectColumnNames.size());
            final List<List<Object>> columnList = N.newArrayList(selectColumnNames == null ? columnCount : selectColumnNames.size());
            final Set<String> selectPropNameSet = selectColumnNames == null ? null : N.newHashSet(selectColumnNames);

            for (int i = 0; i < columnCount; i++) {
                if (selectPropNameSet == null || selectPropNameSet.remove(titles[i])) {
                    PropInfo propInfo = entityInfo.getPropInfo(titles[i]);

                    if (propInfo == null && selectPropNameSet != null) {
                        throw new AbacusException(titles[i] + " is not defined in entity class: " + N.getCanonicalClassName(entityClass));
                    }

                    if (propInfo != null) {
                        columnTypes[i] = propInfo.type;
                        columnNameList.add(titles[i]);
                        columnList.add(N.newArrayList());
                    }
                }
            }

            if (selectPropNameSet != null && selectPropNameSet.size() > 0) {
                throw new AbacusException(selectColumnNames + " are not included in titles: " + N.toString(titles));
            }

            final String[] strs = new String[titles.length];

            while (offset-- > 0 && br.readLine() != null) {
            }

            while (count > 0 && (line = br.readLine()) != null) {
                jsonParser.readString(strs, line);

                if (filter != null && filter.test(strs) == false) {
                    continue;
                }

                for (int i = 0, columnIndex = 0; i < columnCount; i++) {
                    if (columnTypes[i] != null) {
                        columnList.get(columnIndex++).add(columnTypes[i].valueOf(strs[i]));
                    }
                }

                count--;
            }

            return new RowDataSet(entityClass, columnNameList, columnList);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            if (br != csvReader) {
                ObjectFactory.recycle(br);
            }
        }
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final File csvFile) {
        return loadCSV(columnTypeMap, csvFile, 0, Long.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final File csvFile, final long offset, final long count) {
        return loadCSV(columnTypeMap, csvFile, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param columnTypeMap
     * @param csvFile
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final File csvFile, final long offset, final long count,
            final Predicate<String[]> filter) {
        InputStream csvInputStream = null;

        try {
            csvInputStream = new FileInputStream(csvFile);

            return loadCSV(columnTypeMap, csvInputStream, offset, count, filter);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.closeQuietly(csvInputStream);
        }
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final InputStream csvInputStream) {
        return loadCSV(columnTypeMap, csvInputStream, 0, Long.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final InputStream csvInputStream, final long offset, final long count) {
        return loadCSV(columnTypeMap, csvInputStream, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param columnTypeMap
     * @param csvInputStream
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final InputStream csvInputStream, final long offset, final long count,
            final Predicate<String[]> filter) {
        final Reader csvReader = new InputStreamReader(csvInputStream);

        return loadCSV(columnTypeMap, csvReader, offset, count, filter);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final Reader csvReader) {
        return loadCSV(columnTypeMap, csvReader, 0, Long.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final Reader csvReader, long offset, long count) {
        return loadCSV(columnTypeMap, csvReader, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param columnTypeMap
     * @param csvReader
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final Map<String, ? extends Type> columnTypeMap, final Reader csvReader, long offset, long count,
            final Predicate<String[]> filter) {
        if (N.isNullOrEmpty(columnTypeMap)) {
            throw new IllegalArgumentException("columnTypeMap can't be null or empty");
        }

        final BufferedReader br = csvReader instanceof BufferedReader ? (BufferedReader) csvReader : ObjectFactory.createBufferedReader(csvReader);

        try {
            List<String> tmp = N.newArrayList();
            String line = br.readLine();
            jsonParser.readString(tmp, line);
            final String[] titles = tmp.toArray(new String[tmp.size()]);

            final int columnCount = titles.length;
            final Type<?>[] columnTypes = new Type<?>[columnCount];
            final List<String> columnNameList = N.newArrayList(columnTypeMap.size());
            final List<List<Object>> columnList = N.newArrayList(columnTypeMap.size());

            for (int i = 0; i < columnCount; i++) {
                if (columnTypeMap.containsKey(titles[i])) {
                    columnTypes[i] = columnTypeMap.get(titles[i]);
                    columnNameList.add(titles[i]);
                    columnList.add(N.newArrayList());
                }
            }

            if (columnNameList.size() != columnTypeMap.size()) {
                final List<String> keys = N.newArrayList(columnTypeMap.keySet());
                keys.removeAll(columnNameList);
                throw new AbacusException(keys + " are not included in titles: " + N.toString(titles));
            }

            final String[] strs = new String[titles.length];

            while (offset-- > 0 && br.readLine() != null) {
            }

            while (count > 0 && (line = br.readLine()) != null) {
                jsonParser.readString(strs, line);

                if (filter != null && filter.test(strs) == false) {
                    continue;
                }

                for (int i = 0, columnIndex = 0; i < columnCount; i++) {
                    if (columnTypes[i] != null) {
                        columnList.get(columnIndex++).add(columnTypes[i].valueOf(strs[i]));
                    }
                }

                count--;
            }

            return new RowDataSet(columnNameList, columnList);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            if (br != csvReader) {
                ObjectFactory.recycle(br);
            }
        }
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final File csvFile) {
        return loadCSV(columnTypeList, csvFile, 0, Long.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final File csvFile, final long offset, final long count) {
        return loadCSV(columnTypeList, csvFile, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param columnTypeList set the column type to null to skip the column in CSV.
     * @param csvFile
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final File csvFile, final long offset, final long count,
            final Predicate<String[]> filter) {
        InputStream csvInputStream = null;

        try {
            csvInputStream = new FileInputStream(csvFile);

            return loadCSV(columnTypeList, csvInputStream, offset, count, filter);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.closeQuietly(csvInputStream);
        }
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final InputStream csvInputStream) {
        return loadCSV(columnTypeList, csvInputStream, 0, Long.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final InputStream csvInputStream, final long offset, final long count) {
        return loadCSV(columnTypeList, csvInputStream, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param columnTypeList set the column type to null to skip the column in CSV.
     * @param csvInputStream
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final InputStream csvInputStream, final long offset, final long count,
            final Predicate<String[]> filter) {
        final Reader csvReader = new InputStreamReader(csvInputStream);

        return loadCSV(columnTypeList, csvReader, offset, count, filter);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final Reader csvReader) {
        return loadCSV(columnTypeList, csvReader, 0, Long.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final Reader csvReader, long offset, long count) {
        return loadCSV(columnTypeList, csvReader, offset, count, null);
    }

    /**
     * Load the data from CSV.
     * 
     * @param columnTypeList set the column type to null to skip the column in CSV.
     * @param csvReader
     * @param offset
     * @param count
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static DataSet loadCSV(final List<? extends Type> columnTypeList, final Reader csvReader, long offset, long count,
            final Predicate<String[]> filter) {
        if (N.isNullOrEmpty(columnTypeList)) {
            throw new IllegalArgumentException("columnTypeList can't be null or empty");
        }

        final BufferedReader br = csvReader instanceof BufferedReader ? (BufferedReader) csvReader : ObjectFactory.createBufferedReader(csvReader);
        final Type<?>[] columnTypes = columnTypeList.toArray(new Type[columnTypeList.size()]);

        try {
            List<String> tmp = N.newArrayList();
            String line = br.readLine();
            jsonParser.readString(tmp, line);
            final String[] titles = tmp.toArray(new String[tmp.size()]);

            final int columnCount = titles.length;
            final List<String> columnNameList = N.newArrayList(columnCount);
            final List<List<Object>> columnList = N.newArrayList();

            for (int i = 0; i < columnCount; i++) {
                if (columnTypes[i] != null) {
                    columnNameList.add(titles[i]);
                    columnList.add(N.newArrayList());
                }
            }

            final String[] strs = new String[titles.length];

            while (offset-- > 0 && br.readLine() != null) {
            }

            while (count > 0 && (line = br.readLine()) != null) {
                jsonParser.readString(strs, line);

                if (filter != null && filter.test(strs) == false) {
                    continue;
                }

                for (int i = 0, columnIndex = 0; i < columnCount; i++) {
                    if (columnTypes[i] != null) {
                        columnList.get(columnIndex++).add(columnTypes[i].valueOf(strs[i]));
                    }
                }

                count--;
            }

            return new RowDataSet(columnNameList, columnList);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            if (br != csvReader) {
                ObjectFactory.recycle(br);
            }
        }
    }

    /**
     * Exports the data from database to CVS. Title will be added at the first line and columns will be quoted.
     * 
     * @param out
     * @param conn
     * @param querySQL
     * @return
     */
    public static long exportCSV(final File out, final Connection conn, final String querySQL) {
        return exportCSV(out, conn, querySQL, 0, Long.MAX_VALUE, true, true);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param conn
     * @param querySQL
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final File out, final Connection conn, final String querySQL, final long offset, final long count, final boolean writeTitle,
            final boolean quoted) {
        return exportCSV(out, conn, querySQL, null, offset, count, writeTitle, quoted);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param conn
     * @param querySQL
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final File out, final Connection conn, final String querySQL, final Collection<String> selectColumnNames, final long offset,
            final long count, final boolean writeTitle, final boolean quoted) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(querySQL, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
            stmt.setFetchSize(200);

            return exportCSV(out, stmt, selectColumnNames, offset, count, writeTitle, quoted);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    /**
     * Exports the data from database to CVS. Title will be added at the first line and columns will be quoted.
     * 
     * @param out
     * @param stmt
     * @return
     */
    @SuppressWarnings("unchecked")
    public static long exportCSV(final File out, final PreparedStatement stmt) {
        return exportCSV(out, stmt, 0, Long.MAX_VALUE, true, true);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param stmt
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final File out, final PreparedStatement stmt, final long offset, final long count, final boolean writeTitle,
            final boolean quoted) {
        return exportCSV(out, stmt, null, offset, count, writeTitle, quoted);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param stmt
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final File out, final PreparedStatement stmt, final Collection<String> selectColumnNames, final long offset, final long count,
            final boolean writeTitle, final boolean quoted) {
        ResultSet rs = null;

        try {
            rs = stmt.executeQuery();
            // rs.setFetchSize(200);

            return exportCSV(out, rs, selectColumnNames, offset, count, writeTitle, quoted);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(rs);
        }
    }

    /**
     * Exports the data from database to CVS. Title will be added at the first line and columns will be quoted.
     * 
     * @param out
     * @param rs
     * @return
     */
    public static long exportCSV(final File out, final ResultSet rs) {
        return exportCSV(out, rs, 0, Long.MAX_VALUE, true, true);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param rs
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final File out, final ResultSet rs, final long offset, final long count, final boolean writeTitle, final boolean quoted) {
        return exportCSV(out, rs, null, offset, count, writeTitle, quoted);
    }

    /**
     * 
     * @param out
     * @param rs
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final File out, final ResultSet rs, final Collection<String> selectColumnNames, final long offset, final long count,
            final boolean writeTitle, final boolean quoted) {
        OutputStream os = null;

        try {
            if (!out.exists()) {
                out.createNewFile();
            }

            os = new FileOutputStream(out);

            long result = exportCSV(os, rs, selectColumnNames, offset, count, writeTitle, quoted);

            os.flush();

            return result;
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.close(os);
        }
    }

    /**
     * Exports the data from database to CVS. Title will be added at the first line and columns will be quoted.
     * 
     * @param out
     * @param rs
     * @return
     */
    public static long exportCSV(final OutputStream out, final ResultSet rs) {
        return exportCSV(out, rs, 0, Long.MAX_VALUE, true, true);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param rs
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final OutputStream out, final ResultSet rs, final long offset, final long count, final boolean writeTitle,
            final boolean quoted) {
        return exportCSV(out, rs, null, offset, count, writeTitle, quoted);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param rs
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final OutputStream out, final ResultSet rs, final Collection<String> selectColumnNames, final long offset, final long count,
            final boolean writeTitle, final boolean quoted) {
        Writer writer = null;

        try {
            writer = new OutputStreamWriter(out);

            long result = exportCSV(writer, rs, selectColumnNames, offset, count, writeTitle, quoted);

            writer.flush();

            return result;
        } catch (IOException e) {
            throw new AbacusIOException(e);
        }
    }

    /**
     * Exports the data from database to CVS. Title will be added at the first line and columns will be quoted.
     * 
     * @param out
     * @param rs
     * @return
     */
    public static long exportCSV(final Writer out, final ResultSet rs) {
        return exportCSV(out, rs, 0, Long.MAX_VALUE, true, true);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param rs
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final Writer out, final ResultSet rs, final long offset, final long count, final boolean writeTitle, final boolean quoted) {
        return exportCSV(out, rs, null, offset, count, writeTitle, quoted);
    }

    /**
     * Exports the data from database to CVS.
     * 
     * @param out
     * @param rs
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param writeTitle
     * @param quoted
     * @return
     */
    public static long exportCSV(final Writer out, final ResultSet rs, final Collection<String> selectColumnNames, long offset, final long count,
            final boolean writeTitle, final boolean quoted) {
        final JSONSerializationConfig config = JSC.create();
        config.setDateTimeFormat(DateTimeFormat.ISO_8601_TIMESTAMP);

        if (quoted) {
            config.setCharQuotation(D._QUOTATION_D);
            config.setStringQuotation(D._QUOTATION_D);
        } else {
            config.setCharQuotation((char) 0);
            config.setStringQuotation((char) 0);
        }

        long result = 0;
        final Type<Object> strType = N.getType(String.class);
        final BufferedJSONWriter bw = out instanceof BufferedJSONWriter ? (BufferedJSONWriter) out : ObjectFactory.createBufferedJSONWriter(out);

        try {
            final ResultSetMetaData metaData = rs.getMetaData();
            final int columnCount = metaData.getColumnCount();
            final String[] columnNames = new String[columnCount];
            final Set<String> columnNameSet = selectColumnNames == null ? null : N.newHashSet(selectColumnNames);
            String label = null;

            for (int i = 0; i < columnCount; i++) {
                label = metaData.getColumnLabel(i + 1);

                if (columnNameSet == null || columnNameSet.remove(label)) {
                    columnNames[i] = label;
                }
            }

            if (columnNameSet != null && columnNameSet.size() > 0) {
                throw new AbacusException(columnNameSet + " are not included in query result");
            }

            if (writeTitle) {
                for (int i = 0, j = 0, len = columnNames.length; i < len; i++) {
                    if (columnNames[i] == null) {
                        continue;
                    }

                    if (j++ > 0) {
                        bw.write(N.ELEMENT_SEPARATOR_CHAR_ARRAY);
                    }

                    bw.write(columnNames[i]);
                }

                bw.write(N.LINE_SEPARATOR);
            }

            final DataChannel dc = new ResultSetChannel(rs);
            final Type<Object>[] typeArray = new Type[columnCount];
            Type<Object> type = null;
            Object value = null;

            while (offset-- > 0 && rs.next()) {
            }

            while (result < count && rs.next()) {
                if (result++ > 0) {
                    bw.write(N.LINE_SEPARATOR);
                }

                for (int i = 0, j = 0; i < columnCount; i++) {
                    if (columnNames[i] == null) {
                        continue;
                    }

                    if (j++ > 0) {
                        bw.write(N.ELEMENT_SEPARATOR_CHAR_ARRAY);
                    }

                    type = typeArray[i];

                    if (type == null) {
                        value = rs.getObject(i + 1);

                        if (value == null) {
                            bw.write(N.NULL_CHAR_ARRAY);
                        } else {
                            type = N.getType(value.getClass());
                            typeArray[i] = type;

                            if (type.isSerializable()) {
                                type.writeCharacters(bw, value, config);
                            } else {
                                type.writeCharacters(bw, jsonParser.serialize(value, config), config);
                            }
                        }
                    } else {
                        if (type.isSerializable()) {
                            type.writeCharacters(bw, type.get(dc, i), config);
                        } else {
                            strType.writeCharacters(bw, jsonParser.serialize(type.get(dc, i), config), config);
                        }
                    }
                }
            }

            bw.flush();
        } catch (SQLException e) {
            throw new AbacusException(e);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            if (bw != out) {
                ObjectFactory.recycle(bw);
            }
        }

        return result;
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final Connection conn, final String insertSQL, final List<? extends Type> columnTypeList) {
        return importCSV(file, 0, Long.MAX_VALUE, true, conn, insertSQL, columnTypeList, 200, 0);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static long importCSV(final File file, final long offset, final long count, final boolean skipTitle, final Connection conn, final String insertSQL,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval) {
        return importCSV(file, offset, count, skipTitle, conn, insertSQL, columnTypeList, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from CSV to database.
     * 
     * @param file
     * @param offset
     * @param count
     * @param skipTitle
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeList set the column type to null to skip the column in CSV.
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final long offset, final long count, final boolean skipTitle, final Connection conn, final String insertSQL,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(insertSQL);

            return importCSV(file, offset, count, skipTitle, stmt, columnTypeList, batchSize, batchInterval, filter);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final PreparedStatement stmt, final List<? extends Type> columnTypeList) {
        return importCSV(file, 0, Long.MAX_VALUE, true, stmt, columnTypeList, 200, 0);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static long importCSV(final File file, long offset, final long count, final boolean skipTitle, final PreparedStatement stmt,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval) {
        return importCSV(file, offset, count, skipTitle, stmt, columnTypeList, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from CSV to database.
     * 
     * @param file
     * @param offset
     * @param count
     * @param skipTitle
     * @param stmt the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeList set the column type to null to skip the column in CSV.
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final long offset, final long count, final boolean skipTitle, final PreparedStatement stmt,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        Reader reader = null;

        try {
            reader = new FileReader(file);

            return importCSV(reader, offset, count, skipTitle, stmt, columnTypeList, batchSize, batchInterval, filter);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.close(reader);
        }
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final InputStream is, final PreparedStatement stmt, final List<? extends Type> columnTypeList) {
        return importCSV(is, 0, Long.MAX_VALUE, true, stmt, columnTypeList, 200, 0);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static long importCSV(final InputStream is, long offset, final long count, final boolean skipTitle, final PreparedStatement stmt,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval) {
        return importCSV(is, offset, count, skipTitle, stmt, columnTypeList, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from CSV to database.
     * 
     * @param is
     * @param offset
     * @param count
     * @param skipTitle
     * @param stmt the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeList set the column type to null to skip the column in CSV.
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static long importCSV(final InputStream is, final long offset, final long count, final boolean skipTitle, final PreparedStatement stmt,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        final Reader reader = new InputStreamReader(is);

        return importCSV(reader, offset, count, skipTitle, stmt, columnTypeList, batchSize, batchInterval, filter);
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final Reader reader, final PreparedStatement stmt, final List<? extends Type> columnTypeList) {
        return importCSV(reader, 0, Long.MAX_VALUE, true, stmt, columnTypeList, 200, 0);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static long importCSV(final Reader reader, long offset, final long count, final boolean skipTitle, final PreparedStatement stmt,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval) {
        return importCSV(reader, offset, count, skipTitle, stmt, columnTypeList, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from CSV to database. 
     * 
     * @param reader
     * @param offset
     * @param count
     * @param skipTitle
     * @param stmt the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeList set the column type to null to skip the column in CSV.
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static long importCSV(final Reader reader, long offset, final long count, final boolean skipTitle, final PreparedStatement stmt,
            final List<? extends Type> columnTypeList, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        long result = 0;
        final BufferedReader br = ObjectFactory.createBufferedReader(reader);

        try {
            if (skipTitle) {
                br.readLine(); // skip the title line.
            }

            while (offset-- > 0 && br.readLine() != null) {
            }

            final boolean isNullOrEmptyTypes = N.isNullOrEmpty(columnTypeList);
            final Type<Object>[] columnTypes = isNullOrEmptyTypes ? null : columnTypeList.toArray(new Type[columnTypeList.size()]);
            final DataChannel dc = isNullOrEmptyTypes ? null : new StatementDataChannel(stmt);
            final String[] strs = new String[columnTypeList.size()];
            String line = null;
            Type<Object> type = null;

            while (result < count && (line = br.readLine()) != null) {
                jsonParser.readString(strs, line);

                if (filter != null && filter.test(strs) == false) {
                    continue;
                }

                if (isNullOrEmptyTypes) {
                    for (int i = 0, len = strs.length; i < len; i++) {
                        stmt.setObject(i + 1, strs[i]);
                    }
                } else {
                    for (int i = 0, parameterIndex = 0, len = strs.length; i < len; i++) {
                        type = columnTypes[i];

                        if (type == null) {
                            continue;
                        }

                        type.set(dc, parameterIndex++, (strs[i] == null) ? null : type.valueOf(strs[i]));
                    }
                }

                stmt.addBatch();

                result++;

                if ((result % batchSize) == 0) {
                    stmt.executeBatch();
                    stmt.clearBatch();

                    if (batchInterval > 0) {
                        N.sleep(batchInterval);
                    }
                }
            }

            if ((result % batchSize) > 0) {
                stmt.executeBatch();
                stmt.clearBatch();
            }
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            ObjectFactory.recycle(br);
        }

        return result;
    }

    /**
     * Imports the data from CSV to database.
     * 
     * @param file
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the CSV file. the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeMap
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final Connection conn, final String insertSQL, final Map<String, ? extends Type> columnTypeMap) {
        return importCSV(file, 0, Long.MAX_VALUE, conn, insertSQL, columnTypeMap, 200, 0);
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final long offset, final long count, final Connection conn, final String insertSQL,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval) {
        return importCSV(file, offset, count, conn, insertSQL, columnTypeMap, batchSize, batchInterval, null);
    }

    /**
     * 
     * @param file
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeMap
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final long offset, final long count, final Connection conn, final String insertSQL,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(insertSQL);

            return importCSV(file, offset, count, stmt, columnTypeMap, batchSize, batchInterval, filter);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final PreparedStatement stmt, final Map<String, ? extends Type> columnTypeMap) {
        return importCSV(file, 0, Long.MAX_VALUE, stmt, columnTypeMap, 200, 0);
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final long offset, final long count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval) {
        return importCSV(file, offset, count, stmt, columnTypeMap, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from CSV to database.
     * 
     * @param file
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeMap
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static long importCSV(final File file, final long offset, final long count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        Reader reader = null;

        try {
            reader = new FileReader(file);

            return importCSV(reader, offset, count, stmt, columnTypeMap, batchSize, batchInterval, filter);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.close(reader);
        }
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final InputStream is, final PreparedStatement stmt, final Map<String, ? extends Type> columnTypeMap) {
        return importCSV(is, 0, Long.MAX_VALUE, stmt, columnTypeMap, 200, 0);
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final InputStream is, final long offset, final long count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval) {
        return importCSV(is, offset, count, stmt, columnTypeMap, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from CSV to database.
     * 
     * @param is
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeMap
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static long importCSV(final InputStream is, long offset, final long count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        final Reader reader = new InputStreamReader(is);
        return importCSV(reader, offset, count, stmt, columnTypeMap, batchSize, batchInterval, filter);
    }

    @SuppressWarnings("rawtypes")
    public static long importCSV(final Reader reader, final PreparedStatement stmt, final Map<String, ? extends Type> columnTypeMap) {
        return importCSV(reader, 0, Long.MAX_VALUE, stmt, columnTypeMap, 200, 0);
    }

    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static long importCSV(final Reader reader, long offset, final long count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval) {
        return importCSV(reader, offset, count, stmt, columnTypeMap, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from CSV to database. 
     * 
     * @param reader
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the CSV file.
     * @param columnTypeMap
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings({ "unchecked", "rawtypes" })
    public static long importCSV(final Reader reader, long offset, final long count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval, final Predicate<String[]> filter) {
        long result = 0;
        final BufferedReader br = ObjectFactory.createBufferedReader(reader);

        try {
            List<String> tmp = N.newArrayList();
            String line = br.readLine();
            jsonParser.readString(tmp, line);
            final String[] titles = tmp.toArray(new String[tmp.size()]);

            final Type<Object>[] columnTypes = new Type[titles.length];
            final List<String> columnNameList = N.newArrayList(columnTypeMap.size());

            for (int i = 0, columnCount = titles.length; i < columnCount; i++) {
                if (columnTypeMap.containsKey(titles[i])) {
                    columnTypes[i] = columnTypeMap.get(titles[i]);
                    columnNameList.add(titles[i]);
                }
            }

            if (columnNameList.size() != columnTypeMap.size()) {
                final List<String> keys = N.newArrayList(columnTypeMap.keySet());
                keys.removeAll(columnNameList);
                throw new AbacusException(keys + " are not included in titles: " + N.toString(titles));
            }

            while (offset-- > 0 && br.readLine() != null) {
            }

            final boolean isNullOrEmptyTypes = N.isNullOrEmpty(columnTypes);
            final DataChannel dc = isNullOrEmptyTypes ? null : new StatementDataChannel(stmt);
            final String[] strs = new String[titles.length];
            Type<Object> type = null;

            while (result < count && (line = br.readLine()) != null) {
                jsonParser.readString(strs, line);

                if (filter != null && filter.test(strs) == false) {
                    continue;
                }

                if (isNullOrEmptyTypes) {
                    for (int i = 0, len = strs.length; i < len; i++) {
                        stmt.setObject(i + 1, strs[i]);
                    }
                } else {
                    for (int i = 0, parameterIndex = 0, len = strs.length; i < len; i++) {
                        type = columnTypes[i];

                        if (type == null) {
                            continue;
                        }

                        type.set(dc, parameterIndex++, (strs[i] == null) ? null : type.valueOf(strs[i]));
                    }
                }

                stmt.addBatch();

                result++;

                if ((result % batchSize) == 0) {
                    stmt.executeBatch();
                    stmt.clearBatch();

                    if (batchInterval > 0) {
                        N.sleep(batchInterval);
                    }
                }
            }

            if ((result % batchSize) > 0) {
                stmt.executeBatch();
                stmt.clearBatch();
            }
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            ObjectFactory.recycle(br);
        }

        return result;
    }
}
