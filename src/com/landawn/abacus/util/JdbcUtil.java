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

import static com.landawn.abacus.core.AbacusConfiguration.DataSourceConfiguration.DRIVER;
import static com.landawn.abacus.core.AbacusConfiguration.DataSourceConfiguration.PASSWORD;
import static com.landawn.abacus.core.AbacusConfiguration.DataSourceConfiguration.URL;
import static com.landawn.abacus.core.AbacusConfiguration.DataSourceConfiguration.USER;
import static com.landawn.abacus.util.IOUtil.DEFAULT_QUEUE_SIZE_FOR_ROW_PARSER;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.Reader;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.SQLType;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;

import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.DataSource;
import com.landawn.abacus.DataSourceManager;
import com.landawn.abacus.DataSourceSelector;
import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.SliceSelector;
import com.landawn.abacus.core.AbacusConfiguration;
import com.landawn.abacus.core.AbacusConfiguration.DataSourceConfiguration;
import com.landawn.abacus.core.AbacusConfiguration.DataSourceManagerConfiguration;
import com.landawn.abacus.core.RowDataSet;
import com.landawn.abacus.core.sql.dataSource.NonSliceSelector;
import com.landawn.abacus.core.sql.dataSource.SQLDataSource;
import com.landawn.abacus.core.sql.dataSource.SQLDataSourceManager;
import com.landawn.abacus.core.sql.dataSource.SimpleSourceSelector;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.NonUniqueResultException;
import com.landawn.abacus.exception.ParseException;
import com.landawn.abacus.exception.UncheckedIOException;
import com.landawn.abacus.exception.UncheckedSQLException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.SQLExecutor.JdbcSettings;
import com.landawn.abacus.util.stream.Stream;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="http://docs.oracle.com/javase/7/docs/api/java/sql/Connection.html">http://docs.oracle.com/javase/7/docs/api/java/sql/Connection.html</a>
 * @see <a href="http://docs.oracle.com/javase/7/docs/api/java/sql/Statement.html">http://docs.oracle.com/javase/7/docs/api/java/sql/Statement.html</a>
 * @see <a href="http://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html">http://docs.oracle.com/javase/7/docs/api/java/sql/PreparedStatement.html</a>
 * @see <a href="http://docs.oracle.com/javase/7/docs/api/java/sql/ResultSet.html">http://docs.oracle.com/javase/7/docs/api/java/sql/ResultSet.html</a>
 */
public final class JdbcUtil {
    private static final Logger logger = LoggerFactory.getLogger(JdbcUtil.class);

    // ...
    private static final String CURRENT_DIR_PATH = "./";

    private static final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> DEFAULT_STMT_SETTER = new Try.BiConsumer<PreparedStatement, Object[], SQLException>() {
        @Override
        public void accept(PreparedStatement stmt, Object[] parameters) throws SQLException {
            for (int i = 0, len = parameters.length; i < len; i++) {
                stmt.setObject(i + 1, parameters[i]);
            }
        }
    };

    private static final Set<String> sqlStateForTableNotExists = new HashSet<>();

    static {
        sqlStateForTableNotExists.add("42S02"); // for MySQL.
        sqlStateForTableNotExists.add("42P01"); // for PostgreSQL.
        sqlStateForTableNotExists.add("42501"); // for HSQLDB.
    }

    private JdbcUtil() {
        // singleton
    }

    public static DBVersion getDBVersion(final Connection conn) throws UncheckedSQLException {
        try {
            String dbProudctName = conn.getMetaData().getDatabaseProductName();
            String dbProudctVersion = conn.getMetaData().getDatabaseProductVersion();

            DBVersion dbVersion = DBVersion.OTHERS;

            String upperCaseProductName = dbProudctName.toUpperCase();
            if (upperCaseProductName.contains("H2")) {
                dbVersion = DBVersion.H2;
            } else if (upperCaseProductName.contains("HSQL")) {
                dbVersion = DBVersion.HSQLDB;
            } else if (upperCaseProductName.contains("MYSQL")) {
                if (dbProudctVersion.startsWith("5.5")) {
                    dbVersion = DBVersion.MYSQL_5_5;
                } else if (dbProudctVersion.startsWith("5.6")) {
                    dbVersion = DBVersion.MYSQL_5_6;
                } else if (dbProudctVersion.startsWith("5.7")) {
                    dbVersion = DBVersion.MYSQL_5_7;
                } else if (dbProudctVersion.startsWith("5.8")) {
                    dbVersion = DBVersion.MYSQL_5_8;
                } else if (dbProudctVersion.startsWith("5.9")) {
                    dbVersion = DBVersion.MYSQL_5_9;
                } else if (dbProudctVersion.startsWith("6")) {
                    dbVersion = DBVersion.MYSQL_6;
                } else if (dbProudctVersion.startsWith("7")) {
                    dbVersion = DBVersion.MYSQL_7;
                } else {
                    dbVersion = DBVersion.MYSQL_OTHERS;
                }
            } else if (upperCaseProductName.contains("POSTGRESQL")) {
                if (dbProudctVersion.startsWith("9.2")) {
                    dbVersion = DBVersion.POSTGRESQL_9_2;
                } else if (dbProudctVersion.startsWith("9.3")) {
                    dbVersion = DBVersion.POSTGRESQL_9_3;
                } else if (dbProudctVersion.startsWith("9.4")) {
                    dbVersion = DBVersion.POSTGRESQL_9_4;
                } else if (dbProudctVersion.startsWith("9.5")) {
                    dbVersion = DBVersion.POSTGRESQL_9_5;
                } else if (dbProudctVersion.startsWith("10")) {
                    dbVersion = DBVersion.POSTGRESQL_10;
                } else {
                    dbVersion = DBVersion.POSTGRESQL_OTHERS;
                }
            } else if (upperCaseProductName.contains("ORACLE")) {
                dbVersion = DBVersion.ORACLE;
            } else if (upperCaseProductName.contains("DB2")) {
                dbVersion = DBVersion.DB2;
            } else if (upperCaseProductName.contains("SQL SERVER")) {
                dbVersion = DBVersion.SQL_SERVER;
            }

            return dbVersion;
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    /**
     * 
     * @param dataSourceXmlFile
     * @return DataSourceManager
     * 
     * @see DataSource.xsd
     */
    public static DataSourceManager createDataSourceManager(final String dataSourceXmlFile) throws UncheckedIOException, UncheckedSQLException {
        InputStream is = null;
        try {
            is = new FileInputStream(Configuration.findFile(dataSourceXmlFile));
            return createDataSourceManager(is, dataSourceXmlFile);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(is);
        }

    }

    /**
     * 
     * @param dataSourceXmlInputStream
     * @return DataSourceManager
     * 
     * @see DataSource.xsd
     */
    public static DataSourceManager createDataSourceManager(final InputStream dataSourceXmlInputStream) throws UncheckedIOException, UncheckedSQLException {
        return createDataSourceManager(dataSourceXmlInputStream, CURRENT_DIR_PATH);
    }

    private static DataSourceManager createDataSourceManager(final InputStream dataSourceXmlInputStream, final String dataSourceXmlFile)
            throws UncheckedIOException, UncheckedSQLException {
        DocumentBuilder domParser = XMLUtil.createDOMParser();
        Document doc = null;

        try {
            doc = domParser.parse(dataSourceXmlInputStream);

            Element rootElement = doc.getDocumentElement();

            final Map<String, String> props = new HashMap<>();
            List<Element> propertiesElementList = XMLUtil.getElementsByTagName(rootElement, AbacusConfiguration.PROPERTIES);

            if (N.notNullOrEmpty(propertiesElementList)) {
                for (Element propertiesElement : propertiesElementList) {
                    File resourcePropertiesFile = Configuration.findFileByFile(new File(dataSourceXmlFile),
                            propertiesElement.getAttribute(AbacusConfiguration.RESOURCE));
                    java.util.Properties properties = new java.util.Properties();
                    InputStream is = null;

                    try {
                        is = new FileInputStream(resourcePropertiesFile);

                        if (resourcePropertiesFile.getName().endsWith(".xml")) {
                            properties.loadFromXML(is);
                        } else {
                            properties.load(is);
                        }
                    } finally {
                        IOUtil.close(is);
                    }

                    for (Object key : properties.keySet()) {
                        props.put((String) key, (String) properties.get(key));
                    }
                }
            }

            String nodeName = rootElement.getNodeName();
            if (nodeName.equals(DataSourceManagerConfiguration.DATA_SOURCE_MANAGER)) {
                DataSourceManagerConfiguration config = new DataSourceManagerConfiguration(rootElement, props);
                return new SQLDataSourceManager(config);
            } else if (nodeName.equals(DataSourceConfiguration.DATA_SOURCE)) {
                DataSourceConfiguration config = new DataSourceConfiguration(rootElement, props);
                return new SimpleDataSourceManager(new SQLDataSource(config));
            } else {
                throw new AbacusException("Unknown xml format with root element: " + nodeName);
            }
        } catch (SAXException e) {
            throw new ParseException(e);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public static DataSource createDataSource(final String dataSourceFile) throws UncheckedIOException, UncheckedSQLException {
        InputStream is = null;
        try {
            is = new FileInputStream(Configuration.findFile(dataSourceFile));
            return createDataSource(is, dataSourceFile);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(is);
        }
    }

    public static DataSource createDataSource(final InputStream dataSourceInputStream) throws UncheckedIOException, UncheckedSQLException {
        return createDataSource(dataSourceInputStream, CURRENT_DIR_PATH);
    }

    private static DataSource createDataSource(final InputStream dataSourceInputStream, final String dataSourceFile)
            throws UncheckedIOException, UncheckedSQLException {
        final String dataSourceString = IOUtil.readString(dataSourceInputStream);

        if (CURRENT_DIR_PATH.equals(dataSourceFile) || dataSourceFile.endsWith(".xml")) {
            try {
                return createDataSourceManager(new ByteArrayInputStream(dataSourceString.getBytes())).getPrimaryDataSource();
            } catch (ParseException e) {
                // ignore.
            } catch (UncheckedIOException e) {
                // ignore.
            }
        }

        final Map<String, String> newProps = new HashMap<>();
        final java.util.Properties properties = new java.util.Properties();

        try {
            properties.load(new ByteArrayInputStream(dataSourceString.getBytes()));

            Object value = null;

            for (Object key : properties.keySet()) {
                value = properties.get(key);
                newProps.put(key.toString().trim(), value.toString().trim());
            }

        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        return new SQLDataSource(newProps);
    }

    public static DataSource createDataSource(final String url, final String user, final String password) throws UncheckedSQLException {
        return createDataSource(getDriverClasssByUrl(url), url, user, password);
    }

    public static DataSource createDataSource(final String driver, final String url, final String user, final String password) throws UncheckedSQLException {
        final Class<? extends Driver> driverClass = ClassUtil.forClass(driver);

        return createDataSource(driverClass, url, user, password);
    }

    public static DataSource createDataSource(final Class<? extends Driver> driverClass, final String url, final String user, final String password)
            throws UncheckedSQLException {
        final Map<String, Object> props = new HashMap<>();

        props.put(DRIVER, driverClass.getCanonicalName());
        props.put(URL, url);
        props.put(USER, user);
        props.put(PASSWORD, password);

        return createDataSource(props);
    }

    /**
     * 
     * @param props refer to Connection.xsd for the supported properties.
     * @return
     */
    public static DataSource createDataSource(final Map<String, ?> props) throws UncheckedSQLException {
        final String driver = (String) props.get(DRIVER);

        if (N.isNullOrEmpty(driver)) {
            final String url = (String) props.get(URL);

            if (N.isNullOrEmpty(url)) {
                throw new IllegalArgumentException("Url is not specified");
            }

            final Map<String, Object> tmp = new HashMap<>(props);

            tmp.put(DRIVER, getDriverClasssByUrl(url).getCanonicalName());

            return new SQLDataSource(tmp);
        } else {
            return new SQLDataSource(props);
        }
    }

    public static DataSource wrap(final javax.sql.DataSource sqlDataSource) {
        return sqlDataSource instanceof DataSource ? ((DataSource) sqlDataSource) : new SimpleDataSource(sqlDataSource);
    }

    public static Connection createConnection(final String url, final String user, final String password) throws UncheckedSQLException {
        return createConnection(getDriverClasssByUrl(url), url, user, password);
    }

    private static Class<? extends Driver> getDriverClasssByUrl(final String url) {
        Class<? extends Driver> driverClass = null;
        // jdbc:mysql://localhost:3306/abacustest
        if (url.indexOf("mysql") > 0 || StringUtil.indexOfIgnoreCase(url, "mysql") > 0) {
            driverClass = ClassUtil.forClass("com.mysql.jdbc.Driver");
            // jdbc:postgresql://localhost:5432/abacustest
        } else if (url.indexOf("postgresql") > 0 || StringUtil.indexOfIgnoreCase(url, "postgresql") > 0) {
            driverClass = ClassUtil.forClass("org.postgresql.Driver");
            // jdbc:h2:hsql://<host>:<port>/<database>
        } else if (url.indexOf("h2") > 0 || StringUtil.indexOfIgnoreCase(url, "h2") > 0) {
            driverClass = ClassUtil.forClass("org.h2.Driver");
            // jdbc:hsqldb:hsql://localhost/abacustest
        } else if (url.indexOf("hsqldb") > 0 || StringUtil.indexOfIgnoreCase(url, "hsqldb") > 0) {
            driverClass = ClassUtil.forClass("org.hsqldb.jdbc.JDBCDriver");
            // jdbc.url=jdbc:oracle:thin:@localhost:1521:abacustest
        } else if (url.indexOf("oracle") > 0 || StringUtil.indexOfIgnoreCase(url, "oracle") > 0) {
            driverClass = ClassUtil.forClass("oracle.jdbc.driver.OracleDriver");
            // jdbc.url=jdbc:sqlserver://localhost:1433;Database=abacustest
        } else if (url.indexOf("sqlserver") > 0 || StringUtil.indexOfIgnoreCase(url, "sqlserver") > 0) {
            driverClass = ClassUtil.forClass("com.microsoft.sqlserver.jdbc.SQLServerDrive");
            // jdbc:db2://localhost:50000/abacustest
        } else if (url.indexOf("db2") > 0 || StringUtil.indexOfIgnoreCase(url, "db2") > 0) {
            driverClass = ClassUtil.forClass("com.ibm.db2.jcc.DB2Driver");
        } else {
            throw new UncheckedSQLException(
                    "Can not identity the driver class by url: " + url + ". Only mysql, postgresql, hsqldb, sqlserver, oracle and db2 are supported currently");
        }
        return driverClass;
    }

    public static Connection createConnection(final String driverClass, final String url, final String user, final String password)
            throws UncheckedSQLException {
        Class<? extends Driver> cls = ClassUtil.forClass(driverClass);
        return createConnection(cls, url, user, password);
    }

    public static Connection createConnection(final Class<? extends Driver> driverClass, final String url, final String user, final String password)
            throws UncheckedSQLException {
        try {
            DriverManager.registerDriver(N.newInstance(driverClass));

            return DriverManager.getConnection(url, user, password);
        } catch (SQLException e) {
            throw new UncheckedSQLException("Failed to close create connection", e);
        }
    }

    public static void close(final ResultSet rs) throws UncheckedSQLException {
        if (rs != null) {
            try {
                rs.close();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }
    }

    public static void close(final ResultSet rs, final boolean closeStatement) throws UncheckedSQLException {
        close(rs, closeStatement, false);
    }

    /**
     * 
     * @param rs
     * @param closeStatement 
     * @param closeConnection
     * @throws IllegalArgumentException if {@code closeStatement = false} while {@code closeConnection = true}.
     * @throws UncheckedSQLException
     */
    public static void close(final ResultSet rs, final boolean closeStatement, final boolean closeConnection)
            throws IllegalArgumentException, UncheckedSQLException {
        if (closeConnection && closeStatement == false) {
            throw new IllegalArgumentException("'closeStatement' can't be false while 'closeConnection' is true");
        }

        if (rs == null) {
            return;
        }

        Connection conn = null;
        Statement stmt = null;

        try {
            if (closeStatement || closeConnection) {
                stmt = rs.getStatement();
            }

            if (closeConnection && stmt != null) {
                conn = stmt.getConnection();
            }
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            close(rs, stmt, conn);
        }
    }

    public static void close(final Statement stmt) throws UncheckedSQLException {
        if (stmt != null) {
            try {
                if (stmt instanceof PreparedStatement) {
                    try {
                        ((PreparedStatement) stmt).clearParameters();
                    } catch (SQLException e) {
                        logger.error("Failed to clear parameters", e);
                    }
                }

                stmt.close();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }
    }

    public static void close(final Connection conn) throws UncheckedSQLException {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }
    }

    public static void close(final ResultSet rs, final Statement stmt) throws UncheckedSQLException {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            try {
                if (stmt != null) {
                    if (stmt instanceof PreparedStatement) {
                        try {
                            ((PreparedStatement) stmt).clearParameters();
                        } catch (SQLException e) {
                            logger.error("Failed to clear parameters", e);
                        }
                    }
                    stmt.close();
                }
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }
    }

    public static void close(final Statement stmt, final Connection conn) throws UncheckedSQLException {
        try {
            if (stmt != null) {
                if (stmt instanceof PreparedStatement) {
                    try {
                        ((PreparedStatement) stmt).clearParameters();
                    } catch (SQLException e) {
                        logger.error("Failed to clear parameters", e);
                    }
                }
                stmt.close();
            }
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            try {
                if (conn != null) {
                    conn.close();
                }
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }
    }

    public static void close(final ResultSet rs, final Statement stmt, final Connection conn) throws UncheckedSQLException {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            try {
                if (stmt != null) {
                    if (stmt instanceof PreparedStatement) {
                        try {
                            ((PreparedStatement) stmt).clearParameters();
                        } catch (SQLException e) {
                            logger.error("Failed to clear parameters", e);
                        }
                    }
                    stmt.close();
                }
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            } finally {
                try {
                    if (conn != null) {
                        conn.close();
                    }
                } catch (SQLException e) {
                    throw new UncheckedSQLException(e);
                }
            }
        }
    }

    /**
     * Unconditionally close an <code>ResultSet</code>.
     * <p>
     * Equivalent to {@link ResultSet#close()}, except any exceptions will be ignored.
     * This is typically used in finally blocks.
     * 
     * @param rs
     */
    public static void closeQuietly(final ResultSet rs) {
        closeQuietly(rs, null, null);
    }

    public static void closeQuietly(final ResultSet rs, final boolean closeStatement) throws UncheckedSQLException {
        closeQuietly(rs, closeStatement, false);
    }

    /**
     * 
     * @param rs
     * @param closeStatement 
     * @param closeConnection
     * @throws IllegalArgumentException if {@code closeStatement = false} while {@code closeConnection = true}. 
     */
    public static void closeQuietly(final ResultSet rs, final boolean closeStatement, final boolean closeConnection) throws IllegalArgumentException {
        if (closeConnection && closeStatement == false) {
            throw new IllegalArgumentException("'closeStatement' can't be false while 'closeConnection' is true");
        }

        if (rs == null) {
            return;
        }

        Connection conn = null;
        Statement stmt = null;

        try {
            if (closeStatement || closeConnection) {
                stmt = rs.getStatement();
            }

            if (closeConnection && stmt != null) {
                conn = stmt.getConnection();
            }
        } catch (SQLException e) {
            logger.error("Failed to get Statement or Connection by ResultSet", e);
        } finally {
            closeQuietly(rs, stmt, conn);
        }
    }

    /**
     * Unconditionally close an <code>Statement</code>.
     * <p>
     * Equivalent to {@link Statement#close()}, except any exceptions will be ignored.
     * This is typically used in finally blocks.
     * 
     * @param stmt
     */
    public static void closeQuietly(final Statement stmt) {
        closeQuietly(null, stmt, null);
    }

    /**
     * Unconditionally close an <code>Connection</code>.
     * <p>
     * Equivalent to {@link Connection#close()}, except any exceptions will be ignored.
     * This is typically used in finally blocks.
     * 
     * @param conn
     */
    public static void closeQuietly(final Connection conn) {
        closeQuietly(null, null, conn);
    }

    /**
     * Unconditionally close the <code>ResultSet, Statement</code>.
     * <p>
     * Equivalent to {@link ResultSet#close()}, {@link Statement#close()}, except any exceptions will be ignored.
     * This is typically used in finally blocks.
     * 
     * @param rs
     * @param stmt
     */
    public static void closeQuietly(final ResultSet rs, final Statement stmt) {
        closeQuietly(rs, stmt, null);
    }

    /**
     * Unconditionally close the <code>Statement, Connection</code>.
     * <p>
     * Equivalent to {@link Statement#close()}, {@link Connection#close()}, except any exceptions will be ignored.
     * This is typically used in finally blocks.
     * 
     * @param stmt
     * @param conn
     */
    public static void closeQuietly(final Statement stmt, final Connection conn) {
        closeQuietly(null, stmt, conn);
    }

    /**
     * Unconditionally close the <code>ResultSet, Statement, Connection</code>.
     * <p>
     * Equivalent to {@link ResultSet#close()}, {@link Statement#close()}, {@link Connection#close()}, except any exceptions will be ignored.
     * This is typically used in finally blocks.
     * 
     * @param rs
     * @param stmt
     * @param conn
     */
    public static void closeQuietly(final ResultSet rs, final Statement stmt, final Connection conn) {
        if (rs != null) {
            try {
                rs.close();
            } catch (Exception e) {
                logger.error("Failed to close ResultSet", e);
            }
        }

        if (stmt != null) {
            if (stmt instanceof PreparedStatement) {
                try {
                    ((PreparedStatement) stmt).clearParameters();
                } catch (Exception e) {
                    logger.error("Failed to clear parameters", e);
                }
            }

            try {
                stmt.close();
            } catch (Exception e) {
                logger.error("Failed to close Statement", e);
            }
        }

        if (conn != null) {
            try {
                conn.close();
            } catch (Exception e) {
                logger.error("Failed to close Connection", e);
            }
        }
    }

    /**
     * 
     * @param stmt will be closed after any execution methods in {@code PreparedQuery/PreparedCallableQuery} is called.
     * An execution method is a method which doesn't return the instance of {@code PreparedQuery/PreparedCallableQuery}.
     * @return
     * @throws SQLException
     */
    public static PreparedQuery prepareQuery(final PreparedStatement stmt) throws SQLException {
        return new PreparedQuery(stmt);
    }

    /**
     * Never write below code because it will definitely cause {@code Connection} leak:
     * <pre>
     * <code>
     * JdbcUtil.prepareQuery(dataSource.getConnection(), sql);
     * </code>
     * </pre>
     * 
     * @param conn the specified {@code conn} won't be close after this query is executed.
     * @param sql
     * @return
     * @throws SQLException
     */
    public static PreparedQuery prepareQuery(final Connection conn, final String sql) throws SQLException {
        return new PreparedQuery(prepareStatement(conn, sql));
    }

    /** 
     * Never write below code because it will definitely cause {@code Connection} leak:
     * <pre>
     * <code>
     * JdbcUtil.prepareQuery(dataSource.getConnection(), sql);
     * </code>
     * </pre>
     * 
     * @param conn the specified {@code conn} won't be close after this query is executed.
     * @param sql
     * @param autoGeneratedKeys
     * @return
     * @throws SQLException
     */
    public static PreparedQuery prepareQuery(final Connection conn, final String sql, final boolean autoGeneratedKeys) throws SQLException {
        return new PreparedQuery(conn.prepareStatement(sql, autoGeneratedKeys ? Statement.RETURN_GENERATED_KEYS : Statement.NO_GENERATED_KEYS));
    }

    public static PreparedQuery prepareQuery(final javax.sql.DataSource ds, final String sql) throws SQLException {
        final Connection conn = ds.getConnection();

        return prepareQuery(conn, sql).onClose(conn);
    }

    public static PreparedQuery prepareQuery(final javax.sql.DataSource ds, final String sql, final boolean autoGeneratedKeys) throws SQLException {
        final Connection conn = ds.getConnection();

        return prepareQuery(conn, sql, autoGeneratedKeys).onClose(conn);
    }

    /**
     * 
     * @param stmt will be closed after any execution methods in {@code PreparedQuery/PreparedCallableQuery} is called.
     * An execution method is a method which doesn't return the instance of {@code PreparedQuery/PreparedCallableQuery}.
     * @return
     * @throws SQLException
     */
    public static PreparedCallableQuery prepareCallableQuery(final CallableStatement stmt) throws SQLException {
        return new PreparedCallableQuery(stmt);
    }

    /**
     * Never write below code because it will definitely cause {@code Connection} leak:
     * <pre>
     * <code>
     * JdbcUtil.prepareCallableQuery(dataSource.getConnection(), sql);
     * </code>
     * </pre>
     * 
     * @param conn the specified {@code conn} won't be close after this query is executed.
     * @param sql
     * @return
     * @throws SQLException
     */
    public static PreparedCallableQuery prepareCallableQuery(final Connection conn, final String sql) throws SQLException {
        return new PreparedCallableQuery(prepareCall(conn, sql));
    }

    public static PreparedCallableQuery prepareCallableQuery(final javax.sql.DataSource ds, final String sql) throws SQLException {
        final Connection conn = ds.getConnection();

        return prepareCallableQuery(conn, sql).onClose(conn);
    }

    @SafeVarargs
    public static PreparedStatement prepareStatement(final Connection conn, final String sql, final Object... parameters) throws SQLException {
        final NamedSQL namedSQL = NamedSQL.parse(sql);
        final PreparedStatement stmt = conn.prepareStatement(namedSQL.getPureSQL());

        if (N.notNullOrEmpty(parameters)) {
            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
        }

        return stmt;
    }

    @SafeVarargs
    public static CallableStatement prepareCall(final Connection conn, final String sql, final Object... parameters) throws SQLException {
        final NamedSQL namedSQL = NamedSQL.parse(sql);
        final CallableStatement stmt = conn.prepareCall(namedSQL.getPureSQL());

        if (N.notNullOrEmpty(parameters)) {
            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
        }

        return stmt;
    }

    public static PreparedStatement batchPrepareStatement(final Connection conn, final String sql, final List<?> parametersList) throws SQLException {
        final NamedSQL namedSQL = NamedSQL.parse(sql);
        final PreparedStatement stmt = conn.prepareStatement(namedSQL.getPureSQL());

        for (Object parameters : parametersList) {
            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
            stmt.addBatch();
        }

        return stmt;
    }

    public static CallableStatement batchPrepareCall(final Connection conn, final String sql, final List<?> parametersList) throws SQLException {
        final NamedSQL namedSQL = NamedSQL.parse(sql);
        final CallableStatement stmt = conn.prepareCall(namedSQL.getPureSQL());

        for (Object parameters : parametersList) {
            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
            stmt.addBatch();
        }

        return stmt;
    }

    @SafeVarargs
    public static DataSet executeQuery(final Connection conn, final String sql, final Object... parameters) throws UncheckedSQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = prepareStatement(conn, sql, parameters);
            rs = stmt.executeQuery();

            return extractData(rs);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(rs, stmt);
        }
    }

    public static DataSet executeQuery(final PreparedStatement stmt) throws UncheckedSQLException {
        ResultSet rs = null;

        try {
            rs = stmt.executeQuery();

            return extractData(rs);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(rs);
        }
    }

    @SafeVarargs
    public static int executeUpdate(final Connection conn, final String sql, final Object... parameters) throws UncheckedSQLException {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, sql, parameters);

            return stmt.executeUpdate();
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    public static int executeUpdate(final PreparedStatement stmt) throws UncheckedSQLException {
        try {
            return stmt.executeUpdate();
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    /**
     * 
     * @param conn
     * @param sql
     * @param parametersListList
     * @return
     * @throws UncheckedSQLException
     */
    public static int executeBatchUpdate(final Connection conn, final String sql, final List<?> parametersListList) throws UncheckedSQLException {
        return executeBatchUpdate(conn, sql, parametersListList, JdbcSettings.DEFAULT_BATCH_SIZE);
    }

    /**
     * 
     * @param conn
     * @param sql
     * @param parametersListList
     * @param batchSize.
     * @return 
     * @throws UncheckedSQLException
     */
    public static int executeBatchUpdate(final Connection conn, final String sql, final List<?> parametersListList, final int batchSize)
            throws UncheckedSQLException {
        N.checkArgNotNull(conn);
        N.checkArgNotNull(sql);
        N.checkArgument(batchSize > 0, "'batchSize' can't be 0 or negative");

        if (N.isNullOrEmpty(parametersListList)) {
            return 0;
        }

        final NamedSQL namedSQL = NamedSQL.parse(sql);
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(namedSQL.getPureSQL());

            int res = 0;
            int idx = 0;

            for (Object parameters : parametersListList) {
                SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
                stmt.addBatch();

                if (++idx % batchSize == 0) {
                    res += stmt.executeUpdate();
                    stmt.clearBatch();
                }
            }

            if (idx % batchSize != 0) {
                res += stmt.executeUpdate();
                stmt.clearBatch();
            }

            return res;
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.close(stmt);
        }
    }

    @SafeVarargs
    public static boolean execute(final Connection conn, final String sql, final Object... parameters) throws UncheckedSQLException {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, sql, parameters);

            return stmt.execute();
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    public static boolean execute(final PreparedStatement stmt) throws UncheckedSQLException {
        try {
            return stmt.execute();
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    /**
     * 
     * @param rs
     * @param n the count of row to move ahead.
     * @throws UncheckedSQLException
     */
    public static void skip(final ResultSet rs, int n) throws UncheckedSQLException {
        skip(rs, (long) n);
    }

    /**
     * 
     * @param rs
     * @param n the count of row to move ahead.
     * @throws UncheckedSQLException
     * @see {@link ResultSet#absolute(int)}
     */
    public static void skip(final ResultSet rs, long n) throws UncheckedSQLException {
        //    N.checkArgument(offset >= 0, "'offset' can't be negative: %s", offset);
        //
        //    if (offset == 0) {
        //        return;
        //    }

        if (n <= 0) {
            return;
        } else if (n == 1) {
            try {
                rs.next();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        } else if (n <= Integer.MAX_VALUE) {
            try {
                if (n > Integer.MAX_VALUE - rs.getRow()) {
                    try {
                        while (n-- > 0L && rs.next()) {
                        }
                    } catch (SQLException e) {
                        throw new UncheckedSQLException(e);
                    }
                } else {
                    rs.absolute((int) n + rs.getRow());
                }
            } catch (SQLException e) {
                try {
                    while (n-- > 0L && rs.next()) {
                    }
                } catch (SQLException e1) {
                    throw new UncheckedSQLException(e);
                }
            }
        } else {
            try {
                while (n-- > 0L && rs.next()) {
                }
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }
    }

    /**
     * 
     * @param rs
     * @throws UncheckedSQLException
     */
    public static RowIterator iterate(final ResultSet rs) throws UncheckedSQLException {
        return new RowIterator(rs, false, false);
    }

    /**
     * 
     * @param rs
     * @return
     * @throws UncheckedSQLException
     */
    public static DataSet extractData(final ResultSet rs) throws UncheckedSQLException {
        return extractData(rs, false);
    }

    public static DataSet extractData(final ResultSet rs, final boolean closeResultSet) throws UncheckedSQLException {
        return extractData(rs, 0, Integer.MAX_VALUE, closeResultSet);
    }

    public static DataSet extractData(final ResultSet rs, final int offset, final int count) throws UncheckedSQLException {
        return extractData(rs, offset, count, false);
    }

    public static DataSet extractData(final ResultSet rs, final int offset, final int count, final boolean closeResultSet) throws UncheckedSQLException {
        return extractData(rs, offset, count, Fn.alwaysTrue(), closeResultSet);
    }

    public static <E extends Exception> DataSet extractData(final ResultSet rs, int offset, int count, final Try.Predicate<? super Object[], E> filter,
            final boolean closeResultSet) throws UncheckedSQLException, E {
        try {
            // TODO [performance improvement]. it will improve performance a lot if MetaData is cached.
            final ResultSetMetaData metaData = rs.getMetaData();
            final int columnCount = metaData.getColumnCount();
            final List<String> columnNameList = new ArrayList<>(columnCount);
            final List<List<Object>> columnList = new ArrayList<>(columnCount);

            for (int i = 0; i < columnCount;) {
                columnNameList.add(metaData.getColumnLabel(++i));
                columnList.add(new ArrayList<>());
            }

            JdbcUtil.skip(rs, offset);

            final Object[] row = new Object[columnCount];

            while (count > 0 && rs.next()) {
                for (int i = 0; i < columnCount;) {
                    row[i] = rs.getObject(++i);
                }

                if (filter == null || filter.test(row)) {
                    for (int i = 0; i < columnCount; i++) {
                        columnList.get(i).add(row[i]);
                    }

                    count--;
                }
            }

            // return new RowDataSet(null, entityClass, columnNameList, columnList);
            return new RowDataSet(columnNameList, columnList);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            if (closeResultSet) {
                closeQuietly(rs);
            }
        }
    }

    /**
     * Don’t close the specified Connection before the returned {@code Stream} is consumed/collected/terminated.
     * 
     * @param conn
     * @param sql
     * @param parameters
     * @return
     * @throws UncheckedSQLException
     */
    @SafeVarargs
    public static Try<Stream<Object[]>> stream(final Connection conn, final String sql, final Object... parameters) throws UncheckedSQLException {
        return stream2(null, conn, sql, parameters);
    }

    /**
     * Don’t close the specified Connection before the returned {@code Stream} is consumed/collected/terminated.
     * 
     * @param targetType {@code Map} or {@code Entity} class with getter/setter methods.
     * @param conn
     * @param sql
     * @param parameters
     * @return
     * @throws UncheckedSQLException
     */
    @SafeVarargs
    public static <T> Try<Stream<T>> stream(final Class<T> targetType, final Connection conn, final String sql, final Object... parameters)
            throws UncheckedSQLException {
        N.checkArgNotNull(targetType);

        return stream2(targetType, conn, sql, parameters);
    }

    @SafeVarargs
    private static <T> Try<Stream<T>> stream2(final Class<T> targetType, final Connection conn, final String sql, final Object... parameters)
            throws UncheckedSQLException {
        PreparedStatement stmt = null;
        ResultSet rs = null;
        boolean isOK = false;

        try {
            stmt = JdbcUtil.prepareStatement(conn, sql, parameters);
            rs = stmt.executeQuery();

            final PreparedStatement _stmt = stmt;
            final ResultSet _rs = rs;

            @SuppressWarnings("unchecked")
            final Try<Stream<T>> res = (targetType == null ? (Stream<T>) Stream.of(rs) : Stream.of(targetType, rs)).onClose(new Runnable() {
                @Override
                public void run() {
                    JdbcUtil.closeQuietly(_rs, _stmt);
                }
            }).tried();

            isOK = true;

            return res;
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            if (!isOK) {
                JdbcUtil.closeQuietly(rs, stmt);
            }
        }
    }

    /**
     * Don’t close the specified statement before the returned {@code Stream} is consumed/collected/terminated.
     * 
     * @param stmt
     * @return
     * @throws UncheckedSQLException
     */
    public static Try<Stream<Object[]>> stream(final PreparedStatement stmt) throws UncheckedSQLException {
        return stream2(null, stmt);
    }

    /**
     * Don’t close the specified statement before the returned {@code Stream} is consumed/collected/terminated.
     * 
     * @param targetType {@code Map} or {@code Entity} class with getter/setter methods.
     * @param stmt
     * @return
     * @throws UncheckedSQLException
     */
    public static <T> Try<Stream<T>> stream(final Class<T> targetType, final PreparedStatement stmt) throws UncheckedSQLException {
        N.checkArgNotNull(targetType);

        return stream2(targetType, stmt);
    }

    private static <T> Try<Stream<T>> stream2(final Class<T> targetType, final PreparedStatement stmt) throws UncheckedSQLException {
        ResultSet rs = null;
        boolean isOK = false;

        try {
            rs = stmt.executeQuery();

            final ResultSet _rs = rs;

            @SuppressWarnings("unchecked")
            final Try<Stream<T>> res = (targetType == null ? (Stream<T>) Stream.of(rs) : Stream.of(targetType, rs)).onClose(new Runnable() {
                @Override
                public void run() {
                    JdbcUtil.closeQuietly(_rs);
                }
            }).tried();

            isOK = true;

            return res;
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            if (!isOK) {
                JdbcUtil.closeQuietly(rs);
            }
        }
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Connection conn, final String insertSQL) throws UncheckedSQLException {
        return importData(dataset, dataset.columnNameList(), conn, insertSQL);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre> 
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final Connection conn, final String insertSQL)
            throws UncheckedSQLException {
        return importData(dataset, selectColumnNames, 0, dataset.size(), conn, insertSQL);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count, final Connection conn,
            final String insertSQL) throws UncheckedSQLException {
        return importData(dataset, selectColumnNames, offset, count, conn, insertSQL, 200, 0);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>  
     * @param batchSize
     * @param batchInterval
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval) throws UncheckedSQLException {
        return importData(dataset, selectColumnNames, offset, count, Fn.alwaysTrue(), conn, insertSQL, batchSize, batchInterval);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param filter
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @return
     * @throws UncheckedSQLException
     */
    public static <E extends Exception> int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final Try.Predicate<? super Object[], E> filter, final Connection conn, final String insertSQL, final int batchSize, final int batchInterval)
            throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(dataset, selectColumnNames, offset, count, filter, stmt, batchSize, batchInterval);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Connection conn, final String insertSQL, final Map<String, ? extends Type> columnTypeMap)
            throws UncheckedSQLException {
        return importData(dataset, 0, dataset.size(), conn, insertSQL, columnTypeMap);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL,
            final Map<String, ? extends Type> columnTypeMap) throws UncheckedSQLException {
        return importData(dataset, offset, count, conn, insertSQL, 200, 0, columnTypeMap);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL, final int batchSize,
            final int batchInterval, final Map<String, ? extends Type> columnTypeMap) throws UncheckedSQLException {
        return importData(dataset, offset, count, Fn.alwaysTrue(), conn, insertSQL, batchSize, batchInterval, columnTypeMap);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param filter
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static <E extends Exception> int importData(final DataSet dataset, final int offset, final int count,
            final Try.Predicate<? super Object[], E> filter, final Connection conn, final String insertSQL, final int batchSize, final int batchInterval,
            final Map<String, ? extends Type> columnTypeMap) throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(dataset, offset, count, filter, stmt, batchSize, batchInterval, columnTypeMap);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param stmtSetter
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Connection conn, final String insertSQL,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException {
        return importData(dataset, 0, dataset.size(), conn, insertSQL, stmtSetter);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param stmtSetter
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException {
        return importData(dataset, offset, count, conn, insertSQL, 200, 0, stmtSetter);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @param stmtSetter
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL, final int batchSize,
            final int batchInterval, final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException {
        return importData(dataset, offset, count, Fn.alwaysTrue(), conn, insertSQL, batchSize, batchInterval, stmtSetter);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param filter
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = new ArrayList<>(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @param stmtSetter
     * @return
     * @throws UncheckedSQLException
     */
    public static <E extends Exception> int importData(final DataSet dataset, final int offset, final int count,
            final Try.Predicate<? super Object[], E> filter, final Connection conn, final String insertSQL, final int batchSize, final int batchInterval,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(dataset, offset, count, filter, stmt, batchSize, batchInterval, stmtSetter);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final PreparedStatement stmt) throws UncheckedSQLException {
        return importData(dataset, dataset.columnNameList(), stmt);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final PreparedStatement stmt) throws UncheckedSQLException {
        return importData(dataset, selectColumnNames, 0, dataset.size(), stmt);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final PreparedStatement stmt) throws UncheckedSQLException {
        return importData(dataset, selectColumnNames, offset, count, stmt, 200, 0);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final PreparedStatement stmt, final int batchSize, final int batchInterval) throws UncheckedSQLException {
        return importData(dataset, selectColumnNames, offset, count, Fn.alwaysTrue(), stmt, batchSize, batchInterval);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param batchSize
     * @param batchInterval
     * @return
     * @throws UncheckedSQLException
     */
    public static <E extends Exception> int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final Try.Predicate<? super Object[], E> filter, final PreparedStatement stmt, final int batchSize, final int batchInterval)
            throws UncheckedSQLException, E {
        final Type<?> objType = N.typeOf(Object.class);
        final Map<String, Type<?>> columnTypeMap = new HashMap<>();

        for (String propName : selectColumnNames) {
            columnTypeMap.put(propName, objType);
        }

        return importData(dataset, offset, count, filter, stmt, batchSize, batchInterval, columnTypeMap);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final PreparedStatement stmt, final Map<String, ? extends Type> columnTypeMap)
            throws UncheckedSQLException {
        return importData(dataset, 0, dataset.size(), stmt, columnTypeMap);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap) throws UncheckedSQLException {
        return importData(dataset, offset, count, stmt, 200, 0, columnTypeMap);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param columnTypeMap
     * @param filter
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Map<String, ? extends Type> columnTypeMap) throws UncheckedSQLException {
        return importData(dataset, offset, count, Fn.alwaysTrue(), stmt, batchSize, batchInterval, columnTypeMap);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param filter
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param batchSize
     * @param batchInterval
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    @SuppressWarnings("rawtypes")
    public static <E extends Exception> int importData(final DataSet dataset, final int offset, final int count,
            final Try.Predicate<? super Object[], E> filter, final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final Map<String, ? extends Type> columnTypeMap) throws UncheckedSQLException, E {
        N.checkArgument(offset >= 0 && count >= 0, "'offset'=%s and 'count'=%s can't be negative", offset, count);
        N.checkArgument(batchSize > 0 && batchInterval >= 0, "'batchSize'=%s must be greater than 0 and 'batchInterval'=%s can't be negative", batchSize,
                batchInterval);

        int result = 0;

        try {
            final int columnCount = columnTypeMap.size();
            final List<String> columnNameList = dataset.columnNameList();
            final int[] columnIndexes = new int[columnCount];
            final Type<Object>[] columnTypes = new Type[columnCount];
            final Set<String> columnNameSet = new HashSet<>(columnCount);

            int idx = 0;
            for (String columnName : columnNameList) {
                if (columnTypeMap.containsKey(columnName)) {
                    columnIndexes[idx] = dataset.getColumnIndex(columnName);
                    columnTypes[idx] = columnTypeMap.get(columnName);
                    columnNameSet.add(columnName);
                    idx++;
                }
            }

            if (columnNameSet.size() != columnTypeMap.size()) {
                final List<String> keys = new ArrayList<>(columnTypeMap.keySet());
                keys.removeAll(columnNameSet);
                throw new AbacusException(keys + " are not included in titles: " + N.toString(columnNameList));
            }

            final Object[] row = filter == null ? null : new Object[columnCount];
            for (int i = offset, size = dataset.size(); result < count && i < size; i++) {
                dataset.absolute(i);

                if (filter == null) {
                    for (int j = 0; j < columnCount; j++) {
                        columnTypes[j].set(stmt, j + 1, dataset.get(columnIndexes[j]));
                    }
                } else {
                    for (int j = 0; j < columnCount; j++) {
                        row[j] = dataset.get(columnIndexes[j]);
                    }

                    if (filter.test(row) == false) {
                        continue;
                    }

                    for (int j = 0; j < columnCount; j++) {
                        columnTypes[j].set(stmt, j + 1, row[j]);
                    }
                }

                stmt.addBatch();

                if ((++result % batchSize) == 0) {
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
            throw new UncheckedSQLException(e);
        }

        return result;
    }

    public static int importData(final DataSet dataset, final PreparedStatement stmt,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException {
        return importData(dataset, 0, dataset.size(), stmt, stmtSetter);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException {
        return importData(dataset, offset, count, stmt, 200, 0, stmtSetter);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param columnTypeMap
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param filter
     * @param stmtSetter
     * @return
     * @throws UncheckedSQLException
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException {
        return importData(dataset, offset, count, Fn.alwaysTrue(), stmt, batchSize, batchInterval, stmtSetter);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param offset
     * @param count
     * @param filter
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param batchSize
     * @param batchInterval
     * @param stmtSetter
     * @param columnTypeMap
     * @return
     * @throws UncheckedSQLException
     */
    public static <E extends Exception> int importData(final DataSet dataset, final int offset, final int count,
            final Try.Predicate<? super Object[], E> filter, final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException, E {
        N.checkArgument(offset >= 0 && count >= 0, "'offset'=%s and 'count'=%s can't be negative", offset, count);
        N.checkArgument(batchSize > 0 && batchInterval >= 0, "'batchSize'=%s must be greater than 0 and 'batchInterval'=%s can't be negative", batchSize,
                batchInterval);

        final int columnCount = dataset.columnNameList().size();
        final Object[] row = new Object[columnCount];
        int result = 0;

        try {
            for (int i = offset, size = dataset.size(); result < count && i < size; i++) {
                dataset.absolute(i);

                for (int j = 0; j < columnCount; j++) {
                    row[j] = dataset.get(j);
                }

                if (filter != null && filter.test(row) == false) {
                    continue;
                }

                stmtSetter.accept(stmt, row);

                stmt.addBatch();

                if ((++result % batchSize) == 0) {
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
            throw new UncheckedSQLException(e);
        }

        return result;
    }

    public static <E extends Exception> long importData(final File file, final Connection conn, final String insertSQL,
            final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        return importData(file, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static <E extends Exception> long importData(final File file, final long offset, final long count, final Connection conn, final String insertSQL,
            final int batchSize, final int batchInterval, final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(file, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static <E extends Exception> long importData(final File file, final PreparedStatement stmt, final Try.Function<String, Object[], E> func)
            throws UncheckedSQLException, E {
        return importData(file, 0, Long.MAX_VALUE, stmt, 200, 0, func);
    }

    /**
     * Imports the data from file to database.
     * 
     * @param file
     * @param offset
     * @param count
     * @param stmt
     * @param batchSize
     * @param batchInterval
     * @param func convert line to the parameters for record insert. Returns a <code>null</code> array to skip the line. 
     * @return
     * @throws UncheckedSQLException
     */
    public static <E extends Exception> long importData(final File file, final long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        Reader reader = null;

        try {
            reader = new FileReader(file);

            return importData(reader, offset, count, stmt, batchSize, batchInterval, func);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            IOUtil.close(reader);
        }
    }

    public static <E extends Exception> long importData(final InputStream is, final Connection conn, final String insertSQL,
            final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        return importData(is, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static <E extends Exception> long importData(final InputStream is, final long offset, final long count, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval, final Try.Function<String, Object[], E> func)
            throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(is, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static <E extends Exception> long importData(final InputStream is, final PreparedStatement stmt, final Try.Function<String, Object[], E> func)
            throws E {
        return importData(is, 0, Long.MAX_VALUE, stmt, 200, 0, func);
    }

    /**
     * Imports the data from file to database.
     * 
     * @param is
     * @param offset
     * @param count
     * @param stmt
     * @param batchSize
     * @param batchInterval
     * @param func convert line to the parameters for record insert. Returns a <code>null</code> array to skip the line. 
     * @return
     * @throws UncheckedSQLException
     */
    public static <E extends Exception> long importData(final InputStream is, final long offset, final long count, final PreparedStatement stmt,
            final int batchSize, final int batchInterval, final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        final Reader reader = new InputStreamReader(is);

        return importData(reader, offset, count, stmt, batchSize, batchInterval, func);
    }

    public static <E extends Exception> long importData(final Reader reader, final Connection conn, final String insertSQL,
            final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        return importData(reader, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static <E extends Exception> long importData(final Reader reader, final long offset, final long count, final Connection conn, final String insertSQL,
            final int batchSize, final int batchInterval, final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(reader, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static <E extends Exception> long importData(final Reader reader, final PreparedStatement stmt, final Try.Function<String, Object[], E> func)
            throws E {
        return importData(reader, 0, Long.MAX_VALUE, stmt, 200, 0, func);
    }

    /**
     * Imports the data from file to database.
     * 
     * @param reader
     * @param offset
     * @param count
     * @param stmt
     * @param batchSize
     * @param batchInterval
     * @param func convert line to the parameters for record insert. Returns a <code>null</code> array to skip the line. 
     * @return
     * @throws UncheckedSQLException
     */
    public static <E extends Exception> long importData(final Reader reader, long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Try.Function<String, Object[], E> func) throws UncheckedSQLException, E {
        N.checkArgument(offset >= 0 && count >= 0, "'offset'=%s and 'count'=%s can't be negative", offset, count);
        N.checkArgument(batchSize > 0 && batchInterval >= 0, "'batchSize'=%s must be greater than 0 and 'batchInterval'=%s can't be negative", batchSize,
                batchInterval);

        long result = 0;
        final BufferedReader br = ObjectFactory.createBufferedReader(reader);

        try {
            while (offset-- > 0 && br.readLine() != null) {
            }

            String line = null;
            Object[] row = null;

            while (result < count && (line = br.readLine()) != null) {
                row = func.apply(line);

                if (row == null) {
                    continue;
                }

                for (int i = 0, len = row.length; i < len; i++) {
                    stmt.setObject(i + 1, row[i]);
                }

                stmt.addBatch();

                if ((++result % batchSize) == 0) {
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
            throw new UncheckedSQLException(e);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        } finally {
            ObjectFactory.recycle(br);
        }

        return result;
    }

    public static <T, E extends Exception> long importData(final Iterator<T> iter, final Connection conn, final String insertSQL,
            final Try.Function<T, Object[], E> func) throws UncheckedSQLException, E {
        return importData(iter, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static <T, E extends Exception> long importData(final Iterator<T> iter, final long offset, final long count, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval, final Try.Function<T, Object[], E> func) throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(iter, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static <T, E extends Exception> long importData(final Iterator<T> iter, final PreparedStatement stmt, final Try.Function<T, Object[], E> func)
            throws E {
        return importData(iter, 0, Long.MAX_VALUE, stmt, 200, 0, func);
    }

    /**
     * Imports the data from Iterator to database.
     * 
     * @param iter
     * @param offset
     * @param count
     * @param stmt
     * @param batchSize
     * @param batchInterval
     * @param func convert element to the parameters for record insert. Returns a <code>null</code> array to skip the line. 
     * @return
     * @throws UncheckedSQLException
     */
    public static <T, E extends Exception> long importData(final Iterator<T> iter, long offset, final long count, final PreparedStatement stmt,
            final int batchSize, final int batchInterval, final Try.Function<T, Object[], E> func) throws UncheckedSQLException, E {
        N.checkArgument(offset >= 0 && count >= 0, "'offset'=%s and 'count'=%s can't be negative", offset, count);
        N.checkArgument(batchSize > 0 && batchInterval >= 0, "'batchSize'=%s must be greater than 0 and 'batchInterval'=%s can't be negative", batchSize,
                batchInterval);

        long result = 0;

        try {
            while (offset-- > 0 && iter.hasNext()) {
                iter.next();
            }

            Object[] row = null;

            while (result < count && iter.hasNext()) {
                row = func.apply(iter.next());

                if (row == null) {
                    continue;
                }

                for (int i = 0, len = row.length; i < len; i++) {
                    stmt.setObject(i + 1, row[i]);
                }

                stmt.addBatch();

                if ((++result % batchSize) == 0) {
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
            throw new UncheckedSQLException(e);
        }

        return result;
    }

    public static <T> long importData(final Iterator<T> iter, final Connection conn, final String insertSQL,
            final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) {
        return importData(iter, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, stmtSetter);
    }

    public static <T> long importData(final Iterator<T> iter, final long offset, final long count, final Connection conn, final String insertSQL,
            final int batchSize, final int batchInterval, final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) {
        return importData(iter, offset, count, Fn.alwaysTrue(), conn, insertSQL, batchSize, batchInterval, stmtSetter);
    }

    /**
     * 
     * @param iter
     * @param offset
     * @param count
     * @param filter
     * @param conn
     * @param insertSQL
     * @param batchSize
     * @param batchInterval
     * @param stmtSetter
     * @return
     * @throws UncheckedSQLException
     */
    public static <T, E extends Exception> long importData(final Iterator<T> iter, final long offset, final long count,
            final Try.Predicate<? super T, E> filter, final Connection conn, final String insertSQL, final int batchSize, final int batchInterval,
            final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) throws UncheckedSQLException, E {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(iter, offset, count, filter, stmt, batchSize, batchInterval, stmtSetter);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static <T> long importData(final Iterator<T> iter, final PreparedStatement stmt,
            final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) {
        return importData(iter, 0, Long.MAX_VALUE, stmt, 200, 0, stmtSetter);
    }

    public static <T> long importData(final Iterator<T> iter, long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) {
        return importData(iter, offset, count, Fn.alwaysTrue(), stmt, batchSize, batchInterval, stmtSetter);
    }

    /**
     * Imports the data from Iterator to database.
     * 
     * @param iter
     * @param offset
     * @param count
     * @param filter
     * @param stmt
     * @param batchSize
     * @param batchInterval
     * @param stmtSetter 
     * @return
     * @throws UncheckedSQLException
     */
    public static <T, E extends Exception> long importData(final Iterator<T> iter, long offset, final long count, final Try.Predicate<? super T, E> filter,
            final PreparedStatement stmt, final int batchSize, final int batchInterval,
            final Try.BiConsumer<? super PreparedStatement, ? super T, SQLException> stmtSetter) throws UncheckedSQLException, E {
        N.checkArgument(offset >= 0 && count >= 0, "'offset'=%s and 'count'=%s can't be negative", offset, count);
        N.checkArgument(batchSize > 0 && batchInterval >= 0, "'batchSize'=%s must be greater than 0 and 'batchInterval'=%s can't be negative", batchSize,
                batchInterval);

        long result = 0;

        try {
            while (offset-- > 0 && iter.hasNext()) {
                iter.next();
            }
            T next = null;
            while (result < count && iter.hasNext()) {
                next = iter.next();

                if (filter != null && filter.test(next) == false) {
                    continue;
                }

                stmtSetter.accept(stmt, next);
                stmt.addBatch();

                if ((++result % batchSize) == 0) {
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
            throw new UncheckedSQLException(e);
        }

        return result;
    }

    public static <E extends Exception> void parse(final Connection conn, final String sql, final Try.Consumer<Object[], E> rowParser)
            throws UncheckedSQLException, E {
        parse(conn, sql, rowParser, Fn.emptyAction());
    }

    public static <E extends Exception, E2 extends Exception> void parse(final Connection conn, final String sql, final Try.Consumer<Object[], E> rowParser,
            final Try.Runnable<E2> onComplete) throws UncheckedSQLException, E, E2 {
        parse(conn, sql, 0, Long.MAX_VALUE, rowParser, onComplete);
    }

    public static <E extends Exception> void parse(final Connection conn, final String sql, final long offset, final long count,
            final Try.Consumer<Object[], E> rowParser) throws UncheckedSQLException, E {
        parse(conn, sql, offset, count, rowParser, Fn.emptyAction());
    }

    public static <E extends Exception, E2 extends Exception> void parse(final Connection conn, final String sql, final long offset, final long count,
            final Try.Consumer<Object[], E> rowParser, final Try.Runnable<E2> onComplete) throws UncheckedSQLException, E, E2 {
        parse(conn, sql, offset, count, 0, 0, rowParser, onComplete);
    }

    public static <E extends Exception> void parse(final Connection conn, final String sql, final long offset, final long count, final int processThreadNum,
            final int queueSize, final Try.Consumer<Object[], E> rowParser) throws UncheckedSQLException, E {
        parse(conn, sql, offset, count, processThreadNum, queueSize, rowParser, Fn.emptyAction());
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * 
     * @param conn
     * @param sql
     * @param offset
     * @param count
     * @param processThreadNum new threads started to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param rowParser
     * @param onComplete
     * @throws UncheckedSQLException
     */
    public static <E extends Exception, E2 extends Exception> void parse(final Connection conn, final String sql, final long offset, final long count,
            final int processThreadNum, final int queueSize, final Try.Consumer<Object[], E> rowParser, final Try.Runnable<E2> onComplete)
            throws UncheckedSQLException, E, E2 {
        PreparedStatement stmt = null;
        try {
            stmt = prepareStatement(conn, sql);

            stmt.setFetchSize(200);

            parse(stmt, offset, count, processThreadNum, queueSize, rowParser, onComplete);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    public static <E extends Exception> void parse(final PreparedStatement stmt, final Try.Consumer<Object[], E> rowParser) throws UncheckedSQLException, E {
        parse(stmt, rowParser, Fn.emptyAction());
    }

    public static <E extends Exception, E2 extends Exception> void parse(final PreparedStatement stmt, final Try.Consumer<Object[], E> rowParser,
            final Try.Runnable<E2> onComplete) throws UncheckedSQLException, E, E2 {
        parse(stmt, 0, Long.MAX_VALUE, rowParser, onComplete);
    }

    public static <E extends Exception> void parse(final PreparedStatement stmt, final long offset, final long count, final Try.Consumer<Object[], E> rowParser)
            throws E {
        parse(stmt, offset, count, rowParser, Fn.emptyAction());
    }

    public static <E extends Exception, E2 extends Exception> void parse(final PreparedStatement stmt, final long offset, final long count,
            final Try.Consumer<Object[], E> rowParser, final Try.Runnable<E2> onComplete) throws UncheckedSQLException, E, E2 {
        parse(stmt, offset, count, 0, 0, rowParser, onComplete);
    }

    public static <E extends Exception> void parse(final PreparedStatement stmt, final long offset, final long count, final int processThreadNum,
            final int queueSize, final Try.Consumer<Object[], E> rowParser) throws UncheckedSQLException, E {
        parse(stmt, offset, count, processThreadNum, queueSize, rowParser, Fn.emptyAction());
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
     * 
     * @param stmt
     * @param offset
     * @param count
     * @param processThreadNum new threads started to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param rowParser
     * @param onComplete
     * @throws UncheckedSQLException
     */
    public static <E extends Exception, E2 extends Exception> void parse(final PreparedStatement stmt, final long offset, final long count,
            final int processThreadNum, final int queueSize, final Try.Consumer<Object[], E> rowParser, final Try.Runnable<E2> onComplete)
            throws UncheckedSQLException, E, E2 {
        ResultSet rs = null;

        try {
            rs = stmt.executeQuery();

            parse(rs, offset, count, processThreadNum, queueSize, rowParser, onComplete);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(rs);
        }
    }

    public static <E extends Exception> void parse(final ResultSet rs, final Try.Consumer<Object[], E> rowParser) throws UncheckedSQLException, E {
        parse(rs, rowParser, Fn.emptyAction());
    }

    public static <E extends Exception, E2 extends Exception> void parse(final ResultSet rs, final Try.Consumer<Object[], E> rowParser,
            final Try.Runnable<E2> onComplete) throws UncheckedSQLException, E, E2 {
        parse(rs, 0, Long.MAX_VALUE, rowParser, onComplete);
    }

    public static <E extends Exception> void parse(final ResultSet rs, long offset, long count, final Try.Consumer<Object[], E> rowParser)
            throws UncheckedSQLException, E {
        parse(rs, offset, count, rowParser, Fn.emptyAction());
    }

    public static <E extends Exception, E2 extends Exception> void parse(final ResultSet rs, long offset, long count, final Try.Consumer<Object[], E> rowParser,
            final Try.Runnable<E2> onComplete) throws UncheckedSQLException, E, E2 {
        parse(rs, offset, count, 0, 0, rowParser, onComplete);
    }

    public static <E extends Exception> void parse(final ResultSet rs, long offset, long count, final int processThreadNum, final int queueSize,
            final Try.Consumer<Object[], E> rowParser) throws UncheckedSQLException, E {
        parse(rs, offset, count, processThreadNum, queueSize, rowParser, Fn.emptyAction());
    }

    /**
     * Parse the ResultSet.
     * 
     * @param stmt
     * @param offset
     * @param count
     * @param processThreadNum new threads started to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param rowParser
     * @param onComplete
     * @throws UncheckedSQLException
     */
    public static <E extends Exception, E2 extends Exception> void parse(final ResultSet rs, long offset, long count, final int processThreadNum,
            final int queueSize, final Try.Consumer<Object[], E> rowParser, final Try.Runnable<E2> onComplete) throws UncheckedSQLException, E, E2 {
        N.parse(new RowIterator(rs, false, false), offset, count, processThreadNum, queueSize, rowParser, onComplete);
    }

    public static long copy(final Connection sourceConn, final String selectSql, final Connection targetConn, final String insertSql)
            throws UncheckedSQLException {
        return copy(sourceConn, selectSql, 200, 0, Integer.MAX_VALUE, targetConn, insertSql, DEFAULT_STMT_SETTER, 200, 0, false);
    }

    /**
     * 
     * @param sourceConn
     * @param selectSql
     * @param fetchSize
     * @param offset
     * @param count
     * @param targetConn
     * @param insertSql
     * @param stmtSetter
     * @param batchSize
     * @param batchInterval
     * @param inParallel do the read and write in separated threads.
     * @return
     * @throws UncheckedSQLException
     */
    public static long copy(final Connection sourceConn, final String selectSql, final int fetchSize, final long offset, final long count,
            final Connection targetConn, final String insertSql, final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter,
            final int batchSize, final int batchInterval, final boolean inParallel) throws UncheckedSQLException {
        PreparedStatement selectStmt = null;
        PreparedStatement insertStmt = null;

        int result = 0;

        try {
            insertStmt = targetConn.prepareStatement(insertSql);

            selectStmt = sourceConn.prepareStatement(selectSql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
            selectStmt.setFetchSize(fetchSize);

            copy(selectStmt, offset, count, insertStmt, stmtSetter, batchSize, batchInterval, inParallel);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        } finally {
            closeQuietly(selectStmt);
            closeQuietly(insertStmt);
        }

        return result;
    }

    public static long copy(final PreparedStatement selectStmt, final PreparedStatement insertStmt,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter) throws UncheckedSQLException {
        return copy(selectStmt, 0, Integer.MAX_VALUE, insertStmt, stmtSetter, 200, 0, false);
    }

    /**
     * 
     * @param selectStmt
     * @param offset
     * @param count
     * @param insertStmt
     * @param stmtSetter
     * @param batchSize
     * @param batchInterval
     * @param inParallel do the read and write in separated threads.
     * @return
     * @throws UncheckedSQLException
     */
    public static long copy(final PreparedStatement selectStmt, final long offset, final long count, final PreparedStatement insertStmt,
            final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> stmtSetter, final int batchSize, final int batchInterval,
            final boolean inParallel) throws UncheckedSQLException {
        N.checkArgument(offset >= 0 && count >= 0, "'offset'=%s and 'count'=%s can't be negative", offset, count);
        N.checkArgument(batchSize > 0 && batchInterval >= 0, "'batchSize'=%s must be greater than 0 and 'batchInterval'=%s can't be negative", batchSize,
                batchInterval);

        @SuppressWarnings("rawtypes")
        final Try.BiConsumer<? super PreparedStatement, ? super Object[], SQLException> setter = (Try.BiConsumer) (stmtSetter == null ? DEFAULT_STMT_SETTER
                : stmtSetter);
        final AtomicLong result = new AtomicLong();

        final Try.Consumer<Object[], RuntimeException> rowParser = new Try.Consumer<Object[], RuntimeException>() {
            @Override
            public void accept(Object[] row) {
                try {
                    setter.accept(insertStmt, row);

                    insertStmt.addBatch();
                    result.incrementAndGet();

                    if ((result.longValue() % batchSize) == 0) {
                        insertStmt.executeBatch();
                        insertStmt.clearBatch();

                        if (batchInterval > 0) {
                            N.sleep(batchInterval);
                        }
                    }
                } catch (SQLException e) {
                    throw new UncheckedSQLException(e);
                }
            }
        };

        final Try.Runnable<RuntimeException> onComplete = new Try.Runnable<RuntimeException>() {
            @Override
            public void run() {
                if ((result.longValue() % batchSize) > 0) {
                    try {
                        insertStmt.executeBatch();
                        insertStmt.clearBatch();
                    } catch (SQLException e) {
                        throw new UncheckedSQLException(e);
                    }
                }
            }
        };

        parse(selectStmt, offset, count, 0, inParallel ? DEFAULT_QUEUE_SIZE_FOR_ROW_PARSER : 0, rowParser, onComplete);

        return result.longValue();
    }

    public static boolean doesTableExist(final Connection conn, final String tableName) {
        try {
            executeQuery(conn, "SELECT 1 FROM " + tableName + " WHERE 1 > 2");

            return true;
        } catch (UncheckedSQLException e) {
            if (isTableNotExistsException(e)) {
                return false;
            }

            throw e;
        }
    }

    /**
     * Returns {@code true} if succeed to create table, otherwise {@code false} is returned.
     * 
     * @param conn
     * @param tableName
     * @param schema
     * @return
     */
    public static boolean createTableIfNotExists(final Connection conn, final String tableName, final String schema) {
        if (doesTableExist(conn, tableName)) {
            return false;
        }

        try {
            execute(conn, schema);

            return true;
        } catch (UncheckedSQLException e) {
            return false;
        }
    }

    /**
     * Returns {@code true} if succeed to drop table, otherwise {@code false} is returned.
     * 
     * @param conn
     * @param tableName
     * @return
     */
    public static boolean dropTableIfExists(final Connection conn, final String tableName) {
        try {
            if (doesTableExist(conn, tableName)) {
                execute(conn, "DROP TABLE " + tableName);

                return true;
            }
        } catch (UncheckedSQLException e) {
            // ignore.
        }

        return false;
    }

    /**
     * 
     * @param conn
     * @param tableName
     * @return
     * @throws UncheckedSQLException
     */
    public static List<String> getColumnNameList(final Connection conn, final String tableName) throws UncheckedSQLException {
        DataSet dataSet = executeQuery(conn, "SELECT * FROM " + tableName + " WHERE 1 > 2");
        List<String> columnNameList = new ArrayList<>(dataSet.columnNameList().size());

        for (String columName : dataSet.columnNameList()) {
            columnNameList.add(columName.intern());
        }

        return columnNameList;
    }

    /**
     * 
     * @param rs
     * @return
     * @throws UncheckedSQLException
     */
    public static List<String> getColumnLabelList(ResultSet rs) throws UncheckedSQLException {
        try {
            ResultSetMetaData metaData = rs.getMetaData();
            final int columnCount = metaData.getColumnCount();
            final List<String> labelList = new ArrayList<>(columnCount);

            for (int i = 1, n = columnCount + 1; i < n; i++) {
                labelList.add(metaData.getColumnLabel(i));
            }

            return labelList;
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    static boolean isTableNotExistsException(final Throwable e) {
        if (e instanceof SQLException) {
            SQLException sqlException = (SQLException) e;

            if (sqlException.getSQLState() != null && sqlStateForTableNotExists.contains(sqlException.getSQLState())) {
                return true;
            }

            String msg = e.getMessage();
            return N.notNullOrEmpty(msg) && (msg.contains("not exist") || msg.contains("doesn't exist") || msg.contains("not found"));
        } else if (e instanceof UncheckedSQLException) {
            UncheckedSQLException sqlException = (UncheckedSQLException) e;

            if (sqlException.getSQLState() != null && sqlStateForTableNotExists.contains(sqlException.getSQLState())) {
                return true;
            }

            String msg = e.getMessage();
            return N.notNullOrEmpty(msg) && (msg.contains("not exist") || msg.contains("doesn't exist") || msg.contains("not found"));
        }

        return false;
    }

    static class SimpleDataSource implements DataSource {
        static final String PRIMARY = "primary";

        private final javax.sql.DataSource sqlDataSource;
        private final Properties<String, String> props = new Properties<>();
        private final SliceSelector sliceSelector = new NonSliceSelector();

        private final Method closeMethod;
        private boolean isClosed = false;

        public SimpleDataSource(final javax.sql.DataSource sqlDataSource) {
            this.sqlDataSource = sqlDataSource;

            Method method = null;

            try {
                method = ClassUtil.getDeclaredMethod(sqlDataSource.getClass(), "close");
            } catch (Exception e) {

            }

            closeMethod = method != null && Modifier.isPublic(method.getModifiers()) ? method : null;
        }

        @Override
        public Connection getConnection(final String username, final String password) throws SQLException {
            return sqlDataSource.getConnection(username, password);
        }

        @Override
        public PrintWriter getLogWriter() throws SQLException {
            return sqlDataSource.getLogWriter();
        }

        @Override
        public void setLogWriter(final PrintWriter out) throws SQLException {
            sqlDataSource.setLogWriter(out);
        }

        @Override
        public void setLoginTimeout(final int seconds) throws SQLException {
            sqlDataSource.setLoginTimeout(seconds);
        }

        @Override
        public int getLoginTimeout() throws SQLException {
            return sqlDataSource.getLoginTimeout();
        }

        @Override
        public java.util.logging.Logger getParentLogger() throws SQLFeatureNotSupportedException {
            return sqlDataSource.getParentLogger();
        }

        @Override
        public <T> T unwrap(final Class<T> iface) throws SQLException {
            return sqlDataSource.unwrap(iface);
        }

        @Override
        public boolean isWrapperFor(final Class<?> iface) throws SQLException {
            return sqlDataSource.isWrapperFor(iface);
        }

        @Override
        public Connection getConnection() {
            try {
                return sqlDataSource.getConnection();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }

        @Override
        public Connection getReadOnlyConnection() {
            try {
                return sqlDataSource.getConnection();
            } catch (SQLException e) {
                throw new UncheckedSQLException(e);
            }
        }

        @Override
        public SliceSelector getSliceSelector() {
            return sliceSelector;
        }

        @Override
        public String getName() {
            return PRIMARY;
        }

        @Override
        public Properties<String, String> getProperties() {
            return props;
        }

        @Override
        public int getMaxActive() {
            throw new UnsupportedOperationException();
        }

        @Override
        public int getCurrentActive() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void close() {
            if (isClosed) {
                return;
            }

            if (closeMethod != null) {
                try {
                    ClassUtil.invokeMethod(sqlDataSource, closeMethod);
                } catch (Exception e) {
                    // ignore.
                }
            }

            isClosed = true;
        }

        @Override
        public boolean isClosed() {
            return isClosed;
        }
    }

    static class SimpleDataSourceManager implements DataSourceManager {
        private final DataSource primaryDataSource;
        private final Map<String, DataSource> activeDataSources;
        private final Properties<String, String> props = new Properties<>();
        private final DataSourceSelector dataSourceSelector = new SimpleSourceSelector();
        private boolean isClosed = false;

        public SimpleDataSourceManager(final DataSource ds) {
            this.primaryDataSource = ds;

            if (N.isNullOrEmpty(ds.getName())) {
                this.activeDataSources = N.asMap(SimpleDataSource.PRIMARY, ds);
            } else {
                this.activeDataSources = N.asMap(ds.getName(), ds);
            }
        }

        @Override
        public DataSource getPrimaryDataSource() {
            return primaryDataSource;
        }

        @Override
        public Map<String, DataSource> getActiveDataSources() {
            return activeDataSources;
        }

        @Override
        public DataSourceSelector getDataSourceSelector() {
            return dataSourceSelector;
        }

        @Override
        public Properties<String, String> getProperties() {
            return props;
        }

        @Override
        public void close() {
            if (isClosed) {
                return;
            }

            primaryDataSource.close();

            isClosed = true;
        }

        @Override
        public boolean isClosed() {
            return isClosed;
        }
    }

    /**
     * The backed {@code PreparedStatement/CallableStatement} will be closed by default
     * after any execution methods(which not return the instance of {@code PreparedQuery/PreparedCallableQuery}, except {@code #tried()}) is called, 
     * except the {@code 'closeAfterExecution'} flag is set to {@code false} by calling {@code #closeAfterExecution(false)}.
     * 
     * <br />
     * Generally, don't cache or reuse the instance of this class, 
     * except the {@code 'closeAfterExecution'} flag is set to {@code false} by calling {@code #closeAfterExecution(false)}.
     * 
     * <br />
     * Remember: parameter/column index in {@code PreparedStatement/ResultSet} starts from 1, not 0.
     * 
     * @author haiyangl
     *
     * @param <S>
     * @param <Q>
     */
    public static abstract class AbstractPreparedQuery<S extends PreparedStatement, Q extends AbstractPreparedQuery<S, Q>> implements AutoCloseable {
        final PreparedStatement stmt;
        Connection conn;
        boolean closeAfterExecution = true;
        boolean isClosed = false;
        Try.Runnable<SQLException> actionAfterClose;

        AbstractPreparedQuery(java.sql.PreparedStatement stmt) {
            this.stmt = stmt;
        }

        /**
         * It's designed to void try-catch. 
         * This method should be called immediately after {@code JdbcUtil#prepareCallableQuery/SQLExecutor#prepareQuery}.
         * 
         * @return
         * @throws SQLException
         */
        public Try<Q> tried() throws SQLException {
            return Try.of((Q) this);
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @return
         * @throws SQLException
         */
        public Q setNull(int parameterIndex, int sqlType) throws SQLException {
            stmt.setNull(parameterIndex, sqlType);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @param typeName
         * @return
         * @throws SQLException
         */
        public Q setNull(int parameterIndex, int sqlType, String typeName) throws SQLException {
            stmt.setNull(parameterIndex, sqlType, typeName);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setBoolean(int parameterIndex, boolean x) throws SQLException {
            stmt.setBoolean(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setByte(int parameterIndex, byte x) throws SQLException {
            stmt.setByte(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setShort(int parameterIndex, short x) throws SQLException {
            stmt.setShort(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setInt(int parameterIndex, int x) throws SQLException {
            stmt.setInt(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setLong(int parameterIndex, long x) throws SQLException {
            stmt.setLong(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setFloat(int parameterIndex, float x) throws SQLException {
            stmt.setFloat(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setDouble(int parameterIndex, double x) throws SQLException {
            stmt.setDouble(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setBigDecimal(int parameterIndex, BigDecimal x) throws SQLException {
            stmt.setBigDecimal(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setString(int parameterIndex, String x) throws SQLException {
            stmt.setString(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setTime(int parameterIndex, java.sql.Time x) throws SQLException {
            stmt.setTime(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setDate(int parameterIndex, java.sql.Date x) throws SQLException {
            stmt.setDate(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setTimestamp(int parameterIndex, java.sql.Timestamp x) throws SQLException {
            stmt.setTimestamp(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @return
         * @throws SQLException
         */
        public Q setObject(int parameterIndex, Object x) throws SQLException {
            stmt.setObject(parameterIndex, x);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @param sqlType
         * @return
         * @throws SQLException
         */
        public Q setObject(int parameterIndex, Object x, int sqlType) throws SQLException {
            stmt.setObject(parameterIndex, x, sqlType);

            return (Q) this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param x
         * @param sqlType
         * @param scaleOrLength
         * @return
         * @throws SQLException
         */
        public Q setObject(int parameterIndex, Object x, int sqlType, int scaleOrLength) throws SQLException {
            stmt.setObject(parameterIndex, x, sqlType, scaleOrLength);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param x
         * @param sqlType
         * @return
         * @throws SQLException
         */
        public Q setObject(int parameterIndex, Object x, SQLType sqlType) throws SQLException {
            stmt.setObject(parameterIndex, x, sqlType);

            return (Q) this;
        }

        public Q setObject(int parameterIndex, Object x, SQLType sqlType, int scaleOrLength) throws SQLException {
            stmt.setObject(parameterIndex, x, sqlType, scaleOrLength);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param param1
         * @param param2
         * @return
         * @throws SQLException
         */
        public Q setParameters(int startParameterIndex, String param1, String param2) throws SQLException {
            stmt.setString(startParameterIndex++, param1);
            stmt.setString(startParameterIndex++, param2);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param param1
         * @param param2
         * @param param3
         * @return
         * @throws SQLException
         */
        public Q setParameters(int startParameterIndex, String param1, String param2, String param3) throws SQLException {
            stmt.setString(startParameterIndex++, param1);
            stmt.setString(startParameterIndex++, param2);
            stmt.setString(startParameterIndex++, param3);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param param1
         * @param param2
         * @return
         * @throws SQLException
         */
        public Q setParameters(int startParameterIndex, Object param1, Object param2) throws SQLException {
            stmt.setObject(startParameterIndex++, param1);
            stmt.setObject(startParameterIndex++, param2);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param param1
         * @param param2
         * @param param3
         * @return
         * @throws SQLException
         */
        public Q setParameters(int startParameterIndex, Object param1, Object param2, Object param3) throws SQLException {
            stmt.setObject(startParameterIndex++, param1);
            stmt.setObject(startParameterIndex++, param2);
            stmt.setObject(startParameterIndex++, param3);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param param1
         * @param param2
         * @param param3
         * @param param4
         * @return
         * @throws SQLException
         */
        public Q setParameters(int startParameterIndex, Object param1, Object param2, Object param3, Object param4) throws SQLException {
            stmt.setObject(startParameterIndex++, param1);
            stmt.setObject(startParameterIndex++, param2);
            stmt.setObject(startParameterIndex++, param3);
            stmt.setObject(startParameterIndex++, param4);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param param1
         * @param param2
         * @param param3
         * @param param4
         * @param param5
         * @return
         * @throws SQLException
         */
        public Q setParameters(int startParameterIndex, Object param1, Object param2, Object param3, Object param4, Object param5) throws SQLException {
            stmt.setObject(startParameterIndex++, param1);
            stmt.setObject(startParameterIndex++, param2);
            stmt.setObject(startParameterIndex++, param3);
            stmt.setObject(startParameterIndex++, param4);
            stmt.setObject(startParameterIndex++, param5);

            return (Q) this;
        }

        /**
         * 
         * @param startParameterIndex
         * @param parameters
         * @return
         * @throws SQLException
         */
        public Q setParameters(int startParameterIndex, List<?> parameters) throws SQLException {
            N.checkArgNotNull(parameters);

            for (int i = 0, len = parameters.size(); i < len; i++) {
                stmt.setObject(startParameterIndex++, parameters.get(i));
            }

            return (Q) this;
        }

        public <E extends Exception> Q setParameters(StatementSetter<S, E> setter) throws SQLException, E {
            setter.set((S) stmt);

            return (Q) this;
        }

        /**
         * 
         * @param setter
         * @return
         * @throws SQLException
         * @throws E
         * @deprecated
         */
        @Deprecated
        public <E extends Exception> Q setParameters(BiStatementSetter<S, Q, E> setter) throws SQLException, E {
            setter.set((S) stmt, (Q) this);

            return (Q) this;
        }

        public Q addBatch() throws SQLException {
            stmt.addBatch();

            return (Q) this;
        }

        /**
         * 
         * @param direction one of <code>ResultSet.FETCH_FORWARD</code>,
         * <code>ResultSet.FETCH_REVERSE</code>, or <code>ResultSet.FETCH_UNKNOWN</code>
         * @return
         * @throws SQLException
         * @see {@link java.sql.Statement#setFetchDirection(int)}
         */
        public Q setFetchDirection(int direction) throws SQLException {
            stmt.setFetchDirection(direction);

            return (Q) this;
        }

        public Q setFetchSize(int rows) throws SQLException {
            stmt.setFetchSize(rows);

            return (Q) this;
        }

        public Q setMaxRows(int max) throws SQLException {
            stmt.setMaxRows(max);

            return (Q) this;
        }

        public Q setLargeMaxRows(long max) throws SQLException {
            stmt.setLargeMaxRows(max);

            return (Q) this;
        }

        public Q setMaxFieldSize(int max) throws SQLException {
            stmt.setMaxFieldSize(max);

            return (Q) this;
        }

        public Q setQueryTimeout(int seconds) throws SQLException {
            stmt.setQueryTimeout(seconds);

            return (Q) this;
        }

        public Q closeAfterExecution(boolean closeAfterExecution) throws SQLException {
            this.closeAfterExecution = closeAfterExecution;

            return (Q) this;
        }

        /**
         * 
         * @param closeHandler A task to execute after this {@code Query} is closed
         * @return
         * @throws SQLException
         */
        public Q onClose(Try.Runnable<SQLException> closeHandler) throws SQLException {
            this.actionAfterClose = closeHandler;

            return (Q) this;
        }

        public OptionalBoolean queryForBoolean() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalBoolean.of(rs.getBoolean(1)) : OptionalBoolean.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public OptionalChar queryForChar() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalChar.of(Character.valueOf((char) rs.getInt(1))) : OptionalChar.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public OptionalByte queryForByte() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalByte.of(rs.getByte(1)) : OptionalByte.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public OptionalShort queryForShort() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalShort.of(rs.getShort(1)) : OptionalShort.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public OptionalInt queryForInt() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalInt.of(rs.getInt(1)) : OptionalInt.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public OptionalLong queryForLong() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalLong.of(rs.getLong(1)) : OptionalLong.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public OptionalFloat queryForFloat() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalFloat.of(rs.getFloat(1)) : OptionalFloat.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public OptionalDouble queryForDouble() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? OptionalDouble.of(rs.getDouble(1)) : OptionalDouble.empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public Nullable<String> queryForString() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Nullable.of(rs.getString(1)) : Nullable.<String> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public Nullable<BigDecimal> queryBigDecimal() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Nullable.of(rs.getBigDecimal(1)) : Nullable.<BigDecimal> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public Nullable<java.sql.Date> queryForDate() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Nullable.of(rs.getDate(1)) : Nullable.<java.sql.Date> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public Nullable<java.sql.Time> queryForTime() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Nullable.of(rs.getTime(1)) : Nullable.<java.sql.Time> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public Nullable<java.sql.Timestamp> queryForTimestamp() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Nullable.of(rs.getTimestamp(1)) : Nullable.<java.sql.Timestamp> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T> Nullable<T> queryForSingleResult(Class<T> targetClass) throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Nullable.of(N.as(targetClass, rs.getObject(1))) : Nullable.<T> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        @SuppressWarnings("deprecation")
        private <T> T get(Class<T> targetClass, ResultSet rs, List<String> columnLabels) throws SQLException {
            final List<String> columnLabelList = JdbcUtil.getColumnLabelList(rs);
            final int columnCount = columnLabelList.size();
            final Object entity = N.newInstance(targetClass);

            if (List.class.isAssignableFrom(targetClass)) {
                final List<Object> list = (List<Object>) entity;

                for (int i = 0; i < columnCount; i++) {
                    list.add(rs.getObject(i + 1));
                }
            } else if (Map.class.isAssignableFrom(targetClass)) {
                final Map<String, Object> m = (Map<String, Object>) entity;

                for (int i = 0; i < columnCount; i++) {
                    m.put(columnLabelList.get(i), rs.getObject(i + 1));
                }
            } else {
                for (int i = 0; i < columnCount; i++) {
                    ClassUtil.setPropValue(entity, columnLabelList.get(i), rs.getObject(i + 1), true);
                }

                if (N.isDirtyMarker(targetClass)) {
                    ((DirtyMarker) entity).markDirty(false);
                }
            }

            return (T) entity;
        }

        /**
         * 
         * @param targetClass
         * @return
         * @throws NonUniqueResultException If there are more than one record found by the query
         * @throws SQLException
         */
        public <T> Optional<T> get(final Class<T> targetClass) throws NonUniqueResultException, SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                    final T result = get(targetClass, rs, columnLabels);

                    if (rs.next()) {
                        throw new NonUniqueResultException("There are more than one record found by the query");
                    }

                    return Optional.of(result);
                } else {
                    return Optional.empty();
                }
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        /**
         * 
         * @param resultGetter
         * @return
         * @throws NonUniqueResultException If there are more than one record found by the query
         * @throws SQLException
         * @throws E
         */
        public <T, E extends Exception> Optional<T> get(RecordGetter<T, E> resultGetter) throws NonUniqueResultException, SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    final T result = resultGetter.apply(rs);

                    if (rs.next()) {
                        throw new NonUniqueResultException("There are more than one record found by the query");
                    }

                    return Optional.of(result);
                } else {
                    return Optional.empty();
                }

            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        /**
         * 
         * @param resultGetter
         * @return
         * @throws NonUniqueResultException If there are more than one record found by the query
         * @throws SQLException
         * @throws E
         */
        public <T, E extends Exception> Optional<T> get(BiRecordGetter<T, E> resultGetter) throws NonUniqueResultException, SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    final T result = resultGetter.apply(rs, JdbcUtil.getColumnLabelList(rs));

                    if (rs.next()) {
                        throw new NonUniqueResultException("There are more than one record found by the query");
                    }

                    return Optional.of(result);
                } else {
                    return Optional.empty();
                }

            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        /**
         * 
         * @param targetClass
         * @return
         * @throws SQLException
         */
        public <T> Optional<T> findFirst(final Class<T> targetClass) throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                    return Optional.of(get(targetClass, rs, columnLabels));
                } else {
                    return Optional.empty();
                }
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T, E extends Exception> Optional<T> findFirst(RecordGetter<T, E> resultGetter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Optional.of(resultGetter.apply(rs)) : Optional.<T> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T, E extends Exception> Optional<T> findFirst(BiRecordGetter<T, E> resultGetter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next() ? Optional.of(resultGetter.apply(rs, JdbcUtil.getColumnLabelList(rs))) : Optional.<T> empty();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T> List<T> list(final Class<T> targetClass) throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                final List<T> result = new ArrayList<>();

                while (rs.next()) {
                    result.add(get(targetClass, rs, columnLabels));
                }

                return result;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T, E extends Exception> List<T> list(RecordGetter<T, E> recordGetter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<T> result = new ArrayList<>();

                while (rs.next()) {
                    result.add(recordGetter.apply(rs));
                }

                return result;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T, E extends Exception, E2 extends Exception> List<T> list(RecordPredicate<E> recordFilter, RecordGetter<T, E2> recordGetter)
                throws SQLException, E, E2 {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<T> result = new ArrayList<>();

                while (rs.next()) {
                    if (recordFilter.test(rs)) {
                        result.add(recordGetter.apply(rs));
                    }
                }

                return result;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T, E extends Exception> List<T> list(BiRecordGetter<T, E> recordGetter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                final List<T> result = new ArrayList<>();

                while (rs.next()) {
                    result.add(recordGetter.apply(rs, columnLabels));
                }

                return result;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T, E extends Exception, E2 extends Exception> List<T> list(BiRecordPredicate<E> recordFilter, BiRecordGetter<T, E2> recordGetter)
                throws SQLException, E, E2 {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                final List<T> result = new ArrayList<>();

                while (rs.next()) {
                    if (recordFilter.test(rs, columnLabels)) {
                        result.add(recordGetter.apply(rs, columnLabels));
                    }
                }

                return result;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public DataSet query() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return JdbcUtil.extractData(rs);
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public boolean exists() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                return rs.next();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> void ifExists(RecordConsumer<E> recordConsumer) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    recordConsumer.accept(rs);
                }
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> void ifExists(BiRecordConsumer<E> recordConsumer) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                if (rs.next()) {
                    recordConsumer.accept(rs, JdbcUtil.getColumnLabelList(rs));
                }
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public int count() throws SQLException {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                int cnt = 0;

                while (rs.next()) {
                    cnt++;
                }

                return cnt;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> int count(RecordPredicate<E> recordFilter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                int cnt = 0;

                while (rs.next()) {
                    if (recordFilter.test(rs)) {
                        cnt++;
                    }
                }

                return cnt;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> int count(BiRecordPredicate<E> recordFilter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                int cnt = 0;

                while (rs.next()) {
                    if (recordFilter.test(rs, columnLabels)) {
                        cnt++;
                    }
                }

                return cnt;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> boolean anyMatch(RecordPredicate<E> recordFilter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                while (rs.next()) {
                    if (recordFilter.test(rs)) {
                        return true;
                    }
                }

                return false;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> boolean anyMatch(BiRecordPredicate<E> recordFilter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);

                while (rs.next()) {
                    if (recordFilter.test(rs, columnLabels)) {
                        return true;
                    }
                }

                return false;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> boolean allMatch(RecordPredicate<E> recordFilter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                while (rs.next()) {
                    if (recordFilter.test(rs) == false) {
                        return false;
                    }
                }

                return true;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> boolean allMatch(BiRecordPredicate<E> recordFilter) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);

                while (rs.next()) {
                    if (recordFilter.test(rs, columnLabels) == false) {
                        return false;
                    }
                }

                return true;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> boolean noneMatch(RecordPredicate<E> recordFilter) throws SQLException, E {
            return anyMatch(recordFilter) == false;
        }

        public <E extends Exception> boolean noneMatch(BiRecordPredicate<E> recordFilter) throws SQLException, E {
            return anyMatch(recordFilter) == false;
        }

        public <E extends Exception> void forEach(RecordConsumer<E> recordConsumer) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {

                while (rs.next()) {
                    recordConsumer.accept(rs);
                }

            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception, E2 extends Exception> void forEach(RecordPredicate<E> recordFilter, RecordConsumer<E2> recordConsumer)
                throws SQLException, E, E2 {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {

                while (rs.next()) {
                    if (recordFilter.test(rs)) {
                        recordConsumer.accept(rs);
                    }
                }
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> void forEach(BiRecordConsumer<E> recordConsumer) throws SQLException, E {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);

                while (rs.next()) {
                    recordConsumer.accept(rs, columnLabels);
                }

            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception, E2 extends Exception> void forEach(BiRecordPredicate<E> recordFilter, BiRecordConsumer<E2> recordConsumer)
                throws SQLException, E, E2 {
            assertNotClosed();

            try (ResultSet rs = stmt.executeQuery()) {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);

                while (rs.next()) {
                    if (recordFilter.test(rs, columnLabels)) {
                        recordConsumer.accept(rs, columnLabels);
                    }
                }

            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        /**
         * Returns the generated key if it exists.
         * 
         * @return
         */
        public <T> Optional<T> insert() throws SQLException {
            assertNotClosed();

            try {
                stmt.executeUpdate();

                try (ResultSet rs = stmt.getGeneratedKeys()) {
                    return rs.next() ? Optional.of((T) rs.getObject(1)) : Optional.<T> empty();
                }
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        /**
         * Returns the generated key if it exists.
         * 
         * @return
         */
        public <T> List<T> batchInsert() throws SQLException {
            assertNotClosed();

            try {
                stmt.executeBatch();

                try (ResultSet rs = stmt.getGeneratedKeys()) {
                    final List<T> result = new ArrayList<>();

                    while (rs.next()) {
                        result.add((T) rs.getObject(1));
                    }

                    return result;
                } finally {
                    stmt.clearBatch();
                }
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public int upate() throws SQLException {
            assertNotClosed();

            try {
                return stmt.executeUpdate();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public int[] batchUpdate() throws SQLException {
            assertNotClosed();

            try {
                final int[] result = stmt.executeBatch();
                stmt.clearBatch();
                return result;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public long largeUpate() throws SQLException {
            assertNotClosed();

            try {
                return stmt.executeLargeUpdate();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public long[] largeBatchUpdate() throws SQLException {
            assertNotClosed();

            try {
                final long[] result = stmt.executeLargeBatch();
                stmt.clearBatch();
                return result;
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public boolean execute() throws SQLException {
            assertNotClosed();

            try {
                return stmt.execute();
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <T, E extends Exception> T executeThenApply(final StatementGetter<T, S, E> getter) throws SQLException, E {
            assertNotClosed();

            try {
                stmt.execute();

                return getter.apply((S) stmt);
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        public <E extends Exception> void executeThenAccept(final StatementConsumer<S, E> consumer) throws SQLException, E {
            assertNotClosed();

            try {
                stmt.execute();

                consumer.accept((S) stmt);
            } finally {
                closeAfterExecutionIfAllowed();
            }
        }

        Q onClose(Connection conn) {
            this.conn = conn;

            return (Q) this;
        }

        @Override
        public void close() throws SQLException {
            if (isClosed) {
                return;
            }

            isClosed = true;

            try {
                stmt.clearParameters();
                stmt.close();
            } finally {
                if (conn != null) {
                    if (actionAfterClose == null) {
                        conn.close();
                    } else {
                        try {
                            conn.close();
                        } finally {
                            actionAfterClose.run();
                        }
                    }
                } else if (actionAfterClose != null) {
                    actionAfterClose.run();
                }
            }
        }

        private void closeAfterExecutionIfAllowed() throws SQLException {
            if (closeAfterExecution) {
                close();
            }
        }

        private void assertNotClosed() {
            if (isClosed) {
                throw new IllegalStateException();
            }
        }
    }

    /**
     * The backed {@code PreparedStatement/CallableStatement} will be closed by default
     * after any execution methods(which not return the instance of {@code PreparedQuery/PreparedCallableQuery}, except {@code #tried()}) is called, 
     * except the {@code 'closeAfterExecution'} flag is set to {@code false} by calling {@code #closeAfterExecution(false)}.
     * 
     * <br />
     * Generally, don't cache or reuse the instance of this class, 
     * except the {@code 'closeAfterExecution'} flag is set to {@code false} by calling {@code #closeAfterExecution(false)}.
     * 
     * <br />
     * Remember: parameter/column index in {@code PreparedStatement/ResultSet} starts from 1, not 0.
     * 
     * @author haiyangl
     *
     */
    public static class PreparedQuery extends AbstractPreparedQuery<PreparedStatement, PreparedQuery> {
        PreparedQuery(java.sql.PreparedStatement stmt) {
            super(stmt);
        }
    }

    /**
     * The backed {@code PreparedStatement/CallableStatement} will be closed by default
     * after any execution methods(which not return the instance of {@code PreparedQuery/PreparedCallableQuery}, except {@code #tried()}) is called, 
     * except the {@code 'closeAfterExecution'} flag is set to {@code false} by calling {@code #closeAfterExecution(false)}.
     * 
     * <br />
     * Generally, don't cache or reuse the instance of this class, 
     * except the {@code 'closeAfterExecution'} flag is set to {@code false} by calling {@code #closeAfterExecution(false)}.
     * 
     * <br />
     * Remember: parameter/column index in {@code PreparedStatement/ResultSet} starts from 1, not 0.
     * 
     * @author haiyangl
     *
     */
    public static class PreparedCallableQuery extends AbstractPreparedQuery<CallableStatement, PreparedCallableQuery> {
        private final java.sql.CallableStatement stmt;

        PreparedCallableQuery(java.sql.CallableStatement stmt) {
            super(stmt);
            this.stmt = stmt;
        }

        public PreparedCallableQuery setNull(String parameterName, int sqlType) throws SQLException {
            stmt.setNull(parameterName, sqlType);

            return this;
        }

        public PreparedCallableQuery setNull(String parameterName, int sqlType, String typeName) throws SQLException {
            stmt.setNull(parameterName, sqlType, typeName);

            return this;
        }

        public PreparedCallableQuery setBoolean(String parameterName, boolean x) throws SQLException {
            stmt.setBoolean(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setByte(String parameterName, byte x) throws SQLException {
            stmt.setByte(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setShort(String parameterName, short x) throws SQLException {
            stmt.setShort(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setInt(String parameterName, int x) throws SQLException {
            stmt.setInt(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setLong(String parameterName, long x) throws SQLException {
            stmt.setLong(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setFloat(String parameterName, float x) throws SQLException {
            stmt.setFloat(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setDouble(String parameterName, double x) throws SQLException {
            stmt.setDouble(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setBigDecimal(String parameterName, BigDecimal x) throws SQLException {
            stmt.setBigDecimal(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setString(String parameterName, String x) throws SQLException {
            stmt.setString(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setTime(String parameterName, java.sql.Time x) throws SQLException {
            stmt.setTime(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setDate(String parameterName, java.sql.Date x) throws SQLException {
            stmt.setDate(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setTimestamp(String parameterName, java.sql.Timestamp x) throws SQLException {
            stmt.setTimestamp(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setObject(String parameterName, Object x) throws SQLException {
            stmt.setObject(parameterName, x);

            return this;
        }

        public PreparedCallableQuery setObject(String parameterName, Object x, int sqlType) throws SQLException {
            stmt.setObject(parameterName, x, sqlType);

            return this;
        }

        public PreparedCallableQuery setObject(String parameterName, Object x, int sqlType, int scaleOrLength) throws SQLException {
            stmt.setObject(parameterName, x, sqlType, scaleOrLength);

            return this;
        }

        public PreparedCallableQuery setObject(String parameterName, Object x, SQLType sqlType) throws SQLException {
            stmt.setObject(parameterName, x, sqlType);

            return this;
        }

        public PreparedCallableQuery setObject(String parameterName, Object x, SQLType sqlType, int scaleOrLength) throws SQLException {
            stmt.setObject(parameterName, x, sqlType, scaleOrLength);

            return this;
        }

        public PreparedCallableQuery setParameters(Map<String, Object> parameters) throws SQLException {
            N.checkArgNotNull(parameters);

            for (Map.Entry<String, Object> entry : parameters.entrySet()) {
                stmt.setObject(entry.getKey(), entry.getValue());
            }

            return this;
        }

        /**
         * 
         * @param parameterNames
         * @param entity
         * @return
         * @throws SQLException
         */
        public PreparedCallableQuery setParameters(List<String> parameterNames, Object entity) throws SQLException {
            N.checkArgNotNull(parameterNames);
            N.checkArgNotNull(entity);

            for (String parameterName : parameterNames) {
                stmt.setObject(parameterName, ClassUtil.getPropValue(entity, parameterName));
            }

            return this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @return
         * @throws SQLException
         */
        public PreparedCallableQuery registerOutParameter(int parameterIndex, int sqlType) throws SQLException {
            stmt.registerOutParameter(parameterIndex, sqlType);

            return this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @param scale
         * @return
         * @throws SQLException
         */
        public PreparedCallableQuery registerOutParameter(int parameterIndex, int sqlType, int scale) throws SQLException {
            stmt.registerOutParameter(parameterIndex, sqlType, scale);

            return this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @param typeName
         * @return
         * @throws SQLException
         */
        public PreparedCallableQuery registerOutParameter(int parameterIndex, int sqlType, String typeName) throws SQLException {
            stmt.registerOutParameter(parameterIndex, sqlType, typeName);

            return this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @return
         * @throws SQLException
         */
        public PreparedCallableQuery registerOutParameter(int parameterIndex, SQLType sqlType) throws SQLException {
            stmt.registerOutParameter(parameterIndex, sqlType);

            return this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @param scale
         * @return
         * @throws SQLException
         */
        public PreparedCallableQuery registerOutParameter(int parameterIndex, SQLType sqlType, int scale) throws SQLException {
            stmt.registerOutParameter(parameterIndex, sqlType, scale);

            return this;
        }

        /**
         * 
         * @param parameterIndex starts from 1, not 0.
         * @param sqlType
         * @param typeName
         * @return
         * @throws SQLException
         */
        public PreparedCallableQuery registerOutParameter(int parameterIndex, SQLType sqlType, String typeName) throws SQLException {
            stmt.registerOutParameter(parameterIndex, sqlType, typeName);

            return this;
        }

        public PreparedCallableQuery registerOutParameter(String parameterName, int sqlType) throws SQLException {
            stmt.registerOutParameter(parameterName, sqlType);

            return this;
        }

        public PreparedCallableQuery registerOutParameter(String parameterName, int sqlType, int scale) throws SQLException {
            stmt.registerOutParameter(parameterName, sqlType, scale);

            return this;
        }

        public PreparedCallableQuery registerOutParameter(String parameterName, int sqlType, String typeName) throws SQLException {
            stmt.registerOutParameter(parameterName, sqlType, typeName);

            return this;
        }

        public PreparedCallableQuery registerOutParameter(String parameterName, SQLType sqlType) throws SQLException {
            stmt.registerOutParameter(parameterName, sqlType);

            return this;
        }

        public PreparedCallableQuery registerOutParameter(String parameterName, SQLType sqlType, int scale) throws SQLException {
            stmt.registerOutParameter(parameterName, sqlType, scale);

            return this;
        }

        public PreparedCallableQuery registerOutParameter(String parameterName, SQLType sqlType, String typeName) throws SQLException {
            stmt.registerOutParameter(parameterName, sqlType, typeName);

            return this;
        }

        public <E extends Exception> PreparedCallableQuery registerOutParameters(final OutParameterRegister<E> register) throws SQLException, E {
            register.register(stmt);

            return this;
        }

        /**
         * 
         * @param register
         * @return
         * @throws SQLException
         * @throws E
         * @deprecated
         */
        @Deprecated
        public <E extends Exception> PreparedCallableQuery registerOutParameters(final BiOutParameterRegister<E> register) throws SQLException, E {
            register.register(stmt, this);

            return this;
        }
    }

    public static interface RecordGetter<T, E extends Exception> {
        public static final RecordGetter<Object, RuntimeException> SINGLE = new RecordGetter<Object, RuntimeException>() {
            @Override
            public Object apply(ResultSet rs) throws SQLException, RuntimeException {
                return rs.getObject(1);
            }
        };

        public static final RecordGetter<Object[], RuntimeException> ARRAY = new RecordGetter<Object[], RuntimeException>() {
            @Override
            public Object[] apply(ResultSet rs) throws SQLException, RuntimeException {
                final int columnCount = rs.getMetaData().getColumnCount();
                final Object[] result = new Object[columnCount];

                for (int i = 1; i <= columnCount; i++) {
                    result[i - 1] = rs.getObject(i);
                }

                return result;
            }
        };

        public static final RecordGetter<List<Object>, RuntimeException> LIST = new RecordGetter<List<Object>, RuntimeException>() {
            @Override
            public List<Object> apply(ResultSet rs) throws SQLException, RuntimeException {
                final int columnCount = rs.getMetaData().getColumnCount();
                final List<Object> result = new ArrayList<>(columnCount);

                for (int i = 1; i <= columnCount; i++) {
                    result.add(rs.getObject(i));
                }

                return result;
            }
        };

        public static final RecordGetter<Map<String, Object>, RuntimeException> MAP = new RecordGetter<Map<String, Object>, RuntimeException>() {
            @Override
            public Map<String, Object> apply(final ResultSet rs) throws SQLException, RuntimeException {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                final int columnCount = columnLabels.size();
                final Map<String, Object> result = new HashMap<>(columnCount);

                for (int i = 1; i <= columnCount; i++) {
                    result.put(columnLabels.get(i - 1), rs.getObject(i));
                }

                return result;
            }
        };

        public static final RecordGetter<Map<String, Object>, RuntimeException> LINKED_HASH_MAP = new RecordGetter<Map<String, Object>, RuntimeException>() {
            @Override
            public Map<String, Object> apply(final ResultSet rs) throws SQLException, RuntimeException {
                final List<String> columnLabels = JdbcUtil.getColumnLabelList(rs);
                final int columnCount = columnLabels.size();
                final Map<String, Object> result = new LinkedHashMap<>(columnCount);

                for (int i = 1; i <= columnCount; i++) {
                    result.put(columnLabels.get(i - 1), rs.getObject(i));
                }

                return result;
            }
        };

        T apply(ResultSet rs) throws SQLException, E;
    }

    public static interface BiRecordGetter<T, E extends Exception> {
        public static final BiRecordGetter<Object, RuntimeException> SINGLE = new BiRecordGetter<Object, RuntimeException>() {
            @Override
            public Object apply(final ResultSet rs, final List<String> columnLabels) throws SQLException, RuntimeException {
                return rs.getObject(1);
            }
        };

        public static final BiRecordGetter<Object[], RuntimeException> ARRAY = new BiRecordGetter<Object[], RuntimeException>() {
            @Override
            public Object[] apply(final ResultSet rs, final List<String> columnLabels) throws SQLException, RuntimeException {
                final int columnCount = columnLabels.size();
                final Object[] result = new Object[columnCount];

                for (int i = 1; i <= columnCount; i++) {
                    result[i - 1] = rs.getObject(i);
                }

                return result;
            }
        };

        public static final BiRecordGetter<List<Object>, RuntimeException> LIST = new BiRecordGetter<List<Object>, RuntimeException>() {
            @Override
            public List<Object> apply(final ResultSet rs, final List<String> columnLabels) throws SQLException, RuntimeException {
                final int columnCount = columnLabels.size();
                final List<Object> result = new ArrayList<>(columnCount);

                for (int i = 1; i <= columnCount; i++) {
                    result.add(rs.getObject(i));
                }

                return result;
            }
        };

        public static final BiRecordGetter<Map<String, Object>, RuntimeException> MAP = new BiRecordGetter<Map<String, Object>, RuntimeException>() {
            @Override
            public Map<String, Object> apply(final ResultSet rs, final List<String> columnLabels) throws SQLException, RuntimeException {
                final int columnCount = columnLabels.size();
                final Map<String, Object> result = new HashMap<>(columnCount);

                for (int i = 1; i <= columnCount; i++) {
                    result.put(columnLabels.get(i - 1), rs.getObject(i));
                }

                return result;
            }
        };

        public static final BiRecordGetter<Map<String, Object>, RuntimeException> LINKED_HASH_MAP = new BiRecordGetter<Map<String, Object>, RuntimeException>() {
            @Override
            public Map<String, Object> apply(final ResultSet rs, final List<String> columnLabels) throws SQLException, RuntimeException {
                final int columnCount = columnLabels.size();
                final Map<String, Object> result = new LinkedHashMap<>(columnCount);

                for (int i = 1; i <= columnCount; i++) {
                    result.put(columnLabels.get(i - 1), rs.getObject(i));
                }

                return result;
            }
        };

        T apply(ResultSet rs, List<String> columnLabels) throws SQLException, E;
    }

    public static interface RecordConsumer<E extends Exception> {
        void accept(ResultSet rs) throws SQLException, E;
    }

    public static interface BiRecordConsumer<E extends Exception> {
        void accept(ResultSet rs, List<String> columnLabels) throws SQLException, E;
    }

    public static interface RecordPredicate<E extends Exception> {
        boolean test(ResultSet rs) throws SQLException, E;
    }

    public static interface BiRecordPredicate<E extends Exception> {
        boolean test(ResultSet rs, List<String> columnLabels) throws SQLException, E;
    }

    public static interface StatementSetter<S extends PreparedStatement, E extends Exception> {
        void set(S stmt) throws SQLException, E;
    }

    /**
     * 
     * @author haiyangl
     *
     * @param <S>
     * @param <Q>
     * @param <E>
     * @deprecated
     */
    @Deprecated
    public static interface BiStatementSetter<S extends PreparedStatement, Q extends AbstractPreparedQuery<?, ?>, E extends Exception> {
        void set(S stmt, Q query) throws SQLException, E;
    }

    public static interface StatementGetter<T, S extends PreparedStatement, E extends Exception> {
        T apply(S stmt) throws SQLException, E;
    }

    public static interface StatementConsumer<S extends PreparedStatement, E extends Exception> {
        void accept(S stmt) throws SQLException, E;
    }

    public static interface OutParameterRegister<E extends Exception> {
        void register(CallableStatement stmt) throws SQLException, E;
    }

    /**
     * 
     * @author haiyangl
     *
     * @param <E>
     * @deprecated
     */
    @Deprecated
    public static interface BiOutParameterRegister<E extends Exception> {
        void register(CallableStatement stmt, PreparedCallableQuery query) throws SQLException, E;
    }

}
