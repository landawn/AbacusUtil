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
import java.sql.CallableStatement;
import java.sql.Connection;
import java.sql.Driver;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.SQLFeatureNotSupportedException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import javax.xml.parsers.DocumentBuilder;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.DataSource;
import com.landawn.abacus.DataSourceManager;
import com.landawn.abacus.DataSourceSelector;
import com.landawn.abacus.IsolationLevel;
import com.landawn.abacus.SliceSelector;
import com.landawn.abacus.core.AbacusConfiguration;
import com.landawn.abacus.core.AbacusConfiguration.DataSourceConfiguration;
import com.landawn.abacus.core.AbacusConfiguration.DataSourceManagerConfiguration;
import com.landawn.abacus.core.RowDataSet;
import com.landawn.abacus.core.sql.dataSource.NonSliceSelector;
import com.landawn.abacus.core.sql.dataSource.SQLDataSource;
import com.landawn.abacus.core.sql.dataSource.SQLDataSourceManager;
import com.landawn.abacus.core.sql.dataSource.SimpleSourceSelector;
import com.landawn.abacus.dataChannel.DataChannel;
import com.landawn.abacus.dataChannel.StatementDataChannel;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.AbacusIOException;
import com.landawn.abacus.exception.AbacusSQLException;
import com.landawn.abacus.exception.ParseException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.SQLExecutor.StatementSetter;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
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

    private static final StatementSetter DEFAULT_STATEMENT_SETTER = new StatementSetter() {
        @Override
        public void setParameters(NamedSQL namedSQL, PreparedStatement stmt, Object... parameters) throws SQLException {
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

    public static DBVersion getDBVersion(final Connection conn) {
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
            throw new AbacusSQLException(e);
        }
    }

    /**
     * 
     * @param dataSourceXmlFile
     * @return DataSourceManager
     * 
     * @see DataSource.xsd
     */
    public static DataSourceManager createDataSourceManager(final String dataSourceXmlFile) {
        InputStream is = null;
        try {
            is = new FileInputStream(Configuration.findFile(dataSourceXmlFile));
            return createDataSourceManager(is, dataSourceXmlFile);
        } catch (IOException e) {
            throw new AbacusIOException(e);
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
    public static DataSourceManager createDataSourceManager(final InputStream dataSourceXmlInputStream) {
        return createDataSourceManager(dataSourceXmlInputStream, CURRENT_DIR_PATH);
    }

    private static DataSourceManager createDataSourceManager(final InputStream dataSourceXmlInputStream, final String dataSourceXmlFile) {
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
            throw new AbacusIOException(e);
        }
    }

    public static DataSource createDataSource(final String dataSourceFile) {
        InputStream is = null;
        try {
            is = new FileInputStream(Configuration.findFile(dataSourceFile));
            return createDataSource(is, dataSourceFile);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.close(is);
        }
    }

    public static DataSource createDataSource(final InputStream dataSourceInputStream) {
        return createDataSource(dataSourceInputStream, CURRENT_DIR_PATH);
    }

    private static DataSource createDataSource(final InputStream dataSourceInputStream, final String dataSourceFile) {
        final String dataSourceString = IOUtil.readString(dataSourceInputStream);

        if (CURRENT_DIR_PATH.equals(dataSourceFile) || dataSourceFile.endsWith(".xml")) {
            try {
                return createDataSourceManager(new ByteArrayInputStream(dataSourceString.getBytes())).getPrimaryDataSource();
            } catch (ParseException e) {
                // ignore.
            } catch (AbacusIOException e) {
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
            throw new AbacusIOException(e);
        }

        return new SQLDataSource(newProps);
    }

    public static DataSource createDataSource(final String url, final String user, final String password) {
        return createDataSource(getDriverClasssByUrl(url), url, user, password);
    }

    public static DataSource createDataSource(final String driver, final String url, final String user, final String password) {
        final Class<? extends Driver> driverClass = N.forClass(driver);

        return createDataSource(driverClass, url, user, password);
    }

    public static DataSource createDataSource(final Class<? extends Driver> driverClass, final String url, final String user, final String password) {
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
    public static DataSource createDataSource(final Map<String, ?> props) {
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

    public static Connection createConnection(final String url, final String user, final String password) {
        return createConnection(getDriverClasssByUrl(url), url, user, password);
    }

    private static Class<? extends Driver> getDriverClasssByUrl(final String url) {
        Class<? extends Driver> driverClass = null;
        // jdbc:mysql://localhost:3306/abacustest
        if (url.indexOf("mysql") > 0 || N.indexOfIgnoreCase(url, "mysql") > 0) {
            driverClass = N.forClass("com.mysql.jdbc.Driver");
            // jdbc:postgresql://localhost:5432/abacustest
        } else if (url.indexOf("postgresql") > 0 || N.indexOfIgnoreCase(url, "postgresql") > 0) {
            driverClass = N.forClass("org.postgresql.Driver");
            // jdbc:h2:hsql://<host>:<port>/<database>
        } else if (url.indexOf("h2") > 0 || N.indexOfIgnoreCase(url, "h2") > 0) {
            driverClass = N.forClass("org.h2.Driver");
            // jdbc:hsqldb:hsql://localhost/abacustest
        } else if (url.indexOf("hsqldb") > 0 || N.indexOfIgnoreCase(url, "hsqldb") > 0) {
            driverClass = N.forClass("org.hsqldb.jdbc.JDBCDriver");
            // jdbc.url=jdbc:oracle:thin:@localhost:1521:abacustest
        } else if (url.indexOf("oracle") > 0 || N.indexOfIgnoreCase(url, "oracle") > 0) {
            driverClass = N.forClass("oracle.jdbc.driver.OracleDriver");
            // jdbc.url=jdbc:sqlserver://localhost:1433;Database=abacustest
        } else if (url.indexOf("sqlserver") > 0 || N.indexOfIgnoreCase(url, "sqlserver") > 0) {
            driverClass = N.forClass("com.microsoft.sqlserver.jdbc.SQLServerDrive");
            // jdbc:db2://localhost:50000/abacustest
        } else if (url.indexOf("db2") > 0 || N.indexOfIgnoreCase(url, "db2") > 0) {
            driverClass = N.forClass("com.ibm.db2.jcc.DB2Driver");
        } else {
            throw new AbacusException(
                    "Can not identity the driver class by url: " + url + ". Only mysql, postgresql, hsqldb, sqlserver, oracle and db2 are supported currently");
        }
        return driverClass;
    }

    public static Connection createConnection(final String driverClass, final String url, final String user, final String password) {
        Class<? extends Driver> cls = N.forClass(driverClass);
        return createConnection(cls, url, user, password);
    }

    public static Connection createConnection(final Class<? extends Driver> driverClass, final String url, final String user, final String password) {
        try {
            DriverManager.registerDriver(N.newInstance(driverClass));

            return DriverManager.getConnection(url, user, password);
        } catch (SQLException e) {
            throw new AbacusSQLException("Failed to close create connection", e);
        }
    }

    public static void close(final ResultSet rs) {
        if (rs != null) {
            try {
                rs.close();
            } catch (SQLException e) {
                throw new AbacusSQLException(e);
            }
        }
    }

    public static void close(final Statement stmt) {
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
                throw new AbacusSQLException(e);
            }
        }
    }

    public static void close(final Connection conn) {
        if (conn != null) {
            try {
                conn.close();
            } catch (SQLException e) {
                throw new AbacusSQLException(e);
            }
        }
    }

    public static void close(final ResultSet rs, final Statement stmt) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
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
                throw new AbacusSQLException(e);
            }
        }
    }

    public static void close(final Statement stmt, final Connection conn) {
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
            throw new AbacusSQLException(e);
        } finally {
            try {
                if (conn != null) {
                    conn.close();
                }
            } catch (SQLException e) {
                throw new AbacusSQLException(e);
            }
        }
    }

    public static void close(final ResultSet rs, final Statement stmt, final Connection conn) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
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
                throw new AbacusSQLException(e);
            } finally {
                try {
                    if (conn != null) {
                        conn.close();
                    }
                } catch (SQLException e) {
                    throw new AbacusSQLException(e);
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
            } catch (Throwable e) {
                logger.error("Failed to close ResultSet", e);
            }
        }

        if (stmt != null) {
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

        if (conn != null) {
            try {
                conn.close();
            } catch (Throwable e) {
                logger.error("Failed to close Connection", e);
            }
        }
    }

    public static SQLTransaction beginTransaction(final Connection conn, final IsolationLevel isolationLevel) {
        if (isolationLevel == null) {
            throw new IllegalArgumentException("The parameter isolationLevel can't be null");
        }

        return new SQLTransaction(conn, isolationLevel);
    }

    public static PreparedStatement prepareStatement(final Connection conn, final String sql, final Object... parameters) throws SQLException {
        final NamedSQL namedSQL = NamedSQL.parse(sql);
        final PreparedStatement stmt = conn.prepareStatement(namedSQL.getPureSQL());

        if (N.notNullOrEmpty(parameters)) {
            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
        }

        return stmt;
    }

    //    static PreparedStatement prepareStatement(final Connection conn, final String sql, final List<?> parameters) throws SQLException {
    //        final NamedSQL namedSQL = NamedSQL.parse(sql);
    //        final PreparedStatement stmt = conn.prepareStatement(namedSQL.getPureSQL());
    //
    //        if (N.notNullOrEmpty(parameters)) {
    //            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
    //        }
    //
    //        return stmt;
    //    }

    public static CallableStatement prepareCall(final Connection conn, final String sql, final Object... parameters) throws SQLException {
        final NamedSQL namedSQL = NamedSQL.parse(sql);
        final CallableStatement stmt = conn.prepareCall(namedSQL.getPureSQL());

        if (N.notNullOrEmpty(parameters)) {
            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
        }

        return stmt;
    }

    //    static CallableStatement prepareCall(final Connection conn, final String sql, final List<?> parameters) throws SQLException {
    //        final NamedSQL namedSQL = NamedSQL.parse(sql);
    //        final CallableStatement stmt = conn.prepareCall(namedSQL.getPureSQL());
    //
    //        if (N.notNullOrEmpty(parameters)) {
    //            SQLExecutor.DEFAULT_STATEMENT_SETTER.setParameters(namedSQL, stmt, parameters);
    //        }
    //
    //        return stmt;
    //    }

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

    public static DataSet executeQuery(final Connection conn, final String sql, final Object... parameters) {
        PreparedStatement stmt = null;
        ResultSet rs = null;

        try {
            stmt = prepareStatement(conn, sql, parameters);
            rs = stmt.executeQuery();

            return extractData(rs);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(rs, stmt);
        }
    }

    //    static DataSet executeQuery(final Connection conn, final String sql, final List<?> parameters) {
    //        PreparedStatement stmt = null;
    //        ResultSet rs = null;
    //
    //        try {
    //            stmt = prepareStatement(conn, sql, parameters);
    //            rs = stmt.executeQuery();
    //
    //            return extractData(rs);
    //        } catch (SQLException e) {
    //            throw new AbacusSQLException(e);
    //        } finally {
    //            closeQuietly(rs, stmt);
    //        }
    //    }

    public static DataSet executeQuery(final PreparedStatement stmt) {
        ResultSet rs = null;

        try {
            rs = stmt.executeQuery();

            return extractData(rs);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(rs);
        }
    }

    public static int executeUpdate(final Connection conn, final String sql, final Object... parameters) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, sql, parameters);

            return stmt.executeUpdate();
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    //    static int executeUpdate(final Connection conn, final String sql, final List<?> parameters) {
    //        PreparedStatement stmt = null;
    //
    //        try {
    //            stmt = prepareStatement(conn, sql, parameters);
    //
    //            return stmt.executeUpdate();
    //        } catch (SQLException e) {
    //            throw new AbacusSQLException(e);
    //        } finally {
    //            closeQuietly(stmt);
    //        }
    //    }

    public static int executeUpdate(final PreparedStatement stmt) {
        try {
            return stmt.executeUpdate();
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        }
    }

    public static boolean execute(final Connection conn, final String sql, final Object... parameters) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, sql, parameters);

            return stmt.execute();
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    //    static boolean execute(final Connection conn, final String sql, final List<?> parameters) {
    //        PreparedStatement stmt = null;
    //
    //        try {
    //            stmt = prepareStatement(conn, sql, parameters);
    //
    //            return stmt.execute();
    //        } catch (SQLException e) {
    //            throw new AbacusSQLException(e);
    //        } finally {
    //            closeQuietly(stmt);
    //        }
    //    }

    public static boolean execute(final PreparedStatement stmt) {
        try {
            return stmt.execute();
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        }
    }

    /**
     * 
     * @param rs
     */
    public static RowIterator rowIterator(final ResultSet rs) {
        return new RowIterator(rs);
    }

    public static DataSet extractData(final ResultSet rs) {
        return extractData(rs, false);
    }

    public static DataSet extractData(final ResultSet rs, final boolean closeResultSet) {
        return extractData(rs, 0, Integer.MAX_VALUE, closeResultSet);
    }

    public static DataSet extractData(final ResultSet rs, final int offset, final int count) {
        return extractData(rs, offset, count, false);
    }

    public static DataSet extractData(final ResultSet rs, final int offset, final int count, final boolean closeResultSet) {
        return extractData(rs, offset, count, null, closeResultSet);
    }

    public static DataSet extractData(final ResultSet rs, int offset, int count, final Predicate<ResultSet> filter, final boolean closeResultSet) {
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

            while (offset-- > 0 && rs.next()) {

            }

            while (count > 0 && rs.next()) {
                if (filter == null || filter.test(rs)) {
                    for (int i = 0; i < columnCount;) {
                        columnList.get(i).add(rs.getObject(++i));
                    }

                    count--;
                }
            }

            // return new RowDataSet(null, entityClass, columnNameList, columnList);
            return new RowDataSet(columnNameList, columnList);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            if (closeResultSet) {
                closeQuietly(rs);
            }
        }
    }

    public static String[] getColumnLabels(final ResultSet rs) throws SQLException {
        final ResultSetMetaData metaData = rs.getMetaData();
        final int columnCount = metaData.getColumnCount();
        final String[] labels = new String[columnCount];

        for (int i = 0; i < columnCount; i++) {
            labels[i] = metaData.getColumnLabel(i + 1);
        }

        return labels;
    }

    public static List<String> getColumnLabelList(final ResultSet rs) throws SQLException {
        final ResultSetMetaData metaData = rs.getMetaData();
        final int columnCount = metaData.getColumnCount();
        final List<String> labels = new ArrayList<>(columnCount);

        for (int i = 0; i < columnCount; i++) {
            labels.add(metaData.getColumnLabel(i + 1));
        }

        return labels;
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
     */
    public static int importData(final DataSet dataset, final Connection conn, final String insertSQL) {
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
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final Connection conn, final String insertSQL) {
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
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count, final Connection conn,
            final String insertSQL) {
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
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval) {
        return importData(dataset, selectColumnNames, offset, count, null, conn, insertSQL, batchSize, batchInterval);
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
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final Predicate<Object[]> filter, final Connection conn, final String insertSQL, final int batchSize, final int batchInterval) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(dataset, selectColumnNames, offset, count, filter, stmt, batchSize, batchInterval);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
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
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Connection conn, final String insertSQL, final Map<String, ? extends Type> columnTypeMap) {
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
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL,
            final Map<String, ? extends Type> columnTypeMap) {
        return importData(dataset, offset, count, conn, insertSQL, columnTypeMap, 200, 0);
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
     * @param batchSize
     * @param batchInterval
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL,
            final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval) {
        return importData(dataset, offset, count, null, conn, insertSQL, columnTypeMap, batchSize, batchInterval);
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
     * @param columnTypeMap
     * @param batchSize
     * @param batchInterval
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final Predicate<Object[]> filter, final Connection conn,
            final String insertSQL, final Map<String, ? extends Type> columnTypeMap, final int batchSize, final int batchInterval) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(dataset, offset, count, filter, stmt, batchSize, batchInterval, columnTypeMap);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
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
     */
    public static int importData(final DataSet dataset, final Connection conn, final String insertSQL,
            final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
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
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL,
            final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
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
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final Connection conn, final String insertSQL, final int batchSize,
            final int batchInterval, final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
        return importData(dataset, offset, count, null, conn, insertSQL, batchSize, batchInterval, stmtSetter);
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
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final Predicate<Object[]> filter, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval, final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(dataset, offset, count, filter, stmt, batchSize, batchInterval, stmtSetter);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
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
     */
    public static int importData(final DataSet dataset, final PreparedStatement stmt) {
        return importData(dataset, dataset.columnNameList(), stmt);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param selectColumnNames
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final PreparedStatement stmt) {
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
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final PreparedStatement stmt) {
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
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final PreparedStatement stmt, final int batchSize, final int batchInterval) {
        return importData(dataset, selectColumnNames, offset, count, null, stmt, batchSize, batchInterval);
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
     */
    public static int importData(final DataSet dataset, final Collection<String> selectColumnNames, final int offset, final int count,
            final Predicate<Object[]> filter, final PreparedStatement stmt, final int batchSize, final int batchInterval) {
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
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final PreparedStatement stmt, final Map<String, ? extends Type> columnTypeMap) {
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
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt,
            final Map<String, ? extends Type> columnTypeMap) {
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
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Map<String, ? extends Type> columnTypeMap) {
        return importData(dataset, offset, count, null, stmt, batchSize, batchInterval, columnTypeMap);
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
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final int offset, final int count, final Predicate<Object[]> filter, final PreparedStatement stmt,
            final int batchSize, final int batchInterval, final Map<String, ? extends Type> columnTypeMap) {
        if (((offset < 0) || (count < 0) || batchSize < 0) || (batchInterval < 0)) {
            throw new IllegalArgumentException("'offset', 'count' 'batchSize' and 'batchInterval' can't be negative number");
        }

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

            final DataChannel dc = new StatementDataChannel(stmt);
            final Object[] row = filter == null ? null : new Object[columnCount];
            for (int i = offset, size = dataset.size(); result < count && i < size; i++) {
                dataset.absolute(i);

                if (filter == null) {
                    for (int j = 0; j < columnCount; j++) {
                        columnTypes[j].set(dc, j, dataset.get(columnIndexes[j]));
                    }
                } else {
                    for (int j = 0; j < columnCount; j++) {
                        row[j] = dataset.get(columnIndexes[j]);
                    }

                    if (filter.test(row) == false) {
                        continue;
                    }

                    for (int j = 0; j < columnCount; j++) {
                        columnTypes[j].set(dc, j, row[j]);
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
            throw new AbacusSQLException(e);
        }

        return result;
    }

    public static int importData(final DataSet dataset, final PreparedStatement stmt,
            final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
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
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt,
            final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
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
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
        return importData(dataset, offset, count, null, stmt, batchSize, batchInterval, stmtSetter);
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
     */
    public static int importData(final DataSet dataset, final int offset, final int count, final Predicate<Object[]> filter, final PreparedStatement stmt,
            final int batchSize, final int batchInterval, final BiConsumer<? super PreparedStatement, ? super Object[]> stmtSetter) {
        if (((offset < 0) || (count < 0) || batchSize < 0) || (batchInterval < 0)) {
            throw new IllegalArgumentException("'offset', 'count' 'batchSize' and 'batchInterval' can't be negative number");
        }

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
            throw new AbacusSQLException(e);
        }

        return result;
    }

    public static long importData(final File file, final Connection conn, final String insertSQL, final Function<String, Object[]> func) {
        return importData(file, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static long importData(final File file, final long offset, final long count, final Connection conn, final String insertSQL, final int batchSize,
            final int batchInterval, final Function<String, Object[]> func) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(file, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static long importData(final File file, final PreparedStatement stmt, final Function<String, Object[]> func) {
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
     */
    public static long importData(final File file, final long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Function<String, Object[]> func) {
        Reader reader = null;

        try {
            reader = new FileReader(file);

            return importData(reader, offset, count, stmt, batchSize, batchInterval, func);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            IOUtil.close(reader);
        }
    }

    public static long importData(final InputStream is, final Connection conn, final String insertSQL, final Function<String, Object[]> func) {
        return importData(is, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static long importData(final InputStream is, final long offset, final long count, final Connection conn, final String insertSQL, final int batchSize,
            final int batchInterval, final Function<String, Object[]> func) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(is, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static long importData(final InputStream is, final PreparedStatement stmt, final Function<String, Object[]> func) {
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
     */
    public static long importData(final InputStream is, final long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Function<String, Object[]> func) {
        final Reader reader = new InputStreamReader(is);

        return importData(reader, offset, count, stmt, batchSize, batchInterval, func);
    }

    public static long importData(final Reader reader, final Connection conn, final String insertSQL, final Function<String, Object[]> func) {
        return importData(reader, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static long importData(final Reader reader, final long offset, final long count, final Connection conn, final String insertSQL, final int batchSize,
            final int batchInterval, final Function<String, Object[]> func) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(reader, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static long importData(final Reader reader, final PreparedStatement stmt, final Function<String, Object[]> func) {
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
     */
    public static long importData(final Reader reader, long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Function<String, Object[]> func) {
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
            throw new AbacusSQLException(e);
        } catch (IOException e) {
            throw new AbacusIOException(e);
        } finally {
            ObjectFactory.recycle(br);
        }

        return result;
    }

    public static <T> long importData(final Iterator<T> iter, final Connection conn, final String insertSQL, final Function<T, Object[]> func) {
        return importData(iter, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static <T> long importData(final Iterator<T> iter, final long offset, final long count, final Connection conn, final String insertSQL,
            final int batchSize, final int batchInterval, final Function<T, Object[]> func) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(iter, offset, count, stmt, batchSize, batchInterval, func);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static <T> long importData(final Iterator<T> iter, final PreparedStatement stmt, final Function<T, Object[]> func) {
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
     */
    public static <T> long importData(final Iterator<T> iter, long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final Function<T, Object[]> func) {
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
            throw new AbacusSQLException(e);
        }

        return result;
    }

    public static <T> long importData(final Iterator<T> iter, final Connection conn, final String insertSQL,
            final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
        return importData(iter, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, stmtSetter);
    }

    public static <T> long importData(final Iterator<T> iter, final long offset, final long count, final Connection conn, final String insertSQL,
            final int batchSize, final int batchInterval, final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
        return importData(iter, offset, count, null, conn, insertSQL, batchSize, batchInterval, stmtSetter);
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
     */
    public static <T> long importData(final Iterator<T> iter, final long offset, final long count, final Predicate<? super T> filter, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval, final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
        PreparedStatement stmt = null;

        try {
            stmt = prepareStatement(conn, insertSQL);

            return importData(iter, offset, count, filter, stmt, batchSize, batchInterval, stmtSetter);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            JdbcUtil.closeQuietly(stmt);
        }
    }

    public static <T> long importData(final Iterator<T> iter, final PreparedStatement stmt, final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
        return importData(iter, 0, Long.MAX_VALUE, stmt, 200, 0, stmtSetter);
    }

    public static <T> long importData(final Iterator<T> iter, long offset, final long count, final PreparedStatement stmt, final int batchSize,
            final int batchInterval, final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
        return importData(iter, offset, count, null, stmt, batchSize, batchInterval, stmtSetter);
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
     * @param func convert element to the parameters for record insert. Returns a <code>null</code> array to skip the line. 
     * @return
     */
    public static <T> long importData(final Iterator<T> iter, long offset, final long count, final Predicate<? super T> filter, final PreparedStatement stmt,
            final int batchSize, final int batchInterval, final BiConsumer<? super PreparedStatement, ? super T> stmtSetter) {
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
            throw new AbacusSQLException(e);
        }

        return result;
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final Consumer<Object[]> rowParser) {
        parse(conn, sql, 0, Long.MAX_VALUE, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param offset
     * @param count
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final long offset, final long count, final Consumer<Object[]> rowParser) {
        parse(conn, sql, offset, count, 0, 0, rowParser);
    }

    //    /**
    //     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param conn
    //     * @param sql
    //     * @param processThreadNumber thread number used to parse/process the lines/records
    //     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    @Deprecated
    //    static void parse(final Connection conn, final String sql, final int processThreadNumber, final int queueSize, final Consumer<Object[]> rowParser) {
    //        parse(conn, sql, 0, Long.MAX_VALUE, processThreadNumber, queueSize, rowParser);
    //    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param offset
     * @param count
     * @param processThreadNumber thread number used to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final long offset, final long count, final int processThreadNumber, final int queueSize,
            final Consumer<Object[]> rowParser) {
        PreparedStatement stmt = null;
        try {
            stmt = prepareStatement(conn, sql);

            stmt.setFetchSize(200);

            parse(stmt, offset, count, processThreadNumber, queueSize, rowParser);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    //    /**
    //     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param conn
    //     * @param sql
    //     * @param parameters
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    static void parse(final Connection conn, final String sql, final List<?> parameters, final Consumer<Object[]> rowParser) {
    //        parse(conn, sql, parameters, 0, Long.MAX_VALUE, rowParser);
    //    }
    //
    //    /**
    //     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param conn
    //     * @param sql
    //     * @param parameters
    //     * @param offset
    //     * @param count
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    static void parse(final Connection conn, final String sql, final List<?> parameters, final long offset, final long count,
    //            final Consumer<Object[]> rowParser) {
    //        parse(conn, sql, parameters, offset, count, 0, 0, rowParser);
    //    }
    //
    //    /**
    //     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param conn
    //     * @param sql
    //     * @param parameters
    //     * @param processThreadNumber thread number used to parse/process the lines/records
    //     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    @Deprecated
    //    static void parse(final Connection conn, final String sql, final List<?> parameters, final int processThreadNumber, final int queueSize,
    //            final Consumer<Object[]> rowParser) {
    //        parse(conn, sql, parameters, 0, Long.MAX_VALUE, processThreadNumber, queueSize, rowParser);
    //    }
    //
    //    /**
    //     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param conn
    //     * @param sql
    //     * @param parameters
    //     * @param offset
    //     * @param count
    //     * @param processThreadNumber thread number used to parse/process the lines/records
    //     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    static void parse(final Connection conn, final String sql, final List<?> parameters, final long offset, final long count, final int processThreadNumber,
    //            final int queueSize, final Consumer<Object[]> rowParser) {
    //        PreparedStatement stmt = null;
    //
    //        try {
    //            stmt = prepareStatement(conn, sql, parameters);
    //
    //            stmt.setFetchSize(200);
    //
    //            parse(stmt, offset, count, processThreadNumber, queueSize, rowParser);
    //        } catch (SQLException e) {
    //            throw new AbacusSQLException(e);
    //        } finally {
    //            closeQuietly(stmt);
    //        }
    //    }

    /**
     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param stmt
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final PreparedStatement stmt, final Consumer<Object[]> rowParser) {
        parse(stmt, 0, Long.MAX_VALUE, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param stmt
     * @param offset
     * @param count
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final PreparedStatement stmt, final long offset, final long count, final Consumer<Object[]> rowParser) {
        parse(stmt, offset, count, 0, 0, rowParser);
    }

    //    /**
    //     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param stmt
    //     * @param processThreadNumber thread number used to parse/process the lines/records
    //     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    @Deprecated
    //    static void parse(final PreparedStatement stmt, final int processThreadNumber, final int queueSize, final Consumer<Object[]> rowParser) {
    //        parse(stmt, 0, Long.MAX_VALUE, processThreadNumber, queueSize, rowParser);
    //    }

    /**
     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param stmt
     * @param offset
     * @param count
     * @param processThreadNumber thread number used to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final PreparedStatement stmt, final long offset, final long count, final int processThreadNumber, final int queueSize,
            final Consumer<Object[]> rowParser) {
        ResultSet rs = null;

        try {
            rs = stmt.executeQuery();

            parse(rs, offset, count, processThreadNumber, queueSize, rowParser);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(rs);
        }
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param rs
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final ResultSet rs, final Consumer<Object[]> rowParser) {
        parse(rs, 0, Long.MAX_VALUE, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param rs
     * @param offset
     * @param count
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final ResultSet rs, long offset, long count, final Consumer<Object[]> rowParser) {
        parse(rs, offset, count, 0, 0, rowParser);
    }

    //    /**
    //     * Parse the specified ResultSet.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param rs
    //     * @param processThreadNumber thread number used to parse/process the lines/records
    //     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    @Deprecated
    //    static void parse(final ResultSet rs, final int processThreadNumber, final int queueSize, final Consumer<Object[]> rowParser) {
    //        parse(rs, 0, Long.MAX_VALUE, processThreadNumber, queueSize, rowParser);
    //    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param rs
     * @param offset
     * @param count
     * @param processThreadNumber thread number used to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final ResultSet rs, long offset, long count, final int processThreadNumber, final int queueSize,
            final Consumer<Object[]> rowParser) {
        parseII(new RowIterator(rs), offset, count, processThreadNumber, queueSize, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param iter must not return <code>null</code> because <code>null</code> will be set automatically to identify the end of lines/rows.
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final RowIterator iter, final Consumer<Object[]> rowParser) {
        parse(iter, 0, Long.MAX_VALUE, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param iter must not return <code>null</code> because <code>null</code> will be set automatically to identify the end of lines/rows.
     * @param offset
     * @param count
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final RowIterator iter, long offset, long count, final Consumer<Object[]> rowParser) {
        parse(iter, offset, count, 0, 0, rowParser);
    }

    //    /**
    //     * Parse the specified ResultSet.
    //     * The last row will always be null to identity the ending of row set even offset/count is specified.
    //     * 
    //     * @param iter must not return <code>null</code> because <code>null</code> will be set automatically to identify the end of lines/rows.
    //     * @param processThreadNumber thread number used to parse/process the lines/records
    //     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
    //     * @param rowParser always remember to handle row <code>null</code>
    //     */
    //    @Deprecated
    //    static void parse(final RowIterator iter, final int processThreadNumber, final int queueSize, final Consumer<Object[]> rowParser) {
    //        parse(iter, 0, Long.MAX_VALUE, processThreadNumber, queueSize, rowParser);
    //    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param iter must not return <code>null</code> because <code>null</code> will be set automatically to identify the end of lines/rows.
     * @param offset
     * @param count
     * @param processThreadNumber thread number used to parse/process the lines/records
     * @param queueSize size of queue to save the processing records/lines loaded from source data. Default size is 1024.
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final RowIterator iter, long offset, long count, final int processThreadNumber, final int queueSize,
            final Consumer<Object[]> rowParser) {
        parseII(iter, offset, count, processThreadNumber, queueSize, rowParser);
    }

    public static void parse(final Collection<? extends RowIterator> iterators, final Consumer<Object[]> elementParser) {
        if (N.isNullOrEmpty(iterators)) {
            return;
        }

        parse(iterators, 0, Long.MAX_VALUE, elementParser);
    }

    public static void parse(final Collection<? extends RowIterator> iterators, final long offset, final long count, final Consumer<Object[]> elementParser) {
        if (N.isNullOrEmpty(iterators)) {
            return;
        }

        parse(iterators, offset, count, 0, 0, 0, elementParser);
    }

    public static void parse(final Collection<? extends RowIterator> iterators, final int readThreadNumber, final int processThreadNumber, final int queueSize,
            final Consumer<Object[]> elementParser) {
        parse(iterators, 0, Long.MAX_VALUE, readThreadNumber, processThreadNumber, queueSize, elementParser);
    }

    public static void parse(final Collection<? extends RowIterator> iterators, final long offset, final long count, final int readThreadNumber,
            final int processThreadNumber, final int queueSize, final Consumer<Object[]> elementParser) {
        if (N.isNullOrEmpty(iterators)) {
            return;
        }

        IOUtil.parse(iterators, offset, count, readThreadNumber, processThreadNumber, queueSize, elementParser);
    }

    private static void parseII(final RowIterator iter, long offset, long count, final int processThreadNumber, final int queueSize,
            final Consumer<Object[]> rowParser) {
        while (offset-- > 0 && iter.moveToNext()) {
        }

        if (processThreadNumber == 0) {
            while (count-- > 0 && iter.hasNext()) {
                rowParser.accept(iter.next());
            }

            rowParser.accept(null);
        } else {
            try (final Stream<Object[]> stream = Stream.parallelConcat2(N.asList(iter), 1, queueSize)) {
                final Iterator<Object[]> iteratorII = stream.limit(count).iterator();
                final ExecutorService executorService = Executors.newFixedThreadPool(processThreadNumber);
                final AtomicInteger activeThreadNum = new AtomicInteger();
                final Holder<Throwable> errorHolder = new Holder<Throwable>();

                for (int i = 0; i < processThreadNumber; i++) {
                    activeThreadNum.incrementAndGet();

                    executorService.execute(new Runnable() {
                        @Override
                        public void run() {
                            Object[] row = null;

                            try {
                                while (errorHolder.getValue() == null) {
                                    synchronized (iteratorII) {
                                        if (iteratorII.hasNext()) {
                                            row = iteratorII.next();
                                        } else {
                                            break;
                                        }
                                    }

                                    rowParser.accept(row);
                                }
                            } catch (Throwable e) {
                                synchronized (errorHolder) {
                                    if (errorHolder.value() == null) {
                                        errorHolder.setValue(e);
                                    } else {
                                        errorHolder.value().addSuppressed(e);
                                    }
                                }
                            } finally {
                                activeThreadNum.decrementAndGet();
                            }
                        }
                    });
                }

                while (activeThreadNum.get() > 0) {
                    N.sleep(1);
                }

                if (errorHolder.value() == null) {
                    try {
                        rowParser.accept(null);
                    } catch (Throwable e) {
                        errorHolder.setValue(e);
                    }
                }

                if (errorHolder.value() != null) {
                    throw N.toRuntimeException(errorHolder.value());
                }
            }
        }
    }

    public static long copy(final Connection sourceConn, final String selectSql, final Connection targetConn, final String insertSql) {
        return copy(sourceConn, selectSql, 200, 0, Integer.MAX_VALUE, targetConn, insertSql, DEFAULT_STATEMENT_SETTER, 200, 0, false);
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
     * @param parametersSetter
     * @param batchSize
     * @param batchInterval
     * @param inParallel do the read and write in separated threads.
     * @return
     */
    public static long copy(final Connection sourceConn, final String selectSql, final int fetchSize, final long offset, final long count,
            final Connection targetConn, final String insertSql, final StatementSetter parametersSetter, final int batchSize, final int batchInterval,
            final boolean inParallel) {
        PreparedStatement selectStmt = null;
        PreparedStatement insertStmt = null;

        int result = 0;

        try {
            insertStmt = targetConn.prepareStatement(insertSql);

            selectStmt = sourceConn.prepareStatement(selectSql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);
            selectStmt.setFetchSize(fetchSize);

            copy(selectStmt, offset, count, insertStmt, parametersSetter, batchSize, batchInterval, inParallel);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(selectStmt);
            closeQuietly(insertStmt);
        }

        return result;
    }

    public static long copy(final PreparedStatement selectStmt, final PreparedStatement insertStmt, final StatementSetter statementSetter) {
        return copy(selectStmt, 0, Integer.MAX_VALUE, insertStmt, statementSetter, 200, 0, false);
    }

    /**
     * 
     * @param selectStmt
     * @param offset
     * @param count
     * @param insertStmt
     * @param statementSetter
     * @param batchSize
     * @param batchInterval
     * @param inParallel do the read and write in separated threads.
     * @return
     */
    public static long copy(final PreparedStatement selectStmt, final long offset, final long count, final PreparedStatement insertStmt,
            final StatementSetter statementSetter, final int batchSize, final int batchInterval, final boolean inParallel) {

        if (((offset < 0) || (count < 0) || batchSize < 0) || (batchInterval < 0)) {
            throw new IllegalArgumentException("'offset', 'count' 'batchSize' and 'batchInterval' can't be negative number");
        }

        final StatementSetter parametersSetter = statementSetter == null ? DEFAULT_STATEMENT_SETTER : statementSetter;
        final AtomicLong result = new AtomicLong();

        final Consumer<Object[]> rowParser = new Consumer<Object[]>() {
            @Override
            public void accept(Object[] row) {
                try {
                    if (row == null) {
                        if ((result.longValue() % batchSize) > 0) {
                            insertStmt.executeBatch();
                            insertStmt.clearBatch();
                        }
                    } else {
                        parametersSetter.setParameters(null, insertStmt, row);

                        insertStmt.addBatch();
                        result.incrementAndGet();

                        if ((result.longValue() % batchSize) == 0) {
                            insertStmt.executeBatch();
                            insertStmt.clearBatch();

                            if (batchInterval > 0) {
                                N.sleep(batchInterval);
                            }
                        }
                    }
                } catch (SQLException e) {
                    throw new AbacusSQLException(e);
                }
            }
        };

        JdbcUtil.parse(selectStmt, offset, count, inParallel ? 1 : 0, DEFAULT_QUEUE_SIZE_FOR_ROW_PARSER, rowParser);

        return result.longValue();
    }

    public static boolean doesTableExist(final Connection conn, final String tableName) {
        try {
            executeQuery(conn, "SELECT 1 FROM " + tableName + " WHERE 1 > 2");

            return true;
        } catch (AbacusSQLException e) {
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
        } catch (AbacusSQLException e) {
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
        } catch (AbacusSQLException e) {
            // ignore.
        }

        return false;
    }

    /**
     * 
     * @param conn
     * @param tableName
     * @return
     */
    public static List<String> getColumnNameList(final Connection conn, final String tableName) {
        DataSet dataSet = executeQuery(conn, "SELECT * FROM " + tableName + " WHERE 1 > 2");
        List<String> columnNameList = new ArrayList<>(dataSet.columnNameList().size());

        for (String columName : dataSet.columnNameList()) {
            columnNameList.add(columName.intern());
        }

        return columnNameList;
    }

    static boolean isTableNotExistsException(final Throwable e) {
        if (e instanceof SQLException) {
            SQLException sqlException = (SQLException) e;

            if (sqlException.getSQLState() != null && sqlStateForTableNotExists.contains(sqlException.getSQLState())) {
                return true;
            }

            String msg = e.getMessage();
            return N.notNullOrEmpty(msg) && (msg.contains("not exist") || msg.contains("doesn't exist") || msg.contains("not found"));
        } else if (e instanceof AbacusSQLException) {
            AbacusSQLException sqlException = (AbacusSQLException) e;

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
                method = N.getDeclaredMethod(sqlDataSource.getClass(), "close");
            } catch (Throwable e) {

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
                throw new AbacusSQLException(e);
            }
        }

        @Override
        public Connection getReadOnlyConnection() {
            try {
                return sqlDataSource.getConnection();
            } catch (SQLException e) {
                throw new AbacusSQLException(e);
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
                    N.invokeMethod(sqlDataSource, closeMethod);
                } catch (Throwable e) {
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
}
