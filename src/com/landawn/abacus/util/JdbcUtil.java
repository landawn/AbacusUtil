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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentLinkedQueue;

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
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;

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

    private static final StatementSetter DEFAULT_STATEMENT_SETTER = new StatementSetter() {
        @Override
        public void setParameters(NamedSQL namedSQL, PreparedStatement stmt, Object... parameters) throws SQLException {
            for (int i = 0, len = parameters.length; i < len; i++) {
                stmt.setObject(i + 1, parameters[i]);
            }
        }
    };

    private static final Set<String> sqlStateForTableNotExists = N.newHashSet();

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
        return createDataSourceManager(dataSourceXmlInputStream, IOUtil.CURRENT_DIR_PATH);
    }

    private static DataSourceManager createDataSourceManager(final InputStream dataSourceXmlInputStream, final String dataSourceXmlFile) {
        DocumentBuilder domParser = XMLUtil.createDOMParser();
        Document doc = null;

        try {
            doc = domParser.parse(dataSourceXmlInputStream);

            Element rootElement = doc.getDocumentElement();

            final Map<String, String> props = N.newHashMap();
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
        return createDataSource(dataSourceInputStream, IOUtil.CURRENT_DIR_PATH);
    }

    private static DataSource createDataSource(final InputStream dataSourceInputStream, final String dataSourceFile) {
        final String dataSourceString = IOUtil.readString(dataSourceInputStream);

        if (IOUtil.CURRENT_DIR_PATH.equals(dataSourceFile) || dataSourceFile.endsWith(".xml")) {
            try {
                return createDataSourceManager(new ByteArrayInputStream(dataSourceString.getBytes())).getPrimaryDataSource();
            } catch (ParseException e) {
                // ignore.
            } catch (AbacusIOException e) {
                // ignore.
            }
        }

        final Map<String, String> newProps = N.newHashMap();
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

    public static DataSource createDataSource(final String driver, final String url, final String user, final String password) {
        final Class<? extends Driver> driverClass = N.forClass(driver);

        return createDataSource(driverClass, url, user, password);
    }

    public static DataSource createDataSource(final Class<? extends Driver> driverClass, final String url, final String user, final String password) {
        return createDataSource(driverClass, url, user, password, null);
    }

    public static DataSource createDataSource(final String driver, final String url, final String user, final String password, final Map<String, ?> props) {
        final Class<? extends Driver> driverClass = N.forClass(driver);

        return createDataSource(driverClass, url, user, password, props);
    }

    /**
     * 
     * @param driver
     * @param url
     * @param user
     * @param password
     * @param props
     *            refer to Connection.xsd for the supported properties.
     *            {@link com.landawn.abacus.core.AbacusConfiguration.EntityManagerConfiguration.DataSourceConfiguration}
     * @return
     */
    public static DataSource createDataSource(final Class<? extends Driver> driverClass, final String url, final String user, final String password,
            final Map<String, ?> props) {
        Map<String, Object> newProps = N.newHashMap();

        if (N.notNullOrEmpty(props)) {
            for (Map.Entry<String, ?> entry : props.entrySet()) {
                newProps.put(entry.getKey(), entry.getValue());
            }
        }

        newProps.put(DRIVER, driverClass.getCanonicalName());
        newProps.put(URL, url);
        newProps.put(USER, user);
        newProps.put(PASSWORD, password);

        return createDataSource(newProps);
    }

    /**
     * 
     * @param props
     *            refer to Connection.xsd for the supported properties.
     *            {@link com.landawn.abacus.core.AbacusConfiguration.EntityManagerConfiguration.DataSourceConfiguration}
     * @return
     */
    public static DataSource createDataSource(final Map<String, ?> props) {
        return new SQLDataSource(props);
    }

    public static DataSource wrap(final javax.sql.DataSource sqlDataSource) {
        return sqlDataSource instanceof DataSource ? ((DataSource) sqlDataSource) : new SimpleDataSource(sqlDataSource);
    }

    public static Connection createConnection(final String url, final String user, final String password) {
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

        return createConnection(driverClass, url, user, password);
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
     * @param conn
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
     * @param rs
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
        PreparedStatement stmt = conn.prepareStatement(sql);

        if (N.notNullOrEmpty(parameters)) {
            for (int i = 0; i < parameters.length; i++) {
                stmt.setObject(i + 1, parameters[i]);
            }
        }

        return stmt;
    }

    public static PreparedStatement prepareStatement(final Connection conn, final String sql, final List<?> parameters) throws SQLException {
        PreparedStatement stmt = conn.prepareStatement(sql);

        if (N.notNullOrEmpty(parameters)) {
            for (int i = 0; i < parameters.size(); i++) {
                stmt.setObject(i + 1, parameters.get(i));
            }
        }

        return stmt;
    }

    public static CallableStatement prepareCall(final Connection conn, final String sql, final Object... parameters) throws SQLException {
        CallableStatement stmt = conn.prepareCall(sql);

        if (N.notNullOrEmpty(parameters)) {
            for (int i = 0; i < parameters.length; i++) {
                stmt.setObject(i + 1, parameters[i]);
            }
        }

        return stmt;
    }

    public static CallableStatement prepareCall(final Connection conn, final String sql, final List<?> parameters) throws SQLException {
        CallableStatement stmt = conn.prepareCall(sql);

        if (N.notNullOrEmpty(parameters)) {
            for (int i = 0; i < parameters.size(); i++) {
                stmt.setObject(i + 1, parameters.get(i));
            }
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

    public static DataSet executeQuery(final Connection conn, final String sql, final List<?> parameters) {
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

    public static int executeUpdate(final Connection conn, final String sql, final List<?> parameters) {
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

    public static boolean execute(final Connection conn, final String sql, final List<?> parameters) {
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
        return extractData(rs, offset, count, closeResultSet, null);
    }

    public static DataSet extractData(final ResultSet rs, final int offset, final int count, final boolean closeResultSet, final Predicate<ResultSet> filter) {
        return extractData(null, rs, offset, count, closeResultSet, filter);
    }

    public static DataSet extractData(final Class<?> entityClass, final ResultSet rs) {
        return extractData(entityClass, rs, false);
    }

    public static DataSet extractData(final Class<?> entityClass, final ResultSet rs, final boolean closeResultSet) {
        return extractData(entityClass, rs, 0, Integer.MAX_VALUE, closeResultSet);
    }

    public static DataSet extractData(final Class<?> entityClass, final ResultSet rs, final int offset, final int count) {
        return extractData(entityClass, rs, offset, count, false);
    }

    public static DataSet extractData(final Class<?> entityClass, final ResultSet rs, int offset, int count, final boolean closeResultSet) {
        return extractData(entityClass, rs, offset, count, closeResultSet, null);
    }

    public static DataSet extractData(final Class<?> entityClass, final ResultSet rs, int offset, int count, final boolean closeResultSet,
            final Predicate<ResultSet> filter) {
        try {
            // TODO [performance improvement]. it will improve performance a lot if MetaData is cached.
            final ResultSetMetaData metaData = rs.getMetaData();
            final int columnCount = metaData.getColumnCount();
            final List<String> columnNameList = N.newArrayList(columnCount);
            final List<List<Object>> columnList = N.newArrayList(columnCount);

            for (int i = 0; i < columnCount;) {
                columnNameList.add(metaData.getColumnLabel(++i));
                columnList.add(N.newArrayList());
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

            return new RowDataSet(null, entityClass, columnNameList, columnList);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            if (closeResultSet) {
                closeQuietly(rs);
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
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
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
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre> 
     * @return
     */
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final Connection conn, final String insertSQL) {
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
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @return
     */
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final int offset, final int count, final Connection conn,
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
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>  
     * @param batchSize
     * @param batchInterval
     * @return
     */
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final int offset, final int count, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval) {
        return importData(dataset, selectColumnNames, offset, count, conn, insertSQL, batchSize, batchInterval, null);
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
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final int offset, final int count, final Connection conn,
            final String insertSQL, final int batchSize, final int batchInterval, final Predicate<Object[]> filter) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(insertSQL);

            return importData(dataset, selectColumnNames, offset, count, stmt, batchSize, batchInterval, filter);
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
     * @param columnTypeMap
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final Connection conn, final String insertSQL) {
        return importData(dataset, columnTypeMap, 0, dataset.size(), conn, insertSQL);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param columnTypeMap
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final int offset, final int count,
            final Connection conn, final String insertSQL) {
        return importData(dataset, columnTypeMap, offset, count, conn, insertSQL, 200, 0);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param columnTypeMap
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final int offset, final int count,
            final Connection conn, final String insertSQL, final int batchSize, final int batchInterval) {
        return importData(dataset, columnTypeMap, offset, count, conn, insertSQL, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param columnTypeMap
     * @param offset
     * @param count
     * @param conn
     * @param insertSQL the column order in the sql must be consistent with the column order in the DataSet. Here is sample about how to create the sql:
     * <pre><code>
        List<String> columnNameList = N.newArrayList(dataset.columnNameList());
        columnNameList.retainAll(yourSelectColumnNames);        
        String sql = RE.insert(columnNameList).into(tableName).sql();  
     * </code></pre>
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final int offset, final int count,
            final Connection conn, final String insertSQL, final int batchSize, final int batchInterval, final Predicate<Object[]> filter) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(insertSQL);

            return importData(dataset, columnTypeMap, offset, count, stmt, batchSize, batchInterval, filter);
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
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final PreparedStatement stmt) {
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
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final int offset, final int count, final PreparedStatement stmt) {
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
     * @param filter
     * @return
     */
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final int offset, final int count, final PreparedStatement stmt,
            final int batchSize, final int batchInterval) {
        return importData(dataset, selectColumnNames, offset, count, stmt, batchSize, batchInterval, null);
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
     * @param filter
     * @return
     */
    public static int importData(final DataSet dataset, final List<String> selectColumnNames, final int offset, final int count, final PreparedStatement stmt,
            final int batchSize, final int batchInterval, final Predicate<Object[]> filter) {
        final Type<?> objType = N.getType(Object.class);
        final Map<String, Type<?>> columnTypeMap = N.newHashMap();

        for (String propName : selectColumnNames) {
            columnTypeMap.put(propName, objType);
        }

        return importData(dataset, columnTypeMap, offset, count, stmt, batchSize, batchInterval, filter);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param columnTypeMap
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final PreparedStatement stmt) {
        return importData(dataset, columnTypeMap, 0, dataset.size(), stmt);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param columnTypeMap
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final int offset, final int count,
            final PreparedStatement stmt) {
        return importData(dataset, columnTypeMap, offset, count, stmt, 200, 0);
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
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final int offset, final int count,
            final PreparedStatement stmt, final int batchSize, final int batchInterval) {
        return importData(dataset, columnTypeMap, offset, count, stmt, batchSize, batchInterval, null);
    }

    /**
     * Imports the data from <code>DataSet</code> to database. 
     * 
     * @param dataset
     * @param columnTypeMap
     * @param offset
     * @param count
     * @param stmt the column order in the sql must be consistent with the column order in the DataSet.
     * @param batchSize
     * @param batchInterval
     * @param filter
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static int importData(final DataSet dataset, final Map<String, ? extends Type> columnTypeMap, final int offset, final int count,
            final PreparedStatement stmt, final int batchSize, final int batchInterval, final Predicate<Object[]> filter) {
        if (((offset < 0) || (count < 0) || batchSize < 0) || (batchInterval < 0)) {
            throw new IllegalArgumentException("'offset', 'count' 'batchSize' and 'batchInterval' can't be negative number");
        }

        int result = 0;

        try {
            final int columnCount = columnTypeMap.size();
            final List<String> columnNameList = dataset.columnNameList();
            final int[] columnIndexes = new int[columnCount];
            final Type<Object>[] columnTypes = new Type[columnCount];
            final Set<String> columnNameSet = N.newHashSet(columnCount);

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
                final List<String> keys = N.newArrayList(columnTypeMap.keySet());
                keys.removeAll(columnNameSet);
                throw new AbacusException(keys + " are not included in titles: " + N.toString(columnNameList));
            }

            final DataChannel dc = new StatementDataChannel(stmt);
            final Object[] row = filter == null ? null : new Object[columnCount];
            for (int i = offset; result < count; i++) {
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
            stmt = conn.prepareStatement(insertSQL);

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
            stmt = conn.prepareStatement(insertSQL);

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
            stmt = conn.prepareStatement(insertSQL);

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

    public static <T> long importData(final Iterator<T> iter, final Connection conn, final String insertSQL, final Function<T, Object[]> func) {
        return importData(iter, 0, Long.MAX_VALUE, conn, insertSQL, 200, 0, func);
    }

    public static <T> long importData(final Iterator<T> iter, final long offset, final long count, final Connection conn, final String insertSQL,
            final int batchSize, final int batchInterval, final Function<T, Object[]> func) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(insertSQL);

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
        parse(conn, sql, false, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final boolean inParallel, final Consumer<Object[]> rowParser) {
        parse(conn, sql, 0, Long.MAX_VALUE, inParallel, rowParser);
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
        parse(conn, sql, offset, count, false, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param offset
     * @param count
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final long offset, final long count, final boolean inParallel,
            final Consumer<Object[]> rowParser) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(sql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);

            stmt.setFetchSize(200);

            parse(stmt, offset, count, inParallel, rowParser);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param parameters
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final List<?> parameters, final Consumer<Object[]> rowParser) {
        parse(conn, sql, parameters, false, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param parameters
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final List<?> parameters, final boolean inParallel, final Consumer<Object[]> rowParser) {
        parse(conn, sql, parameters, 0, Long.MAX_VALUE, inParallel, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param parameters
     * @param offset
     * @param count
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final List<?> parameters, final long offset, final long count,
            final Consumer<Object[]> rowParser) {
        parse(conn, sql, parameters, offset, count, false, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified Connection and sql.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param conn
     * @param sql
     * @param parameters
     * @param offset
     * @param count
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final Connection conn, final String sql, final List<?> parameters, final long offset, final long count, final boolean inParallel,
            final Consumer<Object[]> rowParser) {
        PreparedStatement stmt = null;

        try {
            stmt = conn.prepareStatement(sql, ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY);

            if (N.notNullOrEmpty(parameters)) {
                for (int i = 0; i < parameters.size(); i++) {
                    stmt.setObject(i + 1, parameters.get(i));
                }
            }

            stmt.setFetchSize(200);

            parse(stmt, offset, count, inParallel, rowParser);
        } catch (SQLException e) {
            throw new AbacusSQLException(e);
        } finally {
            closeQuietly(stmt);
        }
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param stmt
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final PreparedStatement stmt, final Consumer<Object[]> rowParser) {
        parse(stmt, false, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param stmt
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final PreparedStatement stmt, final boolean inParallel, final Consumer<Object[]> rowParser) {
        parse(stmt, 0, Long.MAX_VALUE, inParallel, rowParser);
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
        parse(stmt, offset, count, false, rowParser);
    }

    /**
     * Parse the ResultSet obtained by executing query with the specified PreparedStatement.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param stmt
     * @param offset
     * @param count
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final PreparedStatement stmt, final long offset, final long count, final boolean inParallel, final Consumer<Object[]> rowParser) {
        ResultSet rs = null;

        try {
            rs = stmt.executeQuery();

            parse(rs, offset, count, inParallel, rowParser);
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
        parse(rs, false, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param rs
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final ResultSet rs, final boolean inParallel, final Consumer<Object[]> rowParser) {
        parse(rs, 0, Long.MAX_VALUE, inParallel, rowParser);
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
        parse(rs, offset, count, false, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param rs
     * @param offset
     * @param count
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final ResultSet rs, long offset, long count, final boolean inParallel, final Consumer<Object[]> rowParser) {
        parse(new RowIterator(rs), offset, count, inParallel, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param iter
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final RowIterator iter, final Consumer<Object[]> rowParser) {
        parse(iter, false, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param iter
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final RowIterator iter, final boolean inParallel, final Consumer<Object[]> rowParser) {
        parse(iter, 0, Long.MAX_VALUE, inParallel, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param iter
     * @param offset
     * @param count
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final RowIterator iter, long offset, long count, final Consumer<Object[]> rowParser) {
        parse(iter, offset, count, false, rowParser);
    }

    /**
     * Parse the specified ResultSet.
     * The last row will always be null to identity the ending of row set even offset/count is specified.
     * 
     * @param iter
     * @param offset
     * @param count
     * @param inParallel
     * @param rowParser always remember to handle row <code>null</code>
     */
    public static void parse(final RowIterator iter, long offset, long count, final boolean inParallel, final Consumer<Object[]> rowParser) {
        while (offset-- > 0 && iter.moveToNext()) {
        }

        if (inParallel) {
            final AsyncExecutor asyncExecutor = new AsyncExecutor();
            final Queue<Object[]> rowQueue = new ConcurrentLinkedQueue<Object[]>();
            final MutableBoolean isReadDone = new MutableBoolean(false);
            final MutableBoolean isParseDone = new MutableBoolean(false);
            final Handle<Throwable> exceptionHolder = new Handle<Throwable>();
            final Handle<String> errorMessageHolder = new Handle<String>();

            asyncExecutor.execute(new Runnable() {
                @Override
                public void run() {
                    Object[] row = null;
                    try {
                        while (true) {
                            if (rowQueue.size() > 0) {
                                row = rowQueue.poll();
                                rowParser.accept(row);
                            } else if (isReadDone.booleanValue()) {
                                rowParser.accept(null);
                                break;
                            } else {
                                N.sleep(1);
                            }
                        }
                    } catch (Throwable e) {
                        errorMessageHolder.setValue("### Failed to parse at row: " + row + ". " + AbacusException.getErrorMsg(e));
                        exceptionHolder.setValue(e);
                    } finally {
                        isParseDone.setTrue();
                    }
                }
            });

            while (isParseDone.booleanValue() == false && count-- > 0 && iter.hasNext()) {
                while (isParseDone.booleanValue() == false && rowQueue.size() > 1024) {
                    N.sleep(1);
                }

                rowQueue.add(iter.next());
            }

            isReadDone.setTrue();

            while (isParseDone.booleanValue() == false) {
                N.sleep(10);
            }

            if (exceptionHolder.getValue() != null) {
                logger.error(errorMessageHolder.getValue());
                throw new AbacusException(errorMessageHolder.getValue(), exceptionHolder.getValue());
            }
        } else {
            while (count-- > 0 && iter.hasNext()) {
                rowParser.accept(iter.next());
            }

            rowParser.accept(null);
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
        final MutableLong result = new MutableLong();

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
                        result.increment();

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

        JdbcUtil.parse(selectStmt, offset, count, inParallel, rowParser);

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
        List<String> columnNameList = N.newArrayList(dataSet.columnNameList().size());

        for (String columName : dataSet.columnNameList()) {
            columnNameList.add(columName.intern());
        }

        return columnNameList;
    }

    static boolean isTableNotExistsException(final SQLException e) {
        if (e.getSQLState() != null && sqlStateForTableNotExists.contains(e.getSQLState())) {
            return true;
        }

        String msg = e.getMessage();
        return N.notNullOrEmpty(msg) && (msg.contains("not exist") || msg.contains("doesn't exist") || msg.contains("not found"));
    }

    static boolean isTableNotExistsException(final RuntimeException e) {
        if (e instanceof AbacusSQLException) {
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
        private final Properties<String, String> props = Properties.valueOf();
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
        private final Properties<String, String> props = Properties.valueOf();
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
