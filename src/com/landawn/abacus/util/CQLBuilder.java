/*
 * Copyright (c) 2016, Haiyang Li.
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

import static com.landawn.abacus.util.D._PARENTHESES_L;
import static com.landawn.abacus.util.D._PARENTHESES_R;
import static com.landawn.abacus.util.D._SPACE;
import static com.landawn.abacus.util.SQLBuilder._COMMA_SPACE;
import static com.landawn.abacus.util.SQLBuilder._DELETE;
import static com.landawn.abacus.util.SQLBuilder._INSERT;
import static com.landawn.abacus.util.SQLBuilder._SELECT;
import static com.landawn.abacus.util.SQLBuilder._SPACE_AND_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_AS_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_EQUAL_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_FROM_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_INTO_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_LIMIT_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_ORDER_BY_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_SET_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_USING_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_VALUES_SPACE;
import static com.landawn.abacus.util.SQLBuilder._SPACE_WHERE_SPACE;
import static com.landawn.abacus.util.SQLBuilder._UPDATE;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.condition.Between;
import com.landawn.abacus.condition.Binary;
import com.landawn.abacus.condition.Cell;
import com.landawn.abacus.condition.Condition;
import com.landawn.abacus.condition.ConditionFactory.L;
import com.landawn.abacus.condition.Expression;
import com.landawn.abacus.condition.Junction;
import com.landawn.abacus.condition.SubQuery;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.logging.Logger;
import com.landawn.abacus.logging.LoggerFactory;

/**
 * It's easier to write/maintain the CQL by <code>CQLBuilder</code> and more efficient, comparing to write Cassandra CQL in plain text. 
 * <br>The <code>cql()</code> or <code>pair()</code> method must be called to release resources.
 * <br />Here is a sample:
 * <p>
 * String cql = NE.insert("gui", "firstName", "lastName").into("account").cql();
 * <br />// CQL: INSERT INTO account (gui, first_name, last_name) VALUES (:gui, :firstName, :lastName)
 * </p>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class CQLBuilder {
    private static final Logger logger = LoggerFactory.getLogger(CQLBuilder.class);

    public static final String DISTINCT = D.DISTINCT;
    public static final String COUNT_ALL = "count(*)";

    private static final Map<String, Map<String, String>> entityTablePropColumnNameMap = new ObjectPool<>(1024);
    private static final Map<String, char[]> tableDeleteFrom = new ConcurrentHashMap<>();
    private static final Map<Class<?>, List<String>> classPropNameListPool = new ConcurrentHashMap<>();
    // private static final Map<Class<?>, Set<String>> classPropNameSetPool = new ConcurrentHashMap<>();
    private static final AtomicInteger activeStringBuilderCounter = new AtomicInteger();

    static final char[] _SPACE_USING_TIMESTAMP_SPACE = " USING TIMESTAMP ".toCharArray();
    static final char[] _SPACE_USING_TTL_SPACE = " USING TTL ".toCharArray();
    static final char[] _SPACE_IF_SPACE = " IF ".toCharArray();
    static final char[] _SPACE_IF_EXISTS = " IF EXISTS".toCharArray();
    static final char[] _SPACE_IF_NOT_EXISTS = " IF NOT EXISTS".toCharArray();
    static final char[] _SPACE_ALLOW_FILTERING = " ALLOW FILTERING".toCharArray();

    private final NamingPolicy namingPolicy;
    private final CQLPolicy cqlPolicy;
    private final List<Object> parameters = new ArrayList<>();
    private StringBuilder sb;

    private OperationType op;
    private String tableName;
    private String predicates;
    private String[] columnNames;
    private Collection<String> columnNameList;
    private Map<String, String> columnAliases;
    private Map<String, Object> props;
    private Collection<Map<String, Object>> propsList;

    CQLBuilder(final NamingPolicy namingPolicy, final CQLPolicy cqlPolicy) {
        if (activeStringBuilderCounter.incrementAndGet() > 1024) {
            logger.error("Too many(" + activeStringBuilderCounter.get()
                    + ") StringBuilder instances are created in CQLBuilder. The method cql()/pair() must be called to release resources and close CQLBuilder");
        }

        this.sb = ObjectFactory.createStringBuilder();

        this.namingPolicy = namingPolicy == null ? NamingPolicy.LOWER_CASE_WITH_UNDERSCORE : namingPolicy;
        this.cqlPolicy = cqlPolicy == null ? CQLPolicy.CQL : cqlPolicy;
    }

    /**
     * Register the irregular column names which can not be converted from property name by naming policy.
     * 
     * @param entityTableName
     * @param propColumnNameMap
     */
    public static void registerColumnName(final String entityTableName, final Map<String, String> propColumnNameMap) {
        final Map<String, String> m = new HashMap<>(propColumnNameMap);

        entityTablePropColumnNameMap.put(entityTableName, m);
        entityTablePropColumnNameMap.put(entityTableName.toLowerCase(), m);
        entityTablePropColumnNameMap.put(entityTableName.toUpperCase(), m);
        entityTablePropColumnNameMap.put(RefUtil.toLowerCaseWithUnderscore(entityTableName), m);
        entityTablePropColumnNameMap.put(RefUtil.toUpperCaseWithUnderscore(entityTableName), m);
    }

    //    /**
    //     * Register the irregular column names which can not be converted from property name by naming policy.
    //     * 
    //     * @param propNameTableInterface the interface generated by <code>com.landawn.abacus.util.CodeGenerator</code>
    //     */
    //    public static void registerColumnName(final Class<?> propNameTableInterface) {
    //        final String PCM = "_PCM";
    //
    //        try {
    //            final Map<String, String> _pcm = (Map<String, String>) propNameTableInterface.getField(PCM).get(null);
    //
    //            for (Class<?> cls : propNameTableInterface.getDeclaredClasses()) {
    //                final String entityName = (String) cls.getField(D.UNDERSCORE).get(null);
    //                final Map<String, String> entityPCM = (Map<String, String>) cls.getField(PCM).get(null);
    //
    //                final Map<String, String> propColumnNameMap = new HashMap<>(_pcm);
    //                propColumnNameMap.putAll(entityPCM);
    //
    //                registerColumnName(entityName, propColumnNameMap);
    //            }
    //        } catch (Exception e) {
    //            throw new AbacusException(e);
    //        }
    //    }

    //    /**
    //     * Returns an immutable list of the property name by the specified entity class.
    //     * 
    //     * @param entityClass
    //     * @return
    //     */
    //    public static List<String> propNameList(final Class<?> entityClass) {
    //        List<String> propNameList = classPropNameListPool.get(entityClass);
    //
    //        if (propNameList == null) {
    //            synchronized (classPropNameListPool) {
    //                propNameList = classPropNameListPool.get(entityClass);
    //
    //                if (propNameList == null) {
    //                    propNameList = N.asImmutableList(new ArrayList<>(N.getPropGetMethodList(entityClass).keySet()));
    //                    classPropNameListPool.put(entityClass, propNameList);
    //                }
    //            }
    //        }
    //
    //        return propNameList;
    //    }

    //    /**
    //     * Returns an immutable set of the property name by the specified entity class.
    //     * 
    //     * @param entityClass
    //     * @return
    //     */
    //    public static Set<String> propNameSet(final Class<?> entityClass) {
    //        Set<String> propNameSet = classPropNameSetPool.get(entityClass);
    //
    //        if (propNameSet == null) {
    //            synchronized (classPropNameSetPool) {
    //                propNameSet = classPropNameSetPool.get(entityClass);
    //
    //                if (propNameSet == null) {
    //                    propNameSet = N.asImmutableSet(new LinkedHashSet<>(N.getPropGetMethodList(entityClass).keySet()));
    //                    classPropNameSetPool.put(entityClass, propNameSet);
    //                }
    //            }
    //        }
    //
    //        return propNameSet;
    //    }

    @Beta
    static Map<String, Expression> named(final String... propNames) {
        final Map<String, Expression> m = new LinkedHashMap<>(N.initHashCapacity(propNames.length));

        for (String propName : propNames) {
            m.put(propName, L.QME);
        }

        return m;
    }

    @Beta
    static Map<String, Expression> named(final Collection<String> propNames) {
        final Map<String, Expression> m = new LinkedHashMap<>(N.initHashCapacity(propNames.size()));

        for (String propName : propNames) {
            m.put(propName, L.QME);
        }

        return m;
    }

    public CQLBuilder into(final String tableName) {
        if (op != OperationType.ADD) {
            throw new AbacusException("Invalid operation: " + op);
        }

        if (N.isNullOrEmpty(columnNames) && N.isNullOrEmpty(columnNameList) && N.isNullOrEmpty(props) && N.isNullOrEmpty(propsList)) {
            throw new AbacusException("Column names or props must be set first by insert");
        }

        this.tableName = tableName;

        sb.append(_INSERT);
        sb.append(_SPACE_INTO_SPACE);

        sb.append(formalizeName(tableName));

        sb.append(D._SPACE);
        sb.append(D._PARENTHESES_L);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        if (N.notNullOrEmpty(columnNames)) {
            if (columnNames.length == 1 && columnNames[0].indexOf(D._SPACE) > 0) {
                sb.append(columnNames[0]);
            } else {
                for (int i = 0, len = columnNames.length; i < len; i++) {
                    if (i > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, columnNames[i]));
                }
            }
        } else if (N.notNullOrEmpty(columnNameList)) {
            int i = 0;
            for (String columnName : columnNameList) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeName(propColumnNameMap, columnName));
            }
        } else {
            final Map<String, Object> props = N.isNullOrEmpty(this.props) ? propsList.iterator().next() : this.props;

            int i = 0;
            for (String columnName : props.keySet()) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeName(propColumnNameMap, columnName));
            }
        }

        sb.append(D._PARENTHESES_R);

        sb.append(_SPACE_VALUES_SPACE);

        sb.append(D._PARENTHESES_L);

        if (N.notNullOrEmpty(columnNames)) {
            switch (cqlPolicy) {
                case CQL:
                case RAW_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(D._QUESTION_MARK);
                    }

                    break;
                }

                case NAMED_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(":");
                        sb.append(columnNames[i]);
                    }

                    break;
                }

                case IBATIS_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append("#{");
                        sb.append(columnNames[i]);
                        sb.append('}');
                    }

                    break;
                }

                default:
                    throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
            }
        } else if (N.notNullOrEmpty(columnNameList)) {
            switch (cqlPolicy) {
                case CQL:
                case RAW_CQL: {
                    for (int i = 0, size = columnNameList.size(); i < size; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(D._QUESTION_MARK);
                    }

                    break;
                }

                case NAMED_CQL: {
                    int i = 0;
                    for (String columnName : columnNameList) {
                        if (i++ > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(":");
                        sb.append(columnName);
                    }

                    break;
                }

                case IBATIS_CQL: {
                    int i = 0;
                    for (String columnName : columnNameList) {
                        if (i++ > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append("#{");
                        sb.append(columnName);
                        sb.append('}');
                    }

                    break;
                }

                default:
                    throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
            }
        } else if (N.notNullOrEmpty(props)) {
            appendInsertProps(props);
        } else {
            int i = 0;
            for (Map<String, Object> props : propsList) {
                if (i++ > 0) {
                    sb.append(D._PARENTHESES_R);
                    sb.append(_COMMA_SPACE);
                    sb.append(D._PARENTHESES_L);
                }

                appendInsertProps(props);
            }
        }

        sb.append(D._PARENTHESES_R);

        return this;
    }

    public CQLBuilder into(final Class<?> entityClass) {
        return into(RefUtil.getSimpleClassName(entityClass));
    }

    public CQLBuilder from(String expr) {
        expr = expr.trim();
        String tableName = expr.indexOf(D._COMMA) > 0 ? N.split(expr, D._COMMA, true)[0] : expr;

        if (tableName.indexOf(D.SPACE) > 0) {
            tableName = N.split(tableName, D._SPACE, true)[0];
        }

        return from(tableName, expr);
    }

    public CQLBuilder from(final String... tableNames) {
        if (tableNames.length == 1) {
            return from(tableNames[0]);
        } else {
            String tableName = tableNames[0].trim();

            if (tableName.indexOf(D.SPACE) > 0) {
                tableName = N.split(tableName, D._SPACE, true)[0];
            }

            return from(tableName, N.join(tableNames, D.COMMA_SPACE));
        }
    }

    public CQLBuilder from(final Collection<String> tableNames) {
        String tableName = tableNames.iterator().next().trim();

        if (tableName.indexOf(D.SPACE) > 0) {
            tableName = N.split(tableName, D._SPACE, true)[0];
        }

        return from(tableName, N.join(tableNames, D.SPACE));
    }

    public CQLBuilder from(final Map<String, String> tableAliases) {
        String tableName = tableAliases.keySet().iterator().next().trim();

        if (tableName.indexOf(D.SPACE) > 0) {
            tableName = N.split(tableName, D._SPACE, true)[0];
        }

        String expr = "";

        int i = 0;
        for (Map.Entry<String, String> entry : tableAliases.entrySet()) {
            if (i++ > 0) {
                expr += D.COMMA_SPACE;
            }

            expr += (entry.getKey() + " " + entry.getValue());
        }

        return from(tableName, expr);
    }

    private CQLBuilder from(final String tableName, final String fromCause) {
        if (op != OperationType.QUERY && op != OperationType.DELETE) {
            throw new AbacusException("Invalid operation: " + op);
        }

        if (N.isNullOrEmpty(columnNames) && N.isNullOrEmpty(columnNameList) && N.isNullOrEmpty(columnAliases)) {
            throw new AbacusException("Column names or props must be set first by select");
        }

        this.tableName = tableName;

        sb.append(op == OperationType.QUERY ? _SELECT : _DELETE);
        sb.append(D._SPACE);

        if (N.notNullOrEmpty(predicates)) {
            sb.append(predicates);
            sb.append(D._SPACE);
        }

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        if (N.notNullOrEmpty(columnNames)) {
            if (columnNames.length == 1 && columnNames[0].indexOf(D._SPACE) > 0) {
                sb.append(columnNames[0]);
            } else {
                for (int i = 0, len = columnNames.length; i < len; i++) {
                    if (i > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, columnNames[i]));

                    if (op == OperationType.QUERY && namingPolicy != NamingPolicy.CAMEL_CASE && !D.ASTERISK.equals(columnNames[i])) {
                        sb.append(_SPACE_AS_SPACE);

                        sb.append(D._QUOTATION_D);
                        sb.append(columnNames[i]);
                        sb.append(D._QUOTATION_D);
                    }
                }
            }
        } else if (N.notNullOrEmpty(columnNameList)) {
            int i = 0;
            for (String columnName : columnNameList) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeName(propColumnNameMap, columnName));

                if (op == OperationType.QUERY && namingPolicy != NamingPolicy.CAMEL_CASE && !D.ASTERISK.equals(columnName)) {
                    sb.append(_SPACE_AS_SPACE);

                    sb.append(D._QUOTATION_D);
                    sb.append(columnName);
                    sb.append(D._QUOTATION_D);
                }
            }
        } else {
            int i = 0;
            for (Map.Entry<String, String> entry : columnAliases.entrySet()) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                if (N.notNullOrEmpty(entry.getValue())) {
                    sb.append(_SPACE_AS_SPACE);

                    sb.append(D._QUOTATION_D);
                    sb.append(entry.getValue());
                    sb.append(D._QUOTATION_D);
                }
            }
        }

        sb.append(_SPACE_FROM_SPACE);

        sb.append(formalizeName(fromCause));

        return this;
    }

    public CQLBuilder from(final Class<?> entityClass) {
        return from(RefUtil.getSimpleClassName(entityClass));
    }

    public CQLBuilder where(final String expr) {
        init(true);

        sb.append(_SPACE_WHERE_SPACE);

        appendStringExpr(expr);

        return this;
    }

    /**
     * 
     * @param cond any literal written in <code>Expression</code> condition won't be formalized
     * @return
     */
    public CQLBuilder where(final Condition cond) {
        init(true);

        sb.append(_SPACE_WHERE_SPACE);

        appendCondition(cond);

        return this;
    }

    private void appendStringExpr(final String expr) {
        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);
        final List<String> words = SQLParser.parse(expr);

        String word = null;
        for (int i = 0, len = words.size(); i < len; i++) {
            word = words.get(i);

            if (!N.isAsciiAlpha(word.charAt(0))) {
                sb.append(word);
            } else if (i < len - 1 && words.get(i + 1).charAt(0) == D._PARENTHESES_L) {
                sb.append(word);
            } else {
                sb.append(formalizeName(propColumnNameMap, word));
            }
        }
    }

    public CQLBuilder orderBy(final String expr) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        if (expr.indexOf(D._SPACE) > 0) {
            // sb.append(columnNames[0]);
            appendStringExpr(expr);
        } else {
            sb.append(formalizeName(tableName, expr));
        }

        return this;
    }

    public CQLBuilder orderBy(final String... columnNames) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        if (columnNames.length == 1) {
            if (columnNames[0].indexOf(D._SPACE) > 0) {
                // sb.append(columnNames[0]);
                appendStringExpr(columnNames[0]);
            } else {
                sb.append(formalizeName(tableName, columnNames[0]));
            }
        } else {
            final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

            for (int i = 0, len = columnNames.length; i < len; i++) {
                if (i > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeName(propColumnNameMap, columnNames[i]));
            }
        }

        return this;
    }

    public CQLBuilder orderBy(final String columnName, final SortDirection direction) {
        orderBy(columnName);

        sb.append(D._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public CQLBuilder orderBy(final Collection<String> columnNames) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);
        int i = 0;
        for (String columnName : columnNames) {
            if (i++ > 0) {
                sb.append(_COMMA_SPACE);
            }

            sb.append(formalizeName(propColumnNameMap, columnName));
        }

        return this;
    }

    public CQLBuilder orderBy(final Collection<String> columnNames, final SortDirection direction) {
        orderBy(columnNames);

        sb.append(D._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public CQLBuilder orderBy(final Map<String, SortDirection> orders) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);
        int i = 0;
        for (Map.Entry<String, SortDirection> entry : orders.entrySet()) {
            if (i++ > 0) {
                sb.append(_COMMA_SPACE);
            }

            sb.append(formalizeName(propColumnNameMap, entry.getKey()));

            sb.append(D._SPACE);
            sb.append(entry.getValue().toString());
        }

        return this;
    }

    public CQLBuilder limit(final int count) {
        sb.append(_SPACE_LIMIT_SPACE);

        sb.append(count);

        return this;
    }

    public CQLBuilder set(final String expr) {
        return set(Array.of(expr));
    }

    public CQLBuilder set(final String... columnNames) {
        init(false);

        sb.append(_SPACE_SET_SPACE);

        if (columnNames.length == 1 && SQLParser.parse(columnNames[0]).contains(D.EQUAL)) {
            sb.append(columnNames[0]);
        } else {
            final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

            switch (cqlPolicy) {
                case CQL:
                case RAW_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(formalizeName(propColumnNameMap, columnNames[i]));

                        sb.append(_SPACE_EQUAL_SPACE);

                        sb.append(D._QUESTION_MARK);
                    }

                    break;
                }

                case NAMED_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(formalizeName(propColumnNameMap, columnNames[i]));

                        sb.append(_SPACE_EQUAL_SPACE);

                        sb.append(":");
                        sb.append(columnNames[i]);
                    }

                    break;
                }

                case IBATIS_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(formalizeName(propColumnNameMap, columnNames[i]));

                        sb.append(_SPACE_EQUAL_SPACE);

                        sb.append("#{");
                        sb.append(columnNames[i]);
                        sb.append('}');
                    }

                    break;
                }

                default:
                    throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
            }
        }

        this.columnNameList = null;

        return this;
    }

    public CQLBuilder set(final Collection<String> columnNames) {
        init(false);

        sb.append(_SPACE_SET_SPACE);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        switch (cqlPolicy) {
            case CQL:
            case RAW_CQL: {
                int i = 0;
                for (String columnName : columnNames) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, columnName));

                    sb.append(_SPACE_EQUAL_SPACE);

                    sb.append(D._QUESTION_MARK);
                }

                break;
            }

            case NAMED_CQL: {
                int i = 0;
                for (String columnName : columnNames) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, columnName));

                    sb.append(_SPACE_EQUAL_SPACE);

                    sb.append(":");
                    sb.append(columnName);
                }

                break;
            }

            case IBATIS_CQL: {
                int i = 0;
                for (String columnName : columnNames) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, columnName));

                    sb.append(_SPACE_EQUAL_SPACE);

                    sb.append("#{");
                    sb.append(columnName);
                    sb.append('}');
                }

                break;
            }

            default:
                throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
        }

        this.columnNameList = null;

        return this;
    }

    public CQLBuilder set(final Map<String, Object> props) {
        init(false);

        sb.append(_SPACE_SET_SPACE);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        switch (cqlPolicy) {
            case CQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForCQL(entry.getValue());
                }

                break;
            }

            case RAW_CQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForRawCQL(entry.getValue());
                }

                break;
            }

            case NAMED_CQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForNamedCQL(entry.getKey(), entry.getValue());
                }

                break;
            }

            case IBATIS_CQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForIbatisNamedCQL(entry.getKey(), entry.getValue());
                }

                break;
            }

            default:
                throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
        }

        this.columnNameList = null;

        return this;
    }

    /**
     * Only the dirty properties will be set into the result CQL if the specified entity is a dirty marker entity.
     * 
     * @param entity
     * @return
     */
    @SuppressWarnings("deprecation")
    public CQLBuilder set(final Object entity) {
        if (entity instanceof String) {
            return set(Array.of((String) entity));
        } else if (entity instanceof Map) {
            return set((Map<String, Object>) entity);
        } else {
            if (N.isDirtyMarker(entity.getClass())) {
                final DirtyMarker dirtyMarkerEntity = ((DirtyMarker) entity);
                final Set<String> updatedPropNames = dirtyMarkerEntity.dirtyPropNames();
                final Map<String, Object> updateProps = new HashMap<>();

                for (String propName : updatedPropNames) {
                    updateProps.put(propName, RefUtil.getPropValue(dirtyMarkerEntity, propName));
                }

                return set(updateProps);
            } else {
                return set(Maps.entity2Map(entity));
            }
        }
    }

    public CQLBuilder set(Class<?> entityClass) {
        return set(entityClass, null);
    }

    public CQLBuilder set(Class<?> entityClass, final Set<String> excludedPropNames) {
        return set(getPropNamesByClass(entityClass, excludedPropNames));
    }

    CQLBuilder using(String... options) {
        init(false);

        sb.append(_SPACE_USING_SPACE);

        for (int i = 0, len = options.length; i < len; i++) {
            if (i > 0) {
                sb.append(_SPACE_AND_SPACE);
            }

            sb.append(options[i]);
        }

        return this;
    }

    public CQLBuilder usingTTL(long timestamp) {
        return usingTTL(String.valueOf(timestamp));
    }

    public CQLBuilder usingTTL(String timestamp) {
        init(false);

        sb.append(_SPACE_USING_TTL_SPACE);
        sb.append(timestamp);

        return this;
    }

    public CQLBuilder usingTimestamp(Date timestamp) {
        return usingTimestamp(timestamp.getTime());
    }

    public CQLBuilder usingTimestamp(long timestamp) {
        return usingTimestamp(String.valueOf(timestamp));
    }

    public CQLBuilder usingTimestamp(String timestamp) {
        init(false);

        sb.append(_SPACE_USING_TIMESTAMP_SPACE);
        sb.append(timestamp);

        return this;
    }

    public CQLBuilder iF(final String expr) {
        init(true);

        sb.append(_SPACE_IF_SPACE);

        appendStringExpr(expr);

        return this;
    }

    /**
     * 
     * @param cond any literal written in <code>Expression</code> condition won't be formalized
     * @return
     */
    public CQLBuilder iF(final Condition cond) {
        init(true);

        sb.append(_SPACE_IF_SPACE);

        appendCondition(cond);

        return this;
    }

    public CQLBuilder ifExists() {
        init(true);

        sb.append(_SPACE_IF_EXISTS);

        return this;
    }

    public CQLBuilder ifNotExists() {
        init(true);

        sb.append(_SPACE_IF_NOT_EXISTS);

        return this;
    }

    public CQLBuilder allowFiltering() {
        init(true);

        sb.append(_SPACE_ALLOW_FILTERING);

        return this;
    }

    /**
     * This CQLBuilder will be closed after <code>cql()</code> is called.
     * 
     * @return
     */
    public String cql() {
        if (sb == null) {
            throw new AbacusException("This CQLBuilder has been closed after cql() was called previously");
        }

        init(true);

        try {
            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
            sb = null;

            activeStringBuilderCounter.decrementAndGet();
        }
    }

    public List<Object> parameters() {
        return parameters;
    }

    /**
     *  This CQLBuilder will be closed after <code>pair()</code> is called.
     *  
     * @return the pair of cql and parameters.
     */
    public Pair3 pair() {
        return Pair3.of(cql(), parameters);
    }

    //    /**
    //     * 
    //     * @param cassandraExecutor
    //     * @return
    //     */
    //    @Beta
    //    public boolean exists(final CassandraExecutor cassandraExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return cassandraExecutor.exists(cql(), this.parameters);
    //    }
    //
    //    /**
    //     * 
    //     * @param cassandraExecutor
    //     * @return
    //     */
    //    @Beta
    //    public boolean exists(final CassandraExecutor cassandraExecutor, final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isNullOrEmpty(parameters)) {
    //            return cassandraExecutor.exists(cql(), this.parameters);
    //        } else {
    //            return cassandraExecutor.exists(cql(), parameters);
    //        }
    //    }
    //
    //    /**
    //     * 
    //     * @param cassandraExecutor
    //     * @return <code>DataSet</code> if it's a <code>SELECT</code> statement, otherwise, <code>ResultSet</code> is returned.
    //     */
    //    @Beta
    //    public T execute(final CassandraExecutor cassandraExecutor) {
    //        if (op == OperationType.QUERY) {
    //            return (T) cassandraExecutor.query(cql(), this.parameters);
    //        } else {
    //            return (T) cassandraExecutor.execute(cql(), this.parameters);
    //        }
    //    }
    //
    //    /**
    //     * 
    //     * @param cassandraExecutor
    //     * @param parameters
    //     * @return <code>DataSet</code> if it's a <code>SELECT</code> statement, otherwise, <code>ResultSet</code> is returned.
    //     */
    //    @Beta
    //    public T execute(final CassandraExecutor cassandraExecutor, final Object... parameters) {
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (op == OperationType.QUERY) {
    //                return (T) cassandraExecutor.query(cql(), this.parameters);
    //            } else {
    //                return (T) cassandraExecutor.execute(cql(), this.parameters);
    //            }
    //        } else {
    //            if (op == OperationType.QUERY) {
    //                return (T) cassandraExecutor.query(cql(), parameters);
    //            } else {
    //                return (T) cassandraExecutor.execute(cql(), parameters);
    //            }
    //        }
    //    }
    //
    //    /**
    //     * Returns the target result executed by calling <code>queryForEntity</code> if the target class is entity or map, otherwise <code>queryForSingleResult</code>
    //     * 
    //     * @param targetClass
    //     * @param cassandraExecutor
    //     * @return 
    //     */
    //    @Beta
    //    public <R> OptionalNullable<R> execute(final Class<R> targetClass, final CassandraExecutor cassandraExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //            return OptionalNullable.from(cassandraExecutor.queryForEntity(targetClass, cql(), this.parameters));
    //        } else {
    //            return cassandraExecutor.queryForSingleResult(targetClass, cql(), this.parameters);
    //        }
    //    }
    //
    //    /**
    //     * Returns the target result executed by calling <code>queryForEntity</code> if the target class is entity or map, otherwise <code>queryForSingleResult</code>
    //     * 
    //     * @param targetClass
    //     * @param cassandraExecutor
    //     * @param parameters
    //     * @return
    //     */
    //    @Beta
    //    public <R> OptionalNullable<R> execute(final Class<R> targetClass, final CassandraExecutor cassandraExecutor, final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //                return OptionalNullable.from(cassandraExecutor.queryForEntity(targetClass, cql(), this.parameters));
    //            } else {
    //                return cassandraExecutor.queryForSingleResult(targetClass, cql(), this.parameters);
    //            }
    //        } else {
    //            if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //                return OptionalNullable.from(cassandraExecutor.queryForEntity(targetClass, cql(), parameters));
    //            } else {
    //                return cassandraExecutor.queryForSingleResult(targetClass, cql(), parameters);
    //            }
    //        }
    //    }
    //
    //    @Beta
    //    public CompletableFuture asyncExecute(final CassandraExecutor cassandraExecutor) {
    //        if (op == OperationType.QUERY) {
    //            return (CompletableFuture) cassandraExecutor.asyncQuery(cql(), this.parameters);
    //        } else {
    //            return (CompletableFuture) cassandraExecutor.asyncExecute(cql(), this.parameters);
    //        }
    //    }
    //
    //    @Beta
    //    public CompletableFuture asyncExecute(final CassandraExecutor cassandraExecutor, final Object... parameters) {
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (op == OperationType.QUERY) {
    //                return (CompletableFuture) cassandraExecutor.asyncQuery(cql(), this.parameters);
    //            } else {
    //                return (CompletableFuture) cassandraExecutor.asyncExecute(cql(), this.parameters);
    //            }
    //        } else {
    //            if (op == OperationType.QUERY) {
    //                return (CompletableFuture) cassandraExecutor.asyncQuery(cql(), parameters);
    //            } else {
    //                return (CompletableFuture) cassandraExecutor.asyncExecute(cql(), parameters);
    //            }
    //        }
    //    }
    //
    //    @Beta
    //    public <R> CompletableFuture<OptionalNullable<R>> asyncExecute(final Class<R> targetClass, final CassandraExecutor cassandraExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return cassandraExecutor.asyncExecutor().execute(new Callable<OptionalNullable<R>>() {
    //            @Override
    //            public OptionalNullable<R> call() throws Exception {
    //                return execute(targetClass, cassandraExecutor);
    //            }
    //        });
    //    }
    //
    //    @Beta
    //    public <R> CompletableFuture<OptionalNullable<R>> asyncExecute(final Class<R> targetClass, final CassandraExecutor cassandraExecutor,
    //            final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return cassandraExecutor.asyncExecutor().execute(new Callable<OptionalNullable<R>>() {
    //            @Override
    //            public OptionalNullable<R> call() throws Exception {
    //                return execute(targetClass, cassandraExecutor, parameters);
    //            }
    //        });
    //    }

    void init(boolean setForUpdate) {
        if (sb.length() > 0) {

            if (op == OperationType.UPDATE && setForUpdate && N.notNullOrEmpty(columnNameList)) {
                set(columnNameList);
            }

            return;
        }

        if (op == OperationType.UPDATE) {
            sb.append(_UPDATE);

            sb.append(D._SPACE);
            sb.append(formalizeName(tableName));

            if (setForUpdate && N.notNullOrEmpty(columnNameList)) {
                set(columnNameList);
            }
        } else if (op == OperationType.DELETE) {
            final String newTableName = formalizeName(tableName);

            char[] deleteFromTableChars = tableDeleteFrom.get(newTableName);

            if (deleteFromTableChars == null) {
                deleteFromTableChars = (D.DELETE + D.SPACE + D.FROM + D.SPACE + newTableName).toCharArray();
                tableDeleteFrom.put(newTableName, deleteFromTableChars);
            }

            sb.append(deleteFromTableChars);
        }
    }

    private void setParameterForCQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(D._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(Expression.formalize(propValue));
        }
    }

    private void setParameterForRawCQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(D._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(D._QUESTION_MARK);

            parameters.add(propValue);
        }
    }

    private void setParameterForIbatisNamedCQL(final String propName, final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append("#{");
            sb.append(propName);
            sb.append('}');
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append("#{");
            sb.append(propName);
            sb.append('}');

            parameters.add(propValue);
        }
    }

    private void setParameterForNamedCQL(final String propName, final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(":");
            sb.append(propName);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(":");
            sb.append(propName);

            parameters.add(propValue);
        }
    }

    private void setParameter(final String propName, final Object propValue) {
        switch (cqlPolicy) {
            case CQL: {
                setParameterForCQL(propValue);

                break;
            }

            case RAW_CQL: {
                setParameterForRawCQL(propValue);

                break;
            }

            case NAMED_CQL: {
                setParameterForNamedCQL(propName, propValue);

                break;
            }

            case IBATIS_CQL: {
                setParameterForIbatisNamedCQL(propName, propValue);

                break;
            }

            default:
                throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
        }
    }

    private void appendInsertProps(final Map<String, Object> props) {
        switch (cqlPolicy) {
            case CQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForCQL(propValue);
                }

                break;
            }

            case RAW_CQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForRawCQL(propValue);
                }

                break;
            }

            case NAMED_CQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForNamedCQL(propName, propValue);
                }

                break;
            }

            case IBATIS_CQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForIbatisNamedCQL(propName, propValue);
                }

                break;
            }

            default:
                throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
        }
    }

    private void appendCondition(final Condition cond) {
        if (cond instanceof Binary) {
            final Binary binary = (Binary) cond;
            final String propName = binary.getPropName();

            sb.append(formalizeName(tableName, propName));

            sb.append(D._SPACE);
            sb.append(binary.getOperator().toString());
            sb.append(D._SPACE);

            Object propValue = binary.getPropValue();
            setParameter(propName, propValue);
        } else if (cond instanceof Between) {
            final Between bt = (Between) cond;
            final String propName = bt.getPropName();

            sb.append(formalizeName(tableName, propName));

            sb.append(D._SPACE);
            sb.append(bt.getOperator().toString());
            sb.append(D._SPACE);

            Object minValue = bt.getMinValue();
            if (cqlPolicy == CQLPolicy.NAMED_CQL || cqlPolicy == CQLPolicy.IBATIS_CQL) {
                setParameter("min" + N.capitalize(propName), minValue);
            } else {
                setParameter(propName, minValue);
            }

            sb.append(D._SPACE);
            sb.append(D.AND);
            sb.append(D._SPACE);

            Object maxValue = bt.getMaxValue();
            if (cqlPolicy == CQLPolicy.NAMED_CQL || cqlPolicy == CQLPolicy.IBATIS_CQL) {
                setParameter("max" + N.capitalize(propName), maxValue);
            } else {
                setParameter(propName, maxValue);
            }
        } else if (cond instanceof Cell) {
            final Cell cell = (Cell) cond;

            sb.append(D._SPACE);
            sb.append(cell.getOperator().toString());
            sb.append(D._SPACE);

            appendCondition(cell.getCondition());
        } else if (cond instanceof Junction) {
            final Junction junction = (Junction) cond;
            final List<Condition> conditionList = junction.getConditions();

            if (N.isNullOrEmpty(conditionList)) {
                throw new IllegalArgumentException("The junction condition(" + junction.getOperator().toString() + ") doesn't include any element.");
            }

            if (conditionList.size() == 1) {
                appendCondition(conditionList.get(0));
            } else {
                sb.append(_PARENTHESES_L);

                for (int i = 0, size = conditionList.size(); i < size; i++) {
                    if (i > 0) {
                        sb.append(_SPACE);
                        sb.append(junction.getOperator().toString());
                        sb.append(_SPACE);
                    }

                    sb.append(_PARENTHESES_L);

                    appendCondition(conditionList.get(i));

                    sb.append(_PARENTHESES_R);
                }

                sb.append(_PARENTHESES_R);
            }
        } else if (cond instanceof SubQuery) {
            final SubQuery subQuery = (SubQuery) cond;

            if (N.notNullOrEmpty(subQuery.getSql())) {
                sb.append(subQuery.getSql());
            } else {

                if (this instanceof E) {
                    sb.append(E.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof RE) {
                    sb.append(RE.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof SE) {
                    sb.append(SE.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof NE) {
                    sb.append(NE.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof E2) {
                    sb.append(E2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof RE2) {
                    sb.append(RE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof SE2) {
                    sb.append(SE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof NE2) {
                    sb.append(NE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof E3) {
                    sb.append(E3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof RE3) {
                    sb.append(RE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof SE3) {
                    sb.append(SE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof NE3) {
                    sb.append(NE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else {
                    throw new AbacusException("Unsupproted subQuery condition: " + cond);
                }
            }
        } else if (cond instanceof Expression) {
            sb.append(cond.toString());
        } else {
            throw new AbacusException("Unsupported condtion: " + cond.toString());
        }
    }

    private String formalizeName(final String entityPropName) {
        switch (namingPolicy) {
            case LOWER_CASE_WITH_UNDERSCORE:
                return RefUtil.toLowerCaseWithUnderscore(entityPropName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return RefUtil.toUpperCaseWithUnderscore(entityPropName);

            default:
                return entityPropName;
        }
    }

    private String formalizeName(final String entityTableName, final String propName) {
        if (entityTablePropColumnNameMap.size() == 0) {
            return formalizeName(propName);
        } else {
            return formalizeName(entityTablePropColumnNameMap.get(entityTableName), propName);
        }
    }

    private String formalizeName(final Map<String, String> propColumnNameMap, final String propName) {
        String columnName = propColumnNameMap == null ? null : propColumnNameMap.get(propName);

        if (columnName != null) {
            return columnName;
        }

        switch (namingPolicy) {
            case LOWER_CASE_WITH_UNDERSCORE:
                return RefUtil.toLowerCaseWithUnderscore(propName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return RefUtil.toUpperCaseWithUnderscore(propName);

            default:
                return propName;
        }
    }

    //    @Override
    //    public int hashCode() {
    //        return sb.hashCode();
    //    }
    //
    //    @Override
    //    public boolean equals(Object obj) {
    //        if (obj == this) {
    //            return true;
    //        }
    //
    //        if (obj instanceof CQLBuilder) {
    //            final CQLBuilder other = (CQLBuilder) obj;
    //
    //            return N.equals(this.sb, other.sb) && N.equals(this.parameters, other.parameters);
    //        }
    //
    //        return false;
    //    }

    @Override
    public String toString() {
        return sb.toString();
    }

    private static void parseInsertEntity(final CQLBuilder instance, final Object entity, final Set<String> excludedPropNames) {
        if (entity instanceof String) {
            instance.columnNames = Array.of((String) entity);
        } else if (entity instanceof Map) {
            if (N.isNullOrEmpty(excludedPropNames)) {
                instance.props = (Map<String, Object>) entity;
            } else {
                instance.props = new LinkedHashMap<>((Map<String, Object>) entity);

                for (String propName : excludedPropNames) {
                    instance.props.remove(propName);
                }
            }
        } else {
            instance.props = N.isNullOrEmpty(excludedPropNames) ? Maps.entity2Map(entity) : Maps.entity2Map(entity, excludedPropNames);
        }
    }

    private static Collection<String> getPropNamesByClass(final Class<?> entityClass, final Set<String> excludedPropNames) {
        if (N.isNullOrEmpty(excludedPropNames)) {
            return propNameList(entityClass);
        } else {
            final Set<String> entityPropNameSet = new LinkedHashSet<>(RefUtil.getPropGetMethodList(entityClass).keySet());

            Method propGetMethod = null;
            for (String propName : excludedPropNames) {
                if (!entityPropNameSet.remove(propName)) {
                    propGetMethod = RefUtil.getPropGetMethod(entityClass, propName);

                    if (propGetMethod != null) {
                        entityPropNameSet.remove(RefUtil.getPropNameByMethod(propGetMethod));
                    }
                }
            }

            return entityPropNameSet;
        }
    }

    /**
     * Returns an immutable list of the property name by the specified entity class.
     * 
     * @param entityClass
     * @return
     */
    private static List<String> propNameList(final Class<?> entityClass) {
        List<String> propNameList = classPropNameListPool.get(entityClass);

        if (propNameList == null) {
            synchronized (classPropNameListPool) {
                propNameList = classPropNameListPool.get(entityClass);

                if (propNameList == null) {
                    propNameList = N.asImmutableList(new ArrayList<>(RefUtil.getPropGetMethodList(entityClass).keySet()));
                    classPropNameListPool.put(entityClass, propNameList);
                }
            }
        }

        return propNameList;
    }

    static enum CQLPolicy {
        CQL, RAW_CQL, IBATIS_CQL, NAMED_CQL;
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore.
     * 
     * @author haiyang li 
     *
     */
    public static final class E extends CQLBuilder {
        E() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, CQLPolicy.CQL);
        }

        static E createInstance() {
            return new E();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the cql will be parameterized with question mark.
     * 
     * @author haiyang li 
     *
     */
    public static final class RE extends CQLBuilder {
        RE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, CQLPolicy.RAW_CQL);
        }

        static RE createInstance() {
            return new RE();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the cql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * @author haiyang li 
     *
     */
    public static final class NE extends CQLBuilder {
        NE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, CQLPolicy.NAMED_CQL);
        }

        static NE createInstance() {
            return new NE();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the cql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li 
     *
     */
    static final class SE extends CQLBuilder {
        SE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, CQLPolicy.IBATIS_CQL);
        }

        static SE createInstance() {
            return new SE();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore.
     * 
     * @author haiyang li
     *
     */
    public static final class E2 extends CQLBuilder {
        E2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, CQLPolicy.CQL);
        }

        static E2 createInstance() {
            return new E2();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the cql will be parameterized with question mark.
     * 
     * @author haiyang li
     *
     */
    public static final class RE2 extends CQLBuilder {
        RE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, CQLPolicy.RAW_CQL);
        }

        static RE2 createInstance() {
            return new RE2();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the cql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * 
     * @author haiyang li
     *
     */
    public static final class NE2 extends CQLBuilder {
        NE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, CQLPolicy.NAMED_CQL);
        }

        static NE2 createInstance() {
            return new NE2();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the cql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li
     *
     */
    static final class SE2 extends CQLBuilder {
        SE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, CQLPolicy.IBATIS_CQL);
        }

        static SE2 createInstance() {
            return new SE2();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change.
     * 
     * @author haiyang li
     *
     */
    public static final class E3 extends CQLBuilder {
        E3() {
            super(NamingPolicy.CAMEL_CASE, CQLPolicy.CQL);
        }

        static E3 createInstance() {
            return new E3();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the cql will be parameterized with question mark.
     * 
     * @author haiyang li
     *
     */
    public static final class RE3 extends CQLBuilder {
        RE3() {
            super(NamingPolicy.CAMEL_CASE, CQLPolicy.RAW_CQL);
        }

        static RE3 createInstance() {
            return new RE3();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the cql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * 
     * @author haiyang li
     *
     */
    public static final class NE3 extends CQLBuilder {
        NE3() {
            super(NamingPolicy.CAMEL_CASE, CQLPolicy.NAMED_CQL);
        }

        static NE3 createInstance() {
            return new NE3();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static CQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = RefUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the cql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li
     *
     */
    static final class SE3 extends CQLBuilder {
        SE3() {
            super(NamingPolicy.CAMEL_CASE, CQLPolicy.IBATIS_CQL);
        }

        static SE3 createInstance() {
            return new SE3();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static CQLBuilder insert(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder insert(final Map<String, Object> props) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static CQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static CQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(Array.of(expr));
        }

        public static CQLBuilder select(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNameList = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static CQLBuilder select(final String expr, final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder select(final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnAliases = columnAliases;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnAliases
         * @return
         */
        public static CQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static CQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static CQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static CQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static CQLBuilder update(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder update(final Class<?> entityClass) {
            return update(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder delete(final String expr) {
            return delete(Array.of(expr));
        }

        public static CQLBuilder delete(final String... columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNames = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Collection<String> columnNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static CQLBuilder delete(final Class<?> entityClass) {
            return delete(entityClass, null);
        }

        public static CQLBuilder delete(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(RefUtil.getSimpleClassName(entityClass));
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    public static final class Pair3 {
        public final String cql;
        public final List<Object> parameters;

        Pair3(final String cql, final List<Object> parameters) {
            this.cql = cql;
            this.parameters = parameters;
        }

        public static Pair3 of(final String cql, final List<Object> parameters) {
            return new Pair3(cql, parameters);
        }

        public Pair<String, List<Object>> __() {
            return Pair.of(cql, parameters);
        }

        @Override
        public int hashCode() {
            return N.hashCode(cql) * 31 + N.hashCode(parameters);
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj instanceof Pair3) {
                Pair3 other = (Pair3) obj;

                return N.equals(other.cql, cql) && N.equals(other.parameters, parameters);
            }

            return false;
        }

        @Override
        public String toString() {
            return "{cql=" + cql + ", parameters=" + N.toString(parameters) + "}";
        }
    }
}
