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

import static com.landawn.abacus.util.D._PARENTHESES_L;
import static com.landawn.abacus.util.D._PARENTHESES_R;
import static com.landawn.abacus.util.D._SPACE;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import com.landawn.abacus.DataSet;
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
 * It's easier to write/maintain the sql by <code>SQLBuilder</code> and more efficient, comparing to write sql in plain text. 
 * <br>The <code>sql()</code> or <code>pair()</code> method must be called to release resources.
 * <br />Here is a sample:
 * <p>
 * String sql = NE.insert("gui", "firstName", "lastName").into("account").sql();
 * <br />// SQL: INSERT INTO account (gui, first_name, last_name) VALUES (:gui, :firstName, :lastName)
 * </p>
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class SQLBuilder<T> {
    private static final Logger logger = LoggerFactory.getLogger(SQLBuilder.class);

    public static final String ALL = D.ALL;
    public static final String TOP = D.TOP;
    public static final String UNIQUE = D.UNIQUE;
    public static final String DISTINCT = D.DISTINCT;
    public static final String DISTINCTROW = D.DISTINCTROW;

    public static final String ASTERISK = D.ASTERISK;
    public static final String COUNT_ALL = "count(*)";

    public static final String _1 = "1";
    public static final List<String> _1_list = N.asList(_1);

    static final char[] _INSERT = D.INSERT.toCharArray();
    static final char[] _SPACE_INSERT_SPACE = (D.SPACE + D.INSERT + D.SPACE).toCharArray();
    static final char[] _INTO = D.INTO.toCharArray();
    static final char[] _SPACE_INTO_SPACE = (D.SPACE + D.INTO + D.SPACE).toCharArray();
    static final char[] _VALUES = D.VALUES.toCharArray();
    static final char[] _SPACE_VALUES_SPACE = (D.SPACE + D.VALUES + D.SPACE).toCharArray();

    static final char[] _SELECT = D.SELECT.toCharArray();
    static final char[] _SPACE_SELECT_SPACE = (D.SPACE + D.SELECT + D.SPACE).toCharArray();
    static final char[] _FROM = D.FROM.toCharArray();
    static final char[] _SPACE_FROM_SPACE = (D.SPACE + D.FROM + D.SPACE).toCharArray();

    static final char[] _UPDATE = D.UPDATE.toCharArray();
    static final char[] _SPACE_UPDATE_SPACE = (D.SPACE + D.UPDATE + D.SPACE).toCharArray();
    static final char[] _SET = D.SET.toCharArray();
    static final char[] _SPACE_SET_SPACE = (D.SPACE + D.SET + D.SPACE).toCharArray();

    static final char[] _DELETE = D.DELETE.toCharArray();
    static final char[] _SPACE_DELETE_SPACE = (D.SPACE + D.DELETE + D.SPACE).toCharArray();

    static final char[] _JOIN = D.JOIN.toCharArray();
    static final char[] _SPACE_JOIN_SPACE = (D.SPACE + D.JOIN + D.SPACE).toCharArray();
    static final char[] _LEFT_JOIN = D.LEFT_JOIN.toCharArray();
    static final char[] _SPACE_LEFT_JOIN_SPACE = (D.SPACE + D.LEFT_JOIN + D.SPACE).toCharArray();
    static final char[] _RIGHT_JOIN = D.RIGHT_JOIN.toCharArray();
    static final char[] _SPACE_RIGHT_JOIN_SPACE = (D.SPACE + D.RIGHT_JOIN + D.SPACE).toCharArray();
    static final char[] _FULL_JOIN = D.FULL_JOIN.toCharArray();
    static final char[] _SPACE_FULL_JOIN_SPACE = (D.SPACE + D.FULL_JOIN + D.SPACE).toCharArray();
    static final char[] _CROSS_JOIN = D.CROSS_JOIN.toCharArray();
    static final char[] _SPACE_CROSS_JOIN_SPACE = (D.SPACE + D.CROSS_JOIN + D.SPACE).toCharArray();
    static final char[] _INNER_JOIN = D.INNER_JOIN.toCharArray();
    static final char[] _SPACE_INNER_JOIN_SPACE = (D.SPACE + D.INNER_JOIN + D.SPACE).toCharArray();
    static final char[] _NATURAL_JOIN = D.NATURAL_JOIN.toCharArray();
    static final char[] _SPACE_NATURAL_JOIN_SPACE = (D.SPACE + D.NATURAL_JOIN + D.SPACE).toCharArray();

    static final char[] _ON = D.ON.toCharArray();
    static final char[] _SPACE_ON_SPACE = (D.SPACE + D.ON + D.SPACE).toCharArray();
    static final char[] _USING = D.USING.toCharArray();
    static final char[] _SPACE_USING_SPACE = (D.SPACE + D.USING + D.SPACE).toCharArray();

    static final char[] _WHERE = D.WHERE.toCharArray();
    static final char[] _SPACE_WHERE_SPACE = (D.SPACE + D.WHERE + D.SPACE).toCharArray();
    static final char[] _GROUP_BY = D.GROUP_BY.toCharArray();
    static final char[] _SPACE_GROUP_BY_SPACE = (D.SPACE + D.GROUP_BY + D.SPACE).toCharArray();
    static final char[] _HAVING = D.HAVING.toCharArray();
    static final char[] _SPACE_HAVING_SPACE = (D.SPACE + D.HAVING + D.SPACE).toCharArray();
    static final char[] _ORDER_BY = D.ORDER_BY.toCharArray();
    static final char[] _SPACE_ORDER_BY_SPACE = (D.SPACE + D.ORDER_BY + D.SPACE).toCharArray();
    static final char[] _LIMIT = (D.SPACE + D.LIMIT + D.SPACE).toCharArray();
    static final char[] _SPACE_LIMIT_SPACE = (D.SPACE + D.LIMIT + D.SPACE).toCharArray();
    static final char[] _OFFSET = D.OFFSET.toCharArray();
    static final char[] _SPACE_OFFSET_SPACE = (D.SPACE + D.OFFSET + D.SPACE).toCharArray();
    static final char[] _AND = D.AND.toCharArray();
    static final char[] _SPACE_AND_SPACE = (D.SPACE + D.AND + D.SPACE).toCharArray();
    static final char[] _OR = D.OR.toCharArray();
    static final char[] _SPACE_OR_SPACE = (D.SPACE + D.OR + D.SPACE).toCharArray();

    static final char[] _UNION = D.UNION.toCharArray();
    static final char[] _SPACE_UNION_SPACE = (D.SPACE + D.UNION + D.SPACE).toCharArray();
    static final char[] _UNION_ALL = D.UNION_ALL.toCharArray();
    static final char[] _SPACE_UNION_ALL_SPACE = (D.SPACE + D.UNION_ALL + D.SPACE).toCharArray();
    static final char[] _INTERSECT = D.INTERSECT.toCharArray();
    static final char[] _SPACE_INTERSECT_SPACE = (D.SPACE + D.INTERSECT + D.SPACE).toCharArray();
    static final char[] _EXCEPT = D.EXCEPT.toCharArray();
    static final char[] _SPACE_EXCEPT_SPACE = (D.SPACE + D.EXCEPT + D.SPACE).toCharArray();
    static final char[] _EXCEPT2 = D.EXCEPT2.toCharArray();
    static final char[] _SPACE_EXCEPT2_SPACE = (D.SPACE + D.EXCEPT2 + D.SPACE).toCharArray();

    static final char[] _AS = D.AS.toCharArray();
    static final char[] _SPACE_AS_SPACE = (D.SPACE + D.AS + D.SPACE).toCharArray();

    static final char[] _SPACE_EQUAL_SPACE = (D.SPACE + D.EQUAL + D.SPACE).toCharArray();

    static final char[] _SPACE_FOR_UPDATE = (D.SPACE + D.FOR_UPDATE).toCharArray();

    static final char[] _COMMA_SPACE = D.COMMA_SPACE.toCharArray();

    private static final Map<String, Map<String, String>> entityTablePropColumnNameMap = new ObjectPool<String, Map<String, String>>(1024);
    private static final Map<String, char[]> tableDeleteFrom = new ConcurrentHashMap<>();
    private static final Map<Class<?>, List<String>> classPropNameListPool = new ConcurrentHashMap<>();
    // private static final Map<Class<?>, Set<String>> classPropNameSetPool = new ConcurrentHashMap<>();
    private static final AtomicInteger activeStringBuilderCounter = new AtomicInteger();

    private final NamingPolicy namingPolicy;
    private final SQLPolicy sqlPolicy;
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

    SQLBuilder(final NamingPolicy namingPolicy, final SQLPolicy sqlPolicy) {
        if (activeStringBuilderCounter.incrementAndGet() > 1024) {
            logger.error("Too many(" + activeStringBuilderCounter.get()
                    + ") StringBuilder instances are created in SQLBuilder. The method sql()/pair() must be called to release resources and close SQLBuilder");
        }

        this.sb = ObjectFactory.createStringBuilder();

        this.namingPolicy = namingPolicy == null ? NamingPolicy.LOWER_CASE_WITH_UNDERSCORE : namingPolicy;
        this.sqlPolicy = sqlPolicy == null ? SQLPolicy.SQL : sqlPolicy;
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
        entityTablePropColumnNameMap.put(N.toLowerCaseWithUnderscore(entityTableName), m);
        entityTablePropColumnNameMap.put(N.toUpperCaseWithUnderscore(entityTableName), m);
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

    public SQLBuilder<T> into(final String tableName) {
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
            switch (sqlPolicy) {
                case SQL:
                case RAW_SQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(D._QUESTION_MARK);
                    }

                    break;
                }

                case NAMED_SQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(":");
                        sb.append(columnNames[i]);
                    }

                    break;
                }

                case IBATIS_SQL: {
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
                    throw new AbacusException("Not supported SQL policy: " + sqlPolicy);
            }
        } else if (N.notNullOrEmpty(columnNameList)) {
            switch (sqlPolicy) {
                case SQL:
                case RAW_SQL: {
                    for (int i = 0, size = columnNameList.size(); i < size; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(D._QUESTION_MARK);
                    }

                    break;
                }

                case NAMED_SQL: {
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

                case IBATIS_SQL: {
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
                    throw new AbacusException("Not supported SQL policy: " + sqlPolicy);
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

    public SQLBuilder<T> into(final Class<?> entityClass) {
        return into(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> from(String expr) {
        expr = expr.trim();
        String tableName = expr.indexOf(D._COMMA) > 0 ? N.split(expr, D._COMMA, true)[0] : expr;

        if (tableName.indexOf(D.SPACE) > 0) {
            tableName = N.split(tableName, D._SPACE, true)[0];
        }

        return from(tableName, expr);
    }

    public SQLBuilder<T> from(final String... tableNames) {
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

    public SQLBuilder<T> from(final Collection<String> tableNames) {
        String tableName = tableNames.iterator().next().trim();

        if (tableName.indexOf(D.SPACE) > 0) {
            tableName = N.split(tableName, D._SPACE, true)[0];
        }

        return from(tableName, N.join(tableNames, D.SPACE));
    }

    public SQLBuilder<T> from(final Map<String, String> tableAliases) {
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

    private SQLBuilder<T> from(final String tableName, final String fromCause) {
        if (op != OperationType.QUERY) {
            throw new AbacusException("Invalid operation: " + op);
        }

        if (N.isNullOrEmpty(columnNames) && N.isNullOrEmpty(columnNameList) && N.isNullOrEmpty(columnAliases)) {
            throw new AbacusException("Column names or props must be set first by select");
        }

        this.tableName = tableName;

        sb.append(_SELECT);
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

                    if (namingPolicy != NamingPolicy.CAMEL_CASE && !D.ASTERISK.equals(columnNames[i])) {
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

                if (namingPolicy != NamingPolicy.CAMEL_CASE && !D.ASTERISK.equals(columnName)) {
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

    public SQLBuilder<T> from(final Class<?> entityClass) {
        return from(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> join(final String expr) {
        sb.append(_SPACE_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder<T> join(final Class<?> entityClass) {
        return join(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> leftJoin(final String expr) {
        sb.append(_SPACE_LEFT_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder<T> leftJoin(final Class<?> entityClass) {
        return leftJoin(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> rightJoin(final String expr) {
        sb.append(_SPACE_RIGHT_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder<T> rightJoin(final Class<?> entityClass) {
        return rightJoin(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> fullJoin(final String expr) {
        sb.append(_SPACE_FULL_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder<T> fullJoin(final Class<?> entityClass) {
        return fullJoin(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> crossJoin(final String expr) {
        sb.append(_SPACE_CROSS_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder<T> crossJoin(final Class<?> entityClass) {
        return crossJoin(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> innerJoin(final String expr) {
        sb.append(_SPACE_INNER_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder<T> innerJoin(final Class<?> entityClass) {
        return innerJoin(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> naturalJoin(final String expr) {
        sb.append(_SPACE_NATURAL_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder<T> naturalJoin(final Class<?> entityClass) {
        return naturalJoin(N.getSimpleClassName(entityClass));
    }

    public SQLBuilder<T> on(final String expr) {
        sb.append(_SPACE_ON_SPACE);

        // sb.append(formalizeName(tableName, expr));
        appendStringExpr(expr);

        return this;
    }

    /**
     * 
     * @param cond any literal written in <code>Expression</code> condition won't be formalized
     * @return
     */
    public SQLBuilder<T> on(final Condition cond) {
        sb.append(_SPACE_ON_SPACE);

        appendCondition(cond);

        return this;
    }

    public SQLBuilder<T> using(final String expr) {
        sb.append(_SPACE_USING_SPACE);

        sb.append(formalizeName(tableName, expr));

        return this;
    }

    public SQLBuilder<T> where(final String expr) {
        init(true);

        sb.append(_SPACE_WHERE_SPACE);

        appendStringExpr(expr);

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

    /**
     * 
     * @param cond any literal written in <code>Expression</code> condition won't be formalized
     * @return
     */
    public SQLBuilder<T> where(final Condition cond) {
        init(true);

        sb.append(_SPACE_WHERE_SPACE);

        appendCondition(cond);

        return this;
    }

    public SQLBuilder<T> groupBy(final String expr) {
        sb.append(_SPACE_GROUP_BY_SPACE);

        if (expr.indexOf(D._SPACE) > 0) {
            // sb.append(columnNames[0]);
            appendStringExpr(expr);
        } else {
            sb.append(formalizeName(tableName, expr));
        }

        return this;
    }

    public SQLBuilder<T> groupBy(final String... columnNames) {
        sb.append(_SPACE_GROUP_BY_SPACE);

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

    public SQLBuilder<T> groupBy(final String columnName, final SortDirection direction) {
        groupBy(columnName);

        sb.append(D._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder<T> groupBy(final Collection<String> columnNames) {
        sb.append(_SPACE_GROUP_BY_SPACE);

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

    public SQLBuilder<T> groupBy(final Collection<String> columnNames, final SortDirection direction) {
        groupBy(columnNames);

        sb.append(D._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder<T> groupBy(final Map<String, SortDirection> orders) {
        sb.append(_SPACE_GROUP_BY_SPACE);

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

    public SQLBuilder<T> having(final String expr) {
        sb.append(_SPACE_HAVING_SPACE);

        appendStringExpr(expr);

        return this;
    }

    /**
     * 
     * @param cond any literal written in <code>Expression</code> condition won't be formalized
     * @return
     */
    public SQLBuilder<T> having(final Condition cond) {
        sb.append(_SPACE_HAVING_SPACE);

        appendCondition(cond);

        return this;
    }

    public SQLBuilder<T> orderBy(final String expr) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        if (expr.indexOf(D._SPACE) > 0) {
            // sb.append(columnNames[0]);
            appendStringExpr(expr);
        } else {
            sb.append(formalizeName(tableName, expr));
        }

        return this;
    }

    public SQLBuilder<T> orderBy(final String... columnNames) {
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

    public SQLBuilder<T> orderBy(final String columnName, final SortDirection direction) {
        orderBy(columnName);

        sb.append(D._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder<T> orderBy(final Collection<String> columnNames) {
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

    public SQLBuilder<T> orderBy(final Collection<String> columnNames, final SortDirection direction) {
        orderBy(columnNames);

        sb.append(D._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder<T> orderBy(final Map<String, SortDirection> orders) {
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

    public SQLBuilder<T> limit(final int count) {
        sb.append(_SPACE_LIMIT_SPACE);

        sb.append(count);

        return this;
    }

    public SQLBuilder<T> limit(final int offset, final int count) {
        sb.append(_SPACE_LIMIT_SPACE);

        sb.append(offset);

        sb.append(_COMMA_SPACE);

        sb.append(count);

        return this;
    }

    public SQLBuilder<T> offset(final int offset) {
        sb.append(_SPACE_OFFSET_SPACE);

        sb.append(offset);

        return this;
    }

    public SQLBuilder<T> union(final SQLBuilder<T> sqlBuilder) {
        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return union(sqlBuilder.sql());
    }

    public SQLBuilder<T> union(final String query) {
        return union(Array.of(query));
    }

    public SQLBuilder<T> union(final String... columnNames) {
        op = OperationType.QUERY;

        this.columnNames = columnNames;
        this.columnNameList = null;
        this.columnAliases = null;

        sb.append(_SPACE_UNION_SPACE);

        // it's sub query
        if (isSubQuery(columnNames)) {
            sb.append(columnNames[0]);

            this.columnNames = null;
        } else {
            // build in from method.
        }

        return this;
    }

    public SQLBuilder<T> union(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_UNION_SPACE);

        return this;
    }

    public SQLBuilder<T> unionAll(final SQLBuilder<T> sqlBuilder) {
        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return unionAll(sqlBuilder.sql());
    }

    public SQLBuilder<T> unionAll(final String query) {
        return unionAll(Array.of(query));
    }

    public SQLBuilder<T> unionAll(final String... columnNames) {
        op = OperationType.QUERY;

        this.columnNames = columnNames;
        this.columnNameList = null;
        this.columnAliases = null;

        sb.append(_SPACE_UNION_ALL_SPACE);

        // it's sub query
        if (isSubQuery(columnNames)) {
            sb.append(columnNames[0]);

            this.columnNames = null;
        } else {
            // build in from method.
        }

        return this;
    }

    public SQLBuilder<T> unionAll(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_UNION_ALL_SPACE);

        return this;
    }

    public SQLBuilder<T> intersect(final SQLBuilder<T> sqlBuilder) {
        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return intersect(sqlBuilder.sql());
    }

    public SQLBuilder<T> intersect(final String query) {
        return intersect(Array.of(query));
    }

    public SQLBuilder<T> intersect(final String... columnNames) {
        op = OperationType.QUERY;

        this.columnNames = columnNames;
        this.columnNameList = null;
        this.columnAliases = null;

        sb.append(_SPACE_INTERSECT_SPACE);

        // it's sub query
        if (isSubQuery(columnNames)) {
            sb.append(columnNames[0]);

            this.columnNames = null;
        } else {
            // build in from method.
        }

        return this;
    }

    public SQLBuilder<T> intersect(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_INTERSECT_SPACE);

        return this;
    }

    public SQLBuilder<T> except(final SQLBuilder<T> sqlBuilder) {
        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return except(sqlBuilder.sql());
    }

    public SQLBuilder<T> except(final String query) {
        return except(Array.of(query));
    }

    public SQLBuilder<T> except(final String... columnNames) {
        op = OperationType.QUERY;

        this.columnNames = columnNames;
        this.columnNameList = null;
        this.columnAliases = null;

        sb.append(_SPACE_EXCEPT_SPACE);

        // it's sub query
        if (isSubQuery(columnNames)) {
            sb.append(columnNames[0]);

            this.columnNames = null;
        } else {
            // build in from method.
        }

        return this;
    }

    public SQLBuilder<T> except(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_EXCEPT_SPACE);

        return this;
    }

    public SQLBuilder<T> minus(final SQLBuilder<T> sqlBuilder) {
        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return minus(sqlBuilder.sql());
    }

    public SQLBuilder<T> minus(final String query) {
        return minus(Array.of(query));
    }

    public SQLBuilder<T> minus(final String... columnNames) {
        op = OperationType.QUERY;

        this.columnNames = columnNames;
        this.columnNameList = null;
        this.columnAliases = null;

        sb.append(_SPACE_EXCEPT2_SPACE);

        // it's sub query
        if (isSubQuery(columnNames)) {
            sb.append(columnNames[0]);

            this.columnNames = null;
        } else {
            // build in from method.
        }

        return this;
    }

    public SQLBuilder<T> minus(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_EXCEPT2_SPACE);

        return this;
    }

    public SQLBuilder<T> forUpdate() {
        sb.append(_SPACE_FOR_UPDATE);

        return this;
    }

    public SQLBuilder<T> set(final String expr) {
        return set(Array.of(expr));
    }

    public SQLBuilder<T> set(final String... columnNames) {
        init(false);

        if (columnNames.length == 1 && SQLParser.parse(columnNames[0]).contains(D.EQUAL)) {
            sb.append(columnNames[0]);
        } else {
            final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

            switch (sqlPolicy) {
                case SQL:
                case RAW_SQL: {
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

                case NAMED_SQL: {
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

                case IBATIS_SQL: {
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
                    throw new AbacusException("Not supported SQL policy: " + sqlPolicy);
            }
        }

        columnNameList = null;

        return this;
    }

    public SQLBuilder<T> set(final Collection<String> columnNames) {
        init(false);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        switch (sqlPolicy) {
            case SQL:
            case RAW_SQL: {
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

            case NAMED_SQL: {
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

            case IBATIS_SQL: {
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
                throw new AbacusException("Not supported SQL policy: " + sqlPolicy);
        }

        columnNameList = null;

        return this;
    }

    public SQLBuilder<T> set(final Map<String, Object> props) {
        init(false);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        switch (sqlPolicy) {
            case SQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForSQL(entry.getValue());
                }

                break;
            }

            case RAW_SQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForRawSQL(entry.getValue());
                }

                break;
            }

            case NAMED_SQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForNamedSQL(entry.getKey(), entry.getValue());
                }

                break;
            }

            case IBATIS_SQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForIbatisNamedSQL(entry.getKey(), entry.getValue());
                }

                break;
            }

            default:
                throw new AbacusException("Not supported SQL policy: " + sqlPolicy);
        }

        columnNameList = null;

        return this;
    }

    /**
     * Only the dirty properties will be set into the result SQL if the specified entity is a dirty marker entity.
     * 
     * @param entity
     * @return
     */
    @SuppressWarnings("deprecation")
    public SQLBuilder<T> set(final Object entity) {
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
                    updateProps.put(propName, N.getPropValue(dirtyMarkerEntity, propName));
                }

                return set(updateProps);
            } else {
                return set(N.entity2Map(entity));
            }
        }
    }

    public SQLBuilder<T> set(Class<?> entityClass) {
        return set(entityClass, null);
    }

    public SQLBuilder<T> set(Class<?> entityClass, final Set<String> excludedPropNames) {
        return set(getPropNamesByClass(entityClass, excludedPropNames));
    }

    /**
     * This SQLBuilder<T> will be closed after <code>sql()</code> is called.
     * 
     * @return
     */
    public String sql() {
        if (sb == null) {
            throw new AbacusException("This SQLBuilder<T> has been closed after sql() was called previously");
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
     *  This SQLBuilder will be closed after <code>pair()</code> is called.
     *  
     * @return the pair of sql and parameters.
     */
    public Pair2 pair() {
        return Pair2.of(sql(), parameters);
    }

    //    /**
    //     * 
    //     * @param sqlExecutor
    //     * @return
    //     */
    //    @Beta
    //    public boolean exists(final SQLExecutor sqlExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return sqlExecutor.exists(sql(), this.parameters);
    //    }
    //
    //    /**
    //     * 
    //     * @param sqlExecutor
    //     * @return
    //     */
    //    @Beta
    //    public boolean exists(final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isNullOrEmpty(parameters)) {
    //            return sqlExecutor.exists(sql(), this.parameters);
    //        } else {
    //            return sqlExecutor.exists(sql(), parameters);
    //        }
    //    }
    //
    //    /**
    //     * 
    //     * @param sqlExecutor
    //     * @return <code>DataSet</code> if it's a <code>SELECT</code> statement, otherwise, the updated record count. 
    //     */
    //    @Beta
    //    public T execute(final SQLExecutor sqlExecutor) {
    //        if (op == OperationType.QUERY) {
    //            return (T) sqlExecutor.query(sql(), this.parameters);
    //        } /* else if (op == OperationType.ADD) {
    //            return (T) sqlExecutor.insert(sql(), this.parameters);
    //          } */ else {
    //            return (T) (Integer) sqlExecutor.update(sql(), this.parameters);
    //        }
    //    }
    //
    //    /**
    //     * 
    //     * @param sqlExecutor
    //     * @param parameters
    //     * @return <code>DataSet</code> if it's a <code>SELECT</code> statement, otherwise, the updated record count. 
    //     */
    //    @Beta
    //    public T execute(final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (op == OperationType.QUERY) {
    //                return (T) sqlExecutor.query(sql(), this.parameters);
    //            } /* else if (op == OperationType.ADD) {
    //                 return (T) sqlExecutor.insert(sql(), this.parameters);
    //              } */ else {
    //                return (T) (Integer) sqlExecutor.update(sql(), this.parameters);
    //            }
    //        } else {
    //            if (op == OperationType.QUERY) {
    //                return (T) sqlExecutor.query(sql(), parameters);
    //            } /* else if (op == OperationType.ADD) {
    //                return (T) sqlExecutor.insert(sql(), parameters);
    //              } */ else {
    //                return (T) (Integer) sqlExecutor.update(sql(), parameters);
    //            }
    //        }
    //    }
    //
    //    /**
    //     * Returns the target result executed by calling <code>queryForEntity</code> if the target class is entity or map, otherwise <code>queryForSingleResult</code>
    //     * 
    //     * @param targetClass
    //     * @param sqlExecutor
    //     * @return 
    //     */
    //    @Beta
    //    public <R> OptionalNullable<R> execute(final Class<R> targetClass, final SQLExecutor sqlExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //            return OptionalNullable.from(sqlExecutor.queryForEntity(targetClass, sql(), this.parameters));
    //        } else {
    //            return sqlExecutor.queryForSingleResult(targetClass, sql(), this.parameters);
    //        }
    //    }
    //
    //    /**
    //     * Returns the target result executed by calling <code>queryForEntity</code> if the target class is entity or map, otherwise <code>queryForSingleResult</code>
    //     * 
    //     * @param targetClass
    //     * @param sqlExecutor
    //     * @param parameters
    //     * @return
    //     */
    //    @Beta
    //    public <R> OptionalNullable<R> execute(final Class<R> targetClass, final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //                return OptionalNullable.from(sqlExecutor.queryForEntity(targetClass, sql(), this.parameters));
    //            } else {
    //                return sqlExecutor.queryForSingleResult(targetClass, sql(), this.parameters);
    //            }
    //        } else {
    //            if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //                return OptionalNullable.from(sqlExecutor.queryForEntity(targetClass, sql(), parameters));
    //            } else {
    //                return sqlExecutor.queryForSingleResult(targetClass, sql(), parameters);
    //            }
    //        }
    //    }
    //
    //    @Beta
    //    public CompletableFuture<T> asyncExecute(final SQLExecutor sqlExecutor) {
    //        if (op == OperationType.QUERY) {
    //            return (CompletableFuture<T>) sqlExecutor.asyncExecutor().query(sql(), this.parameters);
    //        } /* else if (op == OperationType.ADD) {
    //            return (CompletableFuture<T>) sqlExecutor.asyncSQLExecutor().insert(sql(), this.parameters);
    //          } */ else {
    //            return (CompletableFuture<T>) sqlExecutor.asyncExecutor().update(sql(), this.parameters);
    //        }
    //    }
    //
    //    @Beta
    //    public CompletableFuture<T> asyncExecute(final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (op == OperationType.QUERY) {
    //                return (CompletableFuture<T>) sqlExecutor.asyncExecutor().query(sql(), this.parameters);
    //            } /* else if (op == OperationType.ADD) {
    //                return (CompletableFuture<T>) sqlExecutor.asyncSQLExecutor().insert(sql(), this.parameters);
    //              } */ else {
    //                return (CompletableFuture<T>) sqlExecutor.asyncExecutor().update(sql(), this.parameters);
    //            }
    //        } else {
    //            if (op == OperationType.QUERY) {
    //                return (CompletableFuture<T>) sqlExecutor.asyncExecutor().query(sql(), parameters);
    //            } /* else if (op == OperationType.ADD) {
    //                return (CompletableFuture<T>) sqlExecutor.asyncSQLExecutor().insert(sql(), parameters);
    //              } */ else {
    //                return (CompletableFuture<T>) sqlExecutor.asyncExecutor().update(sql(), parameters);
    //            }
    //        }
    //    }
    //
    //    @Beta
    //    public <R> CompletableFuture<OptionalNullable<R>> asyncExecute(final Class<R> targetClass, final SQLExecutor sqlExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return sqlExecutor.asyncExecutor().asyncExecutor().execute(new Callable<OptionalNullable<R>>() {
    //            @Override
    //            public OptionalNullable<R> call() throws Exception {
    //                return execute(targetClass, sqlExecutor);
    //            }
    //        });
    //    }
    //
    //    @Beta
    //    public <R> CompletableFuture<OptionalNullable<R>> asyncExecute(final Class<R> targetClass, final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return sqlExecutor.asyncExecutor().asyncExecutor().execute(new Callable<OptionalNullable<R>>() {
    //            @Override
    //            public OptionalNullable<R> call() throws Exception {
    //                return execute(targetClass, sqlExecutor, parameters);
    //            }
    //        });
    //    }

    void init(boolean setForUpdate) {
        if (sb.length() > 0) {
            return;
        }

        if (op == OperationType.UPDATE) {
            sb.append(_UPDATE);

            sb.append(D._SPACE);
            sb.append(formalizeName(tableName));

            sb.append(_SPACE_SET_SPACE);

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

    private void setParameterForSQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(D._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(Expression.formalize(propValue));
        }
    }

    private void setParameterForRawSQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(D._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(D._QUESTION_MARK);

            parameters.add(propValue);
        }
    }

    private void setParameterForIbatisNamedSQL(final String propName, final Object propValue) {
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

    private void setParameterForNamedSQL(final String propName, final Object propValue) {
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
        switch (sqlPolicy) {
            case SQL: {
                setParameterForSQL(propValue);

                break;
            }

            case RAW_SQL: {
                setParameterForRawSQL(propValue);

                break;
            }

            case NAMED_SQL: {
                setParameterForNamedSQL(propName, propValue);

                break;
            }

            case IBATIS_SQL: {
                setParameterForIbatisNamedSQL(propName, propValue);

                break;
            }

            default:
                throw new AbacusException("Not supported SQL policy: " + sqlPolicy);
        }
    }

    private void appendInsertProps(final Map<String, Object> props) {
        switch (sqlPolicy) {
            case SQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForSQL(propValue);
                }

                break;
            }

            case RAW_SQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForRawSQL(propValue);
                }

                break;
            }

            case NAMED_SQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForNamedSQL(propName, propValue);
                }

                break;
            }

            case IBATIS_SQL: {
                int i = 0;
                Object propValue = null;
                for (String propName : props.keySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    propValue = props.get(propName);

                    setParameterForIbatisNamedSQL(propName, propValue);
                }

                break;
            }

            default:
                throw new AbacusException("Not supported SQL policy: " + sqlPolicy);
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
            if (sqlPolicy == SQLPolicy.NAMED_SQL || sqlPolicy == SQLPolicy.IBATIS_SQL) {
                setParameter("min" + N.capitalize(propName), minValue);
            } else {
                setParameter(propName, minValue);
            }

            sb.append(D._SPACE);
            sb.append(D.AND);
            sb.append(D._SPACE);

            Object maxValue = bt.getMaxValue();
            if (sqlPolicy == SQLPolicy.NAMED_SQL || sqlPolicy == SQLPolicy.IBATIS_SQL) {
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
                    sb.append(E.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof RE) {
                    sb.append(RE.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof SE) {
                    sb.append(SE.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof NE) {
                    sb.append(NE.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof E2) {
                    sb.append(E2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof RE2) {
                    sb.append(RE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof SE2) {
                    sb.append(SE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof NE2) {
                    sb.append(NE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof E3) {
                    sb.append(E3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof RE3) {
                    sb.append(RE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof SE3) {
                    sb.append(SE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
                } else if (this instanceof NE3) {
                    sb.append(NE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).sql());
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
                return N.toLowerCaseWithUnderscore(entityPropName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return N.toUpperCaseWithUnderscore(entityPropName);

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
                return N.toLowerCaseWithUnderscore(propName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return N.toUpperCaseWithUnderscore(propName);

            default:
                return propName;
        }
    }

    private boolean isSubQuery(final String... columnNames) {
        if (columnNames.length == 1) {
            int index = SQLParser.indexWord(columnNames[0], D.SELECT, 0, false);

            if (index >= 0) {
                index = SQLParser.indexWord(columnNames[0], D.FROM, index, false);

                return index >= 1;
            }
        }

        return false;
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
    //        if (obj instanceof SQLBuilder) {
    //            final SQLBuilder other = (SQLBuilder) obj;
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

    private static void parseInsertEntity(final SQLBuilder<?> instance, final Object entity, final Set<String> excludedPropNames) {
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
            instance.props = N.isNullOrEmpty(excludedPropNames) ? N.entity2Map(entity) : N.entity2Map(entity, excludedPropNames);
        }
    }

    private static Collection<Map<String, Object>> toInsertPropsList(final Collection<?> propsList) {
        final Iterator<?> it = propsList.iterator();
        final Object first = it.next();

        if (first instanceof Map) {
            return (List<Map<String, Object>>) propsList;
        } else {
            final List<Map<String, Object>> newPropsList = new ArrayList<>(propsList.size());
            newPropsList.add(N.entity2Map(first));

            while (it.hasNext()) {
                newPropsList.add(N.entity2Map(it.next()));
            }

            return newPropsList;
        }
    }

    private static Collection<String> getPropNamesByClass(final Class<?> entityClass, final Set<String> excludedPropNames) {
        if (N.isNullOrEmpty(excludedPropNames)) {
            return propNameList(entityClass);
        } else {
            final Set<String> entityPropNameSet = new LinkedHashSet<>(N.getPropGetMethodList(entityClass).keySet());

            Method propGetMethod = null;
            for (String propName : excludedPropNames) {
                if (!entityPropNameSet.remove(propName)) {
                    propGetMethod = N.getPropGetMethod(entityClass, propName);

                    if (propGetMethod != null) {
                        entityPropNameSet.remove(N.getPropNameByMethod(propGetMethod));
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
                    propNameList = N.asImmutableList(new ArrayList<>(N.getPropGetMethodList(entityClass).keySet()));
                    classPropNameListPool.put(entityClass, propNameList);
                }
            }
        }

        return propNameList;
    }

    static enum SQLPolicy {
        SQL, RAW_SQL, IBATIS_SQL, NAMED_SQL;
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore.
     * 
     * @author haiyang li 
     *
     */
    public static final class E<T> extends SQLBuilder<T> {
        E() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.SQL);
        }

        static <T> E<T> createInstance() {
            return new E<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the sql will be parameterized with question mark.
     * 
     * @author haiyang li 
     *
     */
    public static final class RE<T> extends SQLBuilder<T> {
        RE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.RAW_SQL);
        }

        static <T> RE<T> createInstance() {
            return new RE<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the sql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * @author haiyang li 
     *
     */
    public static final class NE<T> extends SQLBuilder<T> {
        NE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.NAMED_SQL);
        }

        static <T> NE<T> createInstance() {
            return new NE<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the sql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li 
     *
     */
    public static final class SE<T> extends SQLBuilder<T> {
        SE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.IBATIS_SQL);
        }

        static <T> SE<T> createInstance() {
            return new SE<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore.
     * 
     * @author haiyang li
     *
     */
    public static final class E2<T> extends SQLBuilder<T> {
        E2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.SQL);
        }

        static <T> E2<T> createInstance() {
            return new E2<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the sql will be parameterized with question mark.
     * 
     * @author haiyang li
     *
     */
    public static final class RE2<T> extends SQLBuilder<T> {
        RE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.RAW_SQL);
        }

        static <T> RE2<T> createInstance() {
            return new RE2<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the sql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * 
     * @author haiyang li
     *
     */
    public static final class NE2<T> extends SQLBuilder<T> {
        NE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.NAMED_SQL);
        }

        static <T> NE2<T> createInstance() {
            return new NE2<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the sql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li
     *
     */
    public static final class SE2<T> extends SQLBuilder<T> {
        SE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.IBATIS_SQL);
        }

        static <T> SE2<T> createInstance() {
            return new SE2<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change.
     * 
     * @author haiyang li
     *
     */
    public static final class E3<T> extends SQLBuilder<T> {
        E3() {
            super(NamingPolicy.CAMEL_CASE, SQLPolicy.SQL);
        }

        static <T> E3<T> createInstance() {
            return new E3<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the sql will be parameterized with question mark.
     * 
     * @author haiyang li
     *
     */
    public static final class RE3<T> extends SQLBuilder<T> {
        RE3() {
            super(NamingPolicy.CAMEL_CASE, SQLPolicy.RAW_SQL);
        }

        static <T> RE3<T> createInstance() {
            return new RE3<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the sql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * 
     * @author haiyang li
     *
     */
    public static final class NE3<T> extends SQLBuilder<T> {
        NE3() {
            super(NamingPolicy.CAMEL_CASE, SQLPolicy.NAMED_SQL);
        }

        static <T> NE3<T> createInstance() {
            return new NE3<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the sql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li
     *
     */
    public static final class SE3<T> extends SQLBuilder<T> {
        SE3() {
            super(NamingPolicy.CAMEL_CASE, SQLPolicy.IBATIS_SQL);
        }

        static <T> SE3<T> createInstance() {
            return new SE3<T>();
        }

        public static SQLBuilder<Integer> insert(final String expr) {
            return insert(Array.of(expr));
        }

        public static SQLBuilder<Integer> insert(final String... columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Collection<String> columnNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Map<String, Object> props) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder<Integer> insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder<Integer> insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder<Integer> insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder<Integer> batchInsert(final Collection<?> propsList) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        public static SQLBuilder<DataSet> select(final String expr) {
            return select(Array.of(expr));
        }

        public static SQLBuilder<DataSet> select(final String... columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

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
        public static SQLBuilder<DataSet> select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder<DataSet> instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass) {
            return select(entityClass, null);
        }

        public static SQLBuilder<DataSet> select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, excludedPropNames));
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, null);
        }

        public static SQLBuilder<DataSet> selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, excludedPropNames).from(entityClass);
        }

        public static SQLBuilder<Integer> update(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder<Integer> update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = N.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final String tableName) {
            final SQLBuilder<Integer> instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder<Integer> deleteFrom(final Class<?> entityClass) {
            return deleteFrom(N.getSimpleClassName(entityClass));
        }
    }

    public static final class Pair2 {
        public final String sql;
        public final List<Object> parameters;

        Pair2(final String sql, final List<Object> parameters) {
            this.sql = sql;
            this.parameters = parameters;
        }

        public static Pair2 of(final String sql, final List<Object> parameters) {
            return new Pair2(sql, parameters);
        }

        public Pair<String, List<Object>> __() {
            return Pair.of(sql, parameters);
        }

        @Override
        public int hashCode() {
            return N.hashCode(sql) * 31 + N.hashCode(parameters);
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj instanceof Pair2) {
                Pair2 other = (Pair2) obj;

                return N.equals(other.sql, sql) && N.equals(other.parameters, parameters);
            }

            return false;
        }

        @Override
        public String toString() {
            return "{sql=" + sql + ", parameters=" + N.toString(parameters) + "}";
        }
    }
}
