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
import static com.landawn.abacus.util.WD._PARENTHESES_L;
import static com.landawn.abacus.util.WD._PARENTHESES_R;
import static com.landawn.abacus.util.WD._SPACE;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;

import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.annotation.Column;
import com.landawn.abacus.annotation.NonUpdatable;
import com.landawn.abacus.annotation.ReadOnly;
import com.landawn.abacus.annotation.ReadOnlyId;
import com.landawn.abacus.annotation.Table;
import com.landawn.abacus.annotation.Transient;
import com.landawn.abacus.condition.Between;
import com.landawn.abacus.condition.Binary;
import com.landawn.abacus.condition.Cell;
import com.landawn.abacus.condition.Condition;
import com.landawn.abacus.condition.ConditionFactory.L;
import com.landawn.abacus.condition.Expression;
import com.landawn.abacus.condition.In;
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

    public static final String DISTINCT = WD.DISTINCT;
    public static final String COUNT_ALL = "count(*)";

    private static final Map<Class<?>, Map<String, String>> entityTablePropColumnNameMap = new ObjectPool<>(1024);
    private static final Map<Class<?>, Set<String>[]> defaultPropNamesPool = new ObjectPool<>(1024);
    private static final Map<String, char[]> tableDeleteFrom = new ConcurrentHashMap<>();
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
    private Class<?> entityClass;
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

        this.sb = Objectory.createStringBuilder();

        this.namingPolicy = namingPolicy == null ? NamingPolicy.LOWER_CASE_WITH_UNDERSCORE : namingPolicy;
        this.cqlPolicy = cqlPolicy == null ? CQLPolicy.CQL : cqlPolicy;
    }

    /**
     * 
     * @param entityClass annotated with @Table, @Column
     */
    static void registerEntityPropColumnNameMap(final Class<?> entityClass) {
        N.checkArgNotNull(entityClass);

        final Set<Field> allFields = new HashSet<>();

        for (Class<?> superClass : ClassUtil.getAllSuperclasses(entityClass)) {
            allFields.addAll(Array.asList(superClass.getDeclaredFields()));
        }

        allFields.addAll(Array.asList(entityClass.getDeclaredFields()));

        final Map<String, String> propColumnNameMap = new HashMap<>();
        Method getterMethod = null;

        for (Field field : allFields) {
            getterMethod = ClassUtil.getPropGetMethod(entityClass, field.getName());

            if (getterMethod != null) {
                String columnName = null;

                if (field.isAnnotationPresent(Column.class)) {
                    columnName = field.getAnnotation(Column.class).value();
                } else {
                    try {
                        if (field.isAnnotationPresent(javax.persistence.Column.class)) {
                            columnName = field.getAnnotation(javax.persistence.Column.class).name();
                        }
                    } catch (Throwable e) {
                        // ignore
                    }
                }

                if (N.notNullOrEmpty(columnName)) {
                    propColumnNameMap.put(ClassUtil.getPropNameByMethod(getterMethod), columnName);
                }
            }
        }

        final Map<String, String> tmp = entityTablePropColumnNameMap.get(entityClass);

        if (N.notNullOrEmpty(tmp)) {
            propColumnNameMap.putAll(tmp);
        }

        if (N.isNullOrEmpty(propColumnNameMap)) {
            entityTablePropColumnNameMap.put(entityClass, N.<String, String> emptyMap());
        } else {
            entityTablePropColumnNameMap.put(entityClass, propColumnNameMap);
        }
    }

    private static final Map<Class<?>, String> classEntityNameMap = new ConcurrentHashMap<>();

    static String getTableName(final Class<?> entityClass) {
        String entityTableName = classEntityNameMap.get(entityClass);

        if (entityTableName == null) {
            entityTableName = ClassUtil.getSimpleClassName(entityClass);

            if (entityClass.isAnnotationPresent(Table.class)) {
                entityTableName = entityClass.getAnnotation(Table.class).value();
            } else {
                try {
                    if (entityClass.isAnnotationPresent(javax.persistence.Table.class)) {
                        entityTableName = entityClass.getAnnotation(javax.persistence.Table.class).name();
                    }
                } catch (Throwable e) {
                    // ignore.
                }
            }

            classEntityNameMap.put(entityClass, entityTableName);
        }

        return entityTableName;
    }

    static Collection<String> getSelectPropNamesByClass(final Class<?> entityClass, final boolean includeSubEntityProperties,
            final Set<String> excludedPropNames) {
        final Collection<String>[] val = loadPropNamesByClass(entityClass);
        final Collection<String> propNames = includeSubEntityProperties ? val[0] : val[1];

        if (N.isNullOrEmpty(excludedPropNames)) {
            return propNames;
        } else {
            final List<String> tmp = new ArrayList<>(propNames);
            tmp.removeAll(excludedPropNames);
            return tmp;
        }
    }

    static Collection<String> getInsertPropNamesByClass(final Class<?> entityClass, final Set<String> excludedPropNames) {
        final Collection<String>[] val = loadPropNamesByClass(entityClass);
        final Collection<String> propNames = val[2];

        if (N.isNullOrEmpty(excludedPropNames)) {
            return propNames;
        } else {
            final List<String> tmp = new ArrayList<>(propNames);
            tmp.removeAll(excludedPropNames);
            return tmp;
        }
    }

    static Collection<String> getUpdatePropNamesByClass(final Class<?> entityClass, final Set<String> excludedPropNames) {
        final Collection<String>[] val = loadPropNamesByClass(entityClass);
        final Collection<String> propNames = val[3];

        if (N.isNullOrEmpty(excludedPropNames)) {
            return propNames;
        } else {
            final List<String> tmp = new ArrayList<>(propNames);
            tmp.removeAll(excludedPropNames);
            return tmp;
        }
    }

    private static Collection<String> getDeletePropNamesByClass(final Class<?> entityClass, final Set<String> excludedPropNames) {
        final Collection<String>[] val = loadPropNamesByClass(entityClass);
        final Collection<String> propNames = val[0];

        if (N.isNullOrEmpty(excludedPropNames)) {
            return propNames;
        } else {
            final List<String> tmp = new ArrayList<>(propNames);
            tmp.removeAll(excludedPropNames);
            return tmp;
        }
    }

    static Collection<String>[] loadPropNamesByClass(final Class<?> entityClass) {
        Set<String>[] val = defaultPropNamesPool.get(entityClass);

        if (val == null) {
            synchronized (entityClass) {
                final Set<String> entityPropNames = new LinkedHashSet<>(ClassUtil.getPropGetMethodList(entityClass).keySet());

                val = new Set[4];
                val[0] = new LinkedHashSet<>(entityPropNames);
                val[1] = new LinkedHashSet<>(entityPropNames);
                val[2] = new LinkedHashSet<>(entityPropNames);
                val[3] = new LinkedHashSet<>(entityPropNames);

                final Set<String> readOnlyPropNames = new HashSet<>();
                final Set<String> nonUpdatablePropNames = new HashSet<>();
                final Set<String> transientPropNames = new HashSet<>();

                final Set<Field> allFields = new HashSet<>();

                for (Class<?> superClass : ClassUtil.getAllSuperclasses(entityClass)) {
                    allFields.addAll(Array.asList(superClass.getDeclaredFields()));
                }

                allFields.addAll(Array.asList(entityClass.getDeclaredFields()));

                for (Field field : allFields) {
                    if (ClassUtil.getPropGetMethod(entityClass, field.getName()) == null
                            && ClassUtil.getPropGetMethod(entityClass, ClassUtil.formalizePropName(field.getName())) == null) {
                        continue;
                    }

                    if (field.isAnnotationPresent(ReadOnly.class) || field.isAnnotationPresent(ReadOnlyId.class)) {
                        readOnlyPropNames.add(field.getName());
                    }

                    if (field.isAnnotationPresent(NonUpdatable.class)) {
                        nonUpdatablePropNames.add(field.getName());
                    }

                    if (field.isAnnotationPresent(Transient.class) || Modifier.isTransient(field.getModifiers())) {
                        readOnlyPropNames.add(field.getName());

                        transientPropNames.add(field.getName());
                        transientPropNames.add(ClassUtil.formalizePropName(field.getName()));
                    }
                }

                nonUpdatablePropNames.addAll(readOnlyPropNames);

                val[0].removeAll(transientPropNames);
                val[1].removeAll(transientPropNames);
                val[2].removeAll(readOnlyPropNames);
                val[3].removeAll(nonUpdatablePropNames);

                val[0] = ImmutableSet.of(val[0]);
                val[1] = ImmutableSet.of(val[1]);
                val[2] = ImmutableSet.of(val[2]);
                val[3] = ImmutableSet.of(val[3]);

                defaultPropNamesPool.put(entityClass, val);
            }
        }

        return val;
    }

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

        sb.append(WD._SPACE);
        sb.append(WD._PARENTHESES_L);

        final Map<String, String> propColumnNameMap = getPropColumnNameMap();

        if (N.notNullOrEmpty(columnNames)) {
            if (columnNames.length == 1 && columnNames[0].indexOf(WD._SPACE) > 0) {
                sb.append(columnNames[0]);
            } else {
                for (int i = 0, len = columnNames.length; i < len; i++) {
                    if (i > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeColumnName(propColumnNameMap, columnNames[i]));
                }
            }
        } else if (N.notNullOrEmpty(columnNameList)) {
            int i = 0;
            for (String columnName : columnNameList) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeColumnName(propColumnNameMap, columnName));
            }
        } else {
            final Map<String, Object> props = N.isNullOrEmpty(this.props) ? propsList.iterator().next() : this.props;

            int i = 0;
            for (String columnName : props.keySet()) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeColumnName(propColumnNameMap, columnName));
            }
        }

        sb.append(WD._PARENTHESES_R);

        sb.append(_SPACE_VALUES_SPACE);

        sb.append(WD._PARENTHESES_L);

        if (N.notNullOrEmpty(columnNames)) {
            switch (cqlPolicy) {
                case CQL:
                case RAW_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(WD._QUESTION_MARK);
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

                        sb.append(WD._QUESTION_MARK);
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

                default:
                    throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
            }
        } else if (N.notNullOrEmpty(props)) {
            appendInsertProps(props);
        } else {
            int i = 0;
            for (Map<String, Object> props : propsList) {
                if (i++ > 0) {
                    sb.append(WD._PARENTHESES_R);
                    sb.append(_COMMA_SPACE);
                    sb.append(WD._PARENTHESES_L);
                }

                appendInsertProps(props);
            }
        }

        sb.append(WD._PARENTHESES_R);

        return this;
    }

    public CQLBuilder into(final Class<?> entityClass) {
        this.entityClass = entityClass;

        return into(getTableName(entityClass));
    }

    public CQLBuilder from(String expr) {
        expr = expr.trim();
        String tableName = expr.indexOf(WD._COMMA) > 0 ? StringUtil.split(expr, WD._COMMA, true)[0] : expr;

        if (tableName.indexOf(WD.SPACE) > 0) {
            tableName = StringUtil.split(tableName, WD._SPACE, true)[0];
        }

        return from(tableName, expr);
    }

    @SafeVarargs
    public final CQLBuilder from(final String... tableNames) {
        if (tableNames.length == 1) {
            return from(tableNames[0]);
        } else {
            String tableName = tableNames[0].trim();

            if (tableName.indexOf(WD.SPACE) > 0) {
                tableName = StringUtil.split(tableName, WD._SPACE, true)[0];
            }

            return from(tableName, StringUtil.join(tableNames, WD.COMMA_SPACE));
        }
    }

    public CQLBuilder from(final Collection<String> tableNames) {
        String tableName = tableNames.iterator().next().trim();

        if (tableName.indexOf(WD.SPACE) > 0) {
            tableName = StringUtil.split(tableName, WD._SPACE, true)[0];
        }

        return from(tableName, StringUtil.join(tableNames, WD.SPACE));
    }

    public CQLBuilder from(final Map<String, String> tableAliases) {
        String tableName = tableAliases.keySet().iterator().next().trim();

        if (tableName.indexOf(WD.SPACE) > 0) {
            tableName = StringUtil.split(tableName, WD._SPACE, true)[0];
        }

        String expr = "";

        int i = 0;
        for (Map.Entry<String, String> entry : tableAliases.entrySet()) {
            if (i++ > 0) {
                expr += WD.COMMA_SPACE;
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
        sb.append(WD._SPACE);

        if (N.notNullOrEmpty(predicates)) {
            sb.append(predicates);
            sb.append(WD._SPACE);
        }

        final Map<String, String> propColumnNameMap = getPropColumnNameMap();

        if (N.notNullOrEmpty(columnNames)) {
            if (columnNames.length == 1) {
                final String columnName = StringUtil.trim(columnNames[0]);
                int idx = columnName.indexOf(' ');

                if (idx < 0) {
                    idx = columnName.indexOf(',');
                }

                if (idx > 0) {
                    sb.append(columnName);
                } else {
                    sb.append(formalizeColumnName(propColumnNameMap, columnName));

                    if (namingPolicy != NamingPolicy.LOWER_CAMEL_CASE && !WD.ASTERISK.equals(columnName)) {
                        sb.append(_SPACE_AS_SPACE);

                        sb.append(WD._QUOTATION_S);
                        sb.append(columnName);
                        sb.append(WD._QUOTATION_S);
                    }
                }
            } else {
                String columnName = null;

                for (int i = 0, len = columnNames.length; i < len; i++) {
                    columnName = StringUtil.trim(columnNames[i]);

                    if (i > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    int idx = columnName.indexOf(' ');

                    if (idx > 0) {
                        int idx2 = columnName.indexOf(" AS ", idx);

                        if (idx2 < 0) {
                            idx2 = columnName.indexOf(" as ", idx);
                        }

                        sb.append(formalizeColumnName(propColumnNameMap, columnName.substring(0, idx).trim()));

                        sb.append(_SPACE_AS_SPACE);

                        sb.append(WD._QUOTATION_S);
                        sb.append(columnName.substring(idx2 > 0 ? idx2 + 4 : idx + 1).trim());
                        sb.append(WD._QUOTATION_S);
                    } else {
                        sb.append(formalizeColumnName(propColumnNameMap, columnName));

                        if (namingPolicy != NamingPolicy.LOWER_CAMEL_CASE && !WD.ASTERISK.equals(columnName)) {
                            sb.append(_SPACE_AS_SPACE);

                            sb.append(WD._QUOTATION_S);
                            sb.append(columnName);
                            sb.append(WD._QUOTATION_S);
                        }
                    }
                }
            }
        } else if (N.notNullOrEmpty(columnNameList)) {
            int i = 0;
            for (String columnName : columnNameList) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeColumnName(propColumnNameMap, columnName));

                if (op == OperationType.QUERY && namingPolicy != NamingPolicy.LOWER_CAMEL_CASE && !WD.ASTERISK.equals(columnName)) {
                    sb.append(_SPACE_AS_SPACE);

                    sb.append(WD._QUOTATION_S);
                    sb.append(columnName);
                    sb.append(WD._QUOTATION_S);
                }
            }
        } else {
            int i = 0;
            for (Map.Entry<String, String> entry : columnAliases.entrySet()) {
                if (i++ > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeColumnName(propColumnNameMap, entry.getKey()));

                if (N.notNullOrEmpty(entry.getValue())) {
                    sb.append(_SPACE_AS_SPACE);

                    sb.append(WD._QUOTATION_S);
                    sb.append(entry.getValue());
                    sb.append(WD._QUOTATION_S);
                }
            }
        }

        sb.append(_SPACE_FROM_SPACE);

        sb.append(formalizeName(fromCause));

        return this;
    }

    public CQLBuilder from(final Class<?> entityClass) {
        this.entityClass = entityClass;

        return from(getTableName(entityClass));
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
        final Map<String, String> propColumnNameMap = getPropColumnNameMap();
        final List<String> words = SQLParser.parse(expr);

        String word = null;
        for (int i = 0, len = words.size(); i < len; i++) {
            word = words.get(i);

            if (!StringUtil.isAsciiAlpha(word.charAt(0))) {
                sb.append(word);
            } else if (i < len - 1 && words.get(i + 1).charAt(0) == WD._PARENTHESES_L) {
                sb.append(word);
            } else {
                sb.append(formalizeColumnName(propColumnNameMap, word));
            }
        }
    }

    public CQLBuilder orderBy(final String expr) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        if (expr.indexOf(WD._SPACE) > 0) {
            // sb.append(columnNames[0]);
            appendStringExpr(expr);
        } else {
            sb.append(formalizeColumnName(expr));
        }

        return this;
    }

    @SafeVarargs
    public final CQLBuilder orderBy(final String... columnNames) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        if (columnNames.length == 1) {
            if (columnNames[0].indexOf(WD._SPACE) > 0) {
                // sb.append(columnNames[0]);
                appendStringExpr(columnNames[0]);
            } else {
                sb.append(formalizeColumnName(columnNames[0]));
            }
        } else {
            final Map<String, String> propColumnNameMap = getPropColumnNameMap();

            for (int i = 0, len = columnNames.length; i < len; i++) {
                if (i > 0) {
                    sb.append(_COMMA_SPACE);
                }

                sb.append(formalizeColumnName(propColumnNameMap, columnNames[i]));
            }
        }

        return this;
    }

    public CQLBuilder orderBy(final String columnName, final SortDirection direction) {
        orderBy(columnName);

        sb.append(WD._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public CQLBuilder orderBy(final Collection<String> columnNames) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        final Map<String, String> propColumnNameMap = getPropColumnNameMap();
        int i = 0;
        for (String columnName : columnNames) {
            if (i++ > 0) {
                sb.append(_COMMA_SPACE);
            }

            sb.append(formalizeColumnName(propColumnNameMap, columnName));
        }

        return this;
    }

    public CQLBuilder orderBy(final Collection<String> columnNames, final SortDirection direction) {
        orderBy(columnNames);

        sb.append(WD._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public CQLBuilder orderBy(final Map<String, SortDirection> orders) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        final Map<String, String> propColumnNameMap = getPropColumnNameMap();
        int i = 0;
        for (Map.Entry<String, SortDirection> entry : orders.entrySet()) {
            if (i++ > 0) {
                sb.append(_COMMA_SPACE);
            }

            sb.append(formalizeColumnName(propColumnNameMap, entry.getKey()));

            sb.append(WD._SPACE);
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
        return set(N.asArray(expr));
    }

    @SafeVarargs
    public final CQLBuilder set(final String... columnNames) {
        init(false);

        sb.append(_SPACE_SET_SPACE);

        if (columnNames.length == 1 && SQLParser.parse(columnNames[0]).contains(WD.EQUAL)) {
            sb.append(columnNames[0]);
        } else {
            final Map<String, String> propColumnNameMap = getPropColumnNameMap();

            switch (cqlPolicy) {
                case CQL:
                case RAW_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(formalizeColumnName(propColumnNameMap, columnNames[i]));

                        sb.append(_SPACE_EQUAL_SPACE);

                        sb.append(WD._QUESTION_MARK);
                    }

                    break;
                }

                case NAMED_CQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(formalizeColumnName(propColumnNameMap, columnNames[i]));

                        sb.append(_SPACE_EQUAL_SPACE);

                        sb.append(":");
                        sb.append(columnNames[i]);
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

        final Map<String, String> propColumnNameMap = getPropColumnNameMap();

        switch (cqlPolicy) {
            case CQL:
            case RAW_CQL: {
                int i = 0;
                for (String columnName : columnNames) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeColumnName(propColumnNameMap, columnName));

                    sb.append(_SPACE_EQUAL_SPACE);

                    sb.append(WD._QUESTION_MARK);
                }

                break;
            }

            case NAMED_CQL: {
                int i = 0;
                for (String columnName : columnNames) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeColumnName(propColumnNameMap, columnName));

                    sb.append(_SPACE_EQUAL_SPACE);

                    sb.append(":");
                    sb.append(columnName);
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

        final Map<String, String> propColumnNameMap = getPropColumnNameMap();

        switch (cqlPolicy) {
            case CQL: {
                int i = 0;
                for (Map.Entry<String, Object> entry : props.entrySet()) {
                    if (i++ > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    sb.append(formalizeColumnName(propColumnNameMap, entry.getKey()));

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

                    sb.append(formalizeColumnName(propColumnNameMap, entry.getKey()));

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

                    sb.append(formalizeColumnName(propColumnNameMap, entry.getKey()));

                    sb.append(_SPACE_EQUAL_SPACE);

                    setParameterForNamedCQL(entry.getKey(), entry.getValue());
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
            return set(N.asArray((String) entity));
        } else if (entity instanceof Map) {
            return set((Map<String, Object>) entity);
        } else {
            this.entityClass = entity.getClass();

            if (N.isDirtyMarker(entity.getClass())) {
                final DirtyMarker dirtyMarkerEntity = ((DirtyMarker) entity);
                final Set<String> updatedPropNames = dirtyMarkerEntity.dirtyPropNames();
                final Map<String, Object> updateProps = new HashMap<>();

                for (String propName : updatedPropNames) {
                    updateProps.put(propName, ClassUtil.getPropValue(dirtyMarkerEntity, propName));
                }

                return set(updateProps);
            } else {
                return set(Maps.entity2Map(entity));
            }
        }
    }

    /**
     * Only the dirty properties will be set into the result SQL if the specified entity is a dirty marker entity.
     * 
     * @param entity
     * @param excludedPropNames
     * @return
     */
    @SuppressWarnings("deprecation")
    public CQLBuilder set(final Object entity, final Set<String> excludedPropNames) {
        if (entity instanceof String) {
            return set(N.asArray((String) entity));
        } else if (entity instanceof Map) {
            if (N.isNullOrEmpty(excludedPropNames)) {
                return set((Map<String, Object>) entity);
            } else {
                final Map<String, Object> props = new LinkedHashMap<>((Map<String, Object>) entity);
                Maps.removeKeys(props, excludedPropNames);
                return set(props);
            }
        } else {
            this.entityClass = entity.getClass();

            if (N.isDirtyMarker(entity.getClass())) {
                final Map<String, Object> props = new HashMap<>();

                for (String propName : ((DirtyMarker) entity).dirtyPropNames()) {
                    props.put(propName, ClassUtil.getPropValue(entity, propName));
                }

                Maps.removeKeys(props, excludedPropNames);

                return set(props);
            } else {
                return set(N.isNullOrEmpty(excludedPropNames) ? Maps.entity2Map(entity) : Maps.entity2Map(entity, excludedPropNames));
            }
        }
    }

    public CQLBuilder set(Class<?> entityClass) {
        return set(entityClass, null);
    }

    public CQLBuilder set(Class<?> entityClass, final Set<String> excludedPropNames) {
        this.entityClass = entityClass;

        return set(getUpdatePropNamesByClass(entityClass, excludedPropNames));
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

    public CQLBuilder iff(final String expr) {
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
    public CQLBuilder iff(final Condition cond) {
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
            Objectory.recycle(sb);
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
    public CP pair() {
        return new CP(cql(), parameters);
    }

    void init(boolean setForUpdate) {
        if (sb.length() > 0) {

            if (op == OperationType.UPDATE && setForUpdate && N.notNullOrEmpty(columnNameList)) {
                set(columnNameList);
            }

            return;
        }

        if (op == OperationType.UPDATE) {
            sb.append(_UPDATE);

            sb.append(WD._SPACE);
            sb.append(formalizeName(tableName));

            if (setForUpdate && N.notNullOrEmpty(columnNameList)) {
                set(columnNameList);
            }
        } else if (op == OperationType.DELETE) {
            final String newTableName = formalizeName(tableName);

            char[] deleteFromTableChars = tableDeleteFrom.get(newTableName);

            if (deleteFromTableChars == null) {
                deleteFromTableChars = (WD.DELETE + WD.SPACE + WD.FROM + WD.SPACE + newTableName).toCharArray();
                tableDeleteFrom.put(newTableName, deleteFromTableChars);
            }

            sb.append(deleteFromTableChars);
        }
    }

    private void setParameterForCQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(WD._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(Expression.formalize(propValue));
        }
    }

    private void setParameterForRawCQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(WD._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(WD._QUESTION_MARK);

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

            default:
                throw new AbacusException("Not supported CQL policy: " + cqlPolicy);
        }
    }

    private void appendCondition(final Condition cond) {
        if (cond instanceof Binary) {
            final Binary binary = (Binary) cond;
            final String propName = binary.getPropName();

            sb.append(formalizeColumnName(propName));

            sb.append(WD._SPACE);
            sb.append(binary.getOperator().toString());
            sb.append(WD._SPACE);

            Object propValue = binary.getPropValue();
            setParameter(propName, propValue);
        } else if (cond instanceof Between) {
            final Between bt = (Between) cond;
            final String propName = bt.getPropName();

            sb.append(formalizeColumnName(propName));

            sb.append(WD._SPACE);
            sb.append(bt.getOperator().toString());
            sb.append(WD._SPACE);

            Object minValue = bt.getMinValue();
            if (cqlPolicy == CQLPolicy.NAMED_CQL) {
                setParameter("min" + StringUtil.capitalize(propName), minValue);
            } else {
                setParameter(propName, minValue);
            }

            sb.append(WD._SPACE);
            sb.append(WD.AND);
            sb.append(WD._SPACE);

            Object maxValue = bt.getMaxValue();
            if (cqlPolicy == CQLPolicy.NAMED_CQL) {
                setParameter("max" + StringUtil.capitalize(propName), maxValue);
            } else {
                setParameter(propName, maxValue);
            }
        } else if (cond instanceof In) {
            final In in = (In) cond;
            final String propName = in.getPropName();
            final List<Object> parameters = in.getParameters();

            sb.append(formalizeColumnName(propName));

            sb.append(WD._SPACE);
            sb.append(in.getOperator().toString());
            sb.append(WD.SPACE_PARENTHESES_L);

            for (int i = 0, len = parameters.size(); i < len; i++) {
                if (i > 0) {
                    sb.append(WD.COMMA_SPACE);
                }

                if (cqlPolicy == CQLPolicy.NAMED_CQL) {
                    setParameter(propName + (i + 1), parameters.get(i));
                } else {
                    setParameter(propName, parameters.get(i));
                }
            }

            sb.append(WD._PARENTHESES_R);
        } else if (cond instanceof Cell) {
            final Cell cell = (Cell) cond;

            sb.append(WD._SPACE);
            sb.append(cell.getOperator().toString());
            sb.append(WD._SPACE);

            sb.append(_PARENTHESES_L);
            appendCondition(cell.getCondition());
            sb.append(_PARENTHESES_R);
        } else if (cond instanceof Junction) {
            final Junction junction = (Junction) cond;
            final List<Condition> conditionList = junction.getConditions();

            if (N.isNullOrEmpty(conditionList)) {
                throw new IllegalArgumentException("The junction condition(" + junction.getOperator().toString() + ") doesn't include any element.");
            }

            if (conditionList.size() == 1) {
                appendCondition(conditionList.get(0));
            } else {
                // TODO ((id = :id) AND (gui = :gui)) is not support.
                // only (id = :id) AND (gui = :gui) works.
                // sb.append(_PARENTHESES_L);

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

                // sb.append(_PARENTHESES_R);
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
                } else if (this instanceof NE) {
                    sb.append(NE.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof E2) {
                    sb.append(E2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof RE2) {
                    sb.append(RE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof NE2) {
                    sb.append(NE2.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof E3) {
                    sb.append(E3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof RE3) {
                    sb.append(RE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else if (this instanceof NE3) {
                    sb.append(NE3.select(subQuery.getSelectPropNames()).from(subQuery.getEntityName()).where(subQuery.getCondition()).cql());
                } else {
                    throw new AbacusException("Unsupproted subQuery condition: " + cond);
                }
            }
        } else if (cond instanceof Expression) {
            sb.append(cond.toString());
        } else {
            throw new IllegalArgumentException("Unsupported condtion: " + cond.toString());
        }
    }

    private String formalizeName(final String entityPropName) {
        switch (namingPolicy) {
            case LOWER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toLowerCaseWithUnderscore(entityPropName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toUpperCaseWithUnderscore(entityPropName);

            default:
                return entityPropName;
        }
    }

    private String formalizeColumnName(final String propName) {
        return formalizeColumnName(getPropColumnNameMap(), propName);
    }

    private String formalizeColumnName(final Map<String, String> propColumnNameMap, final String propName) {
        String columnName = propColumnNameMap == null ? null : propColumnNameMap.get(propName);

        if (columnName != null) {
            return columnName;
        }

        switch (namingPolicy) {
            case LOWER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toLowerCaseWithUnderscore(propName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toUpperCaseWithUnderscore(propName);

            default:
                return propName;
        }
    }

    private Map<String, String> getPropColumnNameMap() {
        if (entityClass == null || Map.class.isAssignableFrom(entityClass)) {
            return N.emptyMap();
        }

        final Map<String, String> result = entityTablePropColumnNameMap.get(entityClass);

        if (result == null) {
            registerEntityPropColumnNameMap(entityClass);
        }

        return entityTablePropColumnNameMap.get(entityClass);
    }

    public <T, EX extends Exception> T apply(final Try.Function<? super CP, T, EX> func) throws EX {
        return func.apply(this.pair());
    }

    public <EX extends Exception> void accept(final Try.Consumer<? super CP, EX> consumer) throws EX {
        consumer.accept(this.pair());
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
            instance.columnNames = N.asArray((String) entity);
        } else if (entity instanceof Map) {
            if (N.isNullOrEmpty(excludedPropNames)) {
                instance.props = (Map<String, Object>) entity;
            } else {
                instance.props = new LinkedHashMap<>((Map<String, Object>) entity);
                Maps.removeKeys(instance.props, excludedPropNames);
            }
        } else {
            final Collection<String> propNames = getInsertPropNamesByClass(entity.getClass(), excludedPropNames);
            final Map<String, Object> map = N.newHashMap(N.initHashCapacity(propNames.size()));

            for (String propName : propNames) {
                map.put(propName, ClassUtil.getPropValue(entity, propName));
            }

            instance.props = map;
        }
    }

    static enum CQLPolicy {
        CQL, RAW_CQL, NAMED_CQL;
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
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            super(NamingPolicy.LOWER_CAMEL_CASE, CQLPolicy.CQL);
        }

        static E3 createInstance() {
            return new E3();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            super(NamingPolicy.LOWER_CAMEL_CASE, CQLPolicy.RAW_CQL);
        }

        static RE3 createInstance() {
            return new RE3();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
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
            super(NamingPolicy.LOWER_CAMEL_CASE, CQLPolicy.NAMED_CQL);
        }

        static NE3 createInstance() {
            return new NE3();
        }

        public static CQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
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
            instance.entityClass = entity.getClass();

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static CQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.entityClass = entityClass;
            instance.columnNameList = getInsertPropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static CQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        public static CQLBuilder select(final String expr) {
            return select(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.entityClass = entityClass;
            instance.columnNameList = getSelectPropNamesByClass(entityClass, true, excludedPropNames);

            return instance;
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
            instance.entityClass = entityClass;
            instance.tableName = getTableName(entityClass);
            instance.columnNameList = getUpdatePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder delete(final String expr) {
            return delete(N.asArray(expr));
        }

        @SafeVarargs
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
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.entityClass = entityClass;
            instance.columnNameList = getDeletePropNamesByClass(entityClass, excludedPropNames);

            return instance;
        }

        public static CQLBuilder deleteFrom(final String tableName) {
            final CQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(entityClass, null);
        }

        public static CQLBuilder deleteFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return delete(entityClass, excludedPropNames).from(entityClass);
        }
    }

    public static final class CP {
        public final String cql;
        public final List<Object> parameters;

        CP(final String cql, final List<Object> parameters) {
            this.cql = cql;
            this.parameters = ImmutableList.of(parameters);
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

            if (obj instanceof CP) {
                CP other = (CP) obj;

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
