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

import static com.landawn.abacus.util.WD._PARENTHESES_L;
import static com.landawn.abacus.util.WD._PARENTHESES_R;
import static com.landawn.abacus.util.WD._SPACE;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
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
public abstract class SQLBuilder {
    private static final Logger logger = LoggerFactory.getLogger(SQLBuilder.class);

    public static final String ALL = WD.ALL;
    public static final String TOP = WD.TOP;
    public static final String UNIQUE = WD.UNIQUE;
    public static final String DISTINCT = WD.DISTINCT;
    public static final String DISTINCTROW = WD.DISTINCTROW;

    public static final String ASTERISK = WD.ASTERISK;
    public static final String COUNT_ALL = "count(*)";

    public static final String _1 = "1";
    public static final List<String> _1_list = N.asList(_1);

    static final char[] _INSERT = WD.INSERT.toCharArray();
    static final char[] _SPACE_INSERT_SPACE = (WD.SPACE + WD.INSERT + WD.SPACE).toCharArray();
    static final char[] _INTO = WD.INTO.toCharArray();
    static final char[] _SPACE_INTO_SPACE = (WD.SPACE + WD.INTO + WD.SPACE).toCharArray();
    static final char[] _VALUES = WD.VALUES.toCharArray();
    static final char[] _SPACE_VALUES_SPACE = (WD.SPACE + WD.VALUES + WD.SPACE).toCharArray();

    static final char[] _SELECT = WD.SELECT.toCharArray();
    static final char[] _SPACE_SELECT_SPACE = (WD.SPACE + WD.SELECT + WD.SPACE).toCharArray();
    static final char[] _FROM = WD.FROM.toCharArray();
    static final char[] _SPACE_FROM_SPACE = (WD.SPACE + WD.FROM + WD.SPACE).toCharArray();

    static final char[] _UPDATE = WD.UPDATE.toCharArray();
    static final char[] _SPACE_UPDATE_SPACE = (WD.SPACE + WD.UPDATE + WD.SPACE).toCharArray();
    static final char[] _SET = WD.SET.toCharArray();
    static final char[] _SPACE_SET_SPACE = (WD.SPACE + WD.SET + WD.SPACE).toCharArray();

    static final char[] _DELETE = WD.DELETE.toCharArray();
    static final char[] _SPACE_DELETE_SPACE = (WD.SPACE + WD.DELETE + WD.SPACE).toCharArray();

    static final char[] _JOIN = WD.JOIN.toCharArray();
    static final char[] _SPACE_JOIN_SPACE = (WD.SPACE + WD.JOIN + WD.SPACE).toCharArray();
    static final char[] _LEFT_JOIN = WD.LEFT_JOIN.toCharArray();
    static final char[] _SPACE_LEFT_JOIN_SPACE = (WD.SPACE + WD.LEFT_JOIN + WD.SPACE).toCharArray();
    static final char[] _RIGHT_JOIN = WD.RIGHT_JOIN.toCharArray();
    static final char[] _SPACE_RIGHT_JOIN_SPACE = (WD.SPACE + WD.RIGHT_JOIN + WD.SPACE).toCharArray();
    static final char[] _FULL_JOIN = WD.FULL_JOIN.toCharArray();
    static final char[] _SPACE_FULL_JOIN_SPACE = (WD.SPACE + WD.FULL_JOIN + WD.SPACE).toCharArray();
    static final char[] _CROSS_JOIN = WD.CROSS_JOIN.toCharArray();
    static final char[] _SPACE_CROSS_JOIN_SPACE = (WD.SPACE + WD.CROSS_JOIN + WD.SPACE).toCharArray();
    static final char[] _INNER_JOIN = WD.INNER_JOIN.toCharArray();
    static final char[] _SPACE_INNER_JOIN_SPACE = (WD.SPACE + WD.INNER_JOIN + WD.SPACE).toCharArray();
    static final char[] _NATURAL_JOIN = WD.NATURAL_JOIN.toCharArray();
    static final char[] _SPACE_NATURAL_JOIN_SPACE = (WD.SPACE + WD.NATURAL_JOIN + WD.SPACE).toCharArray();

    static final char[] _ON = WD.ON.toCharArray();
    static final char[] _SPACE_ON_SPACE = (WD.SPACE + WD.ON + WD.SPACE).toCharArray();
    static final char[] _USING = WD.USING.toCharArray();
    static final char[] _SPACE_USING_SPACE = (WD.SPACE + WD.USING + WD.SPACE).toCharArray();

    static final char[] _WHERE = WD.WHERE.toCharArray();
    static final char[] _SPACE_WHERE_SPACE = (WD.SPACE + WD.WHERE + WD.SPACE).toCharArray();
    static final char[] _GROUP_BY = WD.GROUP_BY.toCharArray();
    static final char[] _SPACE_GROUP_BY_SPACE = (WD.SPACE + WD.GROUP_BY + WD.SPACE).toCharArray();
    static final char[] _HAVING = WD.HAVING.toCharArray();
    static final char[] _SPACE_HAVING_SPACE = (WD.SPACE + WD.HAVING + WD.SPACE).toCharArray();
    static final char[] _ORDER_BY = WD.ORDER_BY.toCharArray();
    static final char[] _SPACE_ORDER_BY_SPACE = (WD.SPACE + WD.ORDER_BY + WD.SPACE).toCharArray();
    static final char[] _LIMIT = (WD.SPACE + WD.LIMIT + WD.SPACE).toCharArray();
    static final char[] _SPACE_LIMIT_SPACE = (WD.SPACE + WD.LIMIT + WD.SPACE).toCharArray();
    static final char[] _OFFSET = WD.OFFSET.toCharArray();
    static final char[] _SPACE_OFFSET_SPACE = (WD.SPACE + WD.OFFSET + WD.SPACE).toCharArray();
    static final char[] _AND = WD.AND.toCharArray();
    static final char[] _SPACE_AND_SPACE = (WD.SPACE + WD.AND + WD.SPACE).toCharArray();
    static final char[] _OR = WD.OR.toCharArray();
    static final char[] _SPACE_OR_SPACE = (WD.SPACE + WD.OR + WD.SPACE).toCharArray();

    static final char[] _UNION = WD.UNION.toCharArray();
    static final char[] _SPACE_UNION_SPACE = (WD.SPACE + WD.UNION + WD.SPACE).toCharArray();
    static final char[] _UNION_ALL = WD.UNION_ALL.toCharArray();
    static final char[] _SPACE_UNION_ALL_SPACE = (WD.SPACE + WD.UNION_ALL + WD.SPACE).toCharArray();
    static final char[] _INTERSECT = WD.INTERSECT.toCharArray();
    static final char[] _SPACE_INTERSECT_SPACE = (WD.SPACE + WD.INTERSECT + WD.SPACE).toCharArray();
    static final char[] _EXCEPT = WD.EXCEPT.toCharArray();
    static final char[] _SPACE_EXCEPT_SPACE = (WD.SPACE + WD.EXCEPT + WD.SPACE).toCharArray();
    static final char[] _EXCEPT2 = WD.EXCEPT2.toCharArray();
    static final char[] _SPACE_EXCEPT2_SPACE = (WD.SPACE + WD.EXCEPT2 + WD.SPACE).toCharArray();

    static final char[] _AS = WD.AS.toCharArray();
    static final char[] _SPACE_AS_SPACE = (WD.SPACE + WD.AS + WD.SPACE).toCharArray();

    static final char[] _SPACE_EQUAL_SPACE = (WD.SPACE + WD.EQUAL + WD.SPACE).toCharArray();

    static final char[] _SPACE_FOR_UPDATE = (WD.SPACE + WD.FOR_UPDATE).toCharArray();

    static final char[] _COMMA_SPACE = WD.COMMA_SPACE.toCharArray();

    static final String SPACE_AS_SPACE = WD.SPACE + WD.AS + WD.SPACE;

    private static final Map<String, Map<String, String>> entityTablePropColumnNameMap = new ObjectPool<>(1024);
    private static final Map<Class<?>, Set<String>> subEntityPropNamesPool = new ObjectPool<>(1024);
    private static final Map<Class<?>, Set<String>> nonSubEntityPropNamesPool = new ObjectPool<>(1024);
    private static final Map<Class<?>, List<String>> classPropNamesPool = new ConcurrentHashMap<>();
    // private static final Map<Class<?>, Set<String>> classPropNameSetPool = new ConcurrentHashMap<>();
    private static final Map<String, char[]> tableDeleteFrom = new ConcurrentHashMap<>();
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
    public static void registerEntityPropColumnNameMap(final String entityTableName, final Map<String, String> propColumnNameMap) {
        final Map<String, String> m = ImmutableMap.copyOf(propColumnNameMap);

        entityTablePropColumnNameMap.put(entityTableName, m);
        entityTablePropColumnNameMap.put(entityTableName.toLowerCase(), m);
        entityTablePropColumnNameMap.put(entityTableName.toUpperCase(), m);
        entityTablePropColumnNameMap.put(ClassUtil.toLowerCaseWithUnderscore(entityTableName), m);
        entityTablePropColumnNameMap.put(ClassUtil.toUpperCaseWithUnderscore(entityTableName), m);
    }

    /**
     * 
     * 
     * @param entityClass
     * @param nonSubEntityPropNames
     */
    public static void registerNonSubEntityPropNames(final Class<?> entityClass, final Collection<String> nonSubEntityPropNames) {
        final Set<String> set = ImmutableSet.copyOf(nonSubEntityPropNames);

        synchronized (entityClass) {
            nonSubEntityPropNamesPool.put(entityClass, set);

            subEntityPropNamesPool.remove(entityClass);
            classPropNamesPool.remove(entityClass);
        }
    }

    static Collection<String> getPropNamesByClass(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
        Set<String> subEntityPropNames = null;

        if (N.isNullOrEmpty(excludedPropNames)
                && (includeSubEntityProperties == false || N.isNullOrEmpty(subEntityPropNames = getSubEntityPropNames(entityClass)))) {
            return getNonSubEntityPropNames(entityClass);
        } else {
            final Set<String> entityPropNames = new LinkedHashSet<>(ClassUtil.getPropGetMethodList(entityClass).keySet());
            subEntityPropNames = subEntityPropNames == null ? getSubEntityPropNames(entityClass) : subEntityPropNames;

            if (N.notNullOrEmpty(subEntityPropNames)) {
                entityPropNames.removeAll(subEntityPropNames);

                if (includeSubEntityProperties) {
                    Method method = null;
                    Class<?> subEntityClass = null;

                    for (String subEntityPropName : subEntityPropNames) {
                        method = ClassUtil.getPropGetMethod(entityClass, subEntityPropName);
                        subEntityClass = N.isEntity(method.getReturnType()) ? method.getReturnType() : ClassUtil.getTypeArgumentsByMethod(method)[0];

                        for (String pn : getPropNamesByClass(subEntityClass, false, null)) {
                            entityPropNames.add(
                                    N.concat(ClassUtil.getSimpleClassName(subEntityClass), WD.PERIOD, pn, SPACE_AS_SPACE, subEntityPropName, WD.PERIOD, pn));
                        }
                    }
                }
            }

            if (N.notNullOrEmpty(excludedPropNames)) {
                Method method = null;

                for (String propName : excludedPropNames) {
                    if (!entityPropNames.remove(propName)) {
                        method = ClassUtil.getPropGetMethod(entityClass, propName);

                        if (method != null) {
                            entityPropNames.remove(ClassUtil.getPropNameByMethod(method));
                        }
                    }
                }
            }

            return entityPropNames;
        }
    }

    /**
     * Returns an immutable list of the property name by the specified entity class.
     * 
     * @param entityClass
     * @return
     */
    static List<String> getNonSubEntityPropNames(final Class<?> entityClass) {
        synchronized (entityClass) {
            List<String> nonSubEntityPropNames = classPropNamesPool.get(entityClass);

            if (nonSubEntityPropNames == null) {
                nonSubEntityPropNames = new ArrayList<>(ClassUtil.getPropGetMethodList(entityClass).keySet());
                final Set<String> subEntityPropNames = getSubEntityPropNames(entityClass);

                if (N.notNullOrEmpty(subEntityPropNames)) {
                    nonSubEntityPropNames.removeAll(subEntityPropNames);
                }

                classPropNamesPool.put(entityClass, nonSubEntityPropNames);
            }

            return nonSubEntityPropNames;
        }
    }

    static Set<String> getSubEntityPropNames(final Class<?> entityClass) {
        synchronized (entityClass) {
            Set<String> subEntityPropNames = subEntityPropNamesPool.get(entityClass);

            if (subEntityPropNames == null) {
                subEntityPropNames = new LinkedHashSet<>();
                final Map<String, Method> propMethods = ClassUtil.getPropGetMethodList(entityClass);
                final Set<String> nonSubEntityPropNames = nonSubEntityPropNamesPool.get(entityClass);
                Class<?>[] typeParameterClasses = null;

                for (Map.Entry<String, Method> entry : propMethods.entrySet()) {
                    if ((N.isEntity(entry.getValue().getReturnType())
                            || (N.notNullOrEmpty(typeParameterClasses = ClassUtil.getTypeArgumentsByMethod(entry.getValue()))
                                    && N.isEntity(typeParameterClasses[0])))
                            && (nonSubEntityPropNames == null || !nonSubEntityPropNames.contains(entry.getKey()))) {
                        subEntityPropNames.add(entry.getKey());
                    }
                }
            }

            subEntityPropNamesPool.put(entityClass, subEntityPropNames);
            return subEntityPropNames;
        }
    }

    private static List<String> getSelectTableNames(final Class<?> entityClass) {
        final Set<String> subEntityPropNames = getSubEntityPropNames(entityClass);

        if (N.isNullOrEmpty(subEntityPropNames)) {
            return N.emptyList();
        }

        final List<String> res = new ArrayList<>(subEntityPropNames.size() + 1);
        res.add(ClassUtil.getSimpleClassName(entityClass));

        Method method = null;
        Class<?> subEntityClass = null;

        for (String subEntityPropName : subEntityPropNames) {
            method = ClassUtil.getPropGetMethod(entityClass, subEntityPropName);
            subEntityClass = N.isEntity(method.getReturnType()) ? method.getReturnType() : ClassUtil.getTypeArgumentsByMethod(method)[0];
            res.add(ClassUtil.getSimpleClassName(subEntityClass));
        }

        return res;
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

    public SQLBuilder into(final String tableName) {
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

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        if (N.notNullOrEmpty(columnNames)) {
            if (columnNames.length == 1 && columnNames[0].indexOf(WD._SPACE) > 0) {
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

        sb.append(WD._PARENTHESES_R);

        sb.append(_SPACE_VALUES_SPACE);

        sb.append(WD._PARENTHESES_L);

        if (N.notNullOrEmpty(columnNames)) {
            switch (sqlPolicy) {
                case SQL:
                case RAW_SQL: {
                    for (int i = 0, len = columnNames.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(_COMMA_SPACE);
                        }

                        sb.append(WD._QUESTION_MARK);
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

                        sb.append(WD._QUESTION_MARK);
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

    public SQLBuilder into(final Class<?> entityClass) {
        return into(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder from(String expr) {
        expr = expr.trim();
        String tableName = expr.indexOf(WD._COMMA) > 0 ? N.substring(expr, 0, WD._COMMA).get() : expr;

        if (tableName.indexOf(WD._SPACE) > 0) {
            tableName = N.substring(tableName, 0, WD._SPACE).get();
        }

        return from(tableName, tableName.indexOf(WD._SPACE) > 0 ? expr : formalizeName(expr));
    }

    @SafeVarargs
    public final SQLBuilder from(final String... tableNames) {
        if (tableNames.length == 1) {
            return from(tableNames[0]);
        }

        final String tableName = tableNames[0].trim();

        //        if (tableName.indexOf(D._SPACE) > 0) {
        //            tableName = N.substring(tableName, 0, D._SPACE).get();
        //        }

        return from(tableName, fromCause(Arrays.asList(tableNames)));
    }

    public SQLBuilder from(final Collection<String> tableNames) {
        if (tableNames.size() == 1) {
            return from(tableNames.iterator().next());
        }

        final String tableName = tableNames.iterator().next().trim();

        //        if (tableName.indexOf(D._SPACE) > 0) {
        //            tableName = N.substring(tableName, 0, D._SPACE).get();
        //        }

        return from(tableName, fromCause(tableNames));
    }

    private String fromCause(Collection<String> tableNames) {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            for (String tableName : tableNames) {
                if (sb.length() > 0) {
                    sb.append(WD.COMMA_SPACE);
                }

                sb.append(formalizeName(tableName));
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    public SQLBuilder from(final Map<String, String> tableAliases) {
        final String tableName = tableAliases.keySet().iterator().next().trim();

        //        if (tableName.indexOf(D._SPACE) > 0) {
        //            tableName = N.substring(tableName, 0, D._SPACE).get();
        //        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();
        String fromCause = null;

        try {
            for (Map.Entry<String, String> entry : tableAliases.entrySet()) {
                if (sb.length() > 0) {
                    sb.append(WD.COMMA_SPACE);
                }

                sb.append(formalizeName(entry.getKey()));
                sb.append(" ");
                sb.append(entry.getValue());
            }

            fromCause = sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }

        return from(tableName, fromCause);
    }

    private SQLBuilder from(final String tableName, final String fromCause) {
        if (op != OperationType.QUERY) {
            throw new AbacusException("Invalid operation: " + op);
        }

        if (N.isNullOrEmpty(columnNames) && N.isNullOrEmpty(columnNameList) && N.isNullOrEmpty(columnAliases)) {
            throw new AbacusException("Column names or props must be set first by select");
        }

        this.tableName = tableName;

        sb.append(_SELECT);
        sb.append(WD._SPACE);

        if (N.notNullOrEmpty(predicates)) {
            sb.append(predicates);
            sb.append(WD._SPACE);
        }

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);

        if (N.notNullOrEmpty(columnNames)) {
            if (columnNames.length == 1) {
                final String columnName = N.trim(columnNames[0]);
                int idx = columnName.indexOf(' ');

                if (idx < 0) {
                    idx = columnName.indexOf(',');
                }

                if (idx > 0) {
                    sb.append(columnName);
                } else {
                    sb.append(formalizeName(propColumnNameMap, columnName));

                    if (namingPolicy != NamingPolicy.LOWER_CAMEL_CASE && !WD.ASTERISK.equals(columnName)) {
                        sb.append(_SPACE_AS_SPACE);

                        sb.append(WD._QUOTATION_D);
                        sb.append(columnName);
                        sb.append(WD._QUOTATION_D);
                    }
                }
            } else {
                String columnName = null;

                for (int i = 0, len = columnNames.length; i < len; i++) {
                    columnName = N.trim(columnNames[i]);

                    if (i > 0) {
                        sb.append(_COMMA_SPACE);
                    }

                    int idx = columnName.indexOf(' ');

                    if (idx > 0) {
                        int idx2 = columnName.indexOf(" AS ", idx);

                        if (idx2 < 0) {
                            idx2 = columnName.indexOf(" as ", idx);
                        }

                        sb.append(formalizeName(propColumnNameMap, columnName.substring(0, idx).trim()));

                        sb.append(_SPACE_AS_SPACE);

                        sb.append(WD._QUOTATION_D);
                        sb.append(columnName.substring(idx2 > 0 ? idx2 + 4 : idx + 1).trim());
                        sb.append(WD._QUOTATION_D);
                    } else {
                        sb.append(formalizeName(propColumnNameMap, columnName));

                        if (namingPolicy != NamingPolicy.LOWER_CAMEL_CASE && !WD.ASTERISK.equals(columnName)) {
                            sb.append(_SPACE_AS_SPACE);

                            sb.append(WD._QUOTATION_D);
                            sb.append(columnName);
                            sb.append(WD._QUOTATION_D);
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

                sb.append(formalizeName(propColumnNameMap, columnName));

                if (namingPolicy != NamingPolicy.LOWER_CAMEL_CASE && !WD.ASTERISK.equals(columnName)) {
                    sb.append(_SPACE_AS_SPACE);

                    sb.append(WD._QUOTATION_D);
                    sb.append(columnName);
                    sb.append(WD._QUOTATION_D);
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

                    sb.append(WD._QUOTATION_D);
                    sb.append(entry.getValue());
                    sb.append(WD._QUOTATION_D);
                }
            }
        }

        sb.append(_SPACE_FROM_SPACE);

        // sb.append(formalizeName(fromCause));
        sb.append(fromCause);

        return this;
    }

    public SQLBuilder from(final Class<?> entityClass) {
        return from(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder join(final String expr) {
        sb.append(_SPACE_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder join(final Class<?> entityClass) {
        return join(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder leftJoin(final String expr) {
        sb.append(_SPACE_LEFT_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder leftJoin(final Class<?> entityClass) {
        return leftJoin(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder rightJoin(final String expr) {
        sb.append(_SPACE_RIGHT_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder rightJoin(final Class<?> entityClass) {
        return rightJoin(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder fullJoin(final String expr) {
        sb.append(_SPACE_FULL_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder fullJoin(final Class<?> entityClass) {
        return fullJoin(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder crossJoin(final String expr) {
        sb.append(_SPACE_CROSS_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder crossJoin(final Class<?> entityClass) {
        return crossJoin(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder innerJoin(final String expr) {
        sb.append(_SPACE_INNER_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder innerJoin(final Class<?> entityClass) {
        return innerJoin(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder naturalJoin(final String expr) {
        sb.append(_SPACE_NATURAL_JOIN_SPACE);

        sb.append(formalizeName(expr));

        return this;
    }

    public SQLBuilder naturalJoin(final Class<?> entityClass) {
        return naturalJoin(ClassUtil.getSimpleClassName(entityClass));
    }

    public SQLBuilder on(final String expr) {
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
    public SQLBuilder on(final Condition cond) {
        sb.append(_SPACE_ON_SPACE);

        appendCondition(cond);

        return this;
    }

    public SQLBuilder using(final String expr) {
        sb.append(_SPACE_USING_SPACE);

        sb.append(formalizeName(tableName, expr));

        return this;
    }

    public SQLBuilder where(final String expr) {
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
            } else if (i < len - 1 && words.get(i + 1).charAt(0) == WD._PARENTHESES_L) {
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
    public SQLBuilder where(final Condition cond) {
        init(true);

        sb.append(_SPACE_WHERE_SPACE);

        appendCondition(cond);

        return this;
    }

    public SQLBuilder groupBy(final String expr) {
        sb.append(_SPACE_GROUP_BY_SPACE);

        if (expr.indexOf(WD._SPACE) > 0) {
            // sb.append(columnNames[0]);
            appendStringExpr(expr);
        } else {
            sb.append(formalizeName(tableName, expr));
        }

        return this;
    }

    @SafeVarargs
    public final SQLBuilder groupBy(final String... columnNames) {
        sb.append(_SPACE_GROUP_BY_SPACE);

        if (columnNames.length == 1) {
            if (columnNames[0].indexOf(WD._SPACE) > 0) {
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

    public SQLBuilder groupBy(final String columnName, final SortDirection direction) {
        groupBy(columnName);

        sb.append(WD._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder groupBy(final Collection<String> columnNames) {
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

    public SQLBuilder groupBy(final Collection<String> columnNames, final SortDirection direction) {
        groupBy(columnNames);

        sb.append(WD._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder groupBy(final Map<String, SortDirection> orders) {
        sb.append(_SPACE_GROUP_BY_SPACE);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);
        int i = 0;
        for (Map.Entry<String, SortDirection> entry : orders.entrySet()) {
            if (i++ > 0) {
                sb.append(_COMMA_SPACE);
            }

            sb.append(formalizeName(propColumnNameMap, entry.getKey()));

            sb.append(WD._SPACE);
            sb.append(entry.getValue().toString());
        }

        return this;
    }

    public SQLBuilder having(final String expr) {
        sb.append(_SPACE_HAVING_SPACE);

        appendStringExpr(expr);

        return this;
    }

    /**
     * 
     * @param cond any literal written in <code>Expression</code> condition won't be formalized
     * @return
     */
    public SQLBuilder having(final Condition cond) {
        sb.append(_SPACE_HAVING_SPACE);

        appendCondition(cond);

        return this;
    }

    public SQLBuilder orderBy(final String expr) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        if (expr.indexOf(WD._SPACE) > 0) {
            // sb.append(columnNames[0]);
            appendStringExpr(expr);
        } else {
            sb.append(formalizeName(tableName, expr));
        }

        return this;
    }

    @SafeVarargs
    public final SQLBuilder orderBy(final String... columnNames) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        if (columnNames.length == 1) {
            if (columnNames[0].indexOf(WD._SPACE) > 0) {
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

    public SQLBuilder orderBy(final String columnName, final SortDirection direction) {
        orderBy(columnName);

        sb.append(WD._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder orderBy(final Collection<String> columnNames) {
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

    public SQLBuilder orderBy(final Collection<String> columnNames, final SortDirection direction) {
        orderBy(columnNames);

        sb.append(WD._SPACE);
        sb.append(direction.toString());

        return this;
    }

    public SQLBuilder orderBy(final Map<String, SortDirection> orders) {
        sb.append(_SPACE_ORDER_BY_SPACE);

        final Map<String, String> propColumnNameMap = entityTablePropColumnNameMap.get(tableName);
        int i = 0;
        for (Map.Entry<String, SortDirection> entry : orders.entrySet()) {
            if (i++ > 0) {
                sb.append(_COMMA_SPACE);
            }

            sb.append(formalizeName(propColumnNameMap, entry.getKey()));

            sb.append(WD._SPACE);
            sb.append(entry.getValue().toString());
        }

        return this;
    }

    public SQLBuilder limit(final int count) {
        sb.append(_SPACE_LIMIT_SPACE);

        sb.append(count);

        return this;
    }

    public SQLBuilder limit(final int offset, final int count) {
        sb.append(_SPACE_LIMIT_SPACE);

        sb.append(offset);

        sb.append(_COMMA_SPACE);

        sb.append(count);

        return this;
    }

    public SQLBuilder offset(final int offset) {
        sb.append(_SPACE_OFFSET_SPACE);

        sb.append(offset);

        return this;
    }

    public SQLBuilder union(final SQLBuilder sqlBuilder) {
        final String sql = sqlBuilder.sql();

        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return union(sql);
    }

    public SQLBuilder union(final String query) {
        return union(N.asArray(query));
    }

    @SafeVarargs
    public final SQLBuilder union(final String... columnNames) {
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

    public SQLBuilder union(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_UNION_SPACE);

        return this;
    }

    public SQLBuilder unionAll(final SQLBuilder sqlBuilder) {
        final String sql = sqlBuilder.sql();

        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return unionAll(sql);
    }

    public SQLBuilder unionAll(final String query) {
        return unionAll(N.asArray(query));
    }

    @SafeVarargs
    public final SQLBuilder unionAll(final String... columnNames) {
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

    public SQLBuilder unionAll(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_UNION_ALL_SPACE);

        return this;
    }

    public SQLBuilder intersect(final SQLBuilder sqlBuilder) {
        final String sql = sqlBuilder.sql();

        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return intersect(sql);
    }

    public SQLBuilder intersect(final String query) {
        return intersect(N.asArray(query));
    }

    @SafeVarargs
    public final SQLBuilder intersect(final String... columnNames) {
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

    public SQLBuilder intersect(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_INTERSECT_SPACE);

        return this;
    }

    public SQLBuilder except(final SQLBuilder sqlBuilder) {
        final String sql = sqlBuilder.sql();

        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return except(sql);
    }

    public SQLBuilder except(final String query) {
        return except(N.asArray(query));
    }

    @SafeVarargs
    public final SQLBuilder except(final String... columnNames) {
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

    public SQLBuilder except(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_EXCEPT_SPACE);

        return this;
    }

    public SQLBuilder minus(final SQLBuilder sqlBuilder) {
        final String sql = sqlBuilder.sql();

        if (N.notNullOrEmpty(sqlBuilder.parameters())) {
            parameters.addAll(sqlBuilder.parameters());
        }

        return minus(sql);
    }

    public SQLBuilder minus(final String query) {
        return minus(N.asArray(query));
    }

    @SafeVarargs
    public final SQLBuilder minus(final String... columnNames) {
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

    public SQLBuilder minus(final Collection<String> columnNames) {
        op = OperationType.QUERY;

        this.columnNames = null;
        this.columnNameList = columnNames;
        this.columnAliases = null;

        sb.append(_SPACE_EXCEPT2_SPACE);

        return this;
    }

    public SQLBuilder forUpdate() {
        sb.append(_SPACE_FOR_UPDATE);

        return this;
    }

    public SQLBuilder set(final String expr) {
        return set(N.asArray(expr));
    }

    @SafeVarargs
    public final SQLBuilder set(final String... columnNames) {
        init(false);

        if (columnNames.length == 1 && SQLParser.parse(columnNames[0]).contains(WD.EQUAL)) {
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

                        sb.append(WD._QUESTION_MARK);
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

    public SQLBuilder set(final Collection<String> columnNames) {
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

                    sb.append(WD._QUESTION_MARK);
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

    public SQLBuilder set(final Map<String, Object> props) {
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
    public SQLBuilder set(final Object entity) {
        return set(entity, null);
    }

    /**
     * Only the dirty properties will be set into the result SQL if the specified entity is a dirty marker entity.
     * 
     * @param entity
     * @param excludedPropNames
     * @return
     */
    @SuppressWarnings("deprecation")
    public SQLBuilder set(final Object entity, final Set<String> excludedPropNames) {
        if (entity instanceof String) {
            return set(N.asArray((String) entity));
        } else if (entity instanceof Map) {
            if (N.isNullOrEmpty(excludedPropNames)) {
                return set((Map<String, Object>) entity);
            } else {
                final Map<String, Object> props = new LinkedHashMap<>((Map<String, Object>) entity);
                Maps.removeAll(props, excludedPropNames);
                return set(props);
            }
        } else {
            final Collection<String> propNames = getPropNamesByClass(entity.getClass(), false, excludedPropNames);
            final Set<String> dirtyPropNames = N.isDirtyMarker(entity.getClass()) ? ((DirtyMarker) entity).dirtyPropNames() : null;
            final Map<String, Object> props = N.newHashMap(N.initHashCapacity(N.isNullOrEmpty(dirtyPropNames) ? propNames.size() : dirtyPropNames.size()));

            for (String propName : propNames) {
                if (N.isNullOrEmpty(dirtyPropNames) || dirtyPropNames.contains(propName)) {
                    props.put(propName, ClassUtil.getPropValue(entity, propName));
                }
            }

            return set(props);
        }
    }

    public SQLBuilder set(Class<?> entityClass) {
        return set(entityClass, null);
    }

    public SQLBuilder set(Class<?> entityClass, final Set<String> excludedPropNames) {
        return set(getPropNamesByClass(entityClass, false, excludedPropNames));
    }

    public <T, EX extends Exception> T apply(final Try.Function<? super SP, T, EX> func) throws EX {
        return func.apply(this.pair());
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
    //    public <R> Nullable<R> execute(final Class<R> targetClass, final SQLExecutor sqlExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //            return Nullable.from(sqlExecutor.queryForEntity(targetClass, sql(), this.parameters));
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
    //    public <R> Nullable<R> execute(final Class<R> targetClass, final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //                return Nullable.from(sqlExecutor.queryForEntity(targetClass, sql(), this.parameters));
    //            } else {
    //                return sqlExecutor.queryForSingleResult(targetClass, sql(), this.parameters);
    //            }
    //        } else {
    //            if (N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass)) {
    //                return Nullable.from(sqlExecutor.queryForEntity(targetClass, sql(), parameters));
    //            } else {
    //                return sqlExecutor.queryForSingleResult(targetClass, sql(), parameters);
    //            }
    //        }
    //    }
    //
    //    @Beta
    //    public CompletableFuture asyncExecute(final SQLExecutor sqlExecutor) {
    //        if (op == OperationType.QUERY) {
    //            return (CompletableFuture) sqlExecutor.asyncExecutor().query(sql(), this.parameters);
    //        } /* else if (op == OperationType.ADD) {
    //            return (CompletableFuture) sqlExecutor.asyncSQLExecutor().insert(sql(), this.parameters);
    //          } */ else {
    //            return (CompletableFuture) sqlExecutor.asyncExecutor().update(sql(), this.parameters);
    //        }
    //    }
    //
    //    @Beta
    //    public CompletableFuture asyncExecute(final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (N.isNullOrEmpty(parameters)) {
    //            if (op == OperationType.QUERY) {
    //                return (CompletableFuture) sqlExecutor.asyncExecutor().query(sql(), this.parameters);
    //            } /* else if (op == OperationType.ADD) {
    //                return (CompletableFuture) sqlExecutor.asyncSQLExecutor().insert(sql(), this.parameters);
    //              } */ else {
    //                return (CompletableFuture) sqlExecutor.asyncExecutor().update(sql(), this.parameters);
    //            }
    //        } else {
    //            if (op == OperationType.QUERY) {
    //                return (CompletableFuture) sqlExecutor.asyncExecutor().query(sql(), parameters);
    //            } /* else if (op == OperationType.ADD) {
    //                return (CompletableFuture) sqlExecutor.asyncSQLExecutor().insert(sql(), parameters);
    //              } */ else {
    //                return (CompletableFuture) sqlExecutor.asyncExecutor().update(sql(), parameters);
    //            }
    //        }
    //    }
    //
    //    @Beta
    //    public <R> CompletableFuture<Nullable<R>> asyncExecute(final Class<R> targetClass, final SQLExecutor sqlExecutor) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return sqlExecutor.asyncExecutor().asyncExecutor().execute(new Callable<Nullable<R>>() {
    //            @Override
    //            public Nullable<R> call() throws Exception {
    //                return execute(targetClass, sqlExecutor);
    //            }
    //        });
    //    }
    //
    //    @Beta
    //    public <R> CompletableFuture<Nullable<R>> asyncExecute(final Class<R> targetClass, final SQLExecutor sqlExecutor, final Object... parameters) {
    //        if (op != OperationType.QUERY) {
    //            throw new IllegalArgumentException("Only SELECT statement is supported");
    //        }
    //
    //        return sqlExecutor.asyncExecutor().asyncExecutor().execute(new Callable<Nullable<R>>() {
    //            @Override
    //            public Nullable<R> call() throws Exception {
    //                return execute(targetClass, sqlExecutor, parameters);
    //            }
    //        });
    //    }
    /**
     * This SQLBuilder will be closed after <code>sql()</code> is called.
     * 
     * @return
     */
    public String sql() {
        if (sb == null) {
            throw new AbacusException("This SQLBuilder has been closed after sql() was called previously");
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
    public SP pair() {
        return new SP(sql(), parameters);
    }

    void init(boolean setForUpdate) {
        if (sb.length() > 0) {
            return;
        }

        if (op == OperationType.UPDATE) {
            sb.append(_UPDATE);

            sb.append(WD._SPACE);
            sb.append(formalizeName(tableName));

            sb.append(_SPACE_SET_SPACE);

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

    private void setParameterForSQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(WD._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(Expression.formalize(propValue));
        }
    }

    private void setParameterForRawSQL(final Object propValue) {
        if (L.QME.equals(propValue)) {
            sb.append(WD._QUESTION_MARK);
        } else if (propValue instanceof Condition) {
            appendCondition((Condition) propValue);
        } else {
            sb.append(WD._QUESTION_MARK);

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

            sb.append(WD._SPACE);
            sb.append(binary.getOperator().toString());
            sb.append(WD._SPACE);

            Object propValue = binary.getPropValue();
            setParameter(propName, propValue);
        } else if (cond instanceof Between) {
            final Between bt = (Between) cond;
            final String propName = bt.getPropName();

            sb.append(formalizeName(tableName, propName));

            sb.append(WD._SPACE);
            sb.append(bt.getOperator().toString());
            sb.append(WD._SPACE);

            Object minValue = bt.getMinValue();
            if (sqlPolicy == SQLPolicy.NAMED_SQL || sqlPolicy == SQLPolicy.IBATIS_SQL) {
                setParameter("min" + N.capitalize(propName), minValue);
            } else {
                setParameter(propName, minValue);
            }

            sb.append(WD._SPACE);
            sb.append(WD.AND);
            sb.append(WD._SPACE);

            Object maxValue = bt.getMaxValue();
            if (sqlPolicy == SQLPolicy.NAMED_SQL || sqlPolicy == SQLPolicy.IBATIS_SQL) {
                setParameter("max" + N.capitalize(propName), maxValue);
            } else {
                setParameter(propName, maxValue);
            }
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
                // TODO ((id = :id) AND (gui = :gui)) is not support in Cassandra.
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
                return ClassUtil.toLowerCaseWithUnderscore(entityPropName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toUpperCaseWithUnderscore(entityPropName);

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
                return ClassUtil.toLowerCaseWithUnderscore(propName);

            case UPPER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toUpperCaseWithUnderscore(propName);

            default:
                return propName;
        }
    }

    private boolean isSubQuery(final String... columnNames) {
        if (columnNames.length == 1) {
            int index = SQLParser.indexWord(columnNames[0], WD.SELECT, 0, false);

            if (index >= 0) {
                index = SQLParser.indexWord(columnNames[0], WD.FROM, index, false);

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

    private static void parseInsertEntity(final SQLBuilder instance, final Object entity, final Set<String> excludedPropNames) {
        if (entity instanceof String) {
            instance.columnNames = N.asArray((String) entity);
        } else if (entity instanceof Map) {
            if (N.isNullOrEmpty(excludedPropNames)) {
                instance.props = (Map<String, Object>) entity;
            } else {
                instance.props = new LinkedHashMap<>((Map<String, Object>) entity);
                Maps.removeAll(instance.props, excludedPropNames);
            }
        } else {
            final Collection<String> propNames = getPropNamesByClass(entity.getClass(), false, excludedPropNames);
            final Map<String, Object> map = N.newHashMap(N.initHashCapacity(propNames.size()));

            for (String propName : propNames) {
                map.put(propName, ClassUtil.getPropValue(entity, propName));
            }

            instance.props = map;
        }
    }

    private static Collection<Map<String, Object>> toInsertPropsList(final Collection<?> propsList) {
        final Optional<?> first = N.firstNonNull(propsList);

        if (first.isPresent() && first.get() instanceof Map) {
            return (List<Map<String, Object>>) propsList;
        } else {
            final Class<?> cls = first.get().getClass();
            final Collection<String> propNames = getPropNamesByClass(cls, false, null);
            final List<Map<String, Object>> newPropsList = new ArrayList<>(propsList.size());

            for (Object entity : propsList) {
                final Map<String, Object> props = N.newHashMap(N.initHashCapacity(propNames.size()));

                for (String propName : propNames) {
                    props.put(propName, ClassUtil.getPropValue(entity, propName));
                }

                newPropsList.add(props);
            }

            return newPropsList;
        }
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
    public static final class E extends SQLBuilder {
        E() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.SQL);
        }

        static E createInstance() {
            return new E();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the sql will be parameterized with question mark.
     * 
     * @author haiyang li 
     *
     */
    public static final class RE extends SQLBuilder {
        RE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.RAW_SQL);
        }

        static RE createInstance() {
            return new RE();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the sql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * @author haiyang li 
     *
     */
    public static final class NE extends SQLBuilder {
        NE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.NAMED_SQL);
        }

        static NE createInstance() {
            return new NE();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to lower case with underscore and the sql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li 
     *
     */
    public static final class SE extends SQLBuilder {
        SE() {
            super(NamingPolicy.LOWER_CASE_WITH_UNDERSCORE, SQLPolicy.IBATIS_SQL);
        }

        static SE createInstance() {
            return new SE();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore.
     * 
     * @author haiyang li
     *
     */
    public static final class E2 extends SQLBuilder {
        E2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.SQL);
        }

        static E2 createInstance() {
            return new E2();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the sql will be parameterized with question mark.
     * 
     * @author haiyang li
     *
     */
    public static final class RE2 extends SQLBuilder {
        RE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.RAW_SQL);
        }

        static RE2 createInstance() {
            return new RE2();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the sql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * 
     * @author haiyang li
     *
     */
    public static final class NE2 extends SQLBuilder {
        NE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.NAMED_SQL);
        }

        static NE2 createInstance() {
            return new NE2();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be converted to upper case with underscore and the sql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li
     *
     */
    public static final class SE2 extends SQLBuilder {
        SE2() {
            super(NamingPolicy.UPPER_CASE_WITH_UNDERSCORE, SQLPolicy.IBATIS_SQL);
        }

        static SE2 createInstance() {
            return new SE2();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change.
     * 
     * @author haiyang li
     *
     */
    public static final class E3 extends SQLBuilder {
        E3() {
            super(NamingPolicy.LOWER_CAMEL_CASE, SQLPolicy.SQL);
        }

        static E3 createInstance() {
            return new E3();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the sql will be parameterized with question mark.
     * 
     * @author haiyang li
     *
     */
    public static final class RE3 extends SQLBuilder {
        RE3() {
            super(NamingPolicy.LOWER_CAMEL_CASE, SQLPolicy.RAW_SQL);
        }

        static RE3 createInstance() {
            return new RE3();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the sql will be parameterized with named parameter with Hibernate/JPA format <code> :parameterName</code>
     * 
     * @author haiyang li
     *
     */
    public static final class NE3 extends SQLBuilder {
        NE3() {
            super(NamingPolicy.LOWER_CAMEL_CASE, SQLPolicy.NAMED_SQL);
        }

        static NE3 createInstance() {
            return new NE3();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    /**
     * All the property/column names in collection/map/entity/condition will be kept without any change and the sql will be parameterized with named parameter with Ibatis format <code>#{parameterName}</code>.
     * 
     * @author haiyang li
     *
     */
    public static final class SE3 extends SQLBuilder {
        SE3() {
            super(NamingPolicy.LOWER_CAMEL_CASE, SQLPolicy.IBATIS_SQL);
        }

        static SE3 createInstance() {
            return new SE3();
        }

        public static SQLBuilder insert(final String expr) {
            return insert(N.asArray(expr));
        }

        @SafeVarargs
        public static SQLBuilder insert(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder insert(final Map<String, Object> props) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;
            instance.props = props;

            return instance;
        }

        public static SQLBuilder insert(final Object entity) {
            return insert(entity, null);
        }

        public static SQLBuilder insert(final Object entity, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            parseInsertEntity(instance, entity, excludedPropNames);

            return instance;
        }

        public static SQLBuilder insert(final Class<?> entityClass) {
            return insert(entityClass, null);
        }

        public static SQLBuilder insert(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(getPropNamesByClass(entityClass, false, excludedPropNames));
        }

        public static SQLBuilder insertInto(final Class<?> entityClass) {
            return insertInto(entityClass, null);
        }

        public static SQLBuilder insertInto(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return insert(entityClass, excludedPropNames).into(entityClass);
        }

        /**
         * Generate the MySQL style batch insert sql.
         * 
         * @param propsList list of entity or properties map.
         * @return
         */
        @Beta
        public static SQLBuilder batchInsert(final Collection<?> propsList) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.ADD;

            instance.propsList = toInsertPropsList(propsList);

            return instance;
        }

        @SafeVarargs
        public static SQLBuilder select(final String... columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.columnNames = columnNames;

            return instance;
        }

        /**
         * 
         * @param expr <code>ALL | DISTINCT | DISTINCTROW...</code>
         * @param columnNames
         * @return
         */
        public static SQLBuilder select(final String expr, final String[] columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNames = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Collection<String> columnNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnNameList = columnNames;

            return instance;
        }

        public static SQLBuilder select(final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

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
        public static SQLBuilder select(final String expr, final Map<String, String> columnAliases) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.QUERY;
            instance.predicates = expr;
            instance.columnAliases = columnAliases;

            return instance;
        }

        public static SQLBuilder select(final Class<?> entityClass) {
            return select(entityClass, false);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return select(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder select(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return select(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder select(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames));
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass) {
            return selectFrom(entityClass, false);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties) {
            return selectFrom(entityClass, includeSubEntityProperties, null);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final Set<String> excludedPropNames) {
            return selectFrom(entityClass, false, excludedPropNames);
        }

        public static SQLBuilder selectFrom(final Class<?> entityClass, final boolean includeSubEntityProperties, final Set<String> excludedPropNames) {
            if (includeSubEntityProperties) {
                final List<String> selectTableNames = getSelectTableNames(entityClass);

                if (N.isNullOrEmpty(selectTableNames)) {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
                } else {
                    return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames).toArray(new String[0]))
                            .from(selectTableNames);
                }
            } else {
                return select(getPropNamesByClass(entityClass, includeSubEntityProperties, excludedPropNames)).from(entityClass);
            }
        }

        public static SQLBuilder update(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder update(final Class<?> entityClass) {
            return update(entityClass, null);
        }

        public static SQLBuilder update(final Class<?> entityClass, final Set<String> excludedPropNames) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.UPDATE;
            instance.tableName = ClassUtil.getSimpleClassName(entityClass);
            instance.columnNameList = getPropNamesByClass(entityClass, false, excludedPropNames);

            return instance;
        }

        public static SQLBuilder deleteFrom(final String tableName) {
            final SQLBuilder instance = createInstance();

            instance.op = OperationType.DELETE;
            instance.tableName = tableName;

            return instance;
        }

        public static SQLBuilder deleteFrom(final Class<?> entityClass) {
            return deleteFrom(ClassUtil.getSimpleClassName(entityClass));
        }
    }

    public static final class SP {
        public final String sql;
        public final List<Object> parameters;

        SP(final String sql, final List<Object> parameters) {
            this.sql = sql;
            this.parameters = ImmutableList.of(parameters);
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

            if (obj instanceof SP) {
                SP other = (SP) obj;

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
