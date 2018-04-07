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

package com.landawn.abacus.android.util;

import static com.landawn.abacus.util.WD.COMMA_SPACE;
import static com.landawn.abacus.util.WD._BRACE_L;
import static com.landawn.abacus.util.WD._BRACE_R;
import static com.landawn.abacus.util.WD._EQUAL;
import static com.landawn.abacus.util.WD._SPACE;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Date;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.condition.Between;
import com.landawn.abacus.condition.Binary;
import com.landawn.abacus.condition.Condition;
import com.landawn.abacus.condition.ConditionFactory.L;
import com.landawn.abacus.condition.Expression;
import com.landawn.abacus.condition.In;
import com.landawn.abacus.condition.Junction;
import com.landawn.abacus.core.RowDataSet;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.exception.NonUniqueResultException;
import com.landawn.abacus.util.ClassUtil;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NamedSQL;
import com.landawn.abacus.util.NamingPolicy;
import com.landawn.abacus.util.Nullable;
import com.landawn.abacus.util.ObjectFactory;
import com.landawn.abacus.util.ObjectPool;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalBoolean;
import com.landawn.abacus.util.OptionalByte;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalFloat;
import com.landawn.abacus.util.OptionalInt;
import com.landawn.abacus.util.OptionalLong;
import com.landawn.abacus.util.OptionalShort;
import com.landawn.abacus.util.SQLBuilder;
import com.landawn.abacus.util.SQLBuilder.RE;
import com.landawn.abacus.util.SQLBuilder.RE2;
import com.landawn.abacus.util.SQLBuilder.RE3;
import com.landawn.abacus.util.SQLBuilder.SP;
import com.landawn.abacus.util.SQLParser;
import com.landawn.abacus.util.WD;

import android.content.ContentValues;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;

/**
 * It's a simple wrapper of SQliteDatabase on Android.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see android.database.sqlite.SQLiteDatabase
 * @see android.database.sqlite.SQLiteOpenHelper
 * @see com.landawn.abacus.util.SQLBuilder
 */
public final class SQLiteExecutor {
    public static final NamingPolicy DEFAULT_NAMING_POLICY = NamingPolicy.LOWER_CASE_WITH_UNDERSCORE;

    private static final Map<Class<?>, Set<String>> readOnlyPropNamesMap = new ConcurrentHashMap<>();
    private static final Map<Class<?>, Set<String>> readOrWriteOnlyPropNamesMap = new ConcurrentHashMap<>();

    private static final String ID = "id";
    private static final String _ID = "_id";

    private final SQLiteDatabase sqliteDB;
    private final NamingPolicy columnNamingPolicy;

    public SQLiteExecutor(final SQLiteDatabase sqliteDatabase) {
        this(sqliteDatabase, DEFAULT_NAMING_POLICY);
    }

    /**
     * 
     * @param sqliteDatabase
     * @param columnNamingPolicy a naming convention to convert the entity/property names in the Entity/Map parameter to table/column names
     */
    public SQLiteExecutor(final SQLiteDatabase sqliteDatabase, final NamingPolicy columnNamingPolicy) {
        this.sqliteDB = sqliteDatabase;
        this.columnNamingPolicy = columnNamingPolicy == null ? DEFAULT_NAMING_POLICY : columnNamingPolicy;
    }

    /**
     * The properties will be excluded by add/addAll/batchAdd and update/updateAll/batchUpdate operations if the input is an entity.
     * 
     * @param targetClass
     * @param readOnlyPropNames
     */
    public static void registerReadOnlyProps(Class<?> targetClass, Collection<String> readOnlyPropNames) {
        N.checkArgument(N.isEntity(targetClass), ClassUtil.getCanonicalClassName(targetClass) + " is not an entity class with getter/setter methods");
        N.checkNullOrEmpty(readOnlyPropNames, "'readOnlyPropNames'");

        final Set<String> set = new HashSet<String>();

        for (String propName : readOnlyPropNames) {
            set.add(ClassUtil.getPropNameByMethod(ClassUtil.getPropGetMethod(targetClass, propName)));
        }

        readOnlyPropNamesMap.put(targetClass, set);

        if (readOrWriteOnlyPropNamesMap.containsKey(targetClass)) {
            readOrWriteOnlyPropNamesMap.get(targetClass).addAll(set);
        } else {
            readOrWriteOnlyPropNamesMap.put(targetClass, new HashSet<>(set));
        }
    }

    /**
     * The properties will be ignored by update/updateAll/batchUpdate operations if the input is an entity.
     * 
     * @param targetClass
     * @param writeOnlyPropNames
     */
    public static void registerWriteOnlyProps(Class<?> targetClass, Collection<String> writeOnlyPropNames) {
        N.checkArgument(N.isEntity(targetClass), ClassUtil.getCanonicalClassName(targetClass) + " is not an entity class with getter/setter methods");
        N.checkNullOrEmpty(writeOnlyPropNames, "'writeOnlyPropNames'");

        final Set<String> set = new HashSet<String>();

        for (String propName : writeOnlyPropNames) {
            set.add(ClassUtil.getPropNameByMethod(ClassUtil.getPropGetMethod(targetClass, propName)));
        }

        if (readOrWriteOnlyPropNamesMap.containsKey(targetClass)) {
            readOrWriteOnlyPropNamesMap.get(targetClass).addAll(set);
        } else {
            readOrWriteOnlyPropNamesMap.put(targetClass, set);
        }
    }

    public SQLiteDatabase sqliteDB() {
        return sqliteDB;
    }

    static DataSet extractData(Class<?> targetClass, Cursor cursor) {
        return extractData(targetClass, cursor, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter methods.
     * @param cursor
     * @param offset
     * @param count
     * @return
     */
    static DataSet extractData(Class<?> targetClass, Cursor cursor, int offset, int count) {
        final int columnCount = cursor.getColumnCount();
        final List<String> columnNameList = N.asList(cursor.getColumnNames());
        final List<List<Object>> columnList = new ArrayList<>(columnCount);

        for (int i = 0; i < columnCount; i++) {
            columnList.add(new ArrayList<>(count > 9 ? 9 : count));
        }

        final Type<Object>[] selectColumnTypes = new Type[columnCount];

        for (int i = 0; i < columnCount; i++) {
            selectColumnTypes[i] = Type.valueOf(ClassUtil.getPropGetMethod(targetClass, columnNameList.get(i)).getReturnType());
        }

        if (offset > 0) {
            cursor.moveToPosition(offset - 1);
        }

        while (count-- > 0 && cursor.moveToNext()) {
            for (int columnIndex = 0; columnIndex < columnCount; columnIndex++) {
                columnList.get(columnIndex).add(selectColumnTypes[columnIndex].get(cursor, columnIndex));
            }
        }

        return new RowDataSet(columnNameList, columnList);
    }

    @SuppressWarnings("rawtypes")
    static DataSet extractData(Cursor cursor, Class[] selectColumnTypes) {
        return extractData(cursor, selectColumnTypes, 0, Integer.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    static DataSet extractData(Cursor cursor, Class[] selectColumnTypes, int offset, int count) {
        return extractData(cursor, Type.arrayOf(selectColumnTypes), offset, count);
    }

    @SuppressWarnings("rawtypes")
    static DataSet extractData(Cursor cursor, Collection<Class> selectColumnTypes) {
        return extractData(cursor, selectColumnTypes, 0, Integer.MAX_VALUE);
    }

    @SuppressWarnings("rawtypes")
    static DataSet extractData(Cursor cursor, Collection<Class> selectColumnTypes, int offset, int count) {
        return extractData(cursor, Type.arrayOf(selectColumnTypes.toArray(new Class[selectColumnTypes.size()])), offset, count);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    static DataSet extractData(Cursor cursor, Type[] selectColumnTypes) {
        return extractData(cursor, selectColumnTypes, 0, Integer.MAX_VALUE);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    static DataSet extractData(Cursor cursor, Type[] selectColumnTypes, int offset, int count) {
        final int columnCount = cursor.getColumnCount();
        final List<String> columnNameList = N.asList(cursor.getColumnNames());
        final List<List<Object>> columnList = new ArrayList<>(columnCount);

        for (int i = 0; i < columnCount; i++) {
            columnList.add(new ArrayList<>(count > 9 ? 9 : count));
        }

        if (offset > 0) {
            cursor.moveToPosition(offset - 1);
        }

        while (count-- > 0 && cursor.moveToNext()) {
            for (int columnIndex = 0; columnIndex < columnCount; columnIndex++) {
                columnList.get(columnIndex).add(selectColumnTypes[columnIndex].get(cursor, columnIndex));
            }
        }

        return new RowDataSet(columnNameList, columnList);
    }

    /**
     * Returns values from all rows associated with the specified <code>targetClass</code> if the specified <code>targetClass</code> is an entity class, otherwise, only returns values from first column.
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @return
     */
    static <T> List<T> toList(Class<T> targetClass, Cursor cursor) {
        return toList(targetClass, cursor, 0, Integer.MAX_VALUE);
    }

    /** 
     * Returns values from all rows associated with the specified <code>targetClass</code> if the specified <code>targetClass</code> is an entity class, otherwise, only returns values from first column.
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @param offset
     * @param count
     * @return
     */
    static <T> List<T> toList(Class<T> targetClass, Cursor cursor, int offset, int count) {
        if (N.isEntity(targetClass)) {
            final DataSet ds = extractData(targetClass, cursor, offset, count);

            if (ds == null || ds.isEmpty()) {
                return new ArrayList<>();
            } else {
                return ds.toList(targetClass);
            }
        } else {
            return toList(targetClass, cursor, 0, offset, count);
        }
    }

    /**
     * Returns the values from the specified <code>column</code>. 
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @param columnIndex
     * @return
     */
    static <T> List<T> toList(Class<T> targetClass, Cursor cursor, int columnIndex) {
        return toList(targetClass, cursor, columnIndex, 0, Integer.MAX_VALUE);
    }

    /**
     * Returns the values from the specified <code>column</code>. 
     * 
     * @param targetClass entity class or specific column type.
     * @param cursor
     * @param columnIndex
     * @param offset
     * @param count
     * @return
     */
    static <T> List<T> toList(final Class<T> targetClass, final Cursor cursor, final int columnIndex, int offset, int count) {
        if (columnIndex < 0 || columnIndex >= cursor.getColumnCount()) {
            throw new IllegalArgumentException("Invalid column index: " + columnIndex);
        }

        if (offset > 0) {
            cursor.moveToPosition(offset - 1);
        }

        final List<T> resultList = new ArrayList<>();

        if (N.isEntity(targetClass)) {
            final Method propSetMethod = ClassUtil.getPropSetMethod(targetClass, cursor.getColumnName(columnIndex));
            final Type<T> selectColumnType = Type.valueOf(propSetMethod.getParameterTypes()[0]);

            while (count-- > 0 && cursor.moveToNext()) {
                T entity = N.newEntity(targetClass);
                ClassUtil.setPropValue(entity, propSetMethod, selectColumnType.get(cursor, columnIndex));
                resultList.add(entity);
            }

        } else {
            final Type<T> selectColumnType = Type.valueOf(targetClass);

            while (count-- > 0 && cursor.moveToNext()) {
                resultList.add(selectColumnType.get(cursor, columnIndex));
            }
        }

        return resultList;
    }

    /**
     * 
     * @param targetClass entity class with getter/setter methods.
     * @param cursor
     * @return
     */
    static <T> T toEntity(Class<T> targetClass, Cursor cursor) {
        return toEntity(targetClass, cursor, 0);
    }

    /**
     * 
     * @param targetClass entity class with getter/setter methods.
     * @param cursor
     * @param rowNum
     * @return
     */
    static <T> T toEntity(Class<T> targetClass, Cursor cursor, int rowNum) {
        final List<T> list = toList(targetClass, cursor, rowNum, 1);

        return N.isNullOrEmpty(list) ? null : list.get(0);
    }

    static <T> T toEntity(final Class<T> targetClass, final ContentValues contentValues) {
        return toEntity(targetClass, contentValues, NamingPolicy.LOWER_CAMEL_CASE);
    }

    /**
     * 
     * @param targetClass an Map class or Entity class with getter/setter methods.
     * @param contentValues
     * @p
     * @return
     */
    @SuppressWarnings("deprecation")
    static <T> T toEntity(final Class<T> targetClass, final ContentValues contentValues, NamingPolicy namingPolicy) {
        if (!(N.isEntity(targetClass) || targetClass.equals(Map.class))) {
            throw new AbacusException("The target class must be an entity class with getter/setter methods or Map.class. But it is: "
                    + ClassUtil.getCanonicalClassName(targetClass));
        }

        if (targetClass.isAssignableFrom(Map.class)) {
            final Map<String, Object> map = (Map<String, Object>) ((Modifier.isAbstract(targetClass.getModifiers())
                    ? new HashMap<>(N.initHashCapacity(contentValues.size())) : N.newInstance(targetClass)));

            Object propValue = null;

            switch (namingPolicy) {
                case LOWER_CAMEL_CASE: {
                    for (String propName : contentValues.keySet()) {
                        propValue = contentValues.get(propName);

                        if (propValue instanceof ContentValues) {
                            map.put(ClassUtil.formalizePropName(propName), toEntity(targetClass, (ContentValues) propValue, namingPolicy));
                        } else {
                            map.put(ClassUtil.formalizePropName(propName), propValue);
                        }
                    }

                    break;
                }

                case LOWER_CASE_WITH_UNDERSCORE: {
                    for (String propName : contentValues.keySet()) {
                        propValue = contentValues.get(propName);

                        if (propValue instanceof ContentValues) {
                            map.put(ClassUtil.toLowerCaseWithUnderscore(propName), toEntity(targetClass, (ContentValues) propValue, namingPolicy));
                        } else {
                            map.put(ClassUtil.toLowerCaseWithUnderscore(propName), propValue);
                        }
                    }

                    break;
                }

                case UPPER_CASE_WITH_UNDERSCORE: {
                    for (String propName : contentValues.keySet()) {
                        propValue = contentValues.get(propName);

                        if (propValue instanceof ContentValues) {
                            map.put(ClassUtil.toUpperCaseWithUnderscore(propName), toEntity(targetClass, (ContentValues) propValue, namingPolicy));
                        } else {
                            map.put(ClassUtil.toUpperCaseWithUnderscore(propName), propValue);
                        }
                    }

                    break;
                }

                default:
                    throw new IllegalArgumentException("Unsupported NamingPolicy: " + namingPolicy);
            }

            return (T) map;
        } else {
            final T entity = N.newInstance(targetClass);

            Object propValue = null;
            Method propSetMethod = null;
            Class<?> parameterType = null;

            for (String propName : contentValues.keySet()) {
                propSetMethod = ClassUtil.getPropSetMethod(targetClass, propName);

                if (propSetMethod == null) {
                    continue;
                }

                parameterType = propSetMethod.getParameterTypes()[0];
                propValue = contentValues.get(propName);

                if (propValue != null && !parameterType.isAssignableFrom(propValue.getClass())) {
                    if (propValue instanceof ContentValues) {
                        if (parameterType.isAssignableFrom(Map.class) || N.isEntity(parameterType)) {
                            ClassUtil.setPropValue(entity, propSetMethod, toEntity(parameterType, (ContentValues) propValue, namingPolicy));
                        } else {
                            ClassUtil.setPropValue(entity, propSetMethod,
                                    N.valueOf(parameterType, N.stringOf(toEntity(Map.class, (ContentValues) propValue, namingPolicy))));
                        }
                    } else {
                        ClassUtil.setPropValue(entity, propSetMethod, propValue);
                    }
                } else {
                    ClassUtil.setPropValue(entity, propSetMethod, propValue);
                }
            }

            if (isDirtyMarkerEntity(entity.getClass())) {
                ((DirtyMarker) entity).markDirty(false);
            }

            return entity;
        }
    }

    static ContentValues toContentValues(final Object obj, final Collection<String> ignoredPropNames) {
        return toContentValues(obj, ignoredPropNames, DEFAULT_NAMING_POLICY);
    }

    /**
     * 
     * @param obj an instance of Map or Entity.
     * @param namingPolicy
     * @return
     */
    static ContentValues toContentValues(final Object obj, final Collection<String> ignoredPropNames, final NamingPolicy namingPolicy) {
        return toContentValues(obj, ignoredPropNames, namingPolicy, false);
    }

    public <T> T get(Class<T> targetClass, long id) {
        return get(targetClass, id, null);
    }

    /**
     * Find the entity from table specified by simple class name of the <code>targetClass</code> by the specified <code>id</code>
     * 
     * @param targetClass
     * @param id
     * @param selectPropNames
     * @return
     * @throws NonUniqueResultException if more than one records are found.
     */
    public <T> T get(Class<T> targetClass, long id, Collection<String> selectPropNames) {
        final Condition whereClause = L.eq(ID, id);
        final List<T> entities = find(targetClass, selectPropNames, whereClause, null, 0, 2);

        if (entities.size() > 1) {
            throw new NonUniqueResultException("More than one records found by condition: " + whereClause.toString());
        }

        return (entities.size() > 0) ? entities.get(0) : null;
    }

    public <T> Optional<T> gett(Class<T> targetClass, long id) {
        return gett(targetClass, id, null);
    }

    /**
     * Find the entity from table specified by simple class name of the <code>targetClass</code> by the specified <code>id</code>
     * 
     * @param targetClass
     * @param id
     * @param selectPropNames
     * @return
     * @throws NonUniqueResultException if more than one records are found.
     */
    public <T> Optional<T> gett(Class<T> targetClass, long id, Collection<String> selectPropNames) {
        return Optional.ofNullable(get(targetClass, id, selectPropNames));
    }

    @SuppressWarnings("deprecation")
    private static ContentValues toContentValues(final Object obj, final Collection<String> ignoredPropNames, final NamingPolicy namingPolicy,
            final boolean isForUpdate) {
        final ContentValues result = new ContentValues();
        final boolean notNullOrEmptyIgnorePropNames = N.notNullOrEmpty(ignoredPropNames);

        @SuppressWarnings("rawtypes")
        Type type = null;
        if (Map.class.isAssignableFrom(obj.getClass())) {
            Map<String, Object> props = (Map<String, Object>) obj;

            switch (namingPolicy) {
                case LOWER_CAMEL_CASE: {
                    for (Map.Entry<String, Object> entry : props.entrySet()) {
                        if (notNullOrEmptyIgnorePropNames && ignoredPropNames.contains(entry.getKey())) {
                            continue;
                        }

                        if (entry.getValue() == null) {
                            result.putNull(entry.getKey());
                        } else {
                            type = Type.valueOf(entry.getValue().getClass());
                            type.set(result, entry.getKey(), entry.getValue());
                        }
                    }

                    break;
                }

                case LOWER_CASE_WITH_UNDERSCORE: {
                    String propName = null;

                    for (Map.Entry<String, Object> entry : props.entrySet()) {
                        propName = ClassUtil.toLowerCaseWithUnderscore(entry.getKey());

                        if (notNullOrEmptyIgnorePropNames && (ignoredPropNames.contains(entry.getKey()) || ignoredPropNames.contains(propName))) {
                            continue;
                        }

                        if (entry.getValue() == null) {
                            result.putNull(propName);
                        } else {
                            type = Type.valueOf(entry.getValue().getClass());
                            type.set(result, propName, entry.getValue());
                        }
                    }

                    break;
                }

                case UPPER_CASE_WITH_UNDERSCORE: {
                    String propName = null;

                    for (Map.Entry<String, Object> entry : props.entrySet()) {
                        propName = ClassUtil.toUpperCaseWithUnderscore(entry.getKey());

                        if (notNullOrEmptyIgnorePropNames && (ignoredPropNames.contains(entry.getKey()) || ignoredPropNames.contains(propName))) {
                            continue;
                        }

                        if (entry.getValue() == null) {
                            result.putNull(propName);
                        } else {
                            type = Type.valueOf(entry.getValue().getClass());
                            type.set(result, propName, entry.getValue());
                        }
                    }

                    break;
                }

                default:
                    throw new IllegalArgumentException("Unsupported NamingPolicy: " + namingPolicy);
            }

        } else if (N.isEntity(obj.getClass())) {
            if (obj instanceof DirtyMarker) {
                final Class<?> srCls = obj.getClass();
                final Set<String> updatePropNames = isForUpdate ? ((DirtyMarker) obj).dirtyPropNames() : ((DirtyMarker) obj).signedPropNames();

                if (updatePropNames.size() == 0) {
                    // logger.warn("No property is signed/updated in the specified source entity: " + N.toString(entity));
                } else {
                    Method propGetMethod = null;
                    Object propValue = null;

                    switch (namingPolicy) {
                        case LOWER_CAMEL_CASE: {
                            for (String propName : updatePropNames) {
                                propGetMethod = ClassUtil.getPropGetMethod(srCls, propName);
                                propName = ClassUtil.getPropNameByMethod(propGetMethod);

                                if (notNullOrEmptyIgnorePropNames && ignoredPropNames.contains(propName)) {
                                    continue;
                                }

                                propValue = ClassUtil.getPropValue(obj, propGetMethod);

                                if (propValue == null) {
                                    result.putNull(propName);
                                } else {
                                    type = Type.valueOf(propValue.getClass());
                                    type.set(result, propName, propValue);
                                }
                            }

                            break;
                        }

                        case LOWER_CASE_WITH_UNDERSCORE: {
                            for (String propName : updatePropNames) {
                                propGetMethod = ClassUtil.getPropGetMethod(srCls, propName);
                                propName = ClassUtil.getPropNameByMethod(propGetMethod);

                                if (notNullOrEmptyIgnorePropNames && ignoredPropNames.contains(propName)) {
                                    continue;
                                }

                                propValue = ClassUtil.getPropValue(obj, propGetMethod);

                                if (propValue == null) {
                                    result.putNull(ClassUtil.toLowerCaseWithUnderscore(propName));
                                } else {
                                    type = Type.valueOf(propValue.getClass());
                                    type.set(result, ClassUtil.toLowerCaseWithUnderscore(propName), propValue);
                                }
                            }

                            break;
                        }

                        case UPPER_CASE_WITH_UNDERSCORE: {
                            for (String propName : updatePropNames) {
                                propGetMethod = ClassUtil.getPropGetMethod(srCls, propName);
                                propName = ClassUtil.getPropNameByMethod(propGetMethod);

                                if (notNullOrEmptyIgnorePropNames && ignoredPropNames.contains(propName)) {
                                    continue;
                                }

                                propValue = ClassUtil.getPropValue(obj, propGetMethod);

                                if (propValue == null) {
                                    result.putNull(ClassUtil.toUpperCaseWithUnderscore(propName));
                                } else {
                                    type = Type.valueOf(propValue.getClass());
                                    type.set(result, ClassUtil.toUpperCaseWithUnderscore(propName), propValue);
                                }
                            }

                            break;
                        }

                        default:
                            throw new IllegalArgumentException("Unsupported NamingPolicy: " + namingPolicy);
                    }
                }
            } else {
                final Map<String, Method> getterMethodList = ClassUtil.getPropGetMethodList(obj.getClass());
                String propName = null;
                Object propValue = null;

                switch (namingPolicy) {
                    case LOWER_CAMEL_CASE: {
                        for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                            propName = entry.getKey();

                            if (notNullOrEmptyIgnorePropNames && ignoredPropNames.contains(propName)) {
                                continue;
                            }

                            propValue = ClassUtil.getPropValue(obj, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            type = Type.valueOf(propValue.getClass());
                            type.set(result, propName, propValue);
                        }

                        break;
                    }

                    case LOWER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                            propName = entry.getKey();

                            if (notNullOrEmptyIgnorePropNames && ignoredPropNames.contains(propName)) {
                                continue;
                            }

                            propValue = ClassUtil.getPropValue(obj, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            type = Type.valueOf(propValue.getClass());
                            type.set(result, ClassUtil.toLowerCaseWithUnderscore(propName), propValue);
                        }

                        break;
                    }

                    case UPPER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                            propName = entry.getKey();

                            if (notNullOrEmptyIgnorePropNames && ignoredPropNames.contains(propName)) {
                                continue;
                            }

                            propValue = ClassUtil.getPropValue(obj, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            type = Type.valueOf(propValue.getClass());
                            type.set(result, ClassUtil.toUpperCaseWithUnderscore(propName), propValue);
                        }

                        break;
                    }

                    default:
                        throw new IllegalArgumentException("Unsupported NamingPolicy: " + namingPolicy);
                }
            }
        } else {
            throw new IllegalArgumentException("Only entity class with getter/setter methods or Map are supported. "
                    + ClassUtil.getCanonicalClassName(obj.getClass()) + " is not supported");
        }

        return result;
    }

    /**
     * Insert one record into database.
     * To exclude the some properties or default value, invoke {@code com.landawn.abacus.util.N#entity2Map(Object, boolean, Collection, NamingPolicy)}
     * 
     * <p>The target table is identified by the simple class name of the specified entity.</p>
     * 
     * @param entity with getter/setter methods
     * @return
     * 
     * @see com.landawn.abacus.util.Maps#entity2Map(Object, boolean, Collection, NamingPolicy)
     */
    public long insert(Object entity) {
        if (!N.isEntity(entity.getClass())) {
            throw new IllegalArgumentException("The specified parameter must be an entity with getter/setter methods");
        }

        return insert(getTableNameByEntity(entity), entity);
    }

    /**
     * Insert one record into database.
     * To exclude the some properties or default value, invoke {@code com.landawn.abacus.util.N#entity2Map(Object, boolean, Collection, NamingPolicy)}
     * 
     * <p>The target table is identified by the simple class name of the specified entity.</p>
     * 
     * @param entity with getter/setter methods
     * @param conflictAlgorithm
     * @return
     * 
     * @see com.landawn.abacus.util.Maps#entity2Map(Object, boolean, Collection, NamingPolicy)
     */
    public long insert(Object entity, int conflictAlgorithm) {
        if (!N.isEntity(entity.getClass())) {
            throw new IllegalArgumentException("The specified parameter must be an entity with getter/setter methods");
        }

        return insert(getTableNameByEntity(entity), entity, conflictAlgorithm);
    }

    /**
     * Insert one record into database.
     * To exclude the some properties or default value, invoke {@code com.landawn.abacus.util.N#entity2Map(Object, boolean, Collection, NamingPolicy)}
     * 
     * @param table
     * @param record can be <code>Map</code> or <code>entity</code> with getter/setter methods
     * @return
     * 
     * @see com.landawn.abacus.util.Maps#entity2Map(Object, boolean, Collection, NamingPolicy)
     */
    public long insert(String table, Object record) {
        table = formatName(table);

        return insert(table, record, SQLiteDatabase.CONFLICT_NONE);
    }

    /**
     * Insert one record into database.
     * To exclude the some properties or default value, invoke {@code com.landawn.abacus.util.N#entity2Map(Object, boolean, Collection, NamingPolicy)}
     * 
     * @param table
     * @param record can be <code>Map</code> or <code>entity</code> with getter/setter methods
     * @param conflictAlgorithm
     * @return
     * 
     * @see com.landawn.abacus.util.Maps#entity2Map(Object, boolean, Collection, NamingPolicy)
     */
    public long insert(String table, Object record, int conflictAlgorithm) {
        table = formatName(table);
        final ContentValues contentValues = record instanceof ContentValues ? (ContentValues) record
                : toContentValues(record, readOnlyPropNamesMap.get(record.getClass()), columnNamingPolicy, false);

        removeIdDefaultValue(contentValues);

        return sqliteDB.insertWithOnConflict(table, null, contentValues, conflictAlgorithm);
    }

    private void removeIdDefaultValue(final ContentValues initialValues) {
        Object value = initialValues.get(ID);

        if (value != null && (value.equals(0) || value.equals(0L))) {
            initialValues.remove(ID);
        } else {
            value = initialValues.get(_ID);

            if (value != null && (value.equals(0) || value.equals(0L))) {
                initialValues.remove(_ID);
            }
        }
    }

    /**
     * Insert multiple records into data store.
     *
     * @param entities
     * @param withTransaction
     * @return
     * 
     * @since 0.8.10
     */
    @Deprecated
    <T> long[] insert(T[] entities, boolean withTransaction) {
        return insert(this.getTableNameByEntity(entities[0]), entities, withTransaction);
    }

    /**
     * Insert multiple records into data store.
     *
     * @param table
     * @param records
     * @param withTransaction
     * @return
     */
    @Deprecated
    <T> long[] insert(String table, T[] records, boolean withTransaction) {
        if (N.isNullOrEmpty(records)) {
            return N.EMPTY_LONG_ARRAY;
        }

        final long[] ret = new long[records.length];

        table = formatName(table);

        if (withTransaction) {
            beginTransaction();
        }

        try {
            for (int i = 0, len = records.length; i < len; i++) {
                ret[i] = insert(table, records[i]);
            }

            if (withTransaction) {
                sqliteDB.setTransactionSuccessful();
            }
        } finally {
            if (withTransaction) {
                endTransaction();
            }
        }

        return ret;
    }

    /**
     * Insert multiple records into data store.
     *
     * @param records
     * @param withTransaction
     * @return
     * 
     * @since 0.8.10
     */
    public <T> List<Long> insert(Collection<T> records, boolean withTransaction) {
        return insert(this.getTableNameByEntity(records.iterator().next()), records, withTransaction);
    }

    /**
     * Insert multiple records into data store.
     *
     * @param table
     * @param records
     * @param withTransaction
     * @return
     */
    public <T> List<Long> insert(String table, Collection<T> records, boolean withTransaction) {
        if (N.isNullOrEmpty(records)) {
            return new ArrayList<>();
        }

        final List<Long> ret = new ArrayList<>(records.size());

        table = formatName(table);

        if (withTransaction) {
            beginTransaction();
        }

        try {
            for (Object e : records) {
                ret.add(insert(table, e));
            }

            if (withTransaction) {
                sqliteDB.setTransactionSuccessful();
            }
        } finally {
            if (withTransaction) {
                endTransaction();
            }
        }

        return ret;
    }

    //    // mess up
    //    @Deprecated
    //    int update(EntityId entityId, Map<String, Object> props) {
    //        return update(entityId.entityName(), props, EntityManagerUtil.entityId2Condition(entityId));
    //    }

    /**
     * Update the records in data store with the properties which have been updated/set in the specified <code>entity</code> by id property in the entity.
     * if the entity implements <code>DirtyMarker</code> interface, just update the dirty properties.
     * To exclude the some properties or default value, invoke {@code com.landawn.abacus.util.N#entity2Map(Object, boolean, Collection, NamingPolicy)}
     * 
     * @param entity with getter/setter methods
     * @return
     */
    public int update(Object entity) {
        if (!N.isEntity(entity.getClass())) {
            throw new IllegalArgumentException("The specified parameter must be an entity with getter/setter methods");
        }

        Number id = ClassUtil.getPropValue(entity, ID);

        if (id.longValue() == 0) {
            throw new IllegalArgumentException("Please specify value for the id property");
        }

        return update(getTableNameByEntity(entity), entity, L.eq(ID, id));
    }

    /**
     * Update the records in data store with the properties which have been updated/set in the specified <code>entity</code> by the specified condition.
     * if the entity implements <code>DirtyMarker</code> interface, just update the dirty properties.
     * To exclude the some properties or default value, invoke {@code com.landawn.abacus.util.N#entity2Map(Object, boolean, Collection, NamingPolicy)}
     * 
     * <p>The target table is identified by the simple class name of the specified entity.</p>
     *
     * @param entity with getter/setter methods
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @return
     * 
     * @see com.landawn.abacus.util.Maps#entity2Map(Object, boolean, Collection, NamingPolicy)
     */
    public int update(Object entity, Condition whereClause) {
        if (!N.isEntity(entity.getClass())) {
            throw new IllegalArgumentException("The specified parameter must be an entity with getter/setter methods");
        }

        return update(getTableNameByEntity(entity), entity, whereClause);
    }

    private String getTableNameByEntity(Object entity) {
        return getTableNameByEntity(entity.getClass());
    }

    private String getTableNameByEntity(Class<?> entityClass) {
        return ClassUtil.getSimpleClassName(entityClass);
    }

    /**
     * Update the records in data store with the properties which have been updated/set in the specified <code>entity</code> by the specified condition.
     * if the entity implements <code>DirtyMarker</code> interface, just update the dirty properties.
     * To exclude the some properties or default value, invoke {@code com.landawn.abacus.util.N#entity2Map(Object, boolean, Collection, NamingPolicy)}
     *
     * @param table
     * @param record can be <code>Map</code> or <code>entity</code> with getter/setter methods
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @return
     * 
     * @see com.landawn.abacus.util.Maps#entity2Map(Object, boolean, Collection, NamingPolicy)
     */
    public int update(String table, Object record, Condition whereClause) {
        table = formatName(table);
        final ContentValues contentValues = record instanceof ContentValues ? (ContentValues) record
                : toContentValues(record, readOrWriteOnlyPropNamesMap.get(record.getClass()), columnNamingPolicy, true);
        removeIdDefaultValue(contentValues);

        if (whereClause == null) {
            return sqliteDB.update(table, contentValues, null, N.EMPTY_STRING_ARRAY);
        } else {
            final Command cmd = interpretCondition(whereClause);
            return sqliteDB.update(table, contentValues, cmd.getSql(), cmd.getArgs());
        }
    }

    //    // mess up
    //    @Deprecated
    //    int delete(EntityId entityId) {
    //        return delete(entityId.entityName(), EntityManagerUtil.entityId2Condition(entityId));
    //    }

    /**
     * Delete the entity by id value in the entity.
     * 
     * @param entity
     * @return
     */
    public int delete(Object entity) {
        if (!N.isEntity(entity.getClass())) {
            throw new IllegalArgumentException("The specified parameter must be an entity with getter/setter methods");
        }

        Number id = ClassUtil.getPropValue(entity, ID);

        if (id.longValue() == 0) {
            throw new IllegalArgumentException("Please specify value for the id property");
        }

        return delete(getTableNameByEntity(entity), L.eq(ID, id));
    }

    /**
    *
    * @param table
    * @param id
    * @return
    */
    public int delete(String table, long id) {
        return delete(table, L.eq(ID, id));
    }

    /**
    *
    * @param entityClass
    * @param id
    * @return
    * 
    * @since 0.8.10
    */
    public int delete(Class<?> entityClass, long id) {
        return delete(entityClass, L.eq(ID, id));
    }

    /**
     *
     * @param table
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @return
     */
    public int delete(String table, Condition whereClause) {
        if (whereClause == null) {
            return delete(table, null, N.EMPTY_STRING_ARRAY);
        } else {
            final Command cmd = interpretCondition(whereClause);

            return delete(table, cmd.getSql(), cmd.getArgs());
        }
    }

    /**
    *
    * @param entityClass
    * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
    * @return
     * 
     * @since 0.8.10
    */
    public int delete(Class<?> entityClass, Condition whereClause) {
        return delete(getTableNameByEntity(entityClass), whereClause);
    }

    @Deprecated
    int delete(String table, String whereClause, String... whereArgs) {
        table = formatName(table);

        return sqliteDB.delete(table, parseStringCondition(whereClause), whereArgs);
    }

    /**
     * Execute a single SQL statement that is NOT a SELECT or any other SQL statement that returns data.
     *
     * @param sql
     */
    @Deprecated
    void execute(String sql) {
        sqliteDB.execSQL(sql);
    }

    /**
     * Execute a single SQL statement that is NOT a SELECT/INSERT/UPDATE/DELETE.
     *
     * @param sql
     * @param parameters A Object Array/List, and Map/Entity with getter/setter methods for parameterized sql with named parameters
     */
    @Deprecated
    void execute(String sql, Object... parameters) {
        if (N.isNullOrEmpty(parameters)) {
            sqliteDB.execSQL(sql);
        } else {
            final NamedSQL namedSQL = parseSQL(sql);
            final Object[] args = prepareArguments(namedSQL, parameters);
            sqliteDB.execSQL(namedSQL.getPureSQL(), args);
        }
    }

    //    // mess up
    //    @Deprecated
    //    boolean exists(EntityId entityId) {
    //        final Pair2 pair = generateQuerySQL(entityId, NE._1_list);
    //
    //        return exists(pair.sql, pair.parameters);
    //    }
    //
    //    private Pair2 generateQuerySQL(EntityId entityId, Collection<String> selectPropNames) {
    //        final Condition cond = EntityManagerUtil.entityId2Condition(entityId);
    //
    //        switch (columnNamingPolicy) {
    //            case LOWER_CASE_WITH_UNDERSCORE: {
    //                return NE.select(selectPropNames).from(entityId.entityName()).where(cond).limit(2).pair();
    //            }
    //
    //            case UPPER_CASE_WITH_UNDERSCORE: {
    //                return NE2.select(selectPropNames).from(entityId.entityName()).where(cond).limit(2).pair();
    //            }
    //
    //            case CAMEL_CASE: {
    //                return NE3.select(selectPropNames).from(entityId.entityName()).where(cond).limit(2).pair();
    //            }
    //
    //            default:
    //                throw new AbacusException("Unsupported naming policy");
    //        }
    //    }

    public boolean exists(Class<?> entityClass, Condition whereClause) {
        return exists(getTableNameByEntity(entityClass), whereClause);
    }

    public boolean exists(String tableName, Condition whereClause) {
        final SP pair = select(tableName, SQLBuilder._1, whereClause);
        final Object[] parameters = N.isNullOrEmpty(pair.parameters) ? N.EMPTY_OBJECT_ARRAY : pair.parameters.toArray(new Object[pair.parameters.size()]);

        return exists(pair.sql, parameters);
    }

    /**
     * Remember to add {@code limit} condition if big result will be returned by the query.
     *
     * @param sql
     * @param parameters A Object Array/List, and Map/Entity with getter/setter methods for parameterized sql with named parameters
     * @return
     */
    @SafeVarargs
    public final boolean exists(String sql, Object... parameters) {
        final Cursor cursor = rawQuery(sql, parameters);

        try {
            return cursor.moveToNext();
        } finally {
            cursor.close();
        }
    }

    public int count(Class<?> entityClass, Condition whereClause) {
        return count(getTableNameByEntity(entityClass), whereClause);
    }

    public int count(String tableName, Condition whereClause) {
        final SP pair = select(tableName, SQLBuilder.COUNT_ALL, whereClause);
        final Object[] parameters = N.isNullOrEmpty(pair.parameters) ? N.EMPTY_OBJECT_ARRAY : pair.parameters.toArray(new Object[pair.parameters.size()]);

        return count(pair.sql, parameters);
    }

    @SafeVarargs
    public final int count(String sql, Object... parameters) {
        return queryForSingleResult(int.class, sql, parameters).orElse(0);
    }

    private SP select(String tableName, String selectColumnName, Condition whereClause) {
        switch (columnNamingPolicy) {
            case LOWER_CASE_WITH_UNDERSCORE:
                return RE.select(selectColumnName).from(tableName).where(whereClause).pair();

            case UPPER_CASE_WITH_UNDERSCORE:
                return RE2.select(selectColumnName).from(tableName).where(whereClause).pair();

            case LOWER_CAMEL_CASE:
                return RE3.select(selectColumnName).from(tableName).where(whereClause).pair();

            default:
                return RE.select(selectColumnName).from(tableName).where(whereClause).pair();
        }
    }

    //    // mess up
    //    @Deprecated
    //    <T> T get(Class<T> targetClass, EntityId entityId, String... selectPropNames) {
    //        return get(targetClass, entityId, N.asList(selectPropNames));
    //    }
    //
    //    /**
    //     * Find the entity from table specified by entityName in <code>entityId</code> by the id values in <code>entityId</code>
    //     * 
    //     * @param targetClass
    //     * @param entityId
    //     * @param selectPropNames
    //     * @return
    //     * @throws NonUniqueResultException if more than one records are found.
    //     */
    //    // mess up
    //    @Deprecated
    //    <T> T get(Class<T> targetClass, EntityId entityId, Collection<String> selectPropNames) {
    //        final Condition whereClause = EntityManagerUtil.entityId2Condition(entityId);
    //        final List<T> entities = find(targetClass, selectPropNames, whereClause, null, 0, 2);
    //
    //        if (entities.size() > 1) {
    //            throw new NonUniqueResultException("More than one records found by condition: " + whereClause.toString());
    //        }
    //
    //        return (entities.size() > 0) ? entities.get(0) : null;
    //    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalBoolean queryForBoolean(final String sql, final Object... parameters) {
        final Nullable<Boolean> result = queryForSingleResult(boolean.class, sql, parameters);

        return result.isPresent() ? OptionalBoolean.of(result.get()) : OptionalBoolean.empty();
    }

    /**
     *
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalChar queryForChar(final String sql, final Object... parameters) {
        final Nullable<Character> result = queryForSingleResult(char.class, sql, parameters);

        return result.isPresent() ? OptionalChar.of(result.get()) : OptionalChar.empty();
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalByte queryForByte(final String sql, final Object... parameters) {
        final Nullable<Byte> result = queryForSingleResult(byte.class, sql, parameters);

        return result.isPresent() ? OptionalByte.of(result.get()) : OptionalByte.empty();
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalShort queryForShort(final String sql, final Object... parameters) {
        final Nullable<Short> result = queryForSingleResult(short.class, sql, parameters);

        return result.isPresent() ? OptionalShort.of(result.get()) : OptionalShort.empty();
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalInt queryForInt(final String sql, final Object... parameters) {
        final Nullable<Integer> result = queryForSingleResult(int.class, sql, parameters);

        return result.isPresent() ? OptionalInt.of(result.get()) : OptionalInt.empty();
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalLong queryForLong(final String sql, final Object... parameters) {
        final Nullable<Long> result = queryForSingleResult(long.class, sql, parameters);

        return result.isPresent() ? OptionalLong.of(result.get()) : OptionalLong.empty();
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalFloat queryForFloat(final String sql, final Object... parameters) {
        final Nullable<Float> result = queryForSingleResult(float.class, sql, parameters);

        return result.isPresent() ? OptionalFloat.of(result.get()) : OptionalFloat.empty();
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final OptionalDouble queryForDouble(final String sql, final Object... parameters) {
        final Nullable<Double> result = queryForSingleResult(double.class, sql, parameters);

        return result.isPresent() ? OptionalDouble.of(result.get()) : OptionalDouble.empty();
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final Nullable<String> queryForString(final String sql, final Object... parameters) {
        return queryForSingleResult(String.class, sql, parameters);
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final Nullable<Date> queryForDate(final String sql, final Object... parameters) {
        return queryForSingleResult(Date.class, sql, parameters);
    }

    /**
     * @see SQLiteExecutor#queryForSingleResult(Class, String, Object...).
     */
    @SafeVarargs
    public final <T extends Date> Nullable<T> queryForDate(final Class<T> targetClass, final String sql, final Object... parameters) {
        //    final Nullable<Date> date = this.queryForDate(sql, parameters);
        //
        //    if (date.isNotNull()) {
        //        if (targetClass.isAssignableFrom(date.get().getClass())) {
        //            return (Nullable) date;
        //        } else if (targetClass.equals(Timestamp.class)) {
        //            return (Nullable) Nullable.of((new Timestamp(date.get().getTime())));
        //        } else if (targetClass.equals(Time.class)) {
        //            return (Nullable) Nullable.of((new Time(date.get().getTime())));
        //        } else if (targetClass.equals(java.sql.Date.class)) {
        //            return (Nullable) Nullable.of((new java.sql.Date(date.get().getTime())));
        //        } else {
        //            return Nullable.of(N.as(targetClass, date.get()));
        //        }
        //    } else {
        //        return (Nullable<T>) date;
        //    }

        return this.queryForSingleResult(targetClass, sql, parameters);
    }

    /**
     * Just fetch the result in the 1st row and 1st column. {@code null} is returned if no result is found. And this
     * method will try to convert the result to the specified {@code targetClass} if {@code targetClass} is not null and it's not
     * assignable from the result.
     *
     * Special note for type conversion for {@code boolean} or {@code Boolean} type: {@code true} is returned if the
     * {@code String} value of the target column is {@code "true"}, case insensitive. or it's an integer with value > 0.
     * Otherwise, {@code false} is returned.
     *
     * Remember to add {@code limit} condition if big result will be returned by the query.
     *
     * @param targetClass set result type to avoid the NullPointerException if result is null and T is primitive type
     *            "int, long. short ... char, boolean..".
     * @param sql set <code>offset</code> and <code>limit</code> in sql with format: 
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>offsetValue</i>, <i>limitValue</i></code></li>
     * <br>or limit only:</br>
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>limitValue</i></code></li>
     * @param parameters A Object Array/List, and Map/Entity with getter/setter methods for parameterized sql with named parameters
     * @throws ClassCastException
     */
    @SuppressWarnings("unchecked")
    @SafeVarargs
    public final <T> Nullable<T> queryForSingleResult(final Class<T> targetClass, final String sql, Object... parameters) {
        DataSet rs = null;

        final Cursor cursor = rawQuery(sql, parameters);

        try {
            rs = extractData(cursor, Type.arrayOf(targetClass), 0, 1);
        } finally {
            cursor.close();
        }

        if (N.isNullOrEmpty(rs)) {
            return Nullable.empty();
        } else {
            return targetClass == null ? Nullable.of((T) rs.get(0, 0)) : Nullable.of(N.as(targetClass, rs.get(0, 0)));
        }
    }

    @Deprecated
    <T> Nullable<T> queryForSingleResult(final Class<T> targetClass, final String tableName, final String columnName, Condition whereClause) {
        final DataSet rs = query(tableName, N.asArray(columnName), N.asArray(targetClass), whereClause, null, null, null, 0, 1);

        if (N.isNullOrEmpty(rs)) {
            return Nullable.empty();
        } else {
            return targetClass == null ? Nullable.of((T) rs.get(0, 0)) : Nullable.of(N.as(targetClass, rs.get(0, 0)));
        }
    }

    @Deprecated
    <T> Nullable<T> queryForSingleResult(final Class<?> entityClass, final String columnName, Condition whereClause) {
        //        final Method propGetMethod = N.getPropGetMethod(entityClass, columnName);
        //        final Class<T> targetClass = (Class) propGetMethod.getReturnType();
        //        final DataSet rs = query(getTableNameByEntity(entityClass), Array.of(columnName), Array.of(targetClass), whereClause, null, null, null, 0, 1);
        //
        //        if (N.isNullOrEmpty(rs)) {
        //            return N.defaultValueOf(targetClass);
        //        } else {
        //            return N.as(targetClass, rs.absolute(0).get(0));
        //        }

        return queryForSingleResult((Class<T>) ClassUtil.getPropGetMethod(entityClass, columnName).getReturnType(), getTableNameByEntity(entityClass),
                columnName, whereClause);
    }

    //    <T> T queryForEntity(final Class<T> targetClass, String... selectColumnNames) {
    //        return queryForEntity(targetClass, N.asList(selectColumnNames));
    //    }
    //
    //    <T> T queryForEntity(final Class<T> targetClass, Collection<String> selectColumnNames) {
    //        return queryForEntity(targetClass, selectColumnNames, null);
    //    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause) {
        return queryForEntity(targetClass, selectColumnNames, whereClause, null);
    }

    /**
     * Just fetch the result in the 1st row. {@code null} is returned if no result is found. This method will try to
     * convert the column values to the type of mapping entity property if the mapping entity property is not assignable
     * from column value.
     *
     * @param targetClass an entity class with getter/setter methods.
     * @param selectColumnNames
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
     * @return
     */
    public <T> Optional<T> queryForEntity(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause, String orderBy) {
        final List<T> resultList = find(targetClass, selectColumnNames, whereClause, orderBy, 0, 1);

        return N.isNullOrEmpty(resultList) ? (Optional<T>) Optional.empty() : Optional.of(resultList.get(0));
    }

    /**
     * Just fetch the result in the 1st row. {@code null} is returned if no result is found. This method will try to
     * convert the column values to the type of mapping entity property if the mapping entity property is not assignable
     * from the column value.
     *
     * Remember to add {@code limit} condition if big result will be returned by the query.
     *
     * @param targetClass an entity class with getter/setter methods.
     * @param sql set <code>offset</code> and <code>limit</code> in sql with format: 
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>offsetValue</i>, <i>limitValue</i></code></li>
     * <br>or limit only:</br>
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>limitValue</i></code></li>
     * @param parameters A Object Array/List, and Map/Entity with getter/setter methods for parameterized sql with named parameters
     * @return
     */
    @SafeVarargs
    public final <T> Optional<T> queryForEntity(final Class<T> targetClass, final String sql, Object... parameters) {
        final DataSet rs = query(targetClass, sql, 0, 1, parameters);

        return N.isNullOrEmpty(rs) ? (Optional<T>) Optional.empty() : Optional.of(rs.getRow(targetClass, 0));
    }

    //    public <T> Optional<T> queryForEntity2(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause) {
    //        return queryForEntity2(targetClass, selectColumnNames, whereClause, null);
    //    }
    //
    //    /**
    //     * Just fetch the result in the 1st row. {@code null} is returned if no result is found. This method will try to
    //     * convert the column values to the type of mapping entity property if the mapping entity property is not assignable
    //     * from column value.
    //     *
    //     * @param targetClass an entity class with getter/setter methods.
    //     * @param selectColumnNames
    //     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
    //     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
    //     * @return
    //     */
    //    public <T> Optional<T> queryForEntity2(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause, String orderBy) {
    //        final List<T> resultList = find(targetClass, selectColumnNames, whereClause, orderBy, 0, 1);
    //
    //        return Optional.ofNullable(N.isNullOrEmpty(resultList) ? null : resultList.get(0));
    //    }
    //
    //    /**
    //     * Just fetch the result in the 1st row. {@code null} is returned if no result is found. This method will try to
    //     * convert the column values to the type of mapping entity property if the mapping entity property is not assignable
    //     * from the column value.
    //     *
    //     * Remember to add {@code limit} condition if big result will be returned by the query.
    //     *
    //     * @param targetClass an entity class with getter/setter methods.
    //     * @param sql set <code>offset</code> and <code>limit</code> in sql with format: 
    //     * <li><code>SELECT * FROM account where id = ? LIMIT <i>offsetValue</i>, <i>limitValue</i></code></li>
    //     * <br>or limit only:</br>
    //     * <li><code>SELECT * FROM account where id = ? LIMIT <i>limitValue</i></code></li>
    //     * @param parameters A Object Array/List, and Map/Entity with getter/setter methods for parameterized sql with named parameters
    //     * @return
    //     */
    //    public <T> Optional<T> queryForEntity2(final Class<T> targetClass, final String sql, Object... parameters) {
    //        final DataSet rs = query(targetClass, sql, 0, 1, parameters);
    //
    //        return Optional.ofNullable(N.isNullOrEmpty(rs) ? null : rs.getRow(targetClass, 0));
    //    }

    //    <T> List<T> find(final Class<T> targetClass, String... selectColumnNames) {
    //        return find(targetClass, N.asList(selectColumnNames));
    //    }
    //
    //    <T> List<T> find(final Class<T> targetClass, Collection<String> selectColumnNames) {
    //        return find(targetClass, selectColumnNames, null);
    //    }

    public <T> List<T> find(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause) {
        return find(targetClass, selectColumnNames, whereClause, null);
    }

    public <T> List<T> find(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause, String orderBy) {
        return find(targetClass, selectColumnNames, whereClause, orderBy, 0, Integer.MAX_VALUE);
    }

    public <T> List<T> find(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause, String orderBy, int offset, int count) {
        return find(targetClass, selectColumnNames, whereClause, null, null, orderBy, offset, count);
    }

    public <T> List<T> find(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause, String groupBy, String having,
            String orderBy) {
        return find(targetClass, selectColumnNames, whereClause, groupBy, having, orderBy, 0, Integer.MAX_VALUE);
    }

    /**
     * Find the records from database with the specified <code>whereClause, groupby, having, orderBy</code>,
     * and convert result to a list of the specified <code>targetClass</code>.
     *
     * @param targetClass an entity class with getter/setter methods.
     * @param selectColumnNames
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @param groupBy A filter declaring how to group rows, formatted as an SQL GROUP BY clause (excluding the GROUP BY itself). Passing null will cause the rows to not be grouped. 
     * @param having A filter declare which row groups to include in the cursor, if row grouping is being used, formatted as an SQL HAVING clause (excluding the HAVING itself). Passing null will cause all row groups to be included, and is required when row grouping is not being used.
     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
     * @param offset
     * @param count
     * @return
     */
    public <T> List<T> find(final Class<T> targetClass, Collection<String> selectColumnNames, Condition whereClause, String groupBy, String having,
            String orderBy, int offset, int count) {
        final DataSet rs = query(targetClass, selectColumnNames, whereClause, groupBy, having, orderBy, offset, count);

        if (N.isNullOrEmpty(rs)) {
            return new ArrayList<>();
        } else {
            return rs.toList(targetClass);
        }
    }

    /**
     * Find the records from database with the specified <code>sql, parameters</code>,
     * and convert result to a list of the specified <code>targetClass</code>.
     * 
     * @param targetClass an entity class with getter/setter methods.
     * @param sql set <code>offset</code> and <code>limit</code> in sql with format: 
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>offsetValue</i>, <i>limitValue</i></code></li>
     * <br>or limit only:</br>
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>limitValue</i></code></li>
     * @param parameters A Object Array/List, and Map/Entity with getter/setter methods for parameterized sql with named parameters
     * @return
     */
    @SafeVarargs
    public final <T> List<T> find(final Class<T> targetClass, final String sql, Object... parameters) {
        final DataSet rs = query(targetClass, sql, 0, Integer.MAX_VALUE, parameters);

        if (N.isNullOrEmpty(rs)) {
            return new ArrayList<>();
        } else {
            return rs.toList(targetClass);
        }
    }

    //    DataSet query(final Class<?> targetClass, String... selectColumnNames) {
    //        return query(targetClass, N.asList(selectColumnNames));
    //    }
    //
    //    DataSet query(final Class<?> targetClass, Collection<String> selectColumnNames) {
    //        return query(targetClass, selectColumnNames, null);
    //    }

    public DataSet query(final Class<?> targetClass, Collection<String> selectColumnNames, Condition whereClause) {
        return query(targetClass, selectColumnNames, whereClause, null);
    }

    public DataSet query(final Class<?> targetClass, Collection<String> selectColumnNames, Condition whereClause, String orderBy) {
        return query(targetClass, selectColumnNames, whereClause, orderBy, 0, Integer.MAX_VALUE);
    }

    public DataSet query(final Class<?> targetClass, Collection<String> selectColumnNames, Condition whereClause, String orderBy, int offset, int count) {
        return query(targetClass, selectColumnNames, whereClause, null, null, orderBy, offset, count);
    }

    public DataSet query(final Class<?> targetClass, Collection<String> selectColumnNames, Condition whereClause, String groupBy, String having,
            String orderBy) {
        return query(targetClass, selectColumnNames, whereClause, groupBy, having, orderBy, 0, Integer.MAX_VALUE);
    }

    /**
     * Find the records from database with the specified <code>whereClause, groupby, having, orderBy</code> and return the result set.
     *
     * @param targetClass an entity class with getter/setter methods.
     * @param selectColumnNames
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @param groupBy A filter declaring how to group rows, formatted as an SQL GROUP BY clause (excluding the GROUP BY itself). Passing null will cause the rows to not be grouped.
     * @param having A filter declare which row groups to include in the cursor, if row grouping is being used, formatted as an SQL HAVING clause (excluding the HAVING itself). Passing null will cause all row groups to be included, and is required when row grouping is not being used.
     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
     * @param offset
     * @param count
     * @return
     */
    public DataSet query(final Class<?> targetClass, Collection<String> selectColumnNames, Condition whereClause, String groupBy, String having, String orderBy,
            int offset, int count) {
        if (N.isNullOrEmpty(selectColumnNames)) {
            selectColumnNames = ClassUtil.getPropGetMethodList(targetClass).keySet();
        }

        final String[] columns = selectColumnNames.toArray(new String[selectColumnNames.size()]);
        final Type<Object>[] selectColumnTypes = new Type[columns.length];

        for (int i = 0, len = columns.length; i < len; i++) {
            selectColumnTypes[i] = Type.valueOf(ClassUtil.getPropGetMethod(targetClass, columns[i]).getReturnType());
        }

        return query(ClassUtil.getSimpleClassName(targetClass), columns, selectColumnTypes, whereClause, groupBy, having, orderBy, offset, count);
    }

    /**
     * 
     * @param table
     * @param selectColumnNameTypeMap
     * @param whereClause
     * @return
     * 
     * @since 0.8.10
     */
    @SuppressWarnings("rawtypes")
    public DataSet query(String table, Map<String, Class> selectColumnNameTypeMap, Condition whereClause) {
        return query(table, selectColumnNameTypeMap, whereClause, null);
    }

    /**
     * 
     * @param table
     * @param selectColumnNameTypeMap
     * @param whereClause
     * @param orderBy
     * @return
     * 
     * @since 0.8.10
     */
    @SuppressWarnings("rawtypes")
    public DataSet query(String table, Map<String, Class> selectColumnNameTypeMap, Condition whereClause, String orderBy) {
        return query(table, selectColumnNameTypeMap, whereClause, orderBy, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param table
     * @param selectColumnNameTypeMap
     * @param whereClause
     * @param orderBy
     * @param offset
     * @param count
     * @return
     * 
     * @since 0.8.10
     */
    @SuppressWarnings("rawtypes")
    public DataSet query(String table, Map<String, Class> selectColumnNameTypeMap, Condition whereClause, String orderBy, int offset, int count) {
        return query(table, selectColumnNameTypeMap, whereClause, null, null, orderBy, offset, count);
    }

    /**
     * 
     * @param table
     * @param selectColumnNameTypeMap
     * @param whereClause
     * @param groupBy
     * @param having
     * @param orderBy
     * @return
     * 
     * @since 0.8.10
     */
    @SuppressWarnings("rawtypes")
    public DataSet query(String table, Map<String, Class> selectColumnNameTypeMap, Condition whereClause, String groupBy, String having, String orderBy) {
        return query(table, selectColumnNameTypeMap, whereClause, groupBy, having, orderBy, 0, Integer.MAX_VALUE);
    }

    /**
     * Find the records from database with the specified <code>whereClause, groupby, having, orderBy</code> and return the result set.
     *
     * @param table
     * @param selectColumnNameTypeMap
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @param groupBy A filter declaring how to group rows, formatted as an SQL GROUP BY clause (excluding the GROUP BY itself). Passing null will cause the rows to not be grouped.
     * @param having A filter declare which row groups to include in the cursor, if row grouping is being used, formatted as an SQL HAVING clause (excluding the HAVING itself). Passing null will cause all row groups to be included, and is required when row grouping is not being used.
     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
     * @param offset
     * @param count
     * @return
     * 
     * @since 0.8.10
     */
    @SuppressWarnings("rawtypes")
    public DataSet query(String table, Map<String, Class> selectColumnNameTypeMap, Condition whereClause, String groupBy, String having, String orderBy,
            int offset, int count) {
        final String[] selectColumnNames = selectColumnNameTypeMap == null ? null : new String[selectColumnNameTypeMap.size()];
        final Class[] selectColumnTypes = selectColumnNameTypeMap == null ? null : new Class[selectColumnNameTypeMap.size()];

        if (N.notNullOrEmpty(selectColumnNameTypeMap)) {
            int i = 0;
            for (Map.Entry<String, Class> entry : selectColumnNameTypeMap.entrySet()) {
                selectColumnNames[i] = entry.getKey();
                selectColumnTypes[i] = entry.getValue();
                i++;
            }
        }

        return this.query(table, selectColumnNames, selectColumnTypes, whereClause, groupBy, having, orderBy, offset, count);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Class[] selectColumnTypes, Condition whereClause) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, null);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Class[] selectColumnTypes, Condition whereClause, String orderBy) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, orderBy, 0, Integer.MAX_VALUE);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Class[] selectColumnTypes, Condition whereClause, String orderBy, int offset, int count) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, null, null, orderBy, offset, count);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Class[] selectColumnTypes, Condition whereClause, String groupBy, String having, String orderBy) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, groupBy, having, orderBy, 0, Integer.MAX_VALUE);
    }

    /**
     * Find the records from database with the specified <code>whereClause, groupby, having, orderBy</code> and return the result set.
     *
     * @param table
     * @param selectColumnNames
     * @param selectColumnTypes
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @param groupBy A filter declaring how to group rows, formatted as an SQL GROUP BY clause (excluding the GROUP BY itself). Passing null will cause the rows to not be grouped.
     * @param having A filter declare which row groups to include in the cursor, if row grouping is being used, formatted as an SQL HAVING clause (excluding the HAVING itself). Passing null will cause all row groups to be included, and is required when row grouping is not being used.
     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
     * @param offset
     * @param count
     * @return
     */
    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Class[] selectColumnTypes, Condition whereClause, String groupBy, String having, String orderBy,
            int offset, int count) {
        if (whereClause == null) {
            return executeQuery(table, selectColumnNames, Type.arrayOf(selectColumnTypes), (String) null, N.EMPTY_STRING_ARRAY, groupBy, having, orderBy,
                    offset, count);
        } else {
            final Command cmd = interpretCondition(whereClause);
            return executeQuery(table, selectColumnNames, Type.arrayOf(selectColumnTypes), cmd.getSql(), cmd.getArgs(), groupBy, having, orderBy, offset,
                    count);
        }
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, Condition whereClause) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, null);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, Condition whereClause, String orderBy) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, orderBy, 0, Integer.MAX_VALUE);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, Condition whereClause, String orderBy, int offset, int count) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, null, null, orderBy, offset, count);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, Condition whereClause, String groupBy, String having, String orderBy) {
        return query(table, selectColumnNames, selectColumnTypes, whereClause, groupBy, having, orderBy, 0, Integer.MAX_VALUE);
    }

    /**
     *
     * @param table
     * @param selectColumnNames
     * @param selectColumnTypes
     * @param whereClause Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.
     * @param groupBy A filter declaring how to group rows, formatted as an SQL GROUP BY clause (excluding the GROUP BY itself). Passing null will cause the rows to not be grouped.
     * @param having A filter declare which row groups to include in the cursor, if row grouping is being used, formatted as an SQL HAVING clause (excluding the HAVING itself). Passing null will cause all row groups to be included, and is required when row grouping is not being used.
     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
     * @param offset
     * @param count
     * @return
     */
    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, Condition whereClause, String groupBy, String having, String orderBy,
            int offset, int count) {
        if (whereClause == null) {
            return executeQuery(table, selectColumnNames, selectColumnTypes, (String) null, N.EMPTY_STRING_ARRAY, groupBy, having, orderBy, offset, count);
        } else {
            final Command cmd = interpretCondition(whereClause);
            return executeQuery(table, selectColumnNames, selectColumnTypes, cmd.getSql(), cmd.getArgs(), groupBy, having, orderBy, offset, count);
        }
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, String where, String[] whereArgs) {
        return query(table, selectColumnNames, selectColumnTypes, where, whereArgs, null);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, String where, String[] whereArgs, String orderBy) {
        return query(table, selectColumnNames, selectColumnTypes, where, whereArgs, orderBy, 0, Integer.MAX_VALUE);

    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, String where, String[] whereArgs, String orderBy, int offset, int count) {
        return query(table, selectColumnNames, selectColumnTypes, where, whereArgs, null, null, orderBy, offset, count);
    }

    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, String where, String[] whereArgs, String groupBy, String having,
            String orderBy) {
        return query(table, selectColumnNames, selectColumnTypes, where, whereArgs, groupBy, having, orderBy, 0, Integer.MAX_VALUE);
    }

    /**
     *
     * @param table
     * @param selectColumnNames
     * @param selectColumnTypes
     * @param where A filter declaring which rows to return, formatted as an SQL WHERE clause (excluding the WHERE itself). Passing null will return all rows for the given table.
     * @param whereArgs You may include ?s in selection, which will be replaced by the values from selectionArgs, in order that they appear in the selection. The values will be bound as Strings.
     * @param groupBy A filter declaring how to group rows, formatted as an SQL GROUP BY clause (excluding the GROUP BY itself). Passing null will cause the rows to not be grouped.
     * @param having A filter declare which row groups to include in the cursor, if row grouping is being used, formatted as an SQL HAVING clause (excluding the HAVING itself). Passing null will cause all row groups to be included, and is required when row grouping is not being used.
     * @param orderby How to order the rows, formatted as an SQL ORDER BY clause (excluding the ORDER BY itself). Passing null will use the default sort order, which may be unordered.
     * @param offset
     * @param count
     * @return
     */
    @Deprecated
    @SuppressWarnings("rawtypes")
    DataSet query(String table, String[] selectColumnNames, Type[] selectColumnTypes, String where, String[] whereArgs, String groupBy, String having,
            String orderBy, int offset, int count) {
        return executeQuery(table, selectColumnNames, selectColumnTypes, parseStringCondition(where), whereArgs, groupBy, having, orderBy, offset, count);
    }

    @SuppressWarnings("rawtypes")
    private DataSet executeQuery(String table, String[] selectColumnNames, Type[] selectColumnTypes, String where, String[] whereArgs, String groupBy,
            String having, String orderBy, int offset, int count) {

        if (offset < 0 || count < 0) {
            throw new IllegalArgumentException("offset and count can't be negative: offset=" + offset + ", count=" + count);
        }

        table = formatName(table);

        final String[] formattedColumnNames = new String[selectColumnNames.length];

        for (int i = 0, len = selectColumnNames.length; i < len; i++) {
            formattedColumnNames[i] = formatName(selectColumnNames[i]);
        }

        String limit = null;

        if (offset > 0) {
            limit = offset + " , " + count;
        } else if (count < Integer.MAX_VALUE) {
            limit = String.valueOf(count);
        }

        groupBy = groupBy == null ? null : formatName(groupBy);
        having = having == null ? null : parseStringCondition(having);
        orderBy = orderBy == null ? null : formatName(orderBy);
        Cursor cursor = sqliteDB.query(table, formattedColumnNames, where, whereArgs, groupBy, having, orderBy, limit);

        DataSet rs = null;

        try {
            rs = extractData(cursor, selectColumnTypes);
        } finally {
            cursor.close();
        }

        for (int i = 0, len = formattedColumnNames.length; i < len; i++) {
            if (!formattedColumnNames[i].equals(selectColumnNames[i]) && rs.containsColumn(formattedColumnNames[i])) {
                rs.renameColumn(formattedColumnNames[i], selectColumnNames[i]);
            }
        }

        return rs;
    }

    /**
     * Find the records from database with the specified <code>sql, parameters</code> and return the result set.
     * 
     * @param targetClass an entity class with getter/setter methods.
     * @param sql set <code>offset</code> and <code>limit</code> in sql with format: 
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>offsetValue</i>, <i>limitValue</i></code></li>
     * <br>or limit only:</br>
     * <li><code>SELECT * FROM account where id = ? LIMIT <i>limitValue</i></code></li>
     * @param parameters A Object Array/List, and Map/Entity with getter/setter methods for parameterized sql with named parameters
     * @return
     */
    @SafeVarargs
    public final DataSet query(Class<?> targetClass, String sql, Object... parameters) {
        return query(targetClass, sql, 0, Integer.MAX_VALUE, parameters);
    }

    private DataSet query(Class<?> targetClass, String sql, int offset, int count, Object... parameters) {
        final Cursor cursor = rawQuery(sql, parameters);

        try {
            return extractData(targetClass, cursor, offset, count);
        } finally {
            cursor.close();
        }
    }

    private Cursor rawQuery(String sql, Object... parameters) {
        final NamedSQL namedSQL = parseSQL(sql);
        final Object[] args = prepareArguments(namedSQL, parameters);
        final String[] strArgs = new String[args.length];

        for (int i = 0, len = args.length; i < len; i++) {
            strArgs[i] = N.stringOf(args[i]);
        }

        return sqliteDB.rawQuery(namedSQL.getPureSQL(), strArgs);
    }

    public void beginTransaction() {
        sqliteDB.beginTransaction();
    }

    public void beginTransactionNonExclusive() {
        sqliteDB.beginTransactionNonExclusive();
    }

    public boolean inTransaction() {
        return sqliteDB.inTransaction();
    }

    public void setTransactionSuccessful() {
        sqliteDB.setTransactionSuccessful();
    }

    public void endTransaction() {
        sqliteDB.endTransaction();
    }

    private String formatName(String tableName) {
        switch (columnNamingPolicy) {
            case LOWER_CAMEL_CASE:
                return tableName;
            case LOWER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toLowerCaseWithUnderscore(tableName);
            case UPPER_CASE_WITH_UNDERSCORE:
                return ClassUtil.toUpperCaseWithUnderscore(tableName);
            default:
                throw new IllegalArgumentException("Unsupported NamingPolicy: " + columnNamingPolicy);
        }
    }

    private NamedSQL parseSQL(String sql) {
        return NamedSQL.parse(sql, null);
    }

    private String parseStringCondition(String expr) {
        if (N.isNullOrEmpty(expr)) {
            return expr;
        }

        final StringBuilder sb = ObjectFactory.createStringBuilder();

        try {
            final List<String> words = SQLParser.parse(expr);

            String word = null;
            for (int i = 0, len = words.size(); i < len; i++) {
                word = words.get(i);

                if (!N.isAsciiAlpha(word.charAt(0))) {
                    sb.append(word);
                } else if (i < len - 1 && words.get(i + 1).charAt(0) == WD._PARENTHESES_L) {
                    sb.append(word);
                } else {
                    sb.append(formatName(word));
                }
            }

            return sb.toString();
        } finally {
            ObjectFactory.recycle(sb);
        }
    }

    private Object[] prepareArguments(final NamedSQL namedSQL, final Object... parameters) {
        final int parameterCount = namedSQL.getParameterCount();

        if (parameterCount == 0) {
            return N.EMPTY_OBJECT_ARRAY;
        } else if (N.isNullOrEmpty(parameters)) {
            throw new IllegalArgumentException("Null or empty parameters for parameterized query: " + namedSQL.getNamedSQL());
        }

        final Map<Integer, String> namedParameters = namedSQL.getNamedParameters();
        Object[] result = parameters;

        if (N.notNullOrEmpty(namedParameters) && parameters.length == 1 && (parameters[0] instanceof Map || N.isEntity(parameters[0].getClass()))) {
            result = new Object[parameterCount];
            Object parameter_0 = parameters[0];

            if (parameter_0 instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> m = (Map<String, Object>) parameter_0;

                for (int i = 0; i < parameterCount; i++) {
                    result[i] = m.get(namedParameters.get(i));

                    if ((result[i] == null) && !m.containsKey(namedParameters.get(i))) {
                        throw new IllegalArgumentException("Parameter for property '" + namedParameters.get(i) + "' is missed");
                    }
                }
            } else {
                Object entity = parameter_0;
                Class<?> clazz = entity.getClass();
                Method propGetMethod = null;

                for (int i = 0; i < parameterCount; i++) {
                    propGetMethod = ClassUtil.getPropGetMethod(clazz, namedParameters.get(i));

                    if (propGetMethod == null) {
                        throw new IllegalArgumentException("Parameter for property '" + namedParameters.get(i) + "' is missed");
                    }

                    result[i] = ClassUtil.invokeMethod(entity, propGetMethod);
                }
            }
        } else {
            if ((parameters.length == 1) && (parameters[0] != null)) {
                if (parameters[0] instanceof Object[] && ((((Object[]) parameters[0]).length) >= parameterCount)) {
                    return (Object[]) parameters[0];
                } else if (parameters[0] instanceof List && (((List<?>) parameters[0]).size() >= parameterCount)) {
                    final Collection<?> c = (Collection<?>) parameters[0];
                    return c.toArray(new Object[c.size()]);
                }
            }
        }

        return result;
    }

    private Command interpretCondition(Condition condition) {
        if (condition instanceof Binary) {
            return interpretBinary((Binary) condition);
        } else if (condition instanceof Between) {
            return interpretBetween((Between) condition);
        } else if (condition instanceof In) {
            return interpretIn((In) condition);
        } else if (condition instanceof Junction) {
            return interpretJunction((Junction) condition);
        } else if (condition instanceof Expression) {
            return interpretExpression((Expression) condition);
        } else {
            throw new IllegalArgumentException("Unsupported condition type: " + condition.getOperator()
                    + ". Only binary(=, <>, like, IS NULL ...)/between/junction(or, and...) are supported.");
        }
    }

    private Command interpretBinary(Binary binary) {
        final Command cmd = new Command();

        cmd.setSql(formatName(binary.getPropName()) + WD.SPACE + binary.getOperator() + " ?");
        cmd.setArgs(N.asArray(N.stringOf(binary.getPropValue())));

        return cmd;
    }

    private Command interpretBetween(Between bt) {
        final Command cmd = new Command();

        cmd.setSql(formatName(bt.getPropName()) + WD.SPACE + bt.getOperator() + " (?, ?)");
        cmd.setArgs(N.asArray(N.stringOf(bt.getMinValue()), N.stringOf(bt.getMaxValue())));

        return cmd;
    }

    private Command interpretIn(In in) {
        final Command cmd = new Command();

        cmd.setSql(formatName(in.getPropName()) + in.inPart());

        final List<Object> parameters = in.getParameters();
        final String[] args = new String[parameters.size()];

        for (int i = 0, len = args.length; i < len; i++) {
            args[i] = N.stringOf(parameters.get(i));
        }

        cmd.setArgs(args);

        return cmd;
    }

    private Command interpretJunction(Junction junction) {
        final List<Condition> conditionList = junction.getConditions();

        if (N.isNullOrEmpty(conditionList)) {
            throw new IllegalArgumentException("The junction condition(" + junction.getOperator().toString() + ") doesn't include any element.");
        }

        if (conditionList.size() == 1) {
            return interpretCondition(conditionList.get(0));
        } else {
            final List<String> argList = new ArrayList<>();
            final StringBuilder sb = ObjectFactory.createStringBuilder();

            try {
                for (int i = 0; i < conditionList.size(); i++) {
                    if (i > 0) {
                        sb.append(WD._SPACE);
                        sb.append(junction.getOperator().toString());
                        sb.append(WD._SPACE);
                    }

                    sb.append(WD._PARENTHESES_L);

                    Command cmd = interpretCondition(conditionList.get(i));
                    sb.append(cmd.getSql());

                    if (N.notNullOrEmpty(cmd.getArgs())) {
                        for (String arg : cmd.getArgs()) {
                            argList.add(arg);
                        }
                    }

                    sb.append(WD._PARENTHESES_R);
                }

                Command cmd = new Command();
                cmd.setSql(sb.toString());

                if (N.notNullOrEmpty(argList)) {
                    cmd.setArgs(argList.toArray(new String[argList.size()]));
                }

                return cmd;
            } finally {
                ObjectFactory.recycle(sb);
            }
        }
    }

    private Command interpretExpression(Expression exp) {
        Command cmd = new Command();

        cmd.setSql(exp.getLiteral());

        return cmd;
    }

    private static boolean isDirtyMarkerEntity(final Class<?> cls) {
        return DirtyMarker.class.isAssignableFrom(cls) && N.isEntity(cls);
    }

    private static class Command {
        private String sql;
        private String[] args = N.EMPTY_STRING_ARRAY;

        public String getSql() {
            return sql;
        }

        public void setSql(String sql) {
            this.sql = sql;
        }

        public String[] getArgs() {
            return args;
        }

        public void setArgs(String[] args) {
            this.args = args;
        }

        @Override
        public int hashCode() {
            int result = 1;
            result = 31 * result + Arrays.hashCode(args);
            result = 31 * result + ((sql == null) ? 0 : sql.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj instanceof Command) {
                Command other = (Command) obj;

                return N.equals(sql, other.sql) && N.equals(args, other.args);
            }

            return false;
        }

        @Override
        public String toString() {
            if (N.isNullOrEmpty(args)) {
                return sql;
            } else {
                final StringBuilder sb = ObjectFactory.createStringBuilder();

                try {
                    sb.append(sql);
                    sb.append(_SPACE);
                    sb.append(_BRACE_L);

                    for (int i = 0, len = args.length; i < len; i++) {
                        if (i > 0) {
                            sb.append(COMMA_SPACE);
                        }

                        sb.append(i + 1);
                        sb.append(_EQUAL);
                        sb.append(args[i]);
                    }

                    sb.append(_BRACE_R);

                    return sb.toString();

                } finally {
                    ObjectFactory.recycle(sb);
                }
            }
        }
    }

    static abstract class Type<T> {
        public static final Type<String> STRING = new Type<String>(Cursor.FIELD_TYPE_STRING, String.class) {
            @Override
            public String get(Cursor cursor, int columnIndex) {
                return cursor.getString(columnIndex);
            }

            @Override
            public String get(ContentValues contentValues, String key) {
                return contentValues.getAsString(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, String value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<Boolean> BOOLEAN = new Type<Boolean>(Cursor.FIELD_TYPE_STRING, Boolean.class) {
            @Override
            public Boolean get(Cursor cursor, int columnIndex) {
                return Boolean.valueOf(cursor.getString(columnIndex));
            }

            @Override
            public Boolean get(ContentValues contentValues, String key) {
                return contentValues.getAsBoolean(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, Boolean value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<Character> CHAR = new Type<Character>(Cursor.FIELD_TYPE_STRING, Character.class) {
            @Override
            public Character get(Cursor cursor, int columnIndex) {
                return cursor.getString(columnIndex).charAt(0);
            }

            @Override
            public Character get(ContentValues contentValues, String key) {
                return contentValues.getAsString(key).charAt(0);
            }

            @Override
            public void set(ContentValues contentValues, String key, Character value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<Byte> BYTE = new Type<Byte>(Cursor.FIELD_TYPE_INTEGER, Byte.class) {
            @Override
            public Byte get(Cursor cursor, int columnIndex) {
                return (byte) cursor.getShort(columnIndex);
            }

            @Override
            public Byte get(ContentValues contentValues, String key) {
                return contentValues.getAsByte(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, Byte value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<Short> SHORT = new Type<Short>(Cursor.FIELD_TYPE_INTEGER, Short.class) {
            @Override
            public Short get(Cursor cursor, int columnIndex) {
                return cursor.getShort(columnIndex);
            }

            @Override
            public Short get(ContentValues contentValues, String key) {
                return contentValues.getAsShort(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, Short value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<Integer> INT = new Type<Integer>(Cursor.FIELD_TYPE_INTEGER, Integer.class) {
            @Override
            public Integer get(Cursor cursor, int columnIndex) {
                return cursor.getInt(columnIndex);
            }

            @Override
            public Integer get(ContentValues contentValues, String key) {
                return contentValues.getAsInteger(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, Integer value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<Long> LONG = new Type<Long>(Cursor.FIELD_TYPE_INTEGER, Long.class) {
            @Override
            public Long get(Cursor cursor, int columnIndex) {
                return cursor.getLong(columnIndex);
            }

            @Override
            public Long get(ContentValues contentValues, String key) {
                return contentValues.getAsLong(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, Long value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<Float> FLOAT = new Type<Float>(Cursor.FIELD_TYPE_FLOAT, Float.class) {
            @Override
            public Float get(Cursor cursor, int columnIndex) {
                return cursor.getFloat(columnIndex);
            }

            @Override
            public Float get(ContentValues contentValues, String key) {
                return contentValues.getAsFloat(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, Float value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<Double> DOUBLE = new Type<Double>(Cursor.FIELD_TYPE_FLOAT, Double.class) {
            @Override
            public Double get(Cursor cursor, int columnIndex) {
                return cursor.getDouble(columnIndex);
            }

            @Override
            public Double get(ContentValues contentValues, String key) {
                return contentValues.getAsDouble(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, Double value) {
                contentValues.put(key, value);
            }
        };

        public static final Type<BigInteger> BIG_INTEGER = new Type<BigInteger>(Cursor.FIELD_TYPE_STRING, BigInteger.class) {
            @Override
            public BigInteger get(Cursor cursor, int columnIndex) {
                String value = cursor.getString(columnIndex);

                if (N.isNullOrEmpty(value)) {
                    return null;
                }

                return new BigInteger(value);
            }

            @Override
            public BigInteger get(ContentValues contentValues, String key) {
                String value = contentValues.getAsString(key);

                if (N.isNullOrEmpty(value)) {
                    return null;
                }

                return new BigInteger(value);
            }

            @Override
            public void set(ContentValues contentValues, String key, BigInteger value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<BigDecimal> BIG_DECIMAL = new Type<BigDecimal>(Cursor.FIELD_TYPE_STRING, BigDecimal.class) {
            @Override
            public BigDecimal get(Cursor cursor, int columnIndex) {
                String value = cursor.getString(columnIndex);

                if (N.isNullOrEmpty(value)) {
                    return null;
                }

                return new BigDecimal(value);
            }

            @Override
            public BigDecimal get(ContentValues contentValues, String key) {
                String value = contentValues.getAsString(key);

                if (N.isNullOrEmpty(value)) {
                    return null;
                }

                return new BigDecimal(value);
            }

            @Override
            public void set(ContentValues contentValues, String key, BigDecimal value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<Date> DATE = new Type<Date>(Cursor.FIELD_TYPE_STRING, Date.class) {
            @Override
            public Date get(Cursor cursor, int columnIndex) {
                return N.asDate(cursor.getString(columnIndex));
            }

            @Override
            public Date get(ContentValues contentValues, String key) {
                return N.asDate(contentValues.getAsString(key));
            }

            @Override
            public void set(ContentValues contentValues, String key, Date value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<Time> TIME = new Type<Time>(Cursor.FIELD_TYPE_STRING, Time.class) {
            @Override
            public Time get(Cursor cursor, int columnIndex) {
                return N.asTime(cursor.getString(columnIndex));
            }

            @Override
            public Time get(ContentValues contentValues, String key) {
                return N.asTime(contentValues.getAsString(key));
            }

            @Override
            public void set(ContentValues contentValues, String key, Time value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<Timestamp> TIMESTAMP = new Type<Timestamp>(Cursor.FIELD_TYPE_STRING, Timestamp.class) {
            @Override
            public Timestamp get(Cursor cursor, int columnIndex) {
                return N.asTimestamp(cursor.getString(columnIndex));
            }

            @Override
            public Timestamp get(ContentValues contentValues, String key) {
                return N.asTimestamp(contentValues.getAsString(key));
            }

            @Override
            public void set(ContentValues contentValues, String key, Timestamp value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<java.util.Date> JU_DATE = new Type<java.util.Date>(Cursor.FIELD_TYPE_STRING, java.util.Date.class) {
            @Override
            public java.util.Date get(Cursor cursor, int columnIndex) {
                return N.asJUDate(cursor.getString(columnIndex));
            }

            @Override
            public java.util.Date get(ContentValues contentValues, String key) {
                return N.asJUDate(contentValues.getAsString(key));
            }

            @Override
            public void set(ContentValues contentValues, String key, java.util.Date value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<Calendar> CALENDAR = new Type<Calendar>(Cursor.FIELD_TYPE_STRING, Calendar.class) {
            @Override
            public Calendar get(Cursor cursor, int columnIndex) {
                return N.asCalendar(cursor.getString(columnIndex));
            }

            @Override
            public Calendar get(ContentValues contentValues, String key) {
                return N.asCalendar(contentValues.getAsString(key));
            }

            @Override
            public void set(ContentValues contentValues, String key, Calendar value) {
                contentValues.put(key, N.stringOf(value));
            }
        };

        public static final Type<byte[]> BLOB = new Type<byte[]>(Cursor.FIELD_TYPE_BLOB, byte[].class) {
            @Override
            public byte[] get(Cursor cursor, int columnIndex) {
                return cursor.getBlob(columnIndex);
            }

            @Override
            public byte[] get(ContentValues contentValues, String key) {
                return contentValues.getAsByteArray(key);
            }

            @Override
            public void set(ContentValues contentValues, String key, byte[] value) {
                contentValues.put(key, value);
            }
        };

        private static final Map<Class<?>, Type<?>> classSQLiteTypePool = new ObjectPool<>(64);

        static {
            classSQLiteTypePool.put(String.class, STRING);
            classSQLiteTypePool.put(boolean.class, BOOLEAN);
            classSQLiteTypePool.put(Boolean.class, BOOLEAN);
            classSQLiteTypePool.put(char.class, CHAR);
            classSQLiteTypePool.put(Character.class, CHAR);
            classSQLiteTypePool.put(byte.class, BYTE);
            classSQLiteTypePool.put(Byte.class, BYTE);
            classSQLiteTypePool.put(short.class, SHORT);
            classSQLiteTypePool.put(Short.class, SHORT);
            classSQLiteTypePool.put(int.class, INT);
            classSQLiteTypePool.put(Integer.class, INT);
            classSQLiteTypePool.put(long.class, LONG);
            classSQLiteTypePool.put(Long.class, LONG);
            classSQLiteTypePool.put(float.class, FLOAT);
            classSQLiteTypePool.put(Float.class, FLOAT);
            classSQLiteTypePool.put(double.class, DOUBLE);
            classSQLiteTypePool.put(Double.class, DOUBLE);
            classSQLiteTypePool.put(BigInteger.class, BIG_INTEGER);
            classSQLiteTypePool.put(BigDecimal.class, BIG_DECIMAL);
            classSQLiteTypePool.put(Date.class, DATE);
            classSQLiteTypePool.put(Time.class, TIME);
            classSQLiteTypePool.put(Timestamp.class, TIMESTAMP);
            classSQLiteTypePool.put(java.util.Date.class, JU_DATE);
            classSQLiteTypePool.put(Calendar.class, CALENDAR);
            classSQLiteTypePool.put(byte[].class, BLOB);
        }

        private final int androidSQLiteType;
        private final Class<?> typeClass;

        private Type(int androidSQLiteType, Class<T> typeClass) {
            this.androidSQLiteType = androidSQLiteType;
            this.typeClass = typeClass;
        }

        public int getAndroidSQLiteType() {
            return androidSQLiteType;
        }

        public <C> Class<C> getTypeClass() {
            return (Class<C>) typeClass;
        }

        public abstract T get(Cursor cursor, int columnIndex);

        public abstract T get(ContentValues contentValues, String key);

        public abstract void set(ContentValues contentValues, String key, T value);

        public static <T> Type<T> valueOf(final int androidSQLiteType) {
            switch (androidSQLiteType) {
                case Cursor.FIELD_TYPE_INTEGER:
                    return (Type<T>) INT;
                case Cursor.FIELD_TYPE_FLOAT:
                    return (Type<T>) FLOAT;
                case Cursor.FIELD_TYPE_STRING:
                    return (Type<T>) STRING;
                case Cursor.FIELD_TYPE_BLOB:
                    return (Type<T>) BLOB;

                default:
                    throw new IllegalArgumentException("Unsupported android sqlite type: " + androidSQLiteType);
            }
        }

        public static <T, C> Type<T> valueOf(final Class<C> typeClass) {
            Type<C> sqliteType = (Type<C>) classSQLiteTypePool.get(typeClass);

            if (sqliteType == null) {
                sqliteType = new Type<C>(Cursor.FIELD_TYPE_STRING, typeClass) {
                    private final com.landawn.abacus.type.Type<Object> ttType = N.typeOf(typeClass);

                    @Override
                    public C get(Cursor cursor, int columnIndex) {
                        return (C) ttType.valueOf(cursor.getString(columnIndex));
                    }

                    @Override
                    public C get(ContentValues contentValues, String key) {
                        return (C) ttType.valueOf(contentValues.getAsString(key));
                    }

                    @Override
                    public void set(ContentValues contentValues, String key, C value) {
                        contentValues.put(key, ttType.stringOf(value));
                    }
                };

                classSQLiteTypePool.put(typeClass, sqliteType);
            }

            return (Type<T>) sqliteType;
        }

        @SafeVarargs
        public static <T> Type<T>[] arrayOf(final int... androidSQLiteTypes) {

            final Type<T>[] types = new Type[androidSQLiteTypes.length];

            for (int i = 0, len = androidSQLiteTypes.length; i < len; i++) {
                types[i] = valueOf(androidSQLiteTypes[i]);
            }

            return types;
        }

        @SafeVarargs
        public static <T> Type<T>[] arrayOf(final Class<?>... typeClasses) {
            final Type<T>[] types = new Type[typeClasses.length];

            for (int i = 0, len = typeClasses.length; i < len; i++) {
                types[i] = valueOf(typeClasses[i]);
            }

            return types;
        }
    }
}
