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

package com.landawn.abacus.util;

import java.io.Closeable;
import java.io.IOException;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClient;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper;
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig;
import com.amazonaws.services.dynamodbv2.model.AttributeAction;
import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.AttributeValueUpdate;
import com.amazonaws.services.dynamodbv2.model.BatchGetItemRequest;
import com.amazonaws.services.dynamodbv2.model.BatchWriteItemRequest;
import com.amazonaws.services.dynamodbv2.model.BatchWriteItemResult;
import com.amazonaws.services.dynamodbv2.model.ComparisonOperator;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.amazonaws.services.dynamodbv2.model.DeleteItemRequest;
import com.amazonaws.services.dynamodbv2.model.DeleteItemResult;
import com.amazonaws.services.dynamodbv2.model.GetItemRequest;
import com.amazonaws.services.dynamodbv2.model.KeysAndAttributes;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.dynamodbv2.model.PutItemResult;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import com.amazonaws.services.dynamodbv2.model.QueryResult;
import com.amazonaws.services.dynamodbv2.model.ScanRequest;
import com.amazonaws.services.dynamodbv2.model.ScanResult;
import com.amazonaws.services.dynamodbv2.model.UpdateItemRequest;
import com.amazonaws.services.dynamodbv2.model.UpdateItemResult;
import com.amazonaws.services.dynamodbv2.model.WriteRequest;
import com.landawn.abacus.DataSet;
import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.parser.ParserUtil;
import com.landawn.abacus.parser.ParserUtil.EntityInfo;
import com.landawn.abacus.parser.ParserUtil.PropInfo;
import com.landawn.abacus.type.Type;

/**
 * It's a simple wrapper of DynamoDB Java client.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/">com.amazonaws.services.dynamodbv2.AmazonDynamoDB</a>
 */
public final class DynamoDBExecutor implements Closeable {
    private static final Type<AttributeValue> attrValueType = N.typeOf(AttributeValue.class);

    private final AmazonDynamoDBClient dynamoDB;
    private final DynamoDBMapper mapper;
    private final AsyncDynamoDBExecutor asyncDBExecutor;

    public DynamoDBExecutor(final AmazonDynamoDBClient dynamoDB) {
        this(dynamoDB, null);
    }

    public DynamoDBExecutor(final AmazonDynamoDBClient dynamoDB, final DynamoDBMapperConfig config) {
        this(dynamoDB, config, new AsyncExecutor(64, 300, TimeUnit.SECONDS));
    }

    public DynamoDBExecutor(final AmazonDynamoDBClient dynamoDB, final DynamoDBMapperConfig config, final AsyncExecutor asyncExecutor) {
        this.dynamoDB = dynamoDB;
        this.asyncDBExecutor = new AsyncDynamoDBExecutor(this, asyncExecutor);
        this.mapper = config == null ? new DynamoDBMapper(dynamoDB) : new DynamoDBMapper(dynamoDB, config);
    }

    public AmazonDynamoDBClient dynamoDB() {
        return dynamoDB;
    }

    public DynamoDBMapper mapper() {
        return mapper;
    }

    public DynamoDBMapper mapper(final DynamoDBMapperConfig config) {
        return new DynamoDBMapper(dynamoDB, config);
    }

    public AsyncDynamoDBExecutor async() {
        return asyncDBExecutor;
    }

    /**
     * Set value to <code>null</code> by <code>withNULL(Boolean.TRUE)</code> if the specified value is null, 
     * or set it to <code>Boolean</code> by <code>setBOOL((Boolean) value)</code> if it's <code>Boolean</code>,
     * or set it to <code>ByteBuffer</code> by <code>setB((ByteBuffer) value)</code> if it's <code>ByteBuffer</code>, 
     * otherwise, set it to String by <code>setS(N.stringOf(value))</code> for other types. 
     * That's to say all the types except Number/Boolean/ByteBuffer are defined to String. 
     * 
     * @param value
     * @return
     */
    public static AttributeValue attrValueOf(Object value) {
        final AttributeValue attrVal = new AttributeValue();

        if (value == null) {
            attrVal.withNULL(Boolean.TRUE);
        } else {
            final Type<Object> type = N.typeOf(value.getClass());

            if (type.isNumber()) {
                attrVal.setN(type.stringOf(value));
            } else if (type.isBoolean()) {
                attrVal.setBOOL((Boolean) value);
            } else if (type.isByteBuffer()) {
                attrVal.setB((ByteBuffer) value);
            } else {
                attrVal.setS(type.stringOf(value));
            }
        }

        return attrVal;
    }

    /**
     * Returns the <code>AttributeValueUpdate</code> with default <code>AttributeAction.PUT</code>
     * 
     * @param value
     * @return
     */
    public static AttributeValueUpdate attrValueUpdateOf(Object value) {
        return attrValueUpdateOf(value, AttributeAction.PUT);
    }

    public static AttributeValueUpdate attrValueUpdateOf(Object value, AttributeAction action) {
        return new AttributeValueUpdate(attrValueOf(value), action);
    }

    public static Map<String, AttributeValue> asKey(final String keyName, final Object value) {
        return asItem(keyName, value);
    }

    public static Map<String, AttributeValue> asKey(final String keyName, final Object value, final String keyName2, final Object value2) {
        return asItem(keyName, value, keyName2, value2);
    }

    public static Map<String, AttributeValue> asKey(final String keyName, final Object value, final String keyName2, final Object value2, final String keyName3,
            Object value3) {
        return asItem(keyName, value, keyName2, value2, keyName3, value3);
    }

    // May misused with toItem
    @SafeVarargs
    public static Map<String, AttributeValue> asKey(Object... a) {
        return asItem(a);
    }

    public static Map<String, AttributeValue> asItem(final String keyName, final Object value) {
        return N.asLinkedHashMap(keyName, attrValueOf(value));
    }

    public static Map<String, AttributeValue> asItem(final String keyName, final Object value, final String keyName2, final Object value2) {
        return N.asLinkedHashMap(keyName, attrValueOf(value), keyName2, attrValueOf(value2));
    }

    public static Map<String, AttributeValue> asItem(final String keyName, final Object value, final String keyName2, final Object value2,
            final String keyName3, Object value3) {
        return N.asLinkedHashMap(keyName, attrValueOf(value), keyName2, attrValueOf(value2), keyName3, attrValueOf(value3));
    }

    // May misused with toItem
    @SafeVarargs
    public static Map<String, AttributeValue> asItem(Object... a) {
        if (0 != (a.length % 2)) {
            throw new IllegalArgumentException(
                    "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
        }

        final Map<String, AttributeValue> item = new LinkedHashMap<>(N.initHashCapacity(a.length / 2));

        for (int i = 0; i < a.length; i++) {
            item.put((String) a[i], attrValueOf(a[++i]));
        }

        return item;
    }

    public static Map<String, AttributeValueUpdate> asUpdateItem(final String keyName, final Object value) {
        return N.asLinkedHashMap(keyName, attrValueUpdateOf(value));
    }

    public static Map<String, AttributeValueUpdate> asUpdateItem(final String keyName, final Object value, final String keyName2, final Object value2) {
        return N.asLinkedHashMap(keyName, attrValueUpdateOf(value), keyName2, attrValueUpdateOf(value2));
    }

    public static Map<String, AttributeValueUpdate> asUpdateItem(final String keyName, final Object value, final String keyName2, final Object value2,
            final String keyName3, final Object value3) {
        return N.asLinkedHashMap(keyName, attrValueUpdateOf(value), keyName2, attrValueUpdateOf(value2), keyName3, attrValueUpdateOf(value3));
    }

    // May misused with toUpdateItem
    @SafeVarargs
    public static Map<String, AttributeValueUpdate> asUpdateItem(Object... a) {
        if (0 != (a.length % 2)) {
            throw new IllegalArgumentException(
                    "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
        }

        final Map<String, AttributeValueUpdate> item = new LinkedHashMap<>(N.initHashCapacity(a.length / 2));

        for (int i = 0; i < a.length; i++) {
            item.put((String) a[i], attrValueUpdateOf(a[++i]));
        }

        return item;
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param queryResult
     * @return
     */
    public static DataSet extractData(final QueryResult queryResult) {
        return extractData(queryResult, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param queryResult
     * @param offset
     * @param count
     * @return
     */
    public static DataSet extractData(final QueryResult queryResult, int offset, int count) {
        return extractData(queryResult.getItems(), offset, count);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param scanResult
     * @return
     */
    public static DataSet extractData(final ScanResult scanResult) {
        return extractData(scanResult, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param scanResult
     * @param offset
     * @param count
     * @return
     */
    public static DataSet extractData(final ScanResult scanResult, int offset, int count) {
        return extractData(scanResult.getItems(), offset, count);
    }

    static DataSet extractData(final List<Map<String, AttributeValue>> items, int offset, int count) {
        if (offset < 0 || count < 0) {
            throw new IllegalArgumentException("Offset and count can't be negative");
        }

        final List<Map<String, Object>> rowList = new ArrayList<>(items == null ? 0 : items.size());

        if (N.notNullOrEmpty(items)) {
            for (int i = offset, to = items.size(); i < to && count > 0; i++, count--) {
                final Map<String, AttributeValue> item = items.get(i);
                final Map<String, Object> row = new HashMap<>(N.initHashCapacity(item.size()));

                for (Map.Entry<String, AttributeValue> entry : item.entrySet()) {
                    row.put(entry.getKey(), toValue(entry.getValue()));
                }

                rowList.add(row);
            }
        }

        return N.newDataSet(rowList);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param queryResult
     * @return
     */
    public static <T> List<T> toList(final Class<T> targetClass, final QueryResult queryResult) {
        return toList(targetClass, queryResult, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param queryResult
     * @param offset
     * @param count
     * @return
     */
    public static <T> List<T> toList(final Class<T> targetClass, final QueryResult queryResult, int offset, int count) {
        return toList(targetClass, queryResult.getItems(), offset, count);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param scanResult
     * @return
     */
    public static <T> List<T> toList(final Class<T> targetClass, final ScanResult scanResult) {
        return toList(targetClass, scanResult, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param scanResult
     * @param offset
     * @param count
     * @return
     */
    public static <T> List<T> toList(final Class<T> targetClass, final ScanResult scanResult, int offset, int count) {
        return toList(targetClass, scanResult.getItems(), offset, count);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param items
     * @return
     */
    static <T> List<T> toList(final Class<T> targetClass, final List<Map<String, AttributeValue>> items) {
        return toList(targetClass, items, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param items
     * @param offset
     * @param count
     * @return
     */
    static <T> List<T> toList(final Class<T> targetClass, final List<Map<String, AttributeValue>> items, int offset, int count) {
        if (offset < 0 || count < 0) {
            throw new IllegalArgumentException("Offset and count can't be negative");
        }

        final Type<T> type = N.typeOf(targetClass);
        final List<T> resultList = new ArrayList<>();

        if (N.notNullOrEmpty(items)) {
            for (int i = offset, to = items.size(); i < to && count > 0; i++, count--) {
                resultList.add(toValue(type, targetClass, items.get(i)));
            }
        }

        return resultList;
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param tableItems
     * @return
     */
    static <T> Map<String, List<T>> toEntities(final Class<T> targetClass, final Map<String, List<Map<String, AttributeValue>>> tableItems) {
        final Map<String, List<T>> tableEntities = new LinkedHashMap<>();

        if (N.notNullOrEmpty(tableItems)) {
            for (Map.Entry<String, List<Map<String, AttributeValue>>> entry : tableItems.entrySet()) {
                tableEntities.put(entry.getKey(), toList(targetClass, entry.getValue()));
            }
        }

        return tableEntities;
    }

    /**
     * 
     * @param targetClass entity classes with getter/setter methods or basic single value type(Primitive/String/Date...)
     * @param item
     * @return
     */
    public static <T> T toEntity(final Class<T> targetClass, final Map<String, AttributeValue> item) {
        final Type<T> type = N.typeOf(targetClass);

        return toValue(type, targetClass, item);
    }

    private static <T> T toValue(final Type<T> type, final Class<T> targetClass, final Map<String, AttributeValue> item) {
        if (item == null) {
            return null;
        }

        if (type.isMap()) {
            final Map<String, Object> map = (Map<String, Object>) N.newInstance(targetClass);

            for (Map.Entry<String, AttributeValue> entry : item.entrySet()) {
                map.put(entry.getKey(), toValue(entry.getValue()));
            }

            return (T) map;
        } else if (type.isEntity()) {
            if (N.isNullOrEmpty(item)) {
                return null;
            }

            final EntityInfo entityInfo = ParserUtil.getEntityInfo(targetClass);
            final T entity = N.newInstance(targetClass);

            Method propSetMethod = null;
            PropInfo propInfo = null;

            for (Map.Entry<String, AttributeValue> entry : item.entrySet()) {
                propSetMethod = ClassUtil.getPropSetMethod(targetClass, entry.getKey());

                if (propSetMethod == null) {
                    continue;
                }

                propInfo = entityInfo.getPropInfo(entry.getKey());

                ClassUtil.setPropValue(entity, propSetMethod, propInfo.type.valueOf(attrValueType.stringOf(entry.getValue())));
            }

            if (N.isDirtyMarker(entity.getClass())) {
                ((DirtyMarker) entity).markDirty(false);
            }

            return entity;
        } else {
            if (N.isNullOrEmpty(item)) {
                return type.defaultValue();
            }

            return type.valueOf(attrValueType.stringOf(item.values().iterator().next()));
        }
    }

    static <T> T toValue(AttributeValue x) {
        if (x == null || (x.getNULL() != null && x.isNULL())) {
            return null;
        }

        if (N.notNullOrEmpty(x.getS())) {
            return (T) x.getS();
        } else if (N.notNullOrEmpty(x.getN())) {
            return (T) x.getN();
        } else if (x.getBOOL() != null) {
            return (T) x.getBOOL();
        } else if (x.getB() != null) {
            return (T) x.getB();
        } else if (N.notNullOrEmpty(x.getSS())) {
            return (T) x.getSS();
        } else if (N.notNullOrEmpty(x.getNS())) {
            return (T) x.getNS();
        } else if (N.notNullOrEmpty(x.getBS())) {
            return (T) x.getBS();
        } else if (N.notNullOrEmpty(x.getL())) {
            final List<AttributeValue> attrVals = x.getL();
            final List<Object> tmp = new ArrayList<>(attrVals.size());

            for (AttributeValue attrVal : attrVals) {
                tmp.add(toValue(attrVal));
            }

            return (T) tmp;
        } else if (N.notNullOrEmpty(x.getM())) {
            final Map<String, AttributeValue> attrMap = x.getM();
            final Map<String, Object> tmp = attrMap instanceof HashMap ? new HashMap<>(N.initHashCapacity(attrMap.size()))
                    : new LinkedHashMap<>(N.initHashCapacity(attrMap.size()));

            for (Map.Entry<String, AttributeValue> entry : attrMap.entrySet()) {
                tmp.put(entry.getKey(), toValue(entry.getValue()));
            }

            return (T) tmp;
        } else if (x.getS() != null) {
            return (T) x.getS();
        } else if (x.getN() != null) {
            return (T) x.getN();
        } else if (x.getSS() != null) {
            return (T) x.getSS();
        } else if (x.getNS() != null) {
            return (T) x.getNS();
        } else if (x.getBS() != null) {
            return (T) x.getBS();
        } else if (x.getL() != null) {
            return (T) x.getL();
        } else if (x.getM() != null) {
            return (T) x.getM();
        } else if (x.getNULL() != null) {
            return (T) x.getNULL();
        } else {
            throw new AbacusException("Unsupported Attribute type: " + x.toString());
        }
    }

    static <T> void checkEntityClass(final Class<T> targetClass) {
        if (!N.isEntity(targetClass)) {
            throw new IllegalArgumentException("Unsupported type: " + ClassUtil.getCanonicalClassName(targetClass)
                    + ". Only Entity class generated by CodeGenerator with getter/setter methods are supported");
        }
    }

    public static Map<String, AttributeValue> toItem(final Object entity) {
        return toItem(entity, NamingPolicy.CAMEL_CASE);
    }

    public static Map<String, AttributeValue> toItem(final Object entity, NamingPolicy namingPolicy) {
        final Map<String, AttributeValue> attrs = new LinkedHashMap<>();
        final Class<?> cls = entity.getClass();

        if (N.isEntity(cls)) {
            if (N.isDirtyMarker(cls)) {
                @SuppressWarnings("deprecation")
                Set<String> signedPropNames = ((DirtyMarker) entity).signedPropNames();
                Method propGetMethod = null;
                Object propValue = null;

                switch (namingPolicy) {
                    case CAMEL_CASE: {
                        for (String propName : signedPropNames) {
                            propGetMethod = ClassUtil.getPropGetMethod(cls, propName);
                            propName = ClassUtil.getPropNameByMethod(propGetMethod);
                            propValue = ClassUtil.getPropValue(entity, propGetMethod);

                            attrs.put(propName, attrValueOf(propValue));
                        }

                        break;
                    }

                    case LOWER_CASE_WITH_UNDERSCORE: {
                        for (String propName : signedPropNames) {
                            propGetMethod = ClassUtil.getPropGetMethod(cls, propName);
                            propName = ClassUtil.getPropNameByMethod(propGetMethod);
                            propValue = ClassUtil.getPropValue(entity, propGetMethod);

                            attrs.put(ClassUtil.toLowerCaseWithUnderscore(propName), attrValueOf(propValue));
                        }

                        break;
                    }

                    case UPPER_CASE_WITH_UNDERSCORE: {
                        for (String propName : signedPropNames) {
                            propGetMethod = ClassUtil.getPropGetMethod(cls, propName);
                            propName = ClassUtil.getPropNameByMethod(propGetMethod);
                            propValue = ClassUtil.getPropValue(entity, propGetMethod);

                            attrs.put(ClassUtil.toUpperCaseWithUnderscore(propName), attrValueOf(propValue));
                        }

                        break;
                    }

                    default:
                        throw new IllegalArgumentException("Unsupported naming policy: " + namingPolicy);
                }

            } else {
                final Map<String, Method> getMethodMap = ClassUtil.getPropGetMethodList(cls);
                Object propValue = null;

                switch (namingPolicy) {
                    case CAMEL_CASE: {
                        for (Map.Entry<String, Method> entry : getMethodMap.entrySet()) {
                            propValue = ClassUtil.getPropValue(entity, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            attrs.put(entry.getKey(), attrValueOf(propValue));
                        }

                        break;
                    }

                    case LOWER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getMethodMap.entrySet()) {
                            propValue = ClassUtil.getPropValue(entity, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            attrs.put(ClassUtil.toLowerCaseWithUnderscore(entry.getKey()), attrValueOf(propValue));
                        }

                        break;
                    }

                    case UPPER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getMethodMap.entrySet()) {
                            propValue = ClassUtil.getPropValue(entity, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            attrs.put(ClassUtil.toUpperCaseWithUnderscore(entry.getKey()), attrValueOf(propValue));
                        }

                        break;
                    }

                    default:
                        throw new IllegalArgumentException("Unsupported naming policy: " + namingPolicy);
                }
            }

        } else if (Map.class.isAssignableFrom(cls)) {
            final Map<String, Object> map = (Map<String, Object>) entity;

            switch (namingPolicy) {
                case CAMEL_CASE: {
                    for (Map.Entry<String, Object> entry : map.entrySet()) {
                        attrs.put(entry.getKey(), attrValueOf(entry.getValue()));
                    }

                    break;
                }

                case LOWER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Object> entry : map.entrySet()) {
                        attrs.put(ClassUtil.toLowerCaseWithUnderscore(entry.getKey()), attrValueOf(entry.getValue()));
                    }

                    break;
                }

                case UPPER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Object> entry : map.entrySet()) {
                        attrs.put(ClassUtil.toUpperCaseWithUnderscore(entry.getKey()), attrValueOf(entry.getValue()));
                    }

                    break;
                }

                default:
                    throw new IllegalArgumentException("Unsupported naming policy: " + namingPolicy);
            }
        } else if (entity instanceof Object[]) {
            return toItem(N.asProps(entity), namingPolicy);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + ClassUtil.getCanonicalClassName(cls)
                    + ". Only Entity and Map<String, Object> class generated by CodeGenerator with getter/setter methods are supported");
        }

        return attrs;
    }

    static List<Map<String, AttributeValue>> toItem(final Collection<?> entities) {
        return toItem(entities, NamingPolicy.CAMEL_CASE);
    }

    static List<Map<String, AttributeValue>> toItem(final Collection<?> entities, NamingPolicy namingPolicy) {
        final List<Map<String, AttributeValue>> attrsList = new ArrayList<>(entities.size());

        for (Object entity : entities) {
            attrsList.add(toItem(entity, namingPolicy));
        }

        return attrsList;
    }

    /**
     * Only the dirty properties will be set to the result Map if the specified entity is a dirty marker entity.
     * 
     * @param entity
     * @return
     */
    public static Map<String, AttributeValueUpdate> toUpdateItem(final Object entity) {
        return toUpdateItem(entity, NamingPolicy.CAMEL_CASE);
    }

    /**
     * Only the dirty properties will be set to the result Map if the specified entity is a dirty marker entity.
     * 
     * @param entity
     * @param namingPolicy
     * @return
     */
    public static Map<String, AttributeValueUpdate> toUpdateItem(final Object entity, NamingPolicy namingPolicy) {
        final Map<String, AttributeValueUpdate> attrs = new LinkedHashMap<>();
        final Class<?> cls = entity.getClass();

        if (N.isEntity(cls)) {
            if (N.isDirtyMarker(cls)) {
                @SuppressWarnings("deprecation")
                final Set<String> dirtyPropNames = ((DirtyMarker) entity).dirtyPropNames();
                Method propGetMethod = null;
                Object propValue = null;

                switch (namingPolicy) {
                    case CAMEL_CASE: {
                        for (String propName : dirtyPropNames) {
                            propGetMethod = ClassUtil.getPropGetMethod(cls, propName);
                            propName = ClassUtil.getPropNameByMethod(propGetMethod);
                            propValue = ClassUtil.getPropValue(entity, propGetMethod);

                            attrs.put(propName, attrValueUpdateOf(propValue));
                        }

                        break;
                    }

                    case LOWER_CASE_WITH_UNDERSCORE: {
                        for (String propName : dirtyPropNames) {
                            propGetMethod = ClassUtil.getPropGetMethod(cls, propName);
                            propName = ClassUtil.getPropNameByMethod(propGetMethod);
                            propValue = ClassUtil.getPropValue(entity, propGetMethod);

                            attrs.put(ClassUtil.toLowerCaseWithUnderscore(propName), attrValueUpdateOf(propValue));
                        }

                        break;
                    }

                    case UPPER_CASE_WITH_UNDERSCORE: {
                        for (String propName : dirtyPropNames) {
                            propGetMethod = ClassUtil.getPropGetMethod(cls, propName);
                            propName = ClassUtil.getPropNameByMethod(propGetMethod);
                            propValue = ClassUtil.getPropValue(entity, propGetMethod);

                            attrs.put(ClassUtil.toUpperCaseWithUnderscore(propName), attrValueUpdateOf(propValue));
                        }

                        break;
                    }

                    default:
                        throw new IllegalArgumentException("Unsupported naming policy: " + namingPolicy);
                }

            } else {
                final Map<String, Method> getMethodMap = ClassUtil.getPropGetMethodList(cls);
                Object propValue = null;

                switch (namingPolicy) {
                    case CAMEL_CASE: {
                        for (Map.Entry<String, Method> entry : getMethodMap.entrySet()) {
                            propValue = ClassUtil.getPropValue(entity, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            attrs.put(entry.getKey(), attrValueUpdateOf(propValue));
                        }

                        break;
                    }

                    case LOWER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getMethodMap.entrySet()) {
                            propValue = ClassUtil.getPropValue(entity, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            attrs.put(ClassUtil.toLowerCaseWithUnderscore(entry.getKey()), attrValueUpdateOf(propValue));
                        }

                        break;
                    }

                    case UPPER_CASE_WITH_UNDERSCORE: {
                        for (Map.Entry<String, Method> entry : getMethodMap.entrySet()) {
                            propValue = ClassUtil.getPropValue(entity, entry.getValue());

                            if (propValue == null) {
                                continue;
                            }

                            attrs.put(ClassUtil.toUpperCaseWithUnderscore(entry.getKey()), attrValueUpdateOf(propValue));
                        }

                        break;
                    }

                    default:
                        throw new IllegalArgumentException("Unsupported naming policy: " + namingPolicy);
                }
            }

        } else if (Map.class.isAssignableFrom(cls)) {
            final Map<String, Object> map = (Map<String, Object>) entity;

            switch (namingPolicy) {
                case CAMEL_CASE: {
                    for (Map.Entry<String, Object> entry : map.entrySet()) {
                        attrs.put(entry.getKey(), attrValueUpdateOf(entry.getValue()));
                    }

                    break;
                }

                case LOWER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Object> entry : map.entrySet()) {
                        attrs.put(ClassUtil.toLowerCaseWithUnderscore(entry.getKey()), attrValueUpdateOf(entry.getValue()));
                    }

                    break;
                }

                case UPPER_CASE_WITH_UNDERSCORE: {
                    for (Map.Entry<String, Object> entry : map.entrySet()) {
                        attrs.put(ClassUtil.toUpperCaseWithUnderscore(entry.getKey()), attrValueUpdateOf(entry.getValue()));
                    }

                    break;
                }

                default:
                    throw new IllegalArgumentException("Unsupported naming policy: " + namingPolicy);
            }
        } else if (entity instanceof Object[]) {
            return toUpdateItem(N.asProps(entity), namingPolicy);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + ClassUtil.getCanonicalClassName(cls)
                    + ". Only Entity and Map<String, Object> class generated by CodeGenerator with getter/setter methods are supported");
        }

        return attrs;
    }

    static List<Map<String, AttributeValueUpdate>> toUpdateItem(final Collection<?> entities) {
        return toUpdateItem(entities, NamingPolicy.CAMEL_CASE);
    }

    static List<Map<String, AttributeValueUpdate>> toUpdateItem(final Collection<?> entities, NamingPolicy namingPolicy) {
        final List<Map<String, AttributeValueUpdate>> attrsList = new ArrayList<>(entities.size());

        for (Object entity : entities) {
            attrsList.add(toUpdateItem(entity, namingPolicy));
        }

        return attrsList;
    }

    public Map<String, Object> getItem(final String tableName, final Map<String, AttributeValue> key) {
        return getItem(Map.class, tableName, key);
    }

    public Map<String, Object> getItem(final String tableName, final Map<String, AttributeValue> key, final Boolean consistentRead) {
        return getItem(Map.class, tableName, key, consistentRead);
    }

    public Map<String, Object> getItem(final GetItemRequest getItemRequest) {
        return getItem(Map.class, getItemRequest);
    }

    public <T> T getItem(final Class<T> targetClass, final String tableName, final Map<String, AttributeValue> key) {
        return toEntity(targetClass, dynamoDB.getItem(tableName, key).getItem());
    }

    public <T> T getItem(final Class<T> targetClass, final String tableName, final Map<String, AttributeValue> key, final Boolean consistentRead) {
        return toEntity(targetClass, dynamoDB.getItem(tableName, key, consistentRead).getItem());
    }

    public <T> T getItem(final Class<T> targetClass, final GetItemRequest getItemRequest) {
        return toEntity(targetClass, dynamoDB.getItem(getItemRequest).getItem());
    }

    @SuppressWarnings("rawtypes")
    public Map<String, List<Map<String, Object>>> batchGetItem(final Map<String, KeysAndAttributes> requestItems) {
        return (Map) batchGetItem(Map.class, requestItems);
    }

    @SuppressWarnings("rawtypes")
    public Map<String, List<Map<String, Object>>> batchGetItem(final Map<String, KeysAndAttributes> requestItems, final String returnConsumedCapacity) {
        return (Map) batchGetItem(Map.class, requestItems, returnConsumedCapacity);
    }

    @SuppressWarnings("rawtypes")
    public Map<String, List<Map<String, Object>>> batchGetItem(final BatchGetItemRequest batchGetItemRequest) {
        return (Map) batchGetItem(Map.class, batchGetItemRequest);
    }

    public <T> Map<String, List<T>> batchGetItem(final Class<T> targetClass, final Map<String, KeysAndAttributes> requestItems) {
        return toEntities(targetClass, dynamoDB.batchGetItem(requestItems).getResponses());
    }

    public <T> Map<String, List<T>> batchGetItem(final Class<T> targetClass, final Map<String, KeysAndAttributes> requestItems,
            final String returnConsumedCapacity) {
        return toEntities(targetClass, dynamoDB.batchGetItem(requestItems, returnConsumedCapacity).getResponses());
    }

    public <T> Map<String, List<T>> batchGetItem(final Class<T> targetClass, final BatchGetItemRequest batchGetItemRequest) {
        return toEntities(targetClass, dynamoDB.batchGetItem(batchGetItemRequest).getResponses());
    }

    public PutItemResult putItem(final String tableName, final Map<String, AttributeValue> item) {
        return dynamoDB.putItem(tableName, item);
    }

    public PutItemResult putItem(final String tableName, final Map<String, AttributeValue> item, final String returnValues) {
        return dynamoDB.putItem(tableName, item, returnValues);
    }

    public PutItemResult putItem(final PutItemRequest putItemRequest) {
        return dynamoDB.putItem(putItemRequest);
    }

    // There is no too much benefit to add method for "Object entity"
    // And it may cause error because the "Object" is ambiguous to any type. 
    PutItemResult putItem(final String tableName, final Object entity) {
        return putItem(tableName, toItem(entity));
    }

    PutItemResult putItem(final String tableName, final Object entity, final String returnValues) {
        return putItem(tableName, toItem(entity), returnValues);
    }

    public BatchWriteItemResult batchWriteItem(final Map<String, List<WriteRequest>> requestItems) {
        return dynamoDB.batchWriteItem(requestItems);
    }

    public BatchWriteItemResult batchWriteItem(final BatchWriteItemRequest batchWriteItemRequest) {
        return dynamoDB.batchWriteItem(batchWriteItemRequest);
    }

    public UpdateItemResult updateItem(final String tableName, final Map<String, AttributeValue> key,
            final Map<String, AttributeValueUpdate> attributeUpdates) {
        return dynamoDB.updateItem(tableName, key, attributeUpdates);
    }

    public UpdateItemResult updateItem(final String tableName, final Map<String, AttributeValue> key, final Map<String, AttributeValueUpdate> attributeUpdates,
            final String returnValues) {
        return dynamoDB.updateItem(tableName, key, attributeUpdates, returnValues);
    }

    public UpdateItemResult updateItem(final UpdateItemRequest updateItemRequest) {
        return dynamoDB.updateItem(updateItemRequest);
    }

    public DeleteItemResult deleteItem(final String tableName, final Map<String, AttributeValue> key) {
        return dynamoDB.deleteItem(tableName, key);
    }

    public DeleteItemResult deleteItem(final String tableName, final Map<String, AttributeValue> key, final String returnValues) {
        return dynamoDB.deleteItem(tableName, key, returnValues);
    }

    public DeleteItemResult deleteItem(final DeleteItemRequest deleteItemRequest) {
        return dynamoDB.deleteItem(deleteItemRequest);
    }

    @SuppressWarnings("rawtypes")
    public List<Map<String, Object>> find(final QueryRequest queryRequest) {
        return (List) find(Map.class, queryRequest);
    }

    /**
     * 
     * @param targetClass <code>Map</code> or entity class with getter/setter method.
     * @param queryRequest
     * @return
     */
    public <T> List<T> find(final Class<T> targetClass, final QueryRequest queryRequest) {
        return find(targetClass, queryRequest, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass <code>Map</code> or entity class with getter/setter method.
     * @param queryRequest
     * @param pageOffset
     * @param pageCount
     * @return
     * @see <a href="http://docs.aws.amazon.com/amazondynamodb/latest/developerguide/Query.html#Query.Pagination">Query.Pagination</a>
     */
    public <T> List<T> find(final Class<T> targetClass, final QueryRequest queryRequest, int pageOffset, int pageCount) {
        N.checkArgument(pageOffset >= 0 && pageCount >= 0, "'pageOffset' and 'pageCount' can't be negative");

        final List<T> res = new ArrayList<>();
        QueryRequest newQueryRequest = queryRequest;
        QueryResult queryResult = null;

        do {
            if (queryResult != null && N.notNullOrEmpty(queryResult.getLastEvaluatedKey())) {
                if (newQueryRequest == queryRequest) {
                    newQueryRequest = queryRequest.clone();
                }

                newQueryRequest.setExclusiveStartKey(queryResult.getLastEvaluatedKey());
            }

            queryResult = dynamoDB.query(newQueryRequest);
        } while (pageOffset-- > 0 && N.notNullOrEmpty(queryResult.getItems()) && N.notNullOrEmpty(queryResult.getLastEvaluatedKey()));

        if (pageOffset >= 0 || pageCount-- <= 0) {
            return res;
        } else {
            res.addAll(toList(targetClass, queryResult));
        }

        while (pageCount-- > 0 && N.notNullOrEmpty(queryResult.getLastEvaluatedKey())) {
            if (newQueryRequest == queryRequest) {
                newQueryRequest = queryRequest.clone();
            }

            newQueryRequest.setExclusiveStartKey(queryResult.getLastEvaluatedKey());
            queryResult = dynamoDB.query(newQueryRequest);
            res.addAll(toList(targetClass, queryResult));
        }

        return res;
    }

    /**
     * 
     * @param queryRequest
     * @return
     * @see #find(QueryRequest)
     */
    public DataSet query(final QueryRequest queryRequest) {
        return query(Map.class, queryRequest);
    }

    /**
     * 
     * @param targetClass
     * @param queryRequest
     * @return
     * @see #find(Class, QueryRequest)
     */
    public DataSet query(final Class<?> targetClass, final QueryRequest queryRequest) {
        return query(targetClass, queryRequest, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass
     * @param queryRequest
     * @param pageOffset
     * @param pageCount
     * @return
     * @see #find(Class, QueryRequest, int, int)
     */
    public DataSet query(final Class<?> targetClass, final QueryRequest queryRequest, int pageOffset, int pageCount) {
        return N.newDataSet(find(targetClass, queryRequest, pageOffset, pageCount));
    }

    public DataSet scan(final String tableName, final List<String> attributesToGet) {
        return scan(new ScanRequest().withTableName(tableName).withAttributesToGet(attributesToGet));
    }

    public DataSet scan(final String tableName, final Map<String, Condition> scanFilter) {
        return scan(new ScanRequest().withTableName(tableName).withScanFilter(scanFilter));
    }

    public DataSet scan(final String tableName, final List<String> attributesToGet, final Map<String, Condition> scanFilter) {
        return scan(new ScanRequest().withTableName(tableName).withAttributesToGet(attributesToGet).withScanFilter(scanFilter));
    }

    public DataSet scan(final ScanRequest scanRequest) {
        return scan(scanRequest, 0, Integer.MAX_VALUE);
    }

    public DataSet scan(final ScanRequest scanRequest, int pageOffset, int pageCount) {
        return N.newDataSet(scan(Map.class, scanRequest, pageOffset, pageCount));
    }

    public <T> List<T> scan(final Class<T> targetClass, final String tableName, final List<String> attributesToGet) {
        return scan(targetClass, new ScanRequest().withTableName(tableName).withAttributesToGet(attributesToGet));
    }

    public <T> List<T> scan(final Class<T> targetClass, final String tableName, final Map<String, Condition> scanFilter) {
        return scan(targetClass, new ScanRequest().withTableName(tableName).withScanFilter(scanFilter));
    }

    public <T> List<T> scan(final Class<T> targetClass, final String tableName, final List<String> attributesToGet, final Map<String, Condition> scanFilter) {
        return scan(targetClass, new ScanRequest().withTableName(tableName).withAttributesToGet(attributesToGet).withScanFilter(scanFilter));
    }

    public <T> List<T> scan(final Class<T> targetClass, final ScanRequest scanRequest) {
        return scan(targetClass, scanRequest, 0, Integer.MAX_VALUE);
    }

    public <T> List<T> scan(final Class<T> targetClass, final ScanRequest scanRequest, int pageOffset, int pageCount) {
        N.checkArgument(pageOffset >= 0 && pageCount >= 0, "'pageOffset' and 'pageCount' can't be negative");

        final List<T> res = new ArrayList<>();
        ScanRequest newQueryRequest = scanRequest;
        ScanResult queryResult = null;

        do {
            if (queryResult != null && N.notNullOrEmpty(queryResult.getLastEvaluatedKey())) {
                if (newQueryRequest == scanRequest) {
                    newQueryRequest = scanRequest.clone();
                }

                newQueryRequest.setExclusiveStartKey(queryResult.getLastEvaluatedKey());
            }

            queryResult = dynamoDB.scan(newQueryRequest);
        } while (pageOffset-- > 0 && N.notNullOrEmpty(queryResult.getItems()) && N.notNullOrEmpty(queryResult.getLastEvaluatedKey()));

        if (pageOffset >= 0 || pageCount-- <= 0) {
            return res;
        } else {
            res.addAll(toList(targetClass, queryResult));
        }

        while (pageCount-- > 0 && N.notNullOrEmpty(queryResult.getLastEvaluatedKey())) {
            if (newQueryRequest == scanRequest) {
                newQueryRequest = scanRequest.clone();
            }

            newQueryRequest.setExclusiveStartKey(queryResult.getLastEvaluatedKey());
            queryResult = dynamoDB.scan(newQueryRequest);
            res.addAll(toList(targetClass, queryResult));
        }

        return res;
    }

    @Override
    public void close() throws IOException {
        dynamoDB.shutdown();
    }

    public static final class Filters {
        public static Map<String, Condition> eq(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.EQ).withAttributeValueList(attrValueOf(attrValue)));
        }

        public static Map<String, Condition> ne(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.NE).withAttributeValueList(attrValueOf(attrValue)));
        }

        public static Map<String, Condition> gt(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.GT).withAttributeValueList(attrValueOf(attrValue)));
        }

        public static Map<String, Condition> ge(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.GE).withAttributeValueList(attrValueOf(attrValue)));
        }

        public static Map<String, Condition> lt(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.LT).withAttributeValueList(attrValueOf(attrValue)));
        }

        public static Map<String, Condition> le(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.LE).withAttributeValueList(attrValueOf(attrValue)));
        }

        /**
         * Between
         * 
         * @param attrName
         * @param minAttrValue
         * @param maxAttrValue
         * @return
         */
        public static Map<String, Condition> bt(String attrName, Object minAttrValue, Object maxAttrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.BETWEEN).withAttributeValueList(attrValueOf(minAttrValue),
                    attrValueOf(maxAttrValue)));
        }

        public static Map<String, Condition> isNull(String attrName) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.NULL));
        }

        public static Map<String, Condition> notNull(String attrName) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.NOT_NULL));
        }

        public static Map<String, Condition> contains(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.CONTAINS).withAttributeValueList(attrValueOf(attrValue)));
        }

        public static Map<String, Condition> notContains(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.NOT_CONTAINS).withAttributeValueList(attrValueOf(attrValue)));
        }

        public static Map<String, Condition> beginsWith(String attrName, Object attrValue) {
            return N.asMap(attrName, new Condition().withComparisonOperator(ComparisonOperator.BEGINS_WITH).withAttributeValueList(attrValueOf(attrValue)));
        }

        @SafeVarargs
        public static Map<String, Condition> in(String attrName, Object... attrValues) {
            final AttributeValue[] attributeValueList = new AttributeValue[attrValues.length];

            for (int i = 0, len = attrValues.length; i < len; i++) {
                attributeValueList[i] = attrValueOf(attrValues[i]);
            }

            final Condition cond = new Condition().withComparisonOperator(ComparisonOperator.IN).withAttributeValueList(attributeValueList);

            return N.asMap(attrName, cond);
        }

        public static Map<String, Condition> in(String attrName, Collection<?> attrValues) {
            final AttributeValue[] attributeValueList = new AttributeValue[attrValues.size()];

            int i = 0;
            for (Object attrValue : attrValues) {
                attributeValueList[i++] = attrValueOf(attrValue);
            }

            final Condition cond = new Condition().withComparisonOperator(ComparisonOperator.IN).withAttributeValueList(attributeValueList);

            return N.asMap(attrName, cond);
        }
    }
}