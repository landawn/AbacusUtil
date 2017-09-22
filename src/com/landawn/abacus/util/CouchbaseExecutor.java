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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import com.couchbase.client.java.Bucket;
import com.couchbase.client.java.Cluster;
import com.couchbase.client.java.document.Document;
import com.couchbase.client.java.document.JsonDocument;
import com.couchbase.client.java.document.json.JsonArray;
import com.couchbase.client.java.document.json.JsonObject;
import com.couchbase.client.java.query.Query;
import com.couchbase.client.java.query.QueryResult;
import com.couchbase.client.java.query.QueryRow;
import com.landawn.abacus.DataSet;
import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.core.RowDataSet;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.pool.KeyedObjectPool;
import com.landawn.abacus.pool.PoolFactory;
import com.landawn.abacus.pool.PoolableWrapper;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.stream.Stream;

/**
 * It's a simple wrapper of Couchbase Java client.
 * Raw/ibatis(myBatis)/Couchbase style parameterized sql are supported. The parameters can be array/list/map/entity/JsonArray/JsonObject:
 * <li> row parameterized sql with question mark: <code>SELECT * FROM account WHERE accountId = ?</li>.
 * <li> ibatis/myBatis parameterized sql with named parameter: <code>SELECT * FROM account WHERE accountId = #{accountId}</li>.
 * <li> Couchbase parameterized sql with named parameter: <code>SELECT * FROM account WHERE accountId = $accountId</code> or <code>SELECT * FROM account WHERE accountId = $1</li>.
 *
 * <br>
 * <br>We recommend to define "id" property in java entity/bean as the object id in Couchbase to keep things as simple as possible.</br>
 * <br>
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class CouchbaseExecutor implements Closeable {
    /**
     * It's name of object id set in Map/Object array.
     */
    public static final String _ID = "_id";

    /**
     * Property name of id.
     */
    public static final String ID = "id";

    static final int POOLABLE_LENGTH = 1024;

    static final Set<Class<?>> supportedTypes = new HashSet<>();

    static {
        supportedTypes.add(Boolean.class);
        supportedTypes.add(Integer.class);
        supportedTypes.add(Long.class);
        supportedTypes.add(Double.class);
        supportedTypes.add(String.class);
        supportedTypes.add(JsonObject.class);
        supportedTypes.add(JsonArray.class);
    }

    private static final Map<Class<?>, Method> classIdSetMethodPool = new ConcurrentHashMap<>();

    private static final Map<String, String> bucketIdNamePool = new ConcurrentHashMap<>();

    private final KeyedObjectPool<String, PoolableWrapper<Query>> stmtPool = PoolFactory.createKeyedObjectPool(1024, 3000);
    // private final KeyedObjectPool<String, Wrapper<QueryPlan>> preStmtPool = PoolFactory.createKeyedObjectPool(1024, 3000);

    private final Cluster cluster;
    private final Bucket bucket;

    private final SQLMapper sqlMapper;
    private final AsyncExecutor asyncExecutor;

    public CouchbaseExecutor(Cluster cluster) {
        this(cluster, cluster.openBucket());
    }

    public CouchbaseExecutor(Cluster cluster, Bucket bucket) {
        this(cluster, bucket, null);
    }

    public CouchbaseExecutor(Cluster cluster, Bucket bucket, final SQLMapper sqlMapper) {
        this(cluster, bucket, sqlMapper, new AsyncExecutor(64, 300, TimeUnit.SECONDS));
    }

    public CouchbaseExecutor(Cluster cluster, Bucket bucket, final SQLMapper sqlMapper, final AsyncExecutor asyncExecutor) {
        this.cluster = cluster;
        this.bucket = bucket;
        this.sqlMapper = sqlMapper;
        this.asyncExecutor = asyncExecutor;
    }

    public Cluster cluster() {
        return cluster;
    }

    public Bucket bucket() {
        return bucket;
    }

    /**
     * The object id ("_id") property will be read from/write to the specified property 
     * @param cls
     * @param idPropertyName
     */
    public static void registerIdProeprty(Class<?> cls, String idPropertyName) {
        if (ClassUtil.getPropGetMethod(cls, idPropertyName) == null || ClassUtil.getPropSetMethod(cls, idPropertyName) == null) {
            throw new IllegalArgumentException("The specified class: " + ClassUtil.getCanonicalClassName(cls)
                    + " doesn't have getter or setter method for the specified id propery: " + idPropertyName);
        }

        final Method setMethod = ClassUtil.getPropSetMethod(cls, idPropertyName);
        final Class<?> parameterType = setMethod.getParameterTypes()[0];

        if (!(String.class.isAssignableFrom(parameterType) || long.class.isAssignableFrom(parameterType) || Long.class.isAssignableFrom(parameterType))) {
            throw new IllegalArgumentException(
                    "The parameter type of the specified id setter method must be 'String' or long/Long: " + setMethod.toGenericString());
        }

        classIdSetMethodPool.put(cls, setMethod);
    }

    public static DataSet extractData(final QueryResult resultSet) {
        return extractData(Map.class, resultSet);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method or <code>Map.class</code>
     * @param resultSet
     * @return
     */
    public static DataSet extractData(final Class<?> targetClass, final QueryResult resultSet) {
        checkResultError(resultSet);
        checkTargetClass(targetClass);

        final List<QueryRow> allRows = resultSet.allRows();

        if (N.isNullOrEmpty(allRows)) {
            final List<String> columnNameList = new ArrayList<>();
            final List<List<Object>> columnList = new ArrayList<>();
            return new RowDataSet(columnNameList, columnList);
        }

        final List<String> columnNameList = new ArrayList<>(allRows.get(0).value().getNames());
        final List<Object> rowList = new ArrayList<>(allRows.size());

        for (QueryRow row : allRows) {
            rowList.add(toEntity(targetClass, row.value()));
        }

        return N.newDataSet(columnNameList, rowList);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param resultSet
     * @return
     */
    public static <T> List<T> toList(Class<T> targetClass, QueryResult resultSet) {
        checkResultError(resultSet);

        final Type<T> type = N.typeOf(targetClass);
        final List<QueryRow> rowList = resultSet.allRows();

        if (N.isNullOrEmpty(rowList)) {
            return new ArrayList<>();
        }

        final List<Object> resultList = new ArrayList<>(rowList.size());

        if (targetClass.isAssignableFrom(JsonObject.class)) {
            for (QueryRow row : rowList) {
                resultList.add(row.value());
            }
        } else if (type.isEntity() || type.isMap()) {
            for (QueryRow row : rowList) {
                resultList.add(toEntity(targetClass, row));
            }
        } else {
            final JsonObject first = rowList.get(0).value();

            if (first.getNames().size() == 1) {
                final String propName = first.getNames().iterator().next();

                if (first.get(propName) != null && targetClass.isAssignableFrom(first.get(propName).getClass())) {
                    for (QueryRow row : rowList) {
                        resultList.add(row.value().get(propName));
                    }
                } else {
                    for (QueryRow row : rowList) {
                        resultList.add(N.as(targetClass, row.value().get(propName)));
                    }
                }
            } else {
                throw new IllegalArgumentException(
                        "Can't covert result with columns: " + first.getNames().toString() + " to class: " + ClassUtil.getCanonicalClassName(targetClass));
            }
        }

        return (List<T>) resultList;
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method or <code>Map.class</code>
     * @param row
     * @return
     */
    public static <T> T toEntity(final Class<T> targetClass, final QueryRow row) {
        return toEntity(targetClass, row.value());
    }

    /**
     * The id in the specified <code>jsonDocument</code> will be set to the returned object if and only if the id is not null or empty and the content in <code>jsonDocument</code> doesn't contain any "id" property, and it's acceptable to the <code>targetClass</code>.
     * 
     * @param targetClass an entity class with getter/setter method or <code>Map.class</code>
     * @param jsonDocument
     * @return
     */
    public static <T> T toEntity(final Class<T> targetClass, final JsonDocument jsonDocument) {
        final T result = toEntity(targetClass, jsonDocument.content());
        final String id = jsonDocument.id();

        if (N.notNullOrEmpty(id) && result != null) {
            if (Map.class.isAssignableFrom(targetClass)) {
                ((Map<String, Object>) result).put(_ID, id);
            } else {
                final Method idSetMethod = getObjectIdSetMethod(targetClass);

                if (idSetMethod != null) {
                    ClassUtil.setPropValue(result, idSetMethod, id);
                }
            }
        }

        return result;
    }

    private static <T> Method getObjectIdSetMethod(Class<T> targetClass) {
        Method idSetMethod = classIdSetMethodPool.get(targetClass);

        if (idSetMethod == null) {
            //            Method idPropSetMethod = N.getPropSetMethod(targetClass, ID);
            //            Class<?> parameterType = idPropSetMethod == null ? null : idPropSetMethod.getParameterTypes()[0];
            //
            //            if (parameterType != null && String.class.isAssignableFrom(parameterType)) {
            //                idSetMethod = idPropSetMethod;
            //            }
            //
            //            if (idSetMethod == null) {
            //                idSetMethod = N.METHOD_MASK;
            //            }
            //
            //            classIdSetMethodPool.put(targetClass, idSetMethod);

            classIdSetMethodPool.put(targetClass, ClassUtil.METHOD_MASK);
        }

        return idSetMethod == ClassUtil.METHOD_MASK ? null : idSetMethod;
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method or <code>Map.class</code>
     * @param jsonObject
     * @return
     */
    public static <T> T toEntity(final Class<T> targetClass, final JsonObject jsonObject) {
        checkTargetClass(targetClass);

        if (jsonObject == null) {
            return null;
        }

        if (Map.class.isAssignableFrom(targetClass)) {
            final Map<String, Object> m = jsonObject.toMap();

            if (targetClass.isAssignableFrom(m.getClass())) {
                return (T) m;
            } else {
                final Map<String, Object> result = (Map<String, Object>) N.newInstance(targetClass);
                result.putAll(m);
                return (T) result;
            }
        } else {
            final T entity = N.newInstance(targetClass);
            final List<String> columnNameList = new ArrayList<>(jsonObject.getNames());
            Method propSetMethod = null;
            Class<?> parameterType = null;
            Object propValue = null;

            for (String propName : columnNameList) {
                propSetMethod = ClassUtil.getPropSetMethod(targetClass, propName);

                if (propSetMethod == null) {
                    continue;
                }

                parameterType = propSetMethod.getParameterTypes()[0];
                propValue = jsonObject.get(propName);

                if (propValue != null && !parameterType.isAssignableFrom(propValue.getClass())) {
                    if (propValue instanceof JsonObject) {
                        if (parameterType.isAssignableFrom(Map.class) || N.isEntity(parameterType)) {
                            ClassUtil.setPropValue(entity, propSetMethod, toEntity(parameterType, (JsonObject) propValue));
                        } else {
                            ClassUtil.setPropValue(entity, propSetMethod, N.valueOf(parameterType, N.stringOf(toEntity(Map.class, (JsonObject) propValue))));
                        }
                    } else if (propValue instanceof JsonArray) {
                        if (parameterType.isAssignableFrom(List.class)) {
                            ClassUtil.setPropValue(entity, propSetMethod, ((JsonArray) propValue).toList());
                        } else {
                            ClassUtil.setPropValue(entity, propSetMethod, N.valueOf(parameterType, N.stringOf(((JsonArray) propValue).toList())));
                        }
                    } else {
                        ClassUtil.setPropValue(entity, propSetMethod, propValue);
                    }
                } else {
                    ClassUtil.setPropValue(entity, propSetMethod, propValue);
                }
            }

            if (N.isDirtyMarker(entity.getClass())) {
                ((DirtyMarker) entity).markDirty(false);
            }

            return entity;
        }
    }

    public static String toJSON(JsonArray jsonArray) {
        return N.toJSON(jsonArray.toList());
    }

    public static String toJSON(JsonObject jsonObject) {
        return N.toJSON(jsonObject.toMap());
    }

    public static String toJSON(JsonDocument jsonDocument) {
        final Map<String, Object> m = jsonDocument.content().toMap();

        if (N.notNullOrEmpty(jsonDocument.id())) {
            m.put(_ID, jsonDocument.id());
        }

        return N.toJSON(m);
    }

    /**
     * Returns an instance of the specified target class with the property values from the specified JSON String.
     * 
     * @param targetClass <code>JsonArray.class</code>, <code>JsonObject.class</code> or <code>JsonDocument.class</code>
     * @param json
     * @return
     */
    public static <T> T fromJSON(final Class<T> targetClass, final String json) {
        if (targetClass.equals(JsonObject.class)) {
            return (T) JsonObject.from(N.fromJSON(Map.class, json));
        } else if (targetClass.equals(JsonArray.class)) {
            return (T) JsonArray.from(N.fromJSON(List.class, json));
        } else if (targetClass.equals(JsonDocument.class)) {
            final JsonObject jsonObject = JsonObject.from(N.fromJSON(Map.class, json));
            final String id = N.stringOf(jsonObject.get(_ID));

            jsonObject.removeKey(_ID);

            return (T) JsonDocument.create(id, jsonObject);
        } else {
            throw new IllegalArgumentException("Unsupported type: " + ClassUtil.getCanonicalClassName(targetClass));
        }
    }

    /**
     *
     * @param obj an array of pairs of property name and value, or Map<String, Object>, or an entity with getter/setter methods.
     * @return
     */
    public static JsonObject toJsonObject(final Object obj) {
        Map<String, Object> m = null;

        if (obj instanceof Map) {
            m = (Map<String, Object>) obj;
        } else if (N.isEntity(obj.getClass())) {
            m = Maps.entity2Map(obj);
        } else if (obj instanceof Object[]) {
            m = N.asProps(obj);
        } else {
            throw new IllegalArgumentException("The parameters must be a Map, or an entity class with getter/setter methods");
        }

        final JsonObject result = JsonObject.create();

        for (Map.Entry<String, Object> entry : m.entrySet()) {
            if (entry.getValue() == null || supportedTypes.contains(entry.getValue().getClass())) {
                result.put(entry.getKey(), entry.getValue());
            } else {
                Type<Object> valueType = N.typeOf(entry.getValue().getClass());

                if (valueType.isMap() || valueType.isEntity()) {
                    result.put(entry.getKey(), toJsonObject(entry.getValue()));
                } else if (valueType.isObjectArray() || valueType.isCollection()) {
                    result.put(entry.getKey(), toJsonArray(entry.getValue()));
                } else {
                    result.put(entry.getKey(), N.stringOf(entry.getValue()));
                }
            }
        }

        return result;
    }

    @SafeVarargs
    public static JsonObject toJsonObject(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return JsonObject.empty();
        }

        return a.length == 1 ? toJsonObject(a[0]) : toJsonObject((Object) a);
    }

    public static JsonArray toJsonArray(final Object obj) {
        final Type<Object> type = N.typeOf(obj.getClass());
        final JsonArray jsonArray = JsonArray.create();

        if (type.isObjectArray()) {
            for (Object e : (Object[]) obj) {
                if (e == null || supportedTypes.contains(e.getClass())) {
                    jsonArray.add(e);
                } else {
                    Type<Object> eType = N.typeOf(e.getClass());

                    if (eType.isMap() || eType.isEntity()) {
                        jsonArray.add(toJsonObject(e));
                    } else if (eType.isObjectArray() || eType.isCollection()) {
                        jsonArray.add(toJsonArray(e));
                    } else {
                        jsonArray.add(N.stringOf(e));
                    }
                }
            }
        } else if (type.isCollection()) {
            for (Object e : (Collection<Object>) obj) {
                if (e == null || supportedTypes.contains(e.getClass())) {
                    jsonArray.add(e);
                } else {
                    Type<Object> eType = N.typeOf(e.getClass());

                    if (eType.isMap() || eType.isEntity()) {
                        jsonArray.add(toJsonObject(e));
                    } else if (eType.isObjectArray() || eType.isCollection()) {
                        jsonArray.add(toJsonArray(e));
                    } else {
                        jsonArray.add(N.stringOf(e));
                    }
                }
            }
        } else {
            jsonArray.add(N.stringOf(obj));
        }

        return jsonArray;
    }

    @SafeVarargs
    public static JsonArray toJsonArray(final Object... a) {
        return N.isNullOrEmpty(a) ? JsonArray.empty() : toJsonArray((Object) a);
    }

    /**
     * The id for the target document is got from the "id" property in the specified <code>obj</code>.
     * 
     * @param obj an array of pairs of property name and value, or Map<String, Object>, or an entity with getter/setter methods.
     * @return
     * @throws IllegalArgumentException if the specified <code>obj</code> doesn't have any "id" property.
     */
    public static JsonDocument toJsonDocument(final Object obj) {
        return toJsonDocument(obj, toJsonObject(obj));
    }

    /**
     * The id for the target document is got from the "id" property in the specified <code>a</code>.
     * 
     * @param a pairs of property name and value.
     * @return
     * @throws IllegalArgumentException if the specified <code>a</code> doesn't have any "id" property.
     */
    @SafeVarargs
    public static JsonDocument toJsonDocument(final Object... a) {
        return a.length == 1 ? toJsonDocument(a[0], toJsonObject(a[0])) : toJsonDocument(a, toJsonObject(a));
    }

    static JsonDocument toJsonDocument(final Object obj, final JsonObject jsonObject) {
        final Class<?> cls = obj.getClass();
        final Method idSetMethod = getObjectIdSetMethod(obj.getClass());
        final String idPropertyName = N.isEntity(cls) ? (idSetMethod == null ? null : ClassUtil.getPropNameByMethod(idSetMethod)) : _ID;

        String id = null;

        if (idPropertyName != null && jsonObject.containsKey(idPropertyName)) {
            id = N.stringOf(jsonObject.get(idPropertyName));

            jsonObject.removeKey(idPropertyName);
        }

        if (N.isNullOrEmpty(id)) {
            throw new IllegalArgumentException("No id property included the specified object: " + N.toString(jsonObject));
        }

        return JsonDocument.create(id, jsonObject);
    }

    public static String idNameOf(final String bucketName) {
        String idName = bucketIdNamePool.get(bucketName);

        if (idName == null) {
            idName = "meta(" + bucketName + ").id".intern();
            bucketIdNamePool.put(bucketName, idName);
        }

        return idName;
    }

    /**
     * 
     * @param id
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#get(String)
     */
    public JsonDocument get(final String id) {
        return bucket.get(id);
    }

    /**
     * 
     * @param id
     * @param timeout
     * @param timeUnit
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#get(String, long, TimeUnit)
     */
    public JsonDocument get(final String id, final long timeout, final TimeUnit timeUnit) {
        return bucket.get(id, timeout, timeUnit);
    }

    /**
     * 
     * @param targetClass
     * @param id
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#get(String, Class)
     */
    public <T> T get(final Class<T> targetClass, final String id) {
        return toEntityForGet(targetClass, get(id));
    }

    /**
     * 
     * @param targetClass
     * @param id
     * @param timeout
     * @param timeUnit
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#get(String, Class, long, TimeUnit)
     */
    public <T> T get(final Class<T> targetClass, final String id, final long timeout, final TimeUnit timeUnit) {
        return toEntityForGet(targetClass, get(id, timeout, timeUnit));
    }

    private <T> T toEntityForGet(final Class<T> targetClass, final JsonDocument doc) {
        if ((doc == null || doc.content() == null || doc.content().size() == 0)) {
            return null;
        }

        if (targetClass.isAssignableFrom(doc.getClass())) {
            return (T) doc;
        }

        return toEntity(targetClass, doc);
    }

    /**
     * Always remember to set "<code>LIMIT 1</code>" in the sql statement for better performance.
     *
     * @param query
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final boolean exists(final String query, final Object... parameters) {
        final QueryResult resultSet = execute(query, parameters);

        return resultSet.iterator().hasNext();
    }

    @SafeVarargs
    public final long count(final String query, final Object... parameters) {
        return queryForSingleResult(long.class, query, parameters).orElse(0L);
    }

    @SafeVarargs
    public final <T> NullabLe<T> queryForSingleResult(final Class<T> targetClass, final String query, final Object... parameters) {
        final QueryResult resultSet = execute(query, parameters);
        final Iterator<QueryRow> it = resultSet.rows();
        final JsonObject jsonObject = it.hasNext() ? it.next().value() : null;

        if (jsonObject == null || jsonObject.size() == 0) {
            return NullabLe.empty();
        } else {
            return NullabLe.of(N.as(targetClass, jsonObject.get(jsonObject.getNames().iterator().next())));
        }
    }

    @SafeVarargs
    public final <T> Optional<T> queryForEntity(final Class<T> targetClass, final String query, final Object... parameters) {
        final QueryResult resultSet = execute(query, parameters);
        final Iterator<QueryRow> it = resultSet.rows();
        final JsonObject jsonObject = it.hasNext() ? it.next().value() : null;

        if (jsonObject == null || jsonObject.size() == 0) {
            return Optional.empty();
        } else {
            return Optional.of(toEntity(targetClass, jsonObject));
        }
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param query
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final <T> List<T> find(final Class<T> targetClass, final String query, final Object... parameters) {
        final QueryResult resultSet = execute(query, parameters);

        return toList(targetClass, resultSet);
    }

    @SafeVarargs
    public final DataSet query(final String query, final Object... parameters) {
        return extractData(execute(query, parameters));
    }

    @SafeVarargs
    public final DataSet query(final Class<?> targetClass, final String query, final Object... parameters) {
        return extractData(targetClass, execute(query, parameters));
    }

    public DataSet query(final Query query) {
        return extractData(execute(query));
    }

    public DataSet query(final Class<?> targetClass, final Query query) {
        return extractData(targetClass, execute(query));
    }

    public DataSet query(final Query query, final long timeout, final TimeUnit timeUnit) {
        return extractData(execute(query, timeout, timeUnit));
    }

    public DataSet query(final Class<?> targetClass, final Query query, final long timeout, final TimeUnit timeUnit) {
        return extractData(targetClass, execute(query, timeout, timeUnit));
    }

    @SafeVarargs
    public final Stream<JsonObject> stream(final String query, final Object... parameters) {
        return Stream.of(execute(query, parameters).rows()).map(new Function<QueryRow, JsonObject>() {
            @Override
            public JsonObject apply(QueryRow t) {
                return t.value();
            }
        });
    }

    @SafeVarargs
    public final <T> Stream<T> stream(final Class<T> targetClass, final String query, final Object... parameters) {
        return Stream.of(execute(query, parameters).rows()).map(new Function<QueryRow, T>() {
            @Override
            public T apply(QueryRow t) {
                return toEntity(targetClass, t.value());
            }
        });
    }

    public Stream<JsonObject> stream(final Query query) {
        return Stream.of(execute(query).rows()).map(new Function<QueryRow, JsonObject>() {
            @Override
            public JsonObject apply(QueryRow t) {
                return t.value();
            }
        });
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Query query) {
        return Stream.of(execute(query).rows()).map(new Function<QueryRow, T>() {
            @Override
            public T apply(QueryRow t) {
                return toEntity(targetClass, t.value());
            }
        });
    }

    public Stream<JsonObject> stream(final Query query, final long timeout, final TimeUnit timeUnit) {
        return Stream.of(execute(query, timeout, timeUnit).rows()).map(new Function<QueryRow, JsonObject>() {
            @Override
            public JsonObject apply(QueryRow t) {
                return t.value();
            }
        });
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Query query, final long timeout, final TimeUnit timeUnit) {
        return Stream.of(execute(query, timeout, timeUnit).rows()).map(new Function<QueryRow, T>() {
            @Override
            public T apply(QueryRow t) {
                return toEntity(targetClass, t.value());
            }
        });
    }

    /**
     * 
     * @param document
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#insert(Document)
     */
    public <T> T insert(final T document) {
        return (T) toEntityForUpdate(document.getClass(), bucket.insert(toDocument(document)));
    }

    /**
     * 
     * @param document
     * @param timeout
     * @param timeUnit
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#insert(Document, long, TimeUnit)
     */
    public <T> T insert(final T document, final long timeout, final TimeUnit timeUnit) {
        return (T) toEntityForUpdate(document.getClass(), bucket.insert(toDocument(document), timeout, timeUnit));
    }

    /**
     * All the signed properties will be updated/inserted into data store.
     * 
     * @param document
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#upsert(Document)
     */
    public <T> T upsert(final T document) {
        return (T) toEntityForUpdate(document.getClass(), bucket.upsert(toDocument(document)));
    }

    /**
     * All the signed properties will be updated/inserted into data store.
     * 
     * @param document
     * @param timeout
     * @param timeUnit
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#upsert(Document, long, TimeUnit)
     */
    public <T> T upsert(final T document, final long timeout, final TimeUnit timeUnit) {
        return (T) toEntityForUpdate(document.getClass(), bucket.upsert(toDocument(document), timeout, timeUnit));
    }

    /**
     * 
     * @param document
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#replace(Document)
     */
    public <T> T replace(final T document) {
        return (T) toEntityForUpdate(document.getClass(), bucket.replace(toDocument(document)));
    }

    /**
     * 
     * @param document
     * @param timeout
     * @param timeUnit
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#replace(Document, long, TimeUnit)
     */
    public <T> T replace(final T document, final long timeout, final TimeUnit timeUnit) {
        return (T) toEntityForUpdate(document.getClass(), bucket.replace(toDocument(document), timeout, timeUnit));
    }

    private <T> Document<T> toDocument(final Object obj) {
        final Class<?> cls = obj.getClass();

        if (Document.class.isAssignableFrom(cls)) {
            return (Document<T>) obj;
        } else {
            return (Document<T>) toJsonDocument(obj);
        }
    }

    private <T> T toEntityForUpdate(Class<T> cls, final Document<?> document) {
        if (cls.isAssignableFrom(document.getClass())) {
            return (T) document;
        } else {
            return toEntity(cls, (JsonDocument) document);
        }
    }

    /**
     * 
     * @param id
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#remove(String)
     */
    public JsonDocument remove(String id) {
        return bucket.remove(id);
    }

    /**
     * 
     * @param id
     * @param timeout
     * @param timeUnit
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#remove(String, long, TimeUnit)
     */
    public JsonDocument remove(final String id, final long timeout, final TimeUnit timeUnit) {
        return bucket.remove(id, timeout, timeUnit);
    }

    /**
     * 
     * @param targetClass
     * @param id
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#remove(String, Class)
     */
    public <T> T remove(final Class<T> targetClass, final String id) {
        return toEntityForUpdate(targetClass, bucket.remove(id, JsonDocument.class));
    }

    /**
     * 
     * @param targetClass
     * @param id
     * @param timeout
     * @param timeUnit
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#remove(String, Class, long, TimeUnit)
     */
    public <T> T remove(final Class<T> targetClass, final String id, final long timeout, final TimeUnit timeUnit) {
        return toEntityForUpdate(targetClass, bucket.remove(id, JsonDocument.class, timeout, timeUnit));
    }

    /**
     * 
     * @param document
     * @return
     * 
     * @see com.couchbase.client.java.Bucket#remove(Document)
     */
    public <T> T remove(final T document) {
        return (T) toEntityForUpdate(document.getClass(), bucket.remove(toDocument(document)));
    }

    /**
     * 
     * @param document
     * @param timeout
     * @param timeUnit
     * @return
     * @see com.couchbase.client.java.Bucket#remove(Document, long, TimeUnit)
     */
    public <T> T remove(final T document, final long timeout, final TimeUnit timeUnit) {
        return (T) toEntityForUpdate(document.getClass(), bucket.remove(toDocument(document), timeout, timeUnit));
    }

    public QueryResult execute(final String query) {
        return execute(prepareQuery(query));
    }

    @SafeVarargs
    public final QueryResult execute(final String query, final Object... parameters) {
        return execute(prepareQuery(query, parameters));
    }

    public QueryResult execute(final Query query) {
        final QueryResult resultSet = bucket.query(query);

        checkResultError(resultSet);

        return resultSet;
    }

    public QueryResult execute(final Query query, final long timeout, final TimeUnit timeUnit) {
        final QueryResult resultSet = bucket.query(query, timeout, timeUnit);

        checkResultError(resultSet);

        return resultSet;
    }

    public CompletableFuture<JsonDocument> asyncGet(final String id) {
        return asyncExecutor.execute(new Callable<JsonDocument>() {
            @Override
            public JsonDocument call() throws Exception {
                return get(id);
            }
        });
    }

    public CompletableFuture<JsonDocument> asyncGet(final String id, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<JsonDocument>() {
            @Override
            public JsonDocument call() throws Exception {
                return get(id, timeout, timeUnit);
            }
        });
    }

    public <T> CompletableFuture<T> asyncGet(final Class<T> targetClass, final String id) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return get(targetClass, id);
            }
        });
    }

    public <T> CompletableFuture<T> asyncGet(final Class<T> targetClass, final String id, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return get(targetClass, id, timeout, timeUnit);
            }
        });
    }

    /**
     * Always remember to set "<code>LIMIT 1</code>" in the sql statement for better performance.
     *
     * @param query
     * @param parameters
     * @return
     */
    @SafeVarargs
    public final CompletableFuture<Boolean> asyncExists(final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return exists(query, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Long> asyncCount(final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return count(query, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<NullabLe<T>> asyncQueryForSingleResult(final Class<T> targetClass, final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<NullabLe<T>>() {
            @Override
            public NullabLe<T> call() throws Exception {
                return queryForSingleResult(targetClass, query, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<Optional<T>> asyncQueryForEntity(final Class<T> targetClass, final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return queryForEntity(targetClass, query, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<List<T>> asyncFind(final Class<T> targetClass, final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return find(targetClass, query, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> asyncQuery(final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return query(query, parameters);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<DataSet> asyncQuery(final Class<?> targetClass, final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return query(targetClass, query, parameters);
            }
        });
    }

    public CompletableFuture<DataSet> asyncQuery(final Query query) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return query(query);
            }
        });
    }

    public CompletableFuture<DataSet> asyncQuery(final Class<?> targetClass, final Query query) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return query(targetClass, query);
            }
        });
    }

    public CompletableFuture<DataSet> asyncQuery(final Query query, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return query(query, timeout, timeUnit);
            }
        });

    }

    public CompletableFuture<DataSet> asyncQuery(final Class<?> targetClass, final Query query, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return query(targetClass, query, timeout, timeUnit);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<Stream<JsonObject>> asyncStream(final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Stream<JsonObject>>() {
            @Override
            public Stream<JsonObject> call() throws Exception {
                return stream(query, parameters);
            }
        });
    }

    @SafeVarargs
    public final <T> CompletableFuture<Stream<T>> asyncStream(final Class<T> targetClass, final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return stream(targetClass, query, parameters);
            }
        });
    }

    public CompletableFuture<Stream<JsonObject>> asyncStream(final Query query) {
        return asyncExecutor.execute(new Callable<Stream<JsonObject>>() {
            @Override
            public Stream<JsonObject> call() throws Exception {
                return stream(query);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> asyncStream(final Class<T> targetClass, final Query query) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return stream(targetClass, query);
            }
        });
    }

    public CompletableFuture<Stream<JsonObject>> asyncStream(final Query query, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<Stream<JsonObject>>() {
            @Override
            public Stream<JsonObject> call() throws Exception {
                return stream(query, timeout, timeUnit);
            }
        });

    }

    public <T> CompletableFuture<Stream<T>> asyncStream(final Class<T> targetClass, final Query query, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return stream(targetClass, query, timeout, timeUnit);
            }
        });
    }

    public <T> CompletableFuture<T> asyncInsert(final T document) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return insert(document);
            }
        });
    }

    public <T> CompletableFuture<T> asyncInsert(final T document, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return insert(document, timeout, timeUnit);
            }
        });
    }

    public <T> CompletableFuture<T> asyncUpsert(final T document) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return upsert(document);
            }
        });
    }

    public <T> CompletableFuture<T> asyncUpsert(final T document, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return upsert(document, timeout, timeUnit);
            }
        });
    }

    public <T> CompletableFuture<T> asyncReplace(final T document) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return replace(document);
            }
        });
    }

    public <T> CompletableFuture<T> asyncReplace(final T document, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return replace(document, timeout, timeUnit);
            }
        });
    }

    public CompletableFuture<JsonDocument> asyncRemove(final String id) {
        return asyncExecutor.execute(new Callable<JsonDocument>() {
            @Override
            public JsonDocument call() throws Exception {
                return remove(id);
            }
        });
    }

    public CompletableFuture<JsonDocument> asyncRemove(final String id, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<JsonDocument>() {
            @Override
            public JsonDocument call() throws Exception {
                return remove(id, timeout, timeUnit);
            }
        });
    }

    public <T> CompletableFuture<T> asyncRemove(final Class<T> targetClass, final String id) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return remove(targetClass, id);
            }
        });
    }

    public <T> CompletableFuture<T> asyncRemove(final Class<T> targetClass, final String id, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return remove(targetClass, id, timeout, timeUnit);
            }
        });
    }

    public <T> CompletableFuture<T> asyncRemove(final T document) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return remove(document);
            }
        });
    }

    public <T> CompletableFuture<T> asyncRemove(final T document, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return remove(document, timeout, timeUnit);
            }
        });
    }

    public CompletableFuture<QueryResult> asyncExecute(final String query) {
        return asyncExecutor.execute(new Callable<QueryResult>() {
            @Override
            public QueryResult call() throws Exception {
                return execute(query);
            }
        });
    }

    @SafeVarargs
    public final CompletableFuture<QueryResult> asyncExecute(final String query, final Object... parameters) {
        return asyncExecutor.execute(new Callable<QueryResult>() {
            @Override
            public QueryResult call() throws Exception {
                return execute(query, parameters);
            }
        });
    }

    public CompletableFuture<QueryResult> asyncExecute(final Query query) {
        return asyncExecutor.execute(new Callable<QueryResult>() {
            @Override
            public QueryResult call() throws Exception {
                return execute(query);
            }
        });
    }

    public CompletableFuture<QueryResult> asyncExecute(final Query query, final long timeout, final TimeUnit timeUnit) {
        return asyncExecutor.execute(new Callable<QueryResult>() {
            @Override
            public QueryResult call() throws Exception {
                return execute(query, timeout, timeUnit);
            }
        });
    }

    private static void checkTargetClass(final Class<?> targetClass) {
        if (!(N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass))) {
            throw new AbacusException("The target class must be an entity class with getter/setter methods or Map.class. But it is: "
                    + ClassUtil.getCanonicalClassName(targetClass));
        }
    }

    private static void checkResultError(QueryResult resultSet) {
        if (N.notNullOrEmpty(resultSet.errors())) {
            throw new AbacusException("Errors in query result: " + resultSet.errors());
        }
    }

    private Query prepareQuery(final String query) {
        Query result = null;

        if (query.length() <= POOLABLE_LENGTH) {
            PoolableWrapper<Query> wrapper = stmtPool.get(query);

            if (wrapper != null) {
                result = wrapper.value();
            }
        }

        if (result == null) {
            result = Query.simple(query);

            if (query.length() <= POOLABLE_LENGTH) {
                stmtPool.put(query, PoolableWrapper.of(result));
            }
        }

        return result;
    }

    private Query prepareQuery(String query, Object... parameters) {
        if (N.isNullOrEmpty(parameters)) {
            return prepareQuery(query);
        }

        final NamedSQL namedSQL = getNamedSQL(query);
        final String sql = namedSQL.getPureSQL(true);
        final int parameterCount = namedSQL.getParameterCount(true);
        final Map<Integer, String> namedParameters = namedSQL.getNamedParameters(true);

        // Prepared query plan doens't work in Couchbase 4.0 Beta version?

        //        QueryPlan queryPlan = null;
        //
        //        if (query.length() <= POOLABLE_LENGTH) {
        //            Wrapper<QueryPlan> wrapper = preStmtPool.get(query);
        //            if (wrapper != null && wrapper.get() != null) {
        //                queryPlan = wrapper.get();
        //            }
        //        }
        //
        //        if (queryPlan == null) {
        //            queryPlan = bucket.prepare(sql);
        //
        //            if (query.length() <= POOLABLE_LENGTH) {
        //                preStmtPool.put(query, Wrapper.valueOf(queryPlan));
        //            }
        //        }
        //

        if (parameterCount == 0) {
            return Query.simple(query);
        } else if (N.isNullOrEmpty(parameters)) {
            throw new IllegalArgumentException("Null or empty parameters for parameterized query: " + query);
        }

        //        if (parameters.length == 1) {
        //            if (parameters[0] instanceof JsonArray) {
        //                return Query.parametrized(sql, (JsonArray) parameters[0]);
        //            } else if (parameters[0] instanceof JsonObject) {
        //                return Query.parametrized(sql, (JsonObject) parameters[0]);
        //            }
        //        }

        Object[] values = parameters;

        if (N.notNullOrEmpty(namedParameters) && parameters.length == 1
                && (parameters[0] instanceof Map || parameters[0] instanceof JsonObject || N.isEntity(parameters[0].getClass()))) {
            values = new Object[parameterCount];

            final Object parameter_0 = parameters[0];
            String parameterName = null;

            if (parameter_0 instanceof Map) {
                @SuppressWarnings("unchecked")
                Map<String, Object> m = (Map<String, Object>) parameter_0;

                for (int i = 0; i < parameterCount; i++) {
                    parameterName = namedParameters.get(i);
                    values[i] = m.get(parameterName);

                    if ((values[i] == null) && !m.containsKey(parameterName)) {
                        throw new IllegalArgumentException("Parameter for property '" + parameterName + "' is missed");
                    }
                }
            } else if (parameter_0 instanceof JsonObject) {
                @SuppressWarnings("unchecked")
                JsonObject jsonObject = (JsonObject) parameter_0;

                for (int i = 0; i < parameterCount; i++) {
                    parameterName = namedParameters.get(i);
                    values[i] = jsonObject.get(parameterName);

                    if ((values[i] == null) && !jsonObject.containsKey(parameterName)) {
                        throw new IllegalArgumentException("Parameter for property '" + parameterName + "' is missed");
                    }
                }
            } else {
                Object entity = parameter_0;
                Class<?> clazz = entity.getClass();
                Method propGetMethod = null;

                for (int i = 0; i < parameterCount; i++) {
                    parameterName = namedParameters.get(i);
                    propGetMethod = ClassUtil.getPropGetMethod(clazz, parameterName);

                    if (propGetMethod == null) {
                        throw new IllegalArgumentException("Parameter for property '" + parameterName + "' is missed");
                    }

                    values[i] = ClassUtil.invokeMethod(entity, propGetMethod);
                }
            }
        } else if ((parameters.length == 1) && (parameters[0] != null)) {
            if (parameters[0] instanceof Object[] && ((((Object[]) parameters[0]).length) >= parameterCount)) {
                values = (Object[]) parameters[0];
            } else if (parameters[0] instanceof List && (((List<?>) parameters[0]).size() >= parameterCount)) {
                final Collection<?> c = (Collection<?>) parameters[0];
                values = c.toArray(new Object[c.size()]);
            }
        }

        if (values.length == 1 && values[0] instanceof JsonArray) {
            return Query.parametrized(sql, (JsonArray) values[0]);
        } else if (values.length > parameterCount) {
            return Query.parametrized(sql, JsonArray.from(N.copyOfRange(values, 0, parameterCount)));
        } else {
            return Query.parametrized(sql, JsonArray.from(values));
        }
    }

    private NamedSQL getNamedSQL(String sql) {
        NamedSQL namedSQL = null;

        if (sqlMapper != null) {
            namedSQL = sqlMapper.get(sql);
        }

        if (namedSQL == null) {
            namedSQL = NamedSQL.parse(sql, null);
        }

        return namedSQL;
    }

    @Override
    public void close() throws IOException {
        try {
            bucket.close();
        } finally {
            cluster.disconnect();
        }
    }

}
