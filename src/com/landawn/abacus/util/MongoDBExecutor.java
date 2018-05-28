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

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import org.bson.BSONObject;
import org.bson.BasicBSONObject;
import org.bson.BsonReader;
import org.bson.BsonWriter;
import org.bson.Document;
import org.bson.codecs.BsonTypeClassMap;
import org.bson.codecs.Codec;
import org.bson.codecs.DecoderContext;
import org.bson.codecs.DocumentCodec;
import org.bson.codecs.EncoderContext;
import org.bson.codecs.configuration.CodecRegistries;
import org.bson.codecs.configuration.CodecRegistry;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.DirtyMarker;
import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.exception.AbacusException;
import com.landawn.abacus.parser.JSONParser;
import com.landawn.abacus.parser.ParserFactory;
import com.landawn.abacus.type.Type;
import com.landawn.abacus.util.stream.Stream;
import com.mongodb.BasicDBObject;
import com.mongodb.MongoClient;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.MongoIterable;
import com.mongodb.client.model.BulkWriteOptions;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.InsertManyOptions;
import com.mongodb.client.model.UpdateOptions;
import com.mongodb.client.model.WriteModel;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.UpdateResult;

/**
 * It's a simple wrapper of MongoDB Java client.
 * 
 * <br>We recommend to define "id" property in java entity/bean as the object "_id" in MongoDB to keep things as simple as possible.</br>
 *  
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="http://api.mongodb.org/java/3.1/?com/mongodb/client/model/Filters.html">com.mongodb.client.model.Filters</a>
 * @see <a href="http://api.mongodb.org/java/3.1/?com/mongodb/client/model/Aggregates.html">com.mongodb.client.model.Aggregates</a>
 * @see <a href="http://api.mongodb.org/java/3.1/?com/mongodb/client/model/Accumulators.html">com.mongodb.client.model.Accumulators</a>
 * @see <a href="http://api.mongodb.org/java/3.1/?com/mongodb/client/model/Projections.html">com.mongodb.client.model.Projections</a>
 * @see <a href="http://api.mongodb.org/java/3.1/?com/mongodb/client/model/Sorts.html">com.mongodb.client.model.Sorts</a>
 */
public final class MongoDBExecutor {
    /**
     * It's name of object id set in Map/Object array.
     */
    public static final String _ID = "_id";

    /**
     * Property name of id.
     */
    public static final String ID = "id";

    private static final JSONParser jsonParser = ParserFactory.createJSONParser();

    // private static CodecRegistry codecRegistry = CodecRegistries.fromCodecs(new CalendarCodec(), new TimeCodec(), new TimestampCodec());
    private static final CodecRegistry codecRegistry = CodecRegistries.fromRegistries(MongoClient.getDefaultCodecRegistry(), new GeneralCodecRegistry());

    private static final Map<Class<?>, Method> classIdSetMethodPool = new ConcurrentHashMap<>();

    private final Map<String, MongoCollectionExecutor> collExecutorPool = new ConcurrentHashMap<>();

    private final MongoDatabase mongoDB;
    private final AsyncExecutor asyncExecutor;
    private final AsyncMongoDBExecutor asyncDBExecutor;

    public MongoDBExecutor(final MongoDatabase mongoDB) {
        this(mongoDB, new AsyncExecutor(64, 300, TimeUnit.SECONDS));
    }

    public MongoDBExecutor(final MongoDatabase mongoDB, final AsyncExecutor asyncExecutor) {
        this.mongoDB = mongoDB.withCodecRegistry(codecRegistry);
        this.asyncExecutor = asyncExecutor;
        this.asyncDBExecutor = new AsyncMongoDBExecutor(this, this.asyncExecutor);
    }

    public MongoDatabase mongoDB() {
        return mongoDB;
    }

    public AsyncMongoDBExecutor async() {
        return asyncDBExecutor;
    }

    public MongoCollectionExecutor collExecutor(final String collectionName) {
        MongoCollectionExecutor collExecutor = collExecutorPool.get(collectionName);

        if (collExecutor == null) {
            synchronized (collExecutorPool) {
                collExecutor = collExecutorPool.get(collectionName);

                if (collExecutor == null) {
                    collExecutor = new MongoCollectionExecutor(this, mongoDB.getCollection(collectionName), asyncExecutor);

                    collExecutorPool.put(collectionName, collExecutor);
                }
            }
        }

        return collExecutor;
    }

    public MongoCollection<Document> collection(final String collectionName) {
        return mongoDB.getCollection(collectionName);
    }

    public <T> MongoCollection<T> collection(final Class<T> targetClass, final String collectionName) {
        return mongoDB.getCollection(collectionName, targetClass);
    }

    /**
     * The object id ("_id") property will be read from/write to the specified property 
     * @param cls
     * @param idPropertyName
     */
    public static void registerIdProeprty(final Class<?> cls, final String idPropertyName) {
        if (ClassUtil.getPropGetMethod(cls, idPropertyName) == null || ClassUtil.getPropSetMethod(cls, idPropertyName) == null) {
            throw new IllegalArgumentException("The specified class: " + ClassUtil.getCanonicalClassName(cls)
                    + " doesn't have getter or setter method for the specified id propery: " + idPropertyName);
        }

        final Method setMethod = ClassUtil.getPropSetMethod(cls, idPropertyName);
        final Class<?> parameterType = setMethod.getParameterTypes()[0];

        if (!(String.class.isAssignableFrom(parameterType) || ObjectId.class.isAssignableFrom(parameterType))) {
            throw new IllegalArgumentException(
                    "The parameter type of the specified id setter method must be 'String' or 'ObjectId': " + setMethod.toGenericString());
        }

        classIdSetMethodPool.put(cls, setMethod);
    }

    public static DataSet extractData(final MongoIterable<?> findIterable) {
        return extractData(Map.class, findIterable);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method or <code>Map.class/Document.class</code>
     * @param findIterable
     * @return
     */
    public static DataSet extractData(final Class<?> targetClass, final MongoIterable<?> findIterable) {
        return extractData(targetClass, null, findIterable);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method or <code>Map.class/Document.class</code>
     * @param selectPropNames
     * @param findIterable
     * @return
     */
    static DataSet extractData(final Class<?> targetClass, final Collection<String> selectPropNames, final MongoIterable<?> findIterable) {
        checkTargetClass(targetClass);

        final List<Object> rowList = findIterable.into(new ArrayList<>());
        final Optional<Object> first = N.firstNonNull(rowList);

        if (first.isPresent()) {
            /*
            if (Map.class.isAssignableFrom(first.get().getClass())) {
                if (N.isNullOrEmpty(selectPropNames)) {
                    final Set<String> columnNames = new LinkedHashSet<>();
                    @SuppressWarnings("rawtypes")
                    final List<Map<String, Object>> tmp = (List) rowList;
            
                    for (Map<String, Object> row : tmp) {
                        columnNames.addAll(row.keySet());
                    }
            
                    return N.newDataSet(columnNames, rowList);
                } else {
                    return N.newDataSet(selectPropNames, rowList);
                }                
            } else {
                return N.newDataSet(rowList);
            }
            */

            if (Map.class.isAssignableFrom(targetClass) && Map.class.isAssignableFrom(first.get().getClass())) {
                if (N.isNullOrEmpty(selectPropNames)) {
                    final Set<String> columnNames = new LinkedHashSet<>();
                    @SuppressWarnings("rawtypes")
                    final List<Map<String, Object>> tmp = (List) rowList;

                    for (Map<String, Object> row : tmp) {
                        columnNames.addAll(row.keySet());
                    }

                    return N.newDataSet(columnNames, rowList);
                } else {
                    return N.newDataSet(selectPropNames, rowList);
                }
            } else if (Document.class.isAssignableFrom(first.get().getClass())) {
                final List<Object> newRowList = new ArrayList<>(rowList.size());

                for (Object row : rowList) {
                    newRowList.add(toEntity(targetClass, (Document) row));
                }

                return N.newDataSet(selectPropNames, newRowList);
            } else {
                return N.newDataSet(selectPropNames, rowList);
            }

        } else {
            return N.newEmptyDataSet();
        }
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param findIterable
     * @return
     */
    @SuppressWarnings("rawtypes")
    public static <T> List<T> toList(final Class<T> targetClass, final MongoIterable<?> findIterable) {
        final Type<T> type = N.typeOf(targetClass);
        final List<Object> rowList = findIterable.into(new ArrayList<>());
        final Optional<Object> first = N.firstNonNull(rowList);

        if (first.isPresent()) {
            if (targetClass.isAssignableFrom(first.getClass())) {
                return (List<T>) rowList;
            } else {
                final List<Object> resultList = new ArrayList<>(rowList.size());

                if (type.isEntity() || type.isMap()) {
                    if (first.get() instanceof Document) {
                        for (Object row : rowList) {
                            resultList.add(toEntity(targetClass, (Document) row));
                        }
                    } else if (type.isMap()) {
                        for (Object row : rowList) {
                            resultList.add(Maps.entity2Map((Map) N.newInstance(targetClass), row));
                        }
                    } else {
                        for (Object row : rowList) {
                            resultList.add(N.copy(targetClass, row));
                        }
                    }
                } else if (first.get() instanceof Map && ((Map<String, Object>) first.get()).size() == 1) {
                    final Map<String, Object> m = (Map<String, Object>) first.get();
                    final String propName = m.keySet().iterator().next();

                    if (m.get(propName) != null && targetClass.isAssignableFrom(m.get(propName).getClass())) {
                        for (Object row : rowList) {
                            resultList.add(((Map<String, Object>) row).get(propName));
                        }
                    } else {
                        for (Object row : rowList) {
                            resultList.add(N.as(targetClass, ((Map<String, Object>) row).get(propName)));
                        }
                    }
                } else {
                    throw new IllegalArgumentException(
                            "Can't covert document: " + first.toString() + " to class: " + ClassUtil.getCanonicalClassName(targetClass));
                }
                return (List<T>) resultList;
            }
        } else {
            return new ArrayList<>();
        }
    }

    /**
     * The id in the specified <code>doc</code> will be set to the returned object if and only if the id is not null or empty and it's acceptable to the <code>targetClass</code>.
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param doc
     * @return
     */
    @SuppressWarnings("deprecation")
    public static <T> T toEntity(final Class<T> targetClass, final Document doc) {
        checkTargetClass(targetClass);

        if (Map.class.isAssignableFrom(targetClass)) {
            // return (T) new LinkedHashMap<>(doc);
            if (targetClass.isAssignableFrom(doc.getClass())) {
                return (T) doc;
            } else {
                final Map<String, Object> map = (Map<String, Object>) N.newInstance(targetClass);
                map.putAll(doc);
                return (T) map;
            }
        }

        final Method idSetMethod = getObjectIdSetMethod(targetClass);
        final Class<?> parameterType = idSetMethod == null ? null : idSetMethod.getParameterTypes()[0];
        final Object objectId = doc.getObjectId(_ID);
        T entity = null;

        doc.remove(_ID);

        try {
            entity = Maps.map2Entity(targetClass, doc);

            if (objectId != null && parameterType != null) {
                if (parameterType.isAssignableFrom(objectId.getClass())) {
                    ClassUtil.setPropValue(entity, idSetMethod, objectId);
                } else if (parameterType.isAssignableFrom(String.class)) {
                    ClassUtil.setPropValue(entity, idSetMethod, objectId.toString());
                } else {
                    ClassUtil.setPropValue(entity, idSetMethod, objectId);
                }
            }
        } finally {
            doc.put(_ID, objectId);
        }

        if (N.isDirtyMarker(entity.getClass())) {
            ((DirtyMarker) entity).markDirty(false);
        }

        return entity;
    }

    private static <T> Method getObjectIdSetMethod(final Class<T> targetClass) {
        Method idSetMethod = classIdSetMethodPool.get(targetClass);

        if (idSetMethod == null) {
            Method idPropSetMethod = ClassUtil.getPropSetMethod(targetClass, ID);
            Class<?> parameterType = idPropSetMethod == null ? null : idPropSetMethod.getParameterTypes()[0];

            //            if (parameterType != null && (ObjectId.class.isAssignableFrom(parameterType) || String.class.isAssignableFrom(parameterType))) {
            //                idSetMethod = idPropSetMethod;
            //            }
            if (parameterType != null && ObjectId.class.isAssignableFrom(parameterType)) {
                idSetMethod = idPropSetMethod;
            }

            if (idSetMethod == null) {
                idSetMethod = ClassUtil.METHOD_MASK;
            }

            classIdSetMethodPool.put(targetClass, idSetMethod);
        }

        return idSetMethod == ClassUtil.METHOD_MASK ? null : idSetMethod;
    }

    public static String toJSON(final Bson bson) {
        return bson instanceof Map ? N.toJSON(bson) : N.toJSON(bson.toBsonDocument(Document.class, codecRegistry));
    }

    public static String toJSON(final BSONObject bsonObject) {
        return bsonObject instanceof Map ? N.toJSON(bsonObject) : N.toJSON(bsonObject.toMap());
    }

    public static String toJSON(final BasicDBObject bsonObject) {
        return bsonObject instanceof Map ? N.toJSON(bsonObject) : N.toJSON(bsonObject.toMap());
    }

    /**
     * Returns an instance of the specified target class with the property values from the specified JSON String.
     * 
     * @param targetClass <code>Bson.class</code>, <code>Document.class</code>, <code>BasicBSONObject.class</code>, <code>BasicDBObject.class</code>
     * @param json
     * @return
     */
    public static <T> T fromJSON(final Class<T> targetClass, final String json) {
        if (targetClass.equals(Bson.class) || targetClass.equals(Document.class)) {
            final Document doc = new Document();
            jsonParser.readString(doc, json);
            return (T) doc;
        } else if (targetClass.equals(BasicBSONObject.class)) {
            final BasicBSONObject result = new BasicBSONObject();
            jsonParser.readString(result, json);
            return (T) result;
        } else if (targetClass.equals(BasicDBObject.class)) {
            final BasicDBObject result = new BasicDBObject();
            jsonParser.readString(result, json);
            return (T) result;
        } else {
            throw new IllegalArgumentException("Unsupported type: " + ClassUtil.getCanonicalClassName(targetClass));
        }
    }

    /**
     *
     * @param obj an array of pairs of property name and value/Map<String, Object> or an entity with getter/setter methods.
     * @return
     */
    public static Bson toBson(final Object obj) {
        return toDocument(obj);
    }

    /**
     * Create a new document with specified parameter(s). It could an array of property name and value, or a map, or an entity.
     *
     * @param a
     * @return
     */
    @SafeVarargs
    public static Bson toBson(final Object... a) {
        return toDocument(a);
    }

    /**
     *
     * @param obj an array of pairs of property name and value/Map<String, Object> or an entity with getter/setter methods.
     * @return
     */
    public static Document toDocument(final Object obj) {
        return toDocument(obj, false);
    }

    /**
     * Create a new document with specified parameter(s). It could an array of property name and value, or a map, or an entity.
     *
     * @param a
     * @return
     */
    @SafeVarargs
    public static Document toDocument(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new Document();
        }

        return a.length == 1 ? toDocument(a[0]) : toDocument((Object) a);
    }

    @SuppressWarnings("deprecation")
    static Document toDocument(final Object obj, final boolean isForUpdate) {
        final Document result = new Document();

        if (obj instanceof Map) {
            result.putAll((Map<String, Object>) obj);
        } else if (N.isEntity(obj.getClass())) {
            if (obj instanceof DirtyMarker) {
                final Class<?> srCls = obj.getClass();
                final Set<String> updatePropNames = isForUpdate ? ((DirtyMarker) obj).dirtyPropNames() : ((DirtyMarker) obj).signedPropNames();

                if (updatePropNames.size() == 0) {
                    // logger.warn("No property is signed/updated in the specified source entity: " + N.toString(obj));
                } else {
                    Method propGetMethod = null;
                    Object propValue = null;
                    for (String propName : updatePropNames) {
                        propGetMethod = ClassUtil.getPropGetMethod(srCls, propName);
                        propName = ClassUtil.getPropNameByMethod(propGetMethod);
                        propValue = ClassUtil.getPropValue(obj, propGetMethod);

                        result.put(propName, propValue);
                    }
                }
            } else {
                final Map<String, Method> getterMethodList = ClassUtil.getPropGetMethodList(obj.getClass());

                if (getterMethodList.size() == 0) {
                    throw new IllegalArgumentException("No property getter/setter method found in the specified entity: " + obj.getClass().getCanonicalName());
                }

                String propName = null;
                Object propValue = null;

                for (Map.Entry<String, Method> entry : getterMethodList.entrySet()) {
                    propName = entry.getKey();
                    propValue = ClassUtil.getPropValue(obj, entry.getValue());

                    if (propValue == null) {
                        continue;
                    }

                    result.put(propName, propValue);
                }
            }
        } else if (obj instanceof Object[]) {
            final Object[] a = (Object[]) obj;

            if (0 != (a.length % 2)) {
                throw new IllegalArgumentException(
                        "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
            }

            for (int i = 0; i < a.length; i++) {
                result.put((String) a[i], a[++i]);
            }
        } else {
            throw new IllegalArgumentException("The parameters must be a Map, or an entity class with getter/setter methods");
        }

        resetObjectId(obj, result);

        return result;
    }

    /**
     *
     * @param obj an array of pairs of property name and value/Map<String, Object> or an entity with getter/setter methods.
     * @return
     */
    public static BasicBSONObject toBSONObject(final Object obj) {
        final BasicBSONObject result = new BasicBSONObject();

        if (obj instanceof Map) {
            result.putAll((Map<String, Object>) obj);
        } else if (N.isEntity(obj.getClass())) {
            Maps.deepEntity2Map(result, obj);
        } else if (obj instanceof Object[]) {
            final Object[] a = (Object[]) obj;

            if (0 != (a.length % 2)) {
                throw new IllegalArgumentException(
                        "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
            }

            for (int i = 0; i < a.length; i++) {
                result.put((String) a[i], a[++i]);
            }
        } else {
            throw new IllegalArgumentException("The parameters must be a Map, or an entity class with getter/setter methods");
        }

        resetObjectId(obj, result);

        return result;
    }

    @SafeVarargs
    public static BasicBSONObject toBSONObject(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new BasicBSONObject();
        }

        return a.length == 1 ? toBSONObject(a[0]) : toBSONObject((Object) a);
    }

    /**
     *
     * @param obj an array of pairs of property name and value/Map<String, Object> or an entity with getter/setter methods.
     * @return
     */
    public static BasicDBObject toDBObject(final Object obj) {
        final BasicDBObject result = new BasicDBObject();

        if (obj instanceof Map) {
            result.putAll((Map<String, Object>) obj);
        } else if (N.isEntity(obj.getClass())) {
            Maps.deepEntity2Map(result, obj);
        } else if (obj instanceof Object[]) {
            final Object[] a = (Object[]) obj;

            if (0 != (a.length % 2)) {
                throw new IllegalArgumentException(
                        "The parameters must be the pairs of property name and value, or Map, or an entity class with getter/setter methods.");
            }

            for (int i = 0; i < a.length; i++) {
                result.put((String) a[i], a[++i]);
            }
        } else {
            throw new IllegalArgumentException("The parameters must be a Map, or an entity class with getter/setter methods");
        }

        resetObjectId(obj, result);

        return result;
    }

    @SafeVarargs
    public static BasicDBObject toDBObject(final Object... a) {
        if (N.isNullOrEmpty(a)) {
            return new BasicDBObject();
        }

        return a.length == 1 ? toDBObject(a[0]) : toDBObject((Object) a);
    }

    private static void resetObjectId(final Object obj, final Map<String, Object> doc) {
        final Class<?> cls = obj.getClass();
        final Method idSetMethod = getObjectIdSetMethod(obj.getClass());
        final String idPropertyName = N.isEntity(cls) ? (idSetMethod == null ? null : ClassUtil.getPropNameByMethod(idSetMethod)) : _ID;

        if (idPropertyName != null && doc.containsKey(idPropertyName)) {
            Object id = doc.remove(idPropertyName);

            try {
                if (id instanceof String) {
                    id = new ObjectId((String) id);
                } else if (id instanceof Date) {
                    id = new ObjectId((Date) id);
                } else if (id instanceof byte[]) {
                    id = new ObjectId((byte[]) id);
                }
            } finally {
                doc.put(_ID, id);
            }
        }
    }

    public boolean exists(final String collectionName, final String objectId) {
        return collExecutor(collectionName).exists(objectId);
    }

    public boolean exists(final String collectionName, final ObjectId objectId) {
        return collExecutor(collectionName).exists(objectId);
    }

    public boolean exists(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).exists(filter);
    }

    public long count(final String collectionName) {
        return collExecutor(collectionName).count();
    }

    public long count(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).count(filter);
    }

    public long count(final String collectionName, final Bson filter, final CountOptions options) {
        return collExecutor(collectionName).count(filter, options);
    }

    public Document get(final String collectionName, final String objectId) {
        return collExecutor(collectionName).get(objectId);
    }

    public Document get(final String collectionName, final ObjectId objectId) {
        return collExecutor(collectionName).get(objectId);
    }

    public <T> T get(final Class<T> targetClass, final String collectionName, final String objectId) {
        return collExecutor(collectionName).get(targetClass, objectId);
    }

    public <T> T get(final Class<T> targetClass, final String collectionName, final ObjectId objectId) {
        return collExecutor(collectionName).get(targetClass, objectId);
    }

    public <T> T get(final Class<T> targetClass, final String collectionName, final String objectId, final Collection<String> selectPropNames) {
        return collExecutor(collectionName).get(targetClass, objectId, selectPropNames);
    }

    public <T> T get(final Class<T> targetClass, final String collectionName, final ObjectId objectId, final Collection<String> selectPropNames) {
        return collExecutor(collectionName).get(targetClass, objectId, selectPropNames);
    }

    public Optional<Document> gett(final String collectionName, final String objectId) {
        return collExecutor(collectionName).gett(objectId);
    }

    public Optional<Document> gett(final String collectionName, final ObjectId objectId) {
        return collExecutor(collectionName).gett(objectId);
    }

    public <T> Optional<T> gett(final Class<T> targetClass, final String collectionName, final String objectId) {
        return collExecutor(collectionName).gett(targetClass, objectId);
    }

    public <T> Optional<T> gett(final Class<T> targetClass, final String collectionName, final ObjectId objectId) {
        return collExecutor(collectionName).gett(targetClass, objectId);
    }

    public <T> Optional<T> gett(final Class<T> targetClass, final String collectionName, final String objectId, final Collection<String> selectPropNames) {
        return collExecutor(collectionName).gett(targetClass, objectId, selectPropNames);
    }

    public <T> Optional<T> gett(final Class<T> targetClass, final String collectionName, final ObjectId objectId, final Collection<String> selectPropNames) {
        return collExecutor(collectionName).gett(targetClass, objectId, selectPropNames);
    }

    @Beta
    public OptionalBoolean queryForBoolean(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForBoolean(propName, filter);
    }

    @Beta
    public OptionalChar queryForChar(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForChar(propName, filter);
    }

    @Beta
    public OptionalByte queryForByte(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForByte(propName, filter);
    }

    @Beta
    public OptionalShort queryForShort(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForShort(propName, filter);
    }

    @Beta
    public OptionalInt queryForInt(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForInt(propName, filter);
    }

    @Beta
    public OptionalLong queryForLong(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForLong(propName, filter);
    }

    @Beta
    public OptionalFloat queryForFloat(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForFloat(propName, filter);
    }

    @Beta
    public OptionalDouble queryForDouble(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForDouble(propName, filter);
    }

    public Nullable<String> queryForString(final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForString(propName, filter);
    }

    public <T> Nullable<T> queryForSingleResult(final Class<T> targetClass, final String collectionName, final String propName, final Bson filter) {
        return collExecutor(collectionName).queryForSingleResult(targetClass, propName, filter);
    }

    public Optional<Document> queryForEntity(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).queryForEntity(filter);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return collExecutor(collectionName).queryForEntity(targetClass, filter);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter) {
        return collExecutor(collectionName).queryForEntity(targetClass, selectPropNames, filter);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort) {
        return collExecutor(collectionName).queryForEntity(targetClass, selectPropNames, filter, sort);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor(collectionName).queryForEntity(targetClass, filter, sort, projection);
    }

    public List<Document> find(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).find(filter);
    }

    public <T> List<T> find(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return collExecutor(collectionName).find(targetClass, filter);
    }

    public <T> List<T> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter) {
        return collExecutor(collectionName).find(targetClass, selectPropNames, filter);
    }

    public <T> List<T> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final int offset, final int count) {
        return collExecutor(collectionName).find(targetClass, selectPropNames, filter, offset, count);
    }

    public <T> List<T> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort) {
        return collExecutor(collectionName).find(targetClass, selectPropNames, filter, sort);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param collectionName
     * @param selectPropNames
     * @param filter
     * @param sort
     * @param offset
     * @param count
     * @return
     */
    public <T> List<T> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort, final int offset, final int count) {
        return collExecutor(collectionName).find(targetClass, selectPropNames, filter, sort, offset, count);
    }

    public <T> List<T> find(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor(collectionName).find(targetClass, filter, sort, projection);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param collectionName
     * @param filter
     * @param sort
     * @param projection
     * @param offset
     * @param count
     * @return
     */
    public <T> List<T> find(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort, final Bson projection,
            final int offset, final int count) {
        return collExecutor(collectionName).find(targetClass, filter, sort, projection, offset, count);
    }

    public DataSet query(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).query(filter);
    }

    public <T> DataSet query(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return collExecutor(collectionName).query(targetClass, filter);
    }

    public <T> DataSet query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter) {
        return collExecutor(collectionName).query(targetClass, selectPropNames, filter);
    }

    public <T> DataSet query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final int offset, final int count) {
        return collExecutor(collectionName).query(targetClass, selectPropNames, filter, offset, count);
    }

    public <T> DataSet query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort) {
        return collExecutor(collectionName).query(targetClass, selectPropNames, filter, sort);
    }

    public <T> DataSet query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort, final int offset, final int count) {
        return collExecutor(collectionName).query(targetClass, selectPropNames, filter, sort, offset, count);
    }

    public <T> DataSet query(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor(collectionName).query(targetClass, filter, sort, projection);
    }

    public <T> DataSet query(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort, final Bson projection,
            final int offset, final int count) {
        return collExecutor(collectionName).query(targetClass, filter, sort, projection, offset, count);
    }

    public Stream<Document> stream(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).stream(filter);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return collExecutor(collectionName).stream(targetClass, filter);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter) {
        return collExecutor(collectionName).stream(targetClass, selectPropNames, filter);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final int offset, final int count) {
        return collExecutor(collectionName).stream(targetClass, selectPropNames, filter, offset, count);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort) {
        return collExecutor(collectionName).stream(targetClass, selectPropNames, filter, sort);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort, final int offset, final int count) {
        return collExecutor(collectionName).stream(targetClass, selectPropNames, filter, sort, offset, count);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor(collectionName).stream(targetClass, filter, sort, projection);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort, final Bson projection,
            final int offset, final int count) {
        return collExecutor(collectionName).stream(targetClass, filter, sort, projection, offset, count);
    }

    /**
     *
     * @param collectionName
     * @param obj can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     */
    public void insert(final String collectionName, final Object obj) {
        collExecutor(collectionName).insert(obj);
    }

    /**
     *
     * @param collectionName
     * @param objList list of <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @deprecated replaced with {@code insertAll}.
     */
    @Deprecated
    public void insert(final String collectionName, final Collection<?> objList) {
        collExecutor(collectionName).insert(objList);
    }

    /**
     *
     * @param collectionName
     * @param objList list of <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @param options
     * @deprecated replaced with {@code insertAll}.
     */
    @Deprecated
    public void insert(final String collectionName, final Collection<?> objList, final InsertManyOptions options) {
        collExecutor(collectionName).insert(objList, options);
    }

    /**
     *
     * @param collectionName
     * @param objList list of <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     */
    public void insertAll(final String collectionName, final Collection<?> objList) {
        collExecutor(collectionName).insertAll(objList);
    }

    /**
     *
     * @param collectionName
     * @param objList list of <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @param options
     */
    public void insertAll(final String collectionName, final Collection<?> objList, final InsertManyOptions options) {
        collExecutor(collectionName).insertAll(objList, options);
    }

    /**
     * Update the record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>objectId</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param collectionName
     * @param objectId
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult update(final String collectionName, final String objectId, final Object update) {
        return collExecutor(collectionName).update(objectId, update);
    }

    /**
     * Update the record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>objectId</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param collectionName
     * @param objectId
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult update(final String collectionName, final ObjectId objectId, final Object update) {
        return collExecutor(collectionName).update(objectId, update);
    }

    /**
     * Just update one record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param collectionName
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult updateOne(final String collectionName, final Bson filter, final Object update) {
        return collExecutor(collectionName).updateOne(filter, update);
    }

    /**
     * Just update one record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param collectionName
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @param options
     * @return
     */
    public UpdateResult updateOne(final String collectionName, final Bson filter, final Object update, final UpdateOptions options) {
        return collExecutor(collectionName).updateOne(filter, update, options);
    }

    /**
     * Update the records in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param collectionName
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult updateAll(final String collectionName, final Bson filter, final Object update) {
        return collExecutor(collectionName).updateAll(filter, update);
    }

    /**
     * Update the records in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param collectionName
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @param options
     * @return
     */
    public UpdateResult updateAll(final String collectionName, final Bson filter, final Object update, final UpdateOptions options) {
        return collExecutor(collectionName).updateAll(filter, update, options);
    }

    /**
     *
     * @param collectionName
     * @param objectId
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @return
     */
    public UpdateResult replace(final String collectionName, final String objectId, final Object replacement) {
        return collExecutor(collectionName).replace(objectId, replacement);
    }

    /**
     *
     * @param collectionName
     * @param objectId
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @return
     */
    public UpdateResult replace(final String collectionName, final ObjectId objectId, final Object replacement) {
        return collExecutor(collectionName).replace(objectId, replacement);
    }

    /**
     *
     * @param collectionName
     * @param filter
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @return
     */
    public UpdateResult replaceOne(final String collectionName, final Bson filter, final Object replacement) {
        return collExecutor(collectionName).replaceOne(filter, replacement);
    }

    /**
     *
     * @param collectionName
     * @param filter
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @param options
     * @return
     */
    public UpdateResult replaceOne(final String collectionName, final Bson filter, final Object replacement, final UpdateOptions options) {
        return collExecutor(collectionName).replaceOne(filter, replacement, options);
    }

    public DeleteResult delete(final String collectionName, final String objectId) {
        return collExecutor(collectionName).delete(objectId);
    }

    public DeleteResult delete(final String collectionName, final ObjectId objectId) {
        return collExecutor(collectionName).delete(objectId);
    }

    public DeleteResult deleteOne(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).deleteOne(filter);
    }

    public DeleteResult deleteAll(final String collectionName, final Bson filter) {
        return collExecutor(collectionName).deleteAll(filter);
    }

    public int bulkInsert(final String collectionName, final Collection<?> entities) {
        return collExecutor(collectionName).bulkInsert(entities);
    }

    public int bulkInsert(final String collectionName, final Collection<?> entities, final BulkWriteOptions options) {
        return collExecutor(collectionName).bulkInsert(entities, options);
    }

    public BulkWriteResult bulkWrite(final String collectionName, final List<? extends WriteModel<? extends Document>> requests) {
        return collExecutor(collectionName).bulkWrite(requests);
    }

    public BulkWriteResult bulkWrite(final String collectionName, final List<? extends WriteModel<? extends Document>> requests,
            final BulkWriteOptions options) {
        return collExecutor(collectionName).bulkWrite(requests, options);
    }

    public <T> Stream<T> distinct(final Class<T> targetClass, final String collectionName, final String fieldName) {
        return collExecutor(collectionName).distinct(targetClass, fieldName);
    }

    public <T> Stream<T> distinct(final Class<T> targetClass, final String collectionName, final String fieldName, final Bson filter) {
        return collExecutor(collectionName).distinct(targetClass, fieldName, filter);
    }

    public Stream<Document> aggregate(final String collectionName, final List<? extends Bson> pipeline) {
        return collExecutor(collectionName).aggregate(pipeline);
    }

    public <T> Stream<T> aggregate(final Class<T> targetClass, final String collectionName, final List<? extends Bson> pipeline) {
        return collExecutor(collectionName).aggregate(targetClass, pipeline);
    }

    public Stream<Document> mapReduce(final String collectionName, final String mapFunction, final String reduceFunction) {
        return collExecutor(collectionName).mapReduce(mapFunction, reduceFunction);
    }

    public <T> Stream<T> mapReduce(final Class<T> targetClass, final String collectionName, final String mapFunction, final String reduceFunction) {
        return collExecutor(collectionName).mapReduce(targetClass, mapFunction, reduceFunction);
    }

    @Beta
    public Stream<Document> groupBy(final String collectionName, final String fieldName) {
        return collExecutor(collectionName).groupBy(fieldName);
    }

    @Beta
    public Stream<Document> groupBy(final String collectionName, final Collection<String> fieldNames) {
        return collExecutor(collectionName).groupBy(fieldNames);
    }

    @Beta
    public Stream<Document> groupByAndCount(final String collectionName, final String fieldName) {
        return collExecutor(collectionName).groupByAndCount(fieldName);
    }

    @Beta
    public Stream<Document> groupByAndCount(final String collectionName, final Collection<String> fieldNames) {
        return collExecutor(collectionName).groupByAndCount(fieldNames);
    }

    private static <T> void checkTargetClass(final Class<T> targetClass) {
        if (!(N.isEntity(targetClass) || Map.class.isAssignableFrom(targetClass))) {
            throw new AbacusException("The target class must be an entity class with getter/setter methods or Map.class/Document.class. But it is: "
                    + ClassUtil.getCanonicalClassName(targetClass));
        }
    }

    private static class GeneralCodecRegistry implements CodecRegistry {
        private static final Map<Class<?>, Codec<?>> pool = new ObjectPool<Class<?>, Codec<?>>(128);

        @Override
        public <T> Codec<T> get(final Class<T> clazz) {
            Codec<?> codec = pool.get(clazz);

            if (codec == null) {
                codec = new GeneralCodec<T>(clazz);

                pool.put(clazz, codec);
            }

            return (Codec<T>) codec;
        }
    }

    private static class GeneralCodec<T> implements Codec<T> {
        private static final DocumentCodec documentCodec = new DocumentCodec(codecRegistry, new BsonTypeClassMap());
        private final Class<T> cls;
        private final boolean isEntityClass;

        public GeneralCodec(final Class<T> cls) {
            this.cls = cls;
            isEntityClass = N.isEntity(cls);
        }

        @Override
        public void encode(final BsonWriter writer, final T value, final EncoderContext encoderContext) {
            if (isEntityClass) {
                documentCodec.encode(writer, toDocument(value), encoderContext);
            } else {
                writer.writeString(N.stringOf(value));
            }
        }

        @Override
        public T decode(final BsonReader reader, final DecoderContext decoderContext) {
            if (isEntityClass) {
                return toEntity(cls, documentCodec.decode(reader, decoderContext));
            } else {
                return N.valueOf(cls, reader.readString());
            }
        }

        @Override
        public Class<T> getEncoderClass() {
            return cls;
        }
    }
}
