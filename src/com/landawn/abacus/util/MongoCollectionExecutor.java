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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.stream.Stream;
import com.mongodb.BasicDBObject;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.FindIterable;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.model.BulkWriteOptions;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.InsertManyOptions;
import com.mongodb.client.model.InsertOneModel;
import com.mongodb.client.model.Projections;
import com.mongodb.client.model.UpdateOptions;
import com.mongodb.client.model.WriteModel;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.UpdateResult;

/**
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class MongoCollectionExecutor {
    private static final String _$ = "$";
    private static final String _$SET = "$set";
    private static final String _$GROUP = "$group";
    private static final String _$SUM = "$sum";
    private static final String _COUNT = "count";

    private final MongoDBExecutor dbExecutor;
    private final MongoCollection<Document> coll;
    private final AsyncMongoCollectionExecutor asyncCollExecutor;

    /**
     * Call <code>mongoDB.withCodecRegistry(CodecRegistries.fromRegistries(MongoClient.getDefaultCodecRegistry(), new GeneralCodecRegistry()));</code> to support the encode/decode for general type
     * @param coll
     */
    MongoCollectionExecutor(final MongoDBExecutor dbExecutor, final MongoCollection<Document> coll) {
        this(dbExecutor, coll, new AsyncExecutor(64, 300, TimeUnit.SECONDS));
    }

    MongoCollectionExecutor(final MongoDBExecutor dbExecutor, final MongoCollection<Document> coll, final AsyncExecutor asyncExecutor) {
        this.dbExecutor = dbExecutor;
        this.coll = coll;
        this.asyncCollExecutor = new AsyncMongoCollectionExecutor(this, asyncExecutor);
    }

    public MongoDBExecutor dbExecutor() {
        return dbExecutor;
    }

    public MongoCollection<Document> coll() {
        return coll;
    }

    public AsyncMongoCollectionExecutor async() {
        return asyncCollExecutor;
    }

    public boolean exists(final String objectId) {
        return exists(createObjectId(objectId));
    }

    public boolean exists(final ObjectId objectId) {
        return exists(createFilter(objectId));
    }

    public boolean exists(final Bson filter) {
        return coll.count(filter, new CountOptions().limit(1)) > 0;
    }

    public long count() {
        return coll.count();
    }

    public long count(final Bson filter) {
        return coll.count(filter);
    }

    public long count(final Bson filter, final CountOptions options) {
        if (options == null) {
            return coll.count(filter);
        } else {
            return coll.count(filter, options);
        }
    }

    public Document get(final String objectId) {
        return get(createObjectId(objectId));
    }

    public Document get(final ObjectId objectId) {
        return get(Document.class, objectId);
    }

    public <T> T get(final Class<T> targetClass, final String objectId) {
        return get(targetClass, createObjectId(objectId));
    }

    public <T> T get(final Class<T> targetClass, final ObjectId objectId) {
        return get(targetClass, objectId, null);
    }

    public <T> T get(final Class<T> targetClass, final String objectId, final Collection<String> selectPropNames) {
        return get(targetClass, createObjectId(objectId), selectPropNames);
    }

    public <T> T get(final Class<T> targetClass, final ObjectId objectId, final Collection<String> selectPropNames) {
        return queryForEntity(targetClass, selectPropNames, createFilter(objectId), null).orElse(null);
    }

    public <T> NullabLe<T> queryForSingleResult(final Class<T> targetClass, final String propName, final Bson filter) {
        final FindIterable<Document> findIterable = query(N.asList(propName), filter, null, 0, 1);

        Document doc = findIterable.first();

        return N.isNullOrEmpty(doc) ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.as(targetClass, doc.get(propName)));
    }

    public Optional<Document> queryForEntity(final Bson filter) {
        return queryForEntity(Document.class, filter);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final Bson filter) {
        return queryForEntity(targetClass, null, filter);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return queryForEntity(targetClass, selectPropNames, filter, null);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        final FindIterable<Document> findIterable = query(selectPropNames, filter, sort, 0, 1);

        final T result = toEntity(targetClass, findIterable);

        return result == null ? (Optional<T>) Optional.empty() : Optional.of(result);
    }

    public <T> Optional<T> queryForEntity(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        final FindIterable<Document> findIterable = query(filter, sort, projection, 0, 1);

        final T result = toEntity(targetClass, findIterable);

        return result == null ? (Optional<T>) Optional.empty() : Optional.of(result);
    }

    private <T> T toEntity(final Class<T> targetClass, final FindIterable<Document> findIterable) {
        final Document doc = findIterable.first();

        return N.isNullOrEmpty(doc) ? null : MongoDBExecutor.toEntity(targetClass, doc);
    }

    public List<Document> find(final Bson filter) {
        return find(Document.class, filter);
    }

    public <T> List<T> find(final Class<T> targetClass, final Bson filter) {
        return find(targetClass, null, filter);
    }

    public <T> List<T> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return find(targetClass, selectPropNames, filter, 0, Integer.MAX_VALUE);
    }

    public <T> List<T> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset, final int count) {
        return find(targetClass, selectPropNames, filter, null, offset, count);
    }

    public <T> List<T> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return find(targetClass, selectPropNames, filter, sort, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param selectPropNames
     * @param filter
     * @param sort
     * @param offset
     * @param count
     * @return
     */
    public <T> List<T> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort, final int offset,
            final int count) {
        final FindIterable<Document> findIterable = query(selectPropNames, filter, sort, offset, count);

        return MongoDBExecutor.toList(targetClass, findIterable);
    }

    public <T> List<T> find(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return find(targetClass, filter, sort, projection, 0, Integer.MAX_VALUE);
    }

    /**
     * 
     * @param targetClass an entity class with getter/setter method, <code>Map.class</code> or basic single value type(Primitive/String/Date...)
     * @param filter
     * @param sort
     * @param projection
     * @param offset
     * @param count
     * @return
     */
    public <T> List<T> find(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset, final int count) {
        final FindIterable<Document> findIterable = query(filter, sort, projection, offset, count);

        return MongoDBExecutor.toList(targetClass, findIterable);
    }

    public DataSet query(final Bson filter) {
        return query(Document.class, filter);
    }

    public <T> DataSet query(final Class<T> targetClass, final Bson filter) {
        return query(targetClass, null, filter);
    }

    public <T> DataSet query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return query(targetClass, selectPropNames, filter, 0, Integer.MAX_VALUE);
    }

    public <T> DataSet query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset, final int count) {
        return query(targetClass, selectPropNames, filter, null, offset, count);
    }

    public <T> DataSet query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return query(targetClass, selectPropNames, filter, sort, 0, Integer.MAX_VALUE);
    }

    public <T> DataSet query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort, final int offset,
            final int count) {
        final FindIterable<Document> findIterable = query(selectPropNames, filter, sort, offset, count);

        if (N.isNullOrEmpty(selectPropNames)) {
            return MongoDBExecutor.extractData(targetClass, findIterable);
        } else {
            return MongoDBExecutor.extractData(targetClass, selectPropNames, findIterable);
        }
    }

    public <T> DataSet query(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return query(targetClass, filter, sort, projection, 0, Integer.MAX_VALUE);
    }

    public <T> DataSet query(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset, final int count) {
        final FindIterable<Document> findIterable = query(filter, sort, projection, offset, count);

        return MongoDBExecutor.extractData(targetClass, findIterable);
    }

    public Stream<Document> stream(final Bson filter) {
        return stream(Document.class, filter);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Bson filter) {
        return stream(targetClass, null, filter);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return stream(targetClass, selectPropNames, filter, 0, Integer.MAX_VALUE);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset, final int count) {
        return stream(targetClass, selectPropNames, filter, null, offset, count);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return stream(targetClass, selectPropNames, filter, sort, 0, Integer.MAX_VALUE);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort, final int offset,
            final int count) {
        final FindIterable<Document> findIterable = query(selectPropNames, filter, sort, offset, count);

        if (targetClass.isAssignableFrom(Document.class)) {
            return (Stream<T>) Stream.of(findIterable.iterator());
        } else {
            return Stream.of(findIterable.iterator()).map(toEntity(targetClass));
        }
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return stream(targetClass, filter, sort, projection, 0, Integer.MAX_VALUE);
    }

    public <T> Stream<T> stream(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset, final int count) {
        final FindIterable<Document> findIterable = query(filter, sort, projection, offset, count);

        if (targetClass.isAssignableFrom(Document.class)) {
            return (Stream<T>) Stream.of(findIterable.iterator());
        } else {
            return Stream.of(findIterable.iterator()).map(toEntity(targetClass));
        }
    }

    private <T> Function<Document, T> toEntity(final Class<T> targetClass) {
        return new Function<Document, T>() {
            @Override
            public T apply(Document t) {
                return MongoDBExecutor.toEntity(targetClass, t);
            }
        };
    }

    private FindIterable<Document> query(final Collection<String> selectPropNames, final Bson filter, final Bson sort, final int offset, final int count) {
        if (N.isNullOrEmpty(selectPropNames)) {
            return this.query(filter, sort, null, offset, count);
        } else if (selectPropNames instanceof List) {
            return this.query(filter, sort, Projections.include((List<String>) selectPropNames), offset, count);
        } else {
            return this.query(filter, sort, Projections.include(selectPropNames.toArray(new String[selectPropNames.size()])), offset, count);
        }
    }

    private FindIterable<Document> query(final Bson filter, final Bson sort, final Bson projection, final int offset, final int count) {
        if (offset < 0 || count < 0) {
            throw new IllegalArgumentException("offset (" + offset + ") and count(" + count + ") can't be negative");
        }

        FindIterable<Document> findIterable = coll.find(filter);

        if (projection != null) {
            findIterable = findIterable.projection(projection);
        }

        if (sort != null) {
            findIterable = findIterable.sort(sort);
        }

        if (offset > 0) {
            findIterable = findIterable.skip(offset);
        }

        if (count < Integer.MAX_VALUE) {
            findIterable = findIterable.limit(count);
        }

        return findIterable;
    }

    /**
     *
     * @param obj can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     */
    public void insert(final Object obj) {
        coll.insertOne(createDocument(obj));
    }

    /**
     *
     * @param objList list of <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     */
    public void insert(final Collection<?> objList) {
        insert(objList, null);
    }

    /**
     *
     * @param objList list of <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @param options
     */
    public void insert(final Collection<?> objList, final InsertManyOptions options) {
        List<Document> docs = null;

        if (objList.iterator().next() instanceof Document) {
            if (objList instanceof List) {
                docs = (List<Document>) objList;
            } else {
                docs = new ArrayList<>((Collection<Document>) objList);
            }
        } else {
            docs = new ArrayList<>(objList.size());

            for (Object entity : objList) {
                docs.add(createDocument(entity));
            }
        }

        if (options == null) {
            coll.insertMany(docs);
        } else {
            coll.insertMany(docs, options);
        }
    }

    /**
     * Update the record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>objectId</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param objectId
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult update(final String objectId, final Object update) {
        return update(createObjectId(objectId), update);
    }

    /**
     * Update the record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>objectId</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param objectId
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult update(final ObjectId objectId, final Object update) {
        return updateOne(createFilter(objectId), update);
    }

    /**
     * Just update one record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult updateOne(final Bson filter, final Object update) {
        return updateOne(filter, update, null);
    }

    /**
     * Just update one record in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @param options
     * @return
     */
    public UpdateResult updateOne(final Bson filter, final Object update, final UpdateOptions options) {
        if (options == null) {
            return coll.updateOne(filter, checkUpdate(update));
        } else {
            return coll.updateOne(filter, checkUpdate(update), options);
        }
    }

    /**
     * Update the records in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @return
     */
    public UpdateResult updateAll(final Bson filter, final Object update) {
        return updateAll(filter, update, null);
    }

    /**
     * Update the records in data store with the properties which have been updated/set in the specified <code>update</code> by the specified <code>filter</code>.
     * if the <code>update</code> implements <code>DirtyMarker</code> interface, just update the dirty properties.
     *
     * @param filter
     * @param update can be <code>Bson/Document/Map<String, Object>/entity class with getter/setter method.
     * @param options
     * @return
     */
    public UpdateResult updateAll(final Bson filter, final Object update, final UpdateOptions options) {
        if (options == null) {
            return coll.updateMany(filter, checkUpdate(update));
        } else {
            return coll.updateMany(filter, checkUpdate(update), options);
        }
    }

    /**
     *
     * @param objectId
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @return
     */
    public UpdateResult replace(final String objectId, final Object replacement) {
        return replace(createObjectId(objectId), replacement);
    }

    /**
     *
     * @param objectId
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @return
     */
    public UpdateResult replace(final ObjectId objectId, final Object replacement) {
        return replaceOne(createFilter(objectId), replacement);
    }

    /**
     *
     * @param filter
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @return
     */
    public UpdateResult replaceOne(final Bson filter, final Object replacement) {
        return replaceOne(filter, replacement, null);
    }

    /**
     *
     * @param filter
     * @param replacement can be <code>Document/Map<String, Object>/entity</code> class with getter/setter method.
     * @param options
     * @return
     */
    public UpdateResult replaceOne(final Bson filter, final Object replacement, final UpdateOptions options) {
        if (options == null) {
            return coll.replaceOne(filter, createDocument(replacement));
        } else {
            return coll.replaceOne(filter, createDocument(replacement), options);
        }
    }

    public DeleteResult delete(final String objectId) {
        return delete(createObjectId(objectId));
    }

    public DeleteResult delete(final ObjectId objectId) {
        return deleteOne(createFilter(objectId));
    }

    public DeleteResult deleteOne(final Bson filter) {
        return coll.deleteOne(filter);
    }

    public DeleteResult deleteAll(final Bson filter) {
        return coll.deleteMany(filter);
    }

    public int bulkInsert(final Collection<?> entities) {
        return bulkInsert(entities, null);
    }

    public int bulkInsert(final Collection<?> entities, final BulkWriteOptions options) {
        final List<InsertOneModel<Document>> list = new ArrayList<>(entities.size());

        for (Object entity : entities) {
            if (entity instanceof Document) {
                list.add(new InsertOneModel<Document>((Document) entity));
            } else {
                list.add(new InsertOneModel<Document>(MongoDBExecutor.toDocument(entity)));
            }
        }

        return bulkWrite(list, options).getInsertedCount();
    }

    public BulkWriteResult bulkWrite(final List<? extends WriteModel<? extends Document>> requests) {
        return bulkWrite(requests, null);
    }

    public BulkWriteResult bulkWrite(final List<? extends WriteModel<? extends Document>> requests, final BulkWriteOptions options) {
        if (options == null) {
            return coll.bulkWrite(requests);
        } else {
            return coll.bulkWrite(requests, options);
        }
    }

    public <T> Stream<T> distinct(final Class<T> targetClass, final String fieldName) {
        return Stream.of(coll.distinct(fieldName, targetClass).iterator());
    }

    public <T> Stream<T> distinct(final Class<T> targetClass, final String fieldName, final Bson filter) {
        return Stream.of(coll.distinct(fieldName, filter, targetClass).iterator());
    }

    public Stream<Document> aggregate(final List<? extends Bson> pipeline) {
        return aggregate(Document.class, pipeline);
    }

    public <T> Stream<T> aggregate(final Class<T> targetClass, final List<? extends Bson> pipeline) {
        return Stream.of(coll.aggregate(pipeline, Document.class).iterator()).map(toEntity(targetClass));
    }

    public Stream<Document> mapReduce(final String mapFunction, final String reduceFunction) {
        return mapReduce(Document.class, mapFunction, reduceFunction);
    }

    public <T> Stream<T> mapReduce(final Class<T> targetClass, final String mapFunction, final String reduceFunction) {
        return Stream.of(coll.mapReduce(mapFunction, reduceFunction, Document.class).iterator()).map(toEntity(targetClass));
    }

    @Beta
    public Stream<Document> groupBy(final String fieldName) {
        return aggregate(N.asList(new Document(_$GROUP, new Document(MongoDBExecutor._ID, _$ + fieldName)))) ;
    }

    @Beta
    public Stream<Document> groupBy(final Collection<String> fieldNames) {
        final Document groupFields = new Document();

        for (String fieldName : fieldNames) {
            groupFields.put(fieldName, _$ + fieldName);
        }

        return aggregate(N.asList(new Document(_$GROUP, new Document(MongoDBExecutor._ID, groupFields)))) ;
    }

    @Beta
    public Stream<Document> groupByAndCount(final String fieldName) {
        return aggregate(N.asList(new Document(_$GROUP, new Document(MongoDBExecutor._ID, _$ + fieldName).append(_COUNT, new Document(_$SUM, 1)))))
                 ;
    }

    @Beta
    public Stream<Document> groupByAndCount(final Collection<String> fieldNames) {
        final Document groupFields = new Document();

        for (String fieldName : fieldNames) {
            groupFields.put(fieldName, _$ + fieldName);
        }

        return aggregate(N.asList(new Document(_$GROUP, new Document(MongoDBExecutor._ID, groupFields).append(_COUNT, new Document(_$SUM, 1))))) ;
    }
 

    //
    //    private String getCollectionName(final Class<?> cls) {
    //        final String collectionName = classCollectionMapper.get(cls);
    //
    //        if (N.isNullOrEmpty(collectionName)) {
    //            throw new IllegalArgumentException("No collection is mapped to class: " + cls);
    //        }
    //        return collectionName;
    //    }

    //
    //    private String getObjectId(Object obj) {
    //        String objectId = null;
    //
    //        try {
    //            objectId = N.as(String.class, N.getPropValue(obj, "id"));
    //        } catch (Exception e) {
    //            // ignore
    //
    //            try {
    //                objectId = N.as(String.class, N.getPropValue(obj, "objectId"));
    //            } catch (Exception e2) {
    //                // ignore
    //            }
    //        }
    //
    //        if (N.isNullOrEmpty(objectId)) {
    //            throw new IllegalArgumentException("Property value of 'id' or 'objectId' can't be null or empty for update or delete");
    //        }
    //
    //        return objectId;
    //    }
    //

    private Bson checkUpdate(final Object update) {
        Bson bson = update instanceof Bson ? (Bson) update : MongoDBExecutor.toDocument(update, true);

        if (bson instanceof Document) {
            Document doc = (Document) bson;

            if (doc.size() > 0 && doc.keySet().iterator().next().startsWith(_$)) {
                return doc;
            }
        } else if (bson instanceof BasicDBObject) {
            BasicDBObject dbObject = (BasicDBObject) bson;

            if (dbObject.size() > 0 && dbObject.keySet().iterator().next().startsWith(_$)) {
                return dbObject;
            }
        }

        return new Document(_$SET, bson);
    }

    private ObjectId createObjectId(final String objectId) {
        if (N.isNullOrEmpty(objectId)) {
            throw new IllegalArgumentException("Object id cant' be null or empty");
        }

        return new ObjectId(objectId);
    }

    private Bson createFilter(final ObjectId objectId) {
        return new Document(MongoDBExecutor._ID, objectId);
    }

    private Document createDocument(final Object obj) {
        return obj instanceof Document ? (Document) obj : MongoDBExecutor.toDocument(obj);
    }
}
