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

import java.util.Collection;
import java.util.Date;
import java.util.List;

import org.bson.Document;
import org.bson.conversions.Bson;
import org.bson.types.ObjectId;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.stream.Stream;
import com.mongodb.bulk.BulkWriteResult;
import com.mongodb.client.model.BulkWriteOptions;
import com.mongodb.client.model.CountOptions;
import com.mongodb.client.model.DeleteOptions;
import com.mongodb.client.model.InsertManyOptions;
import com.mongodb.client.model.InsertOneOptions;
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
public final class MongoCollectionMapper<T> {
    private final MongoCollectionExecutor collExecutor;
    private final Class<T> targetClass;

    MongoCollectionMapper(final MongoCollectionExecutor collExecutor, final Class<T> targetClass) {
        this.collExecutor = collExecutor;
        this.targetClass = targetClass;
    }

    public MongoCollectionExecutor collExecutor() {
        return collExecutor;
    }

    public boolean exists(final String objectId) {
        return collExecutor.exists(objectId);
    }

    public boolean exists(final ObjectId objectId) {
        return collExecutor.exists(objectId);
    }

    public boolean exists(final Bson filter) {
        return collExecutor.exists(filter);
    }

    public long count() {
        return collExecutor.count();
    }

    public long count(final Bson filter) {
        return collExecutor.count(filter);
    }

    public long count(final Bson filter, final CountOptions options) {
        return collExecutor.count(filter, options);
    }

    public Optional<T> get(final String objectId) {
        return collExecutor.get(targetClass, objectId);
    }

    public Optional<T> get(final ObjectId objectId) {
        return collExecutor.get(targetClass, objectId);
    }

    public Optional<T> get(final String objectId, final Collection<String> selectPropNames) {
        return collExecutor.get(targetClass, objectId, selectPropNames);
    }

    public Optional<T> get(final ObjectId objectId, final Collection<String> selectPropNames) {
        return collExecutor.get(targetClass, objectId, selectPropNames);
    }

    public T gett(final String objectId) {
        return collExecutor.gett(targetClass, objectId);
    }

    public T gett(final ObjectId objectId) {
        return collExecutor.gett(targetClass, objectId);
    }

    public T gett(final String objectId, final Collection<String> selectPropNames) {
        return collExecutor.gett(targetClass, objectId, selectPropNames);
    }

    public T gett(final ObjectId objectId, final Collection<String> selectPropNames) {
        return collExecutor.gett(targetClass, objectId, selectPropNames);
    }

    public Optional<T> findFirst(final Bson filter) {
        return collExecutor.findFirst(targetClass, filter);
    }

    public Optional<T> findFirst(final Collection<String> selectPropNames, final Bson filter) {
        return collExecutor.findFirst(targetClass, selectPropNames, filter);
    }

    public Optional<T> findFirst(final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return collExecutor.findFirst(targetClass, selectPropNames, filter, sort);
    }

    public Optional<T> findFirst(final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor.findFirst(targetClass, filter, sort, projection);
    }

    public List<T> list(final Bson filter) {
        return collExecutor.list(targetClass, filter);
    }

    public List<T> list(final Collection<String> selectPropNames, final Bson filter) {
        return collExecutor.list(targetClass, selectPropNames, filter);
    }

    public List<T> list(final Collection<String> selectPropNames, final Bson filter, final int offset, final int count) {
        return collExecutor.list(targetClass, selectPropNames, filter, offset, count);
    }

    public List<T> list(final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return collExecutor.list(targetClass, selectPropNames, filter, sort);
    }

    public List<T> list(final Collection<String> selectPropNames, final Bson filter, final Bson sort, final int offset, final int count) {
        return collExecutor.list(targetClass, selectPropNames, filter, sort, offset, count);
    }

    public List<T> list(final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor.list(targetClass, filter, sort, projection);
    }

    public List<T> list(final Bson filter, final Bson sort, final Bson projection, final int offset, final int count) {
        return collExecutor.list(targetClass, filter, sort, projection, offset, count);
    }

    @Beta
    public OptionalBoolean queryForBoolean(final String propName, final Bson filter) {
        return collExecutor.queryForBoolean(propName, filter);
    }

    @Beta
    public OptionalChar queryForChar(final String propName, final Bson filter) {
        return collExecutor.queryForChar(propName, filter);
    }

    @Beta
    public OptionalByte queryForByte(final String propName, final Bson filter) {
        return collExecutor.queryForByte(propName, filter);
    }

    @Beta
    public OptionalShort queryForShort(final String propName, final Bson filter) {
        return collExecutor.queryForShort(propName, filter);
    }

    @Beta
    public OptionalInt queryForInt(final String propName, final Bson filter) {
        return collExecutor.queryForInt(propName, filter);
    }

    @Beta
    public OptionalLong queryForLong(final String propName, final Bson filter) {
        return collExecutor.queryForLong(propName, filter);
    }

    @Beta
    public OptionalFloat queryForFloat(final String propName, final Bson filter) {
        return collExecutor.queryForFloat(propName, filter);
    }

    @Beta
    public OptionalDouble queryForDouble(final String propName, final Bson filter) {
        return collExecutor.queryForDouble(propName, filter);
    }

    @Beta
    public Nullable<String> queryForString(final String propName, final Bson filter) {
        return collExecutor.queryForString(propName, filter);
    }

    @Beta
    public Nullable<Date> queryForDate(final String propName, final Bson filter) {
        return collExecutor.queryForDate(propName, filter);
    }

    public <P extends Date> Nullable<P> queryForDate(final Class<P> targetPropClass, final String propName, final Bson filter) {
        return collExecutor.queryForDate(targetPropClass, propName, filter);
    }

    public <V> Nullable<V> queryForSingleResult(final Class<V> targetPropClass, final String propName, final Bson filter) {
        return collExecutor.queryForSingleResult(targetPropClass, propName, filter);
    }

    public DataSet query(final Bson filter) {
        return collExecutor.query(targetClass, filter);
    }

    public DataSet query(final Collection<String> selectPropNames, final Bson filter) {
        return collExecutor.query(targetClass, selectPropNames, filter);
    }

    public DataSet query(final Collection<String> selectPropNames, final Bson filter, final int offset, final int count) {
        return collExecutor.query(targetClass, selectPropNames, filter, offset, count);
    }

    public DataSet query(final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return collExecutor.query(targetClass, selectPropNames, filter, sort);
    }

    public DataSet query(final Collection<String> selectPropNames, final Bson filter, final Bson sort, final int offset, final int count) {
        return collExecutor.query(targetClass, selectPropNames, filter, sort, offset, count);
    }

    public DataSet query(final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor.query(targetClass, filter, sort, projection);
    }

    public DataSet query(final Bson filter, final Bson sort, final Bson projection, final int offset, final int count) {
        return collExecutor.query(targetClass, filter, sort, projection, offset, count);
    }

    public Stream<T> stream(final Bson filter) {
        return collExecutor.stream(targetClass, filter);
    }

    public Stream<T> stream(final Collection<String> selectPropNames, final Bson filter) {
        return collExecutor.stream(targetClass, selectPropNames, filter);
    }

    public Stream<T> stream(final Collection<String> selectPropNames, final Bson filter, final int offset, final int count) {
        return collExecutor.stream(targetClass, selectPropNames, filter, offset, count);
    }

    public Stream<T> stream(final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return collExecutor.stream(targetClass, selectPropNames, filter, sort);
    }

    public Stream<T> stream(final Collection<String> selectPropNames, final Bson filter, final Bson sort, final int offset, final int count) {
        return collExecutor.stream(targetClass, selectPropNames, filter, sort, offset, count);
    }

    public Stream<T> stream(final Bson filter, final Bson sort, final Bson projection) {
        return collExecutor.stream(targetClass, filter, sort, projection);
    }

    public Stream<T> stream(final Bson filter, final Bson sort, final Bson projection, final int offset, final int count) {
        return collExecutor.stream(targetClass, filter, sort, projection, offset, count);
    }

    public void insert(final T obj) {
        collExecutor.insert(obj);
    }

    public void insert(final T obj, final InsertOneOptions options) {
        collExecutor.insert(obj, options);
    }

    public void insertAll(final Collection<? extends T> objList) {
        collExecutor.insertAll(objList);
    }

    public void insertAll(final Collection<? extends T> objList, final InsertManyOptions options) {
        collExecutor.insertAll(objList, options);
    }

    public UpdateResult update(final String objectId, final T update) {
        return collExecutor.update(objectId, update);
    }

    public UpdateResult update(final ObjectId objectId, final T update) {
        return collExecutor.update(objectId, update);
    }

    public UpdateResult updateOne(final Bson filter, final T update) {
        return collExecutor.updateOne(filter, update);
    }

    public UpdateResult updateOne(final Bson filter, final T update, final UpdateOptions options) {
        return collExecutor.updateOne(filter, update, options);
    }

    public UpdateResult updateAll(final Bson filter, final T update) {
        return collExecutor.updateAll(filter, update);
    }

    public UpdateResult updateAll(final Bson filter, final T update, final UpdateOptions options) {
        return collExecutor.updateAll(filter, update, options);
    }

    public UpdateResult replace(final String objectId, final T replacement) {
        return collExecutor.replace(objectId, replacement);
    }

    public UpdateResult replace(final ObjectId objectId, final T replacement) {
        return collExecutor.replace(objectId, replacement);
    }

    public UpdateResult replaceOne(final Bson filter, final T replacement) {
        return collExecutor.replaceOne(filter, replacement);
    }

    public UpdateResult replaceOne(final Bson filter, final T replacement, final UpdateOptions options) {
        return collExecutor.replaceOne(filter, replacement, options);
    }

    public DeleteResult delete(final String objectId) {
        return collExecutor.delete(objectId);
    }

    public DeleteResult delete(final ObjectId objectId) {
        return collExecutor.delete(objectId);
    }

    public DeleteResult deleteOne(final Bson filter) {
        return collExecutor.deleteOne(filter);
    }

    public DeleteResult deleteOne(final Bson filter, final DeleteOptions options) {
        return collExecutor.deleteOne(filter, options);
    }

    public DeleteResult deleteAll(final Bson filter) {
        return collExecutor.deleteAll(filter);
    }

    public DeleteResult deleteAll(Bson filter, DeleteOptions options) {
        return collExecutor.deleteAll(filter, options);
    }

    public int bulkInsert(final Collection<? extends T> entities) {
        return collExecutor.bulkInsert(entities);
    }

    public int bulkInsert(final Collection<? extends T> entities, final BulkWriteOptions options) {
        return collExecutor.bulkInsert(entities, options);
    }

    public BulkWriteResult bulkWrite(final List<? extends WriteModel<? extends Document>> requests) {
        return collExecutor.bulkWrite(requests);
    }

    public BulkWriteResult bulkWrite(final List<? extends WriteModel<? extends Document>> requests, final BulkWriteOptions options) {
        return collExecutor.bulkWrite(requests, options);
    }

    public Stream<T> distinct(final String fieldName) {
        return collExecutor.distinct(targetClass, fieldName);
    }

    public Stream<T> distinct(final String fieldName, final Bson filter) {
        return collExecutor.distinct(targetClass, fieldName, filter);
    }

    public Stream<T> aggregate(final List<? extends Bson> pipeline) {
        return collExecutor.aggregate(targetClass, pipeline);
    }

    public Stream<T> mapReduce(final String mapFunction, final String reduceFunction) {
        return collExecutor.mapReduce(targetClass, mapFunction, reduceFunction);
    }

    // TODO
    //    @Beta
    //    public Stream<Document> groupBy(final String fieldName) {
    //        return collExecutor.groupBy(fieldName);
    //    }
    //
    //    @Beta
    //    public Stream<Document> groupBy(final Collection<String> fieldNames) {
    //        return collExecutor.groupBy(fieldNames);
    //    }
    //
    //    @Beta
    //    public Stream<Document> groupByAndCount(final String fieldName) {
    //        return collExecutor.groupByAndCount(fieldName);
    //    }
    //
    //    @Beta
    //    public Stream<Document> groupByAndCount(final Collection<String> fieldNames) {
    //        return collExecutor.groupByAndCount(fieldNames);
    //    }
}
