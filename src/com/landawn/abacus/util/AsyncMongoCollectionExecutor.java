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
import java.util.concurrent.Callable;

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
 * Asynchronous <code>MongoCollectionExecutor</code>.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class AsyncMongoCollectionExecutor {
    private final MongoCollectionExecutor collExecutor;
    private final AsyncExecutor asyncExecutor;

    AsyncMongoCollectionExecutor(final MongoCollectionExecutor collExecutor, final AsyncExecutor asyncExecutor) {
        this.collExecutor = collExecutor;
        this.asyncExecutor = asyncExecutor;
    }

    public MongoCollectionExecutor sync() {
        return collExecutor;
    }

    public ContinuableFuture<Boolean> exists(final String objectId) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return collExecutor.exists(objectId);
            }
        });
    }

    public ContinuableFuture<Boolean> exists(final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return collExecutor.exists(objectId);
            }
        });
    }

    public ContinuableFuture<Boolean> exists(final Bson filter) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return collExecutor.exists(filter);
            }
        });
    }

    public ContinuableFuture<Long> count() {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return collExecutor.count();
            }
        });
    }

    public ContinuableFuture<Long> count(final Bson filter) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return collExecutor.count(filter);
            }
        });
    }

    public ContinuableFuture<Long> count(final Bson filter, final CountOptions options) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return collExecutor.count(filter, options);
            }
        });
    }

    public ContinuableFuture<Document> get(final String objectId) {
        return asyncExecutor.execute(new Callable<Document>() {
            @Override
            public Document call() throws Exception {
                return collExecutor.get(objectId);
            }
        });
    }

    public ContinuableFuture<Document> get(final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Document>() {
            @Override
            public Document call() throws Exception {
                return collExecutor.get(objectId);
            }
        });
    }

    public <T> ContinuableFuture<T> get(final Class<T> targetClass, final String objectId) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId);
            }
        });
    }

    public <T> ContinuableFuture<T> get(final Class<T> targetClass, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId);
            }
        });
    }

    public <T> ContinuableFuture<T> get(final Class<T> targetClass, final String objectId, final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId, selectPropNames);
            }
        });
    }

    public <T> ContinuableFuture<T> get(final Class<T> targetClass, final ObjectId objectId, final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId, selectPropNames);
            }
        });
    }

    public ContinuableFuture<Optional<Document>> gett(final String objectId) {
        return asyncExecutor.execute(new Callable<Optional<Document>>() {
            @Override
            public Optional<Document> call() throws Exception {
                return collExecutor.gett(objectId);
            }
        });
    }

    public ContinuableFuture<Optional<Document>> gett(final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Optional<Document>>() {
            @Override
            public Optional<Document> call() throws Exception {
                return collExecutor.gett(objectId);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final String objectId) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.gett(targetClass, objectId);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.gett(targetClass, objectId);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final String objectId, final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.gett(targetClass, objectId, selectPropNames);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> gett(final Class<T> targetClass, final ObjectId objectId, final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.gett(targetClass, objectId, selectPropNames);
            }
        });
    }

    public ContinuableFuture<Optional<Document>> findFirst(final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<Document>>() {
            @Override
            public Optional<Document> call() throws Exception {
                return collExecutor.findFirst(filter);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.findFirst(targetClass, filter);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.findFirst(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.findFirst(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> ContinuableFuture<Optional<T>> findFirst(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.findFirst(targetClass, filter, sort, projection);
            }
        });
    }

    public ContinuableFuture<List<Document>> find(final Bson filter) {
        return asyncExecutor.execute(new Callable<List<Document>>() {
            @Override
            public List<Document> call() throws Exception {
                return collExecutor.find(filter);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, filter);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort,
            final int offset, final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, filter, sort, projection);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> find(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, filter, sort, projection, offset, count);
            }
        });
    }

    public ContinuableFuture<OptionalBoolean> queryForBoolean(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalBoolean>() {
            @Override
            public OptionalBoolean call() throws Exception {
                return collExecutor.queryForBoolean(propName, filter);
            }
        });
    }

    public ContinuableFuture<OptionalChar> queryForChar(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalChar>() {
            @Override
            public OptionalChar call() throws Exception {
                return collExecutor.queryForChar(propName, filter);
            }
        });
    }

    public ContinuableFuture<OptionalByte> queryForByte(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalByte>() {
            @Override
            public OptionalByte call() throws Exception {
                return collExecutor.queryForByte(propName, filter);
            }
        });
    }

    public ContinuableFuture<OptionalShort> queryForShort(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalShort>() {
            @Override
            public OptionalShort call() throws Exception {
                return collExecutor.queryForShort(propName, filter);
            }
        });
    }

    public ContinuableFuture<OptionalInt> queryForInt(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalInt>() {
            @Override
            public OptionalInt call() throws Exception {
                return collExecutor.queryForInt(propName, filter);
            }
        });
    }

    public ContinuableFuture<OptionalLong> queryForLong(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalLong>() {
            @Override
            public OptionalLong call() throws Exception {
                return collExecutor.queryForLong(propName, filter);
            }
        });
    }

    public ContinuableFuture<OptionalFloat> queryForFloat(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalFloat>() {
            @Override
            public OptionalFloat call() throws Exception {
                return collExecutor.queryForFloat(propName, filter);
            }
        });
    }

    public ContinuableFuture<OptionalDouble> queryForDouble(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalDouble>() {
            @Override
            public OptionalDouble call() throws Exception {
                return collExecutor.queryForDouble(propName, filter);
            }
        });
    }

    public ContinuableFuture<Nullable<String>> queryForString(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<String>>() {
            @Override
            public Nullable<String> call() throws Exception {
                return collExecutor.queryForString(propName, filter);
            }
        });
    }

    public ContinuableFuture<Nullable<Date>> queryForDate(final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<Date>>() {
            @Override
            public Nullable<Date> call() throws Exception {
                return collExecutor.queryForDate(propName, filter);
            }
        });
    }

    public <T extends Date> ContinuableFuture<Nullable<T>> queryForDate(final Class<T> targetClass, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return collExecutor.queryForDate(targetClass, propName, filter);
            }
        });
    }

    public <T> ContinuableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return collExecutor.queryForSingleResult(targetClass, propName, filter);
            }
        });
    }

    public ContinuableFuture<DataSet> query(final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(filter);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, filter);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort,
            final int offset, final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, filter, sort, projection);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, filter, sort, projection, offset, count);
            }
        });
    }

    public ContinuableFuture<Stream<Document>> stream(final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.stream(filter);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, filter);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort,
            final int offset, final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, filter, sort, projection);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, filter, sort, projection, offset, count);
            }
        });
    }

    public ContinuableFuture<Void> insert(final Object obj) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                collExecutor.insert(obj);
                return null;
            }
        });
    }

    public ContinuableFuture<Void> insert(final Object obj, final InsertOneOptions options) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                collExecutor.insert(obj, options);
                return null;
            }
        });
    }

    public ContinuableFuture<Void> insertAll(final Collection<?> objList) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                collExecutor.insertAll(objList);
                return null;
            }
        });
    }

    public ContinuableFuture<Void> insertAll(final Collection<?> objList, final InsertManyOptions options) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                collExecutor.insertAll(objList, options);
                return null;
            }
        });
    }

    public ContinuableFuture<UpdateResult> update(final String objectId, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.update(objectId, update);
            }
        });
    }

    public ContinuableFuture<UpdateResult> update(final ObjectId objectId, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.update(objectId, update);
            }
        });
    }

    public ContinuableFuture<UpdateResult> updateOne(final Bson filter, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateOne(filter, update);
            }
        });
    }

    public ContinuableFuture<UpdateResult> updateOne(final Bson filter, final Object update, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateOne(filter, update, options);
            }
        });
    }

    public ContinuableFuture<UpdateResult> updateAll(final Bson filter, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateAll(filter, update);
            }
        });
    }

    public ContinuableFuture<UpdateResult> updateAll(final Bson filter, final Object update, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateAll(filter, update, options);
            }
        });
    }

    public ContinuableFuture<UpdateResult> replace(final String objectId, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replace(objectId, replacement);
            }
        });
    }

    public ContinuableFuture<UpdateResult> replace(final ObjectId objectId, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replace(objectId, replacement);
            }
        });
    }

    public ContinuableFuture<UpdateResult> replaceOne(final Bson filter, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replaceOne(filter, replacement);
            }
        });
    }

    public ContinuableFuture<UpdateResult> replaceOne(final Bson filter, final Object replacement, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replaceOne(filter, replacement, options);
            }
        });
    }

    public ContinuableFuture<DeleteResult> delete(final String objectId) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.delete(objectId);
            }
        });
    }

    public ContinuableFuture<DeleteResult> delete(final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.delete(objectId);
            }
        });
    }

    public ContinuableFuture<DeleteResult> deleteOne(final Bson filter) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.deleteOne(filter);
            }
        });
    }

    public ContinuableFuture<DeleteResult> deleteOne(final Bson filter, final DeleteOptions options) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.deleteOne(filter, options);
            }
        });
    }

    public ContinuableFuture<DeleteResult> deleteAll(final Bson filter) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.deleteAll(filter);
            }
        });
    }

    public ContinuableFuture<DeleteResult> deleteAll(final Bson filter, final DeleteOptions options) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.deleteAll(filter, options);
            }
        });
    }

    public ContinuableFuture<Integer> bulkInsert(final Collection<?> entities) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return collExecutor.bulkInsert(entities);
            }
        });
    }

    public ContinuableFuture<Integer> bulkInsert(final Collection<?> entities, final BulkWriteOptions options) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return collExecutor.bulkInsert(entities, options);
            }
        });
    }

    public ContinuableFuture<BulkWriteResult> bulkWrite(final List<? extends WriteModel<? extends Document>> requests) {
        return asyncExecutor.execute(new Callable<BulkWriteResult>() {
            @Override
            public BulkWriteResult call() throws Exception {
                return collExecutor.bulkWrite(requests);
            }
        });
    }

    public ContinuableFuture<BulkWriteResult> bulkWrite(final List<? extends WriteModel<? extends Document>> requests, final BulkWriteOptions options) {
        return asyncExecutor.execute(new Callable<BulkWriteResult>() {
            @Override
            public BulkWriteResult call() throws Exception {
                return collExecutor.bulkWrite(requests, options);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> distinct(final Class<T> targetClass, final String fieldName) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.distinct(targetClass, fieldName);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> distinct(final Class<T> targetClass, final String fieldName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.distinct(targetClass, fieldName, filter);
            }
        });
    }

    public ContinuableFuture<Stream<Document>> aggregate(final List<? extends Bson> pipeline) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.aggregate(pipeline);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> aggregate(final Class<T> targetClass, final List<? extends Bson> pipeline) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.aggregate(targetClass, pipeline);
            }
        });
    }

    public ContinuableFuture<Stream<Document>> mapReduce(final String mapFunction, final String reduceFunction) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.mapReduce(mapFunction, reduceFunction);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> mapReduce(final Class<T> targetClass, final String mapFunction, final String reduceFunction) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.mapReduce(targetClass, mapFunction, reduceFunction);
            }
        });
    }

    @Beta
    public ContinuableFuture<Stream<Document>> groupBy(final String fieldName) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.groupBy(fieldName);
            }
        });
    }

    @Beta
    public ContinuableFuture<Stream<Document>> groupBy(final Collection<String> fieldNames) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.groupBy(fieldNames);
            }
        });
    }

    @Beta
    public ContinuableFuture<Stream<Document>> groupByAndCount(final String fieldName) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.groupByAndCount(fieldName);
            }
        });
    }

    @Beta
    public ContinuableFuture<Stream<Document>> groupByAndCount(final Collection<String> fieldNames) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.groupByAndCount(fieldNames);
            }
        });
    }
}
