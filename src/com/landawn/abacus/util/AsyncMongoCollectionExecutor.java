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
import com.mongodb.client.model.InsertManyOptions;
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

    public CompletableFuture<Boolean> exists(final String objectId) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return collExecutor.exists(objectId);
            }
        });
    }

    public CompletableFuture<Boolean> exists(final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return collExecutor.exists(objectId);
            }
        });
    }

    public CompletableFuture<Boolean> exists(final Bson filter) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return collExecutor.exists(filter);
            }
        });
    }

    public CompletableFuture<Long> count() {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return collExecutor.count();
            }
        });
    }

    public CompletableFuture<Long> count(final Bson filter) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return collExecutor.count(filter);
            }
        });
    }

    public CompletableFuture<Long> count(final Bson filter, final CountOptions options) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return collExecutor.count(filter, options);
            }
        });
    }

    public CompletableFuture<Document> get(final String objectId) {
        return asyncExecutor.execute(new Callable<Document>() {
            @Override
            public Document call() throws Exception {
                return collExecutor.get(objectId);
            }
        });
    }

    public CompletableFuture<Document> get(final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Document>() {
            @Override
            public Document call() throws Exception {
                return collExecutor.get(objectId);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final String objectId) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final String objectId, final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId, selectPropNames);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final ObjectId objectId, final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return collExecutor.get(targetClass, objectId, selectPropNames);
            }
        });
    }

    public <T> CompletableFuture<NullabLe<T>> queryForSingleResult(final Class<T> targetClass, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<NullabLe<T>>() {
            @Override
            public NullabLe<T> call() throws Exception {
                return collExecutor.queryForSingleResult(targetClass, propName, filter);
            }
        });
    }

    public CompletableFuture<Optional<Document>> queryForEntity(final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<Document>>() {
            @Override
            public Optional<Document> call() throws Exception {
                return collExecutor.queryForEntity(filter);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.queryForEntity(targetClass, filter);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.queryForEntity(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter,
            final Bson sort) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.queryForEntity(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return collExecutor.queryForEntity(targetClass, filter, sort, projection);
            }
        });
    }

    public CompletableFuture<List<Document>> find(final Bson filter) {
        return asyncExecutor.execute(new Callable<List<Document>>() {
            @Override
            public List<Document> call() throws Exception {
                return collExecutor.find(filter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, filter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort,
            final int offset, final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, filter, sort, projection);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.find(targetClass, filter, sort, projection, offset, count);
            }
        });
    }

    public CompletableFuture<DataSet> query(final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(filter);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, filter);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort,
            final int offset, final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, filter, sort, projection);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.query(targetClass, filter, sort, projection, offset, count);
            }
        });
    }

    public CompletableFuture<Stream<Document>> stream(final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return collExecutor.stream(filter);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, filter);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final Collection<String> selectPropNames, final Bson filter, final Bson sort,
            final int offset, final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, filter, sort, projection);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final Bson filter, final Bson sort, final Bson projection, final int offset,
            final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return collExecutor.stream(targetClass, filter, sort, projection, offset, count);
            }
        });
    }

    public CompletableFuture<Void> insert(final Object obj) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                collExecutor.insert(obj);
                return null;
            }
        });
    }

    public CompletableFuture<Void> insert(final Collection<?> objList) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                collExecutor.insert(objList);
                return null;
            }
        });
    }

    public CompletableFuture<Void> insert(final Collection<?> objList, final InsertManyOptions options) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                collExecutor.insert(objList, options);
                return null;
            }
        });
    }

    public CompletableFuture<UpdateResult> update(final String objectId, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.update(objectId, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> update(final ObjectId objectId, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.update(objectId, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateOne(final Bson filter, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateOne(filter, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateOne(final Bson filter, final Object update, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateOne(filter, update, options);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateAll(final Bson filter, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateAll(filter, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateAll(final Bson filter, final Object update, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.updateAll(filter, update, options);
            }
        });
    }

    public CompletableFuture<UpdateResult> replace(final String objectId, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replace(objectId, replacement);
            }
        });
    }

    public CompletableFuture<UpdateResult> replace(final ObjectId objectId, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replace(objectId, replacement);
            }
        });
    }

    public CompletableFuture<UpdateResult> replaceOne(final Bson filter, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replaceOne(filter, replacement);
            }
        });
    }

    public CompletableFuture<UpdateResult> replaceOne(final Bson filter, final Object replacement, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return collExecutor.replaceOne(filter, replacement, options);
            }
        });
    }

    public CompletableFuture<DeleteResult> delete(final String objectId) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.delete(objectId);
            }
        });
    }

    public CompletableFuture<DeleteResult> delete(final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.delete(objectId);
            }
        });
    }

    public CompletableFuture<DeleteResult> deleteOne(final Bson filter) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.deleteOne(filter);
            }
        });
    }

    public CompletableFuture<DeleteResult> deleteAll(final Bson filter) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return collExecutor.deleteAll(filter);
            }
        });
    }

    public CompletableFuture<Integer> bulkInsert(final Collection<?> entities) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return collExecutor.bulkInsert(entities);
            }
        });
    }

    public CompletableFuture<Integer> bulkInsert(final Collection<?> entities, final BulkWriteOptions options) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return collExecutor.bulkInsert(entities, options);
            }
        });
    }

    public CompletableFuture<BulkWriteResult> bulkWrite(final List<? extends WriteModel<? extends Document>> requests) {
        return asyncExecutor.execute(new Callable<BulkWriteResult>() {
            @Override
            public BulkWriteResult call() throws Exception {
                return collExecutor.bulkWrite(requests);
            }
        });
    }

    public CompletableFuture<BulkWriteResult> bulkWrite(final List<? extends WriteModel<? extends Document>> requests, final BulkWriteOptions options) {
        return asyncExecutor.execute(new Callable<BulkWriteResult>() {
            @Override
            public BulkWriteResult call() throws Exception {
                return collExecutor.bulkWrite(requests, options);
            }
        });
    }

    public <T> CompletableFuture<List<T>> distinct(final Class<T> targetClass, final String fieldName) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.distinct(targetClass, fieldName);
            }
        });
    }

    public <T> CompletableFuture<List<T>> distinct(final Class<T> targetClass, final String fieldName, final Bson filter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.distinct(targetClass, fieldName, filter);
            }
        });
    }

    @Beta
    public CompletableFuture<DataSet> groupBy(final String fieldName) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.groupBy(fieldName);
            }
        });
    }

    @Beta
    public CompletableFuture<DataSet> groupBy(final Collection<String> fieldNames) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.groupBy(fieldNames);
            }
        });
    }

    @Beta
    public CompletableFuture<DataSet> groupByAndCount(final String fieldName) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.groupByAndCount(fieldName);
            }
        });
    }

    @Beta
    public CompletableFuture<DataSet> groupByAndCount(final Collection<String> fieldNames) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return collExecutor.groupByAndCount(fieldNames);
            }
        });
    }

    public CompletableFuture<List<Document>> aggregate(final List<? extends Bson> pipeline) {
        return asyncExecutor.execute(new Callable<List<Document>>() {
            @Override
            public List<Document> call() throws Exception {
                return collExecutor.aggregate(pipeline);
            }
        });
    }

    public <T> CompletableFuture<List<T>> aggregate(final Class<T> targetClass, final List<? extends Bson> pipeline) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.aggregate(targetClass, pipeline);
            }
        });
    }

    public CompletableFuture<List<Document>> mapReduce(final String mapFunction, final String reduceFunction) {
        return asyncExecutor.execute(new Callable<List<Document>>() {
            @Override
            public List<Document> call() throws Exception {
                return collExecutor.mapReduce(mapFunction, reduceFunction);
            }
        });
    }

    public <T> CompletableFuture<List<T>> mapReduce(final Class<T> targetClass, final String mapFunction, final String reduceFunction) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return collExecutor.mapReduce(targetClass, mapFunction, reduceFunction);
            }
        });
    }
}
