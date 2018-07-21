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
import com.mongodb.client.model.InsertManyOptions;
import com.mongodb.client.model.UpdateOptions;
import com.mongodb.client.model.WriteModel;
import com.mongodb.client.result.DeleteResult;
import com.mongodb.client.result.UpdateResult;

/**
 * Asynchronous <code>MongoDBExecutor</code>.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class AsyncMongoDBExecutor {
    private final MongoDBExecutor dbExecutor;
    private final AsyncExecutor asyncExecutor;

    AsyncMongoDBExecutor(final MongoDBExecutor dbExecutor, final AsyncExecutor asyncExecutor) {
        this.dbExecutor = dbExecutor;
        this.asyncExecutor = asyncExecutor;
    }

    public MongoDBExecutor sync() {
        return dbExecutor;
    }

    public CompletableFuture<Boolean> exists(final String collectionName, final String objectId) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return dbExecutor.exists(collectionName, objectId);
            }
        });
    }

    public CompletableFuture<Boolean> exists(final String collectionName, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return dbExecutor.exists(collectionName, objectId);
            }
        });
    }

    public CompletableFuture<Boolean> exists(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Boolean>() {
            @Override
            public Boolean call() throws Exception {
                return dbExecutor.exists(collectionName, filter);
            }
        });
    }

    public CompletableFuture<Long> count(final String collectionName) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return dbExecutor.count(collectionName);
            }
        });
    }

    public CompletableFuture<Long> count(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return dbExecutor.count(collectionName, filter);
            }
        });
    }

    public CompletableFuture<Long> count(final String collectionName, final Bson filter, final CountOptions options) {
        return asyncExecutor.execute(new Callable<Long>() {
            @Override
            public Long call() throws Exception {
                return dbExecutor.count(collectionName, filter, options);
            }
        });
    }

    public CompletableFuture<Document> get(final String collectionName, final String objectId) {
        return asyncExecutor.execute(new Callable<Document>() {
            @Override
            public Document call() throws Exception {
                return dbExecutor.get(collectionName, objectId);
            }
        });
    }

    public CompletableFuture<Document> get(final String collectionName, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Document>() {
            @Override
            public Document call() throws Exception {
                return dbExecutor.get(collectionName, objectId);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final String collectionName, final String objectId) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.get(targetClass, collectionName, objectId);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final String collectionName, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.get(targetClass, collectionName, objectId);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final String collectionName, final String objectId,
            final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.get(targetClass, collectionName, objectId, selectPropNames);
            }
        });
    }

    public <T> CompletableFuture<T> get(final Class<T> targetClass, final String collectionName, final ObjectId objectId,
            final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.get(targetClass, collectionName, objectId, selectPropNames);
            }
        });
    }

    public CompletableFuture<Optional<Document>> gett(final String collectionName, final String objectId) {
        return asyncExecutor.execute(new Callable<Optional<Document>>() {
            @Override
            public Optional<Document> call() throws Exception {
                return dbExecutor.gett(collectionName, objectId);
            }
        });
    }

    public CompletableFuture<Optional<Document>> gett(final String collectionName, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Optional<Document>>() {
            @Override
            public Optional<Document> call() throws Exception {
                return dbExecutor.gett(collectionName, objectId);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> gett(final Class<T> targetClass, final String collectionName, final String objectId) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.gett(targetClass, collectionName, objectId);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> gett(final Class<T> targetClass, final String collectionName, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.gett(targetClass, collectionName, objectId);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> gett(final Class<T> targetClass, final String collectionName, final String objectId,
            final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.gett(targetClass, collectionName, objectId, selectPropNames);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> gett(final Class<T> targetClass, final String collectionName, final ObjectId objectId,
            final Collection<String> selectPropNames) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.gett(targetClass, collectionName, objectId, selectPropNames);
            }
        });
    }

    public CompletableFuture<OptionalBoolean> queryForBoolean(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalBoolean>() {
            @Override
            public OptionalBoolean call() throws Exception {
                return dbExecutor.queryForBoolean(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<OptionalChar> queryForChar(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalChar>() {
            @Override
            public OptionalChar call() throws Exception {
                return dbExecutor.queryForChar(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<OptionalByte> queryForByte(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalByte>() {
            @Override
            public OptionalByte call() throws Exception {
                return dbExecutor.queryForByte(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<OptionalShort> queryForShort(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalShort>() {
            @Override
            public OptionalShort call() throws Exception {
                return dbExecutor.queryForShort(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<OptionalInt> queryForInt(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalInt>() {
            @Override
            public OptionalInt call() throws Exception {
                return dbExecutor.queryForInt(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<OptionalLong> queryForLong(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalLong>() {
            @Override
            public OptionalLong call() throws Exception {
                return dbExecutor.queryForLong(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<OptionalFloat> queryForFloat(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalFloat>() {
            @Override
            public OptionalFloat call() throws Exception {
                return dbExecutor.queryForFloat(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<OptionalDouble> queryForDouble(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<OptionalDouble>() {
            @Override
            public OptionalDouble call() throws Exception {
                return dbExecutor.queryForDouble(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<Nullable<String>> queryForString(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<String>>() {
            @Override
            public Nullable<String> call() throws Exception {
                return dbExecutor.queryForString(collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<Nullable<Date>> queryForDate(final String collectionName, final String propName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<Date>>() {
            @Override
            public Nullable<Date> call() throws Exception {
                return dbExecutor.queryForDate(collectionName, propName, filter);
            }
        });
    }

    public <T extends Date> CompletableFuture<Nullable<T>> queryForDate(final Class<T> targetClass, final String collectionName, final String propName,
            final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return dbExecutor.queryForDate(targetClass, collectionName, propName, filter);
            }
        });
    }

    public <T> CompletableFuture<Nullable<T>> queryForSingleResult(final Class<T> targetClass, final String collectionName, final String propName,
            final Bson filter) {
        return asyncExecutor.execute(new Callable<Nullable<T>>() {
            @Override
            public Nullable<T> call() throws Exception {
                return dbExecutor.queryForSingleResult(targetClass, collectionName, propName, filter);
            }
        });
    }

    public CompletableFuture<Optional<Document>> queryForEntity(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<Document>>() {
            @Override
            public Optional<Document> call() throws Exception {
                return dbExecutor.queryForEntity(collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.queryForEntity(targetClass, collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.queryForEntity(targetClass, collectionName, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.queryForEntity(targetClass, collectionName, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<Optional<T>> queryForEntity(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort,
            final Bson projection) {
        return asyncExecutor.execute(new Callable<Optional<T>>() {
            @Override
            public Optional<T> call() throws Exception {
                return dbExecutor.queryForEntity(targetClass, collectionName, filter, sort, projection);
            }
        });
    }

    public CompletableFuture<List<Document>> find(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<List<Document>>() {
            @Override
            public List<Document> call() throws Exception {
                return dbExecutor.find(collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, collectionName, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, collectionName, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, collectionName, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final Bson sort, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, collectionName, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort,
            final Bson projection) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, collectionName, filter, sort, projection);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort,
            final Bson projection, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, collectionName, filter, sort, projection, offset, count);
            }
        });
    }

    public CompletableFuture<DataSet> query(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, collectionName, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, collectionName, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, collectionName, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final Bson sort, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, collectionName, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort,
            final Bson projection) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, collectionName, filter, sort, projection);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort,
            final Bson projection, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, collectionName, filter, sort, projection, offset, count);
            }
        });
    }

    public CompletableFuture<Stream<Document>> stream(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return dbExecutor.stream(collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, collectionName, filter);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, collectionName, selectPropNames, filter);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, collectionName, selectPropNames, filter, offset, count);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final Bson sort) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, collectionName, selectPropNames, filter, sort);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final String collectionName, final Collection<String> selectPropNames,
            final Bson filter, final Bson sort, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, collectionName, selectPropNames, filter, sort, offset, count);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort,
            final Bson projection) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, collectionName, filter, sort, projection);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> stream(final Class<T> targetClass, final String collectionName, final Bson filter, final Bson sort,
            final Bson projection, final int offset, final int count) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, collectionName, filter, sort, projection, offset, count);
            }
        });
    }

    public CompletableFuture<Void> insert(final String collectionName, final Object obj) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                dbExecutor.insert(collectionName, obj);
                return null;
            }
        });
    }

    /**
     * 
     * @param collectionName
     * @param objList
     * @return
     * @deprecated replaced with {@code insertAll}.
     */
    @Deprecated
    public CompletableFuture<Void> insert(final String collectionName, final Collection<?> objList) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                dbExecutor.insert(collectionName, objList);
                return null;
            }
        });
    }

    /**
     * 
     * @param collectionName
     * @param objList
     * @param options
     * @return
     * @deprecated replaced with {@code insertAll}.
     */
    @Deprecated
    public CompletableFuture<Void> insert(final String collectionName, final Collection<?> objList, final InsertManyOptions options) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                dbExecutor.insert(collectionName, objList, options);
                return null;
            }
        });
    }

    public CompletableFuture<Void> insertAll(final String collectionName, final Collection<?> objList) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                dbExecutor.insertAll(collectionName, objList);
                return null;
            }
        });
    }

    public CompletableFuture<Void> insertAll(final String collectionName, final Collection<?> objList, final InsertManyOptions options) {
        return asyncExecutor.execute(new Callable<Void>() {
            @Override
            public Void call() throws Exception {
                dbExecutor.insertAll(collectionName, objList, options);
                return null;
            }
        });
    }

    public CompletableFuture<UpdateResult> update(final String collectionName, final String objectId, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.update(collectionName, objectId, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> update(final String collectionName, final ObjectId objectId, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.update(collectionName, objectId, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateOne(final String collectionName, final Bson filter, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.updateOne(collectionName, filter, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateOne(final String collectionName, final Bson filter, final Object update, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.updateOne(collectionName, filter, update, options);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateAll(final String collectionName, final Bson filter, final Object update) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.updateAll(collectionName, filter, update);
            }
        });
    }

    public CompletableFuture<UpdateResult> updateAll(final String collectionName, final Bson filter, final Object update, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.updateAll(collectionName, filter, update, options);
            }
        });
    }

    public CompletableFuture<UpdateResult> replace(final String collectionName, final String objectId, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.replace(collectionName, objectId, replacement);
            }
        });
    }

    public CompletableFuture<UpdateResult> replace(final String collectionName, final ObjectId objectId, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.replace(collectionName, objectId, replacement);
            }
        });
    }

    public CompletableFuture<UpdateResult> replaceOne(final String collectionName, final Bson filter, final Object replacement) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.replaceOne(collectionName, filter, replacement);
            }
        });
    }

    public CompletableFuture<UpdateResult> replaceOne(final String collectionName, final Bson filter, final Object replacement, final UpdateOptions options) {
        return asyncExecutor.execute(new Callable<UpdateResult>() {
            @Override
            public UpdateResult call() throws Exception {
                return dbExecutor.replaceOne(collectionName, filter, replacement, options);
            }
        });
    }

    public CompletableFuture<DeleteResult> delete(final String collectionName, final String objectId) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return dbExecutor.delete(collectionName, objectId);
            }
        });
    }

    public CompletableFuture<DeleteResult> delete(final String collectionName, final ObjectId objectId) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return dbExecutor.delete(collectionName, objectId);
            }
        });
    }

    public CompletableFuture<DeleteResult> deleteOne(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return dbExecutor.deleteOne(collectionName, filter);
            }
        });
    }

    public CompletableFuture<DeleteResult> deleteAll(final String collectionName, final Bson filter) {
        return asyncExecutor.execute(new Callable<DeleteResult>() {
            @Override
            public DeleteResult call() throws Exception {
                return dbExecutor.deleteAll(collectionName, filter);
            }
        });
    }

    public CompletableFuture<Integer> bulkInsert(final String collectionName, final Collection<?> entities) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return dbExecutor.bulkInsert(collectionName, entities);
            }
        });
    }

    public CompletableFuture<Integer> bulkInsert(final String collectionName, final Collection<?> entities, final BulkWriteOptions options) {
        return asyncExecutor.execute(new Callable<Integer>() {
            @Override
            public Integer call() throws Exception {
                return dbExecutor.bulkInsert(collectionName, entities, options);
            }
        });
    }

    public CompletableFuture<BulkWriteResult> bulkWrite(final String collectionName, final List<? extends WriteModel<? extends Document>> requests) {
        return asyncExecutor.execute(new Callable<BulkWriteResult>() {
            @Override
            public BulkWriteResult call() throws Exception {
                return dbExecutor.bulkWrite(collectionName, requests);
            }
        });
    }

    public CompletableFuture<BulkWriteResult> bulkWrite(final String collectionName, final List<? extends WriteModel<? extends Document>> requests,
            final BulkWriteOptions options) {
        return asyncExecutor.execute(new Callable<BulkWriteResult>() {
            @Override
            public BulkWriteResult call() throws Exception {
                return dbExecutor.bulkWrite(collectionName, requests, options);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> distinct(final Class<T> targetClass, final String collectionName, final String fieldName) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.distinct(targetClass, collectionName, fieldName);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> distinct(final Class<T> targetClass, final String collectionName, final String fieldName, final Bson filter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.distinct(targetClass, collectionName, fieldName, filter);
            }
        });
    }

    @Beta
    public CompletableFuture<Stream<Document>> groupBy(final String collectionName, final String fieldName) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return dbExecutor.groupBy(collectionName, fieldName);
            }
        });
    }

    @Beta
    public CompletableFuture<Stream<Document>> groupBy(final String collectionName, final Collection<String> fieldNames) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return dbExecutor.groupBy(collectionName, fieldNames);
            }
        });
    }

    @Beta
    public CompletableFuture<Stream<Document>> groupByAndCount(final String collectionName, final String fieldName) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return dbExecutor.groupByAndCount(collectionName, fieldName);
            }
        });
    }

    @Beta
    public CompletableFuture<Stream<Document>> groupByAndCount(final String collectionName, final Collection<String> fieldNames) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return dbExecutor.groupByAndCount(collectionName, fieldNames);
            }
        });
    }

    public CompletableFuture<Stream<Document>> aggregate(final String collectionName, final List<? extends Bson> pipeline) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return dbExecutor.aggregate(collectionName, pipeline);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> aggregate(final Class<T> targetClass, final String collectionName, final List<? extends Bson> pipeline) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.aggregate(targetClass, collectionName, pipeline);
            }
        });
    }

    public CompletableFuture<Stream<Document>> mapReduce(final String collectionName, final String mapFunction, final String reduceFunction) {
        return asyncExecutor.execute(new Callable<Stream<Document>>() {
            @Override
            public Stream<Document> call() throws Exception {
                return dbExecutor.mapReduce(collectionName, mapFunction, reduceFunction);
            }
        });
    }

    public <T> CompletableFuture<Stream<T>> mapReduce(final Class<T> targetClass, final String collectionName, final String mapFunction,
            final String reduceFunction) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.mapReduce(targetClass, collectionName, mapFunction, reduceFunction);
            }
        });
    }
}
