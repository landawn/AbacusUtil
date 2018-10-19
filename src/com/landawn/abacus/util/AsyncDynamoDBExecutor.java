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

import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

import com.amazonaws.services.dynamodbv2.model.AttributeValue;
import com.amazonaws.services.dynamodbv2.model.AttributeValueUpdate;
import com.amazonaws.services.dynamodbv2.model.BatchGetItemRequest;
import com.amazonaws.services.dynamodbv2.model.BatchWriteItemRequest;
import com.amazonaws.services.dynamodbv2.model.BatchWriteItemResult;
import com.amazonaws.services.dynamodbv2.model.Condition;
import com.amazonaws.services.dynamodbv2.model.DeleteItemRequest;
import com.amazonaws.services.dynamodbv2.model.DeleteItemResult;
import com.amazonaws.services.dynamodbv2.model.GetItemRequest;
import com.amazonaws.services.dynamodbv2.model.KeysAndAttributes;
import com.amazonaws.services.dynamodbv2.model.PutItemRequest;
import com.amazonaws.services.dynamodbv2.model.PutItemResult;
import com.amazonaws.services.dynamodbv2.model.QueryRequest;
import com.amazonaws.services.dynamodbv2.model.ScanRequest;
import com.amazonaws.services.dynamodbv2.model.UpdateItemRequest;
import com.amazonaws.services.dynamodbv2.model.UpdateItemResult;
import com.amazonaws.services.dynamodbv2.model.WriteRequest;
import com.landawn.abacus.DataSet;
import com.landawn.abacus.util.stream.Stream;

/**
 * Asynchronous <code>DynamoDBExecutor</code>.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class AsyncDynamoDBExecutor {
    private final DynamoDBExecutor dbExecutor;
    private final AsyncExecutor asyncExecutor;

    AsyncDynamoDBExecutor(final DynamoDBExecutor dbExecutor, final AsyncExecutor asyncExecutor) {
        this.dbExecutor = dbExecutor;
        this.asyncExecutor = asyncExecutor;
    }

    public DynamoDBExecutor sync() {
        return dbExecutor;
    }

    public ContinuableFuture<Map<String, Object>> getItem(final String tableName, final Map<String, AttributeValue> key) {
        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
            @Override
            public Map<String, Object> call() throws Exception {
                return dbExecutor.getItem(tableName, key);
            }
        });
    }

    public ContinuableFuture<Map<String, Object>> getItem(final String tableName, final Map<String, AttributeValue> key, final Boolean consistentRead) {
        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
            @Override
            public Map<String, Object> call() throws Exception {
                return dbExecutor.getItem(tableName, key, consistentRead);
            }
        });
    }

    public ContinuableFuture<Map<String, Object>> getItem(final GetItemRequest getItemRequest) {
        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
            @Override
            public Map<String, Object> call() throws Exception {
                return dbExecutor.getItem(getItemRequest);
            }
        });
    }

    public <T> ContinuableFuture<T> getItem(final Class<T> targetClass, final String tableName, final Map<String, AttributeValue> key) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.getItem(targetClass, tableName, key);
            }
        });
    }

    public <T> ContinuableFuture<T> getItem(final Class<T> targetClass, final String tableName, final Map<String, AttributeValue> key,
            final Boolean consistentRead) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.getItem(targetClass, tableName, key, consistentRead);
            }
        });
    }

    public <T> ContinuableFuture<T> getItem(final Class<T> targetClass, final GetItemRequest getItemRequest) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.getItem(targetClass, getItemRequest);
            }
        });
    }

    public ContinuableFuture<Map<String, List<Map<String, Object>>>> batchGetItem(final Map<String, KeysAndAttributes> requestItems) {
        return asyncExecutor.execute(new Callable<Map<String, List<Map<String, Object>>>>() {
            @Override
            public Map<String, List<Map<String, Object>>> call() throws Exception {
                return dbExecutor.batchGetItem(requestItems);
            }
        });
    }

    public ContinuableFuture<Map<String, List<Map<String, Object>>>> batchGetItem(final Map<String, KeysAndAttributes> requestItems,
            final String returnConsumedCapacity) {
        return asyncExecutor.execute(new Callable<Map<String, List<Map<String, Object>>>>() {
            @Override
            public Map<String, List<Map<String, Object>>> call() throws Exception {
                return dbExecutor.batchGetItem(requestItems, returnConsumedCapacity);
            }
        });
    }

    public ContinuableFuture<Map<String, List<Map<String, Object>>>> batchGetItem(final BatchGetItemRequest batchGetItemRequest) {
        return asyncExecutor.execute(new Callable<Map<String, List<Map<String, Object>>>>() {
            @Override
            public Map<String, List<Map<String, Object>>> call() throws Exception {
                return dbExecutor.batchGetItem(batchGetItemRequest);
            }
        });
    }

    public <T> ContinuableFuture<Map<String, List<T>>> batchGetItem(final Class<T> targetClass, final Map<String, KeysAndAttributes> requestItems) {
        return asyncExecutor.execute(new Callable<Map<String, List<T>>>() {
            @Override
            public Map<String, List<T>> call() throws Exception {
                return dbExecutor.batchGetItem(targetClass, requestItems);
            }
        });
    }

    public <T> ContinuableFuture<Map<String, List<T>>> batchGetItem(final Class<T> targetClass, final Map<String, KeysAndAttributes> requestItems,
            final String returnConsumedCapacity) {
        return asyncExecutor.execute(new Callable<Map<String, List<T>>>() {
            @Override
            public Map<String, List<T>> call() throws Exception {
                return dbExecutor.batchGetItem(targetClass, requestItems, returnConsumedCapacity);
            }
        });
    }

    public <T> ContinuableFuture<Map<String, List<T>>> batchGetItem(final Class<T> targetClass, final BatchGetItemRequest batchGetItemRequest) {
        return asyncExecutor.execute(new Callable<Map<String, List<T>>>() {
            @Override
            public Map<String, List<T>> call() throws Exception {
                return dbExecutor.batchGetItem(targetClass, batchGetItemRequest);
            }
        });
    }

    public ContinuableFuture<PutItemResult> putItem(final String tableName, final Map<String, AttributeValue> item) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, item);
            }
        });
    }

    public ContinuableFuture<PutItemResult> putItem(final String tableName, final Map<String, AttributeValue> item, final String returnValues) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, item, returnValues);
            }
        });
    }

    public ContinuableFuture<PutItemResult> putItem(final PutItemRequest putItemRequest) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(putItemRequest);
            }
        });
    }

    // There is no too much benefit to add method for "Object entity"
    // And it may cause error because the "Object" is ambiguous to any type. 
    ContinuableFuture<PutItemResult> putItem(final String tableName, final Object entity) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, entity);
            }
        });
    }

    // There is no too much benefit to add method for "Object entity"
    // And it may cause error because the "Object" is ambiguous to any type. 
    ContinuableFuture<PutItemResult> putItem(final String tableName, final Object entity, final String returnValues) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, entity, returnValues);
            }
        });
    }

    public ContinuableFuture<BatchWriteItemResult> batchWriteItem(final Map<String, List<WriteRequest>> requestItems) {
        return asyncExecutor.execute(new Callable<BatchWriteItemResult>() {
            @Override
            public BatchWriteItemResult call() throws Exception {
                return dbExecutor.batchWriteItem(requestItems);
            }
        });
    }

    public ContinuableFuture<BatchWriteItemResult> batchWriteItem(final BatchWriteItemRequest batchWriteItemRequest) {
        return asyncExecutor.execute(new Callable<BatchWriteItemResult>() {
            @Override
            public BatchWriteItemResult call() throws Exception {
                return dbExecutor.batchWriteItem(batchWriteItemRequest);
            }
        });
    }

    public ContinuableFuture<UpdateItemResult> updateItem(final String tableName, final Map<String, AttributeValue> key,
            final Map<String, AttributeValueUpdate> attributeUpdates) {
        return asyncExecutor.execute(new Callable<UpdateItemResult>() {
            @Override
            public UpdateItemResult call() throws Exception {
                return dbExecutor.updateItem(tableName, key, attributeUpdates);
            }
        });
    }

    public ContinuableFuture<UpdateItemResult> updateItem(final String tableName, final Map<String, AttributeValue> key,
            final Map<String, AttributeValueUpdate> attributeUpdates, final String returnValues) {
        return asyncExecutor.execute(new Callable<UpdateItemResult>() {
            @Override
            public UpdateItemResult call() throws Exception {
                return dbExecutor.updateItem(tableName, key, attributeUpdates, returnValues);
            }
        });
    }

    public ContinuableFuture<UpdateItemResult> updateItem(final UpdateItemRequest updateItemRequest) {
        return asyncExecutor.execute(new Callable<UpdateItemResult>() {
            @Override
            public UpdateItemResult call() throws Exception {
                return dbExecutor.updateItem(updateItemRequest);
            }
        });
    }

    public ContinuableFuture<DeleteItemResult> deleteItem(final String tableName, final Map<String, AttributeValue> key) {
        return asyncExecutor.execute(new Callable<DeleteItemResult>() {
            @Override
            public DeleteItemResult call() throws Exception {
                return dbExecutor.deleteItem(tableName, key);
            }
        });
    }

    public ContinuableFuture<DeleteItemResult> deleteItem(final String tableName, final Map<String, AttributeValue> key, final String returnValues) {
        return asyncExecutor.execute(new Callable<DeleteItemResult>() {
            @Override
            public DeleteItemResult call() throws Exception {
                return dbExecutor.deleteItem(tableName, key, returnValues);
            }
        });
    }

    public ContinuableFuture<DeleteItemResult> deleteItem(final DeleteItemRequest deleteItemRequest) {
        return asyncExecutor.execute(new Callable<DeleteItemResult>() {
            @Override
            public DeleteItemResult call() throws Exception {
                return dbExecutor.deleteItem(deleteItemRequest);
            }
        });
    }

    public ContinuableFuture<List<Map<String, Object>>> list(final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<List<Map<String, Object>>>() {
            @Override
            public List<Map<String, Object>> call() throws Exception {
                return dbExecutor.list(queryRequest);
            }
        });
    }

    public <T> ContinuableFuture<List<T>> list(final Class<T> targetClass, final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.list(targetClass, queryRequest);
            }
        });
    }

    public ContinuableFuture<DataSet> query(final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(queryRequest);
            }
        });
    }

    public <T> ContinuableFuture<DataSet> query(final Class<T> targetClass, final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, queryRequest);
            }
        });
    }

    public ContinuableFuture<Stream<Map<String, Object>>> stream(final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<Stream<Map<String, Object>>>() {
            @Override
            public Stream<Map<String, Object>> call() throws Exception {
                return dbExecutor.stream(queryRequest);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> stream(final Class<T> targetClass, final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.stream(targetClass, queryRequest);
            }
        });
    }

    public ContinuableFuture<Stream<Map<String, Object>>> scan(final String tableName, final List<String> attributesToGet) {
        return asyncExecutor.execute(new Callable<Stream<Map<String, Object>>>() {
            @Override
            public Stream<Map<String, Object>> call() throws Exception {
                return dbExecutor.scan(tableName, attributesToGet);
            }
        });
    }

    public ContinuableFuture<Stream<Map<String, Object>>> scan(final String tableName, final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<Stream<Map<String, Object>>>() {
            @Override
            public Stream<Map<String, Object>> call() throws Exception {
                return dbExecutor.scan(tableName, scanFilter);
            }
        });
    }

    public ContinuableFuture<Stream<Map<String, Object>>> scan(final String tableName, final List<String> attributesToGet,
            final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<Stream<Map<String, Object>>>() {
            @Override
            public Stream<Map<String, Object>> call() throws Exception {
                return dbExecutor.scan(tableName, attributesToGet, scanFilter);
            }
        });
    }

    public ContinuableFuture<Stream<Map<String, Object>>> scan(final ScanRequest scanRequest) {
        return asyncExecutor.execute(new Callable<Stream<Map<String, Object>>>() {
            @Override
            public Stream<Map<String, Object>> call() throws Exception {
                return dbExecutor.scan(scanRequest);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> scan(final Class<T> targetClass, final String tableName, final List<String> attributesToGet) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.scan(targetClass, tableName, attributesToGet);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> scan(final Class<T> targetClass, final String tableName, final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.scan(targetClass, tableName, scanFilter);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> scan(final Class<T> targetClass, final String tableName, final List<String> attributesToGet,
            final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.scan(targetClass, tableName, attributesToGet, scanFilter);
            }
        });
    }

    public <T> ContinuableFuture<Stream<T>> scan(final Class<T> targetClass, final ScanRequest scanRequest) {
        return asyncExecutor.execute(new Callable<Stream<T>>() {
            @Override
            public Stream<T> call() throws Exception {
                return dbExecutor.scan(targetClass, scanRequest);
            }
        });
    }
}
