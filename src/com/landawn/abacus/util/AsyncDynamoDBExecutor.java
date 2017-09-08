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

    public CompletableFuture<Map<String, Object>> getItem(final String tableName, final Map<String, AttributeValue> key) {
        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
            @Override
            public Map<String, Object> call() throws Exception {
                return dbExecutor.getItem(tableName, key);
            }
        });
    }

    public CompletableFuture<Map<String, Object>> getItem(final String tableName, final Map<String, AttributeValue> key, final Boolean consistentRead) {
        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
            @Override
            public Map<String, Object> call() throws Exception {
                return dbExecutor.getItem(tableName, key, consistentRead);
            }
        });
    }

    public CompletableFuture<Map<String, Object>> getItem(final GetItemRequest getItemRequest) {
        return asyncExecutor.execute(new Callable<Map<String, Object>>() {
            @Override
            public Map<String, Object> call() throws Exception {
                return dbExecutor.getItem(getItemRequest);
            }
        });
    }

    public <T> CompletableFuture<T> getItem(final Class<T> targetClass, final String tableName, final Map<String, AttributeValue> key) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.getItem(targetClass, tableName, key);
            }
        });
    }

    public <T> CompletableFuture<T> getItem(final Class<T> targetClass, final String tableName, final Map<String, AttributeValue> key,
            final Boolean consistentRead) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.getItem(targetClass, tableName, key, consistentRead);
            }
        });
    }

    public <T> CompletableFuture<T> getItem(final Class<T> targetClass, final GetItemRequest getItemRequest) {
        return asyncExecutor.execute(new Callable<T>() {
            @Override
            public T call() throws Exception {
                return dbExecutor.getItem(targetClass, getItemRequest);
            }
        });
    }

    public CompletableFuture<Map<String, List<Map<String, Object>>>> batchGetItem(final Map<String, KeysAndAttributes> requestItems) {
        return asyncExecutor.execute(new Callable<Map<String, List<Map<String, Object>>>>() {
            @Override
            public Map<String, List<Map<String, Object>>> call() throws Exception {
                return dbExecutor.batchGetItem(requestItems);
            }
        });
    }

    public CompletableFuture<Map<String, List<Map<String, Object>>>> batchGetItem(final Map<String, KeysAndAttributes> requestItems,
            final String returnConsumedCapacity) {
        return asyncExecutor.execute(new Callable<Map<String, List<Map<String, Object>>>>() {
            @Override
            public Map<String, List<Map<String, Object>>> call() throws Exception {
                return dbExecutor.batchGetItem(requestItems, returnConsumedCapacity);
            }
        });
    }

    public CompletableFuture<Map<String, List<Map<String, Object>>>> batchGetItem(final BatchGetItemRequest batchGetItemRequest) {
        return asyncExecutor.execute(new Callable<Map<String, List<Map<String, Object>>>>() {
            @Override
            public Map<String, List<Map<String, Object>>> call() throws Exception {
                return dbExecutor.batchGetItem(batchGetItemRequest);
            }
        });
    }

    public <T> CompletableFuture<Map<String, List<T>>> batchGetItem(final Class<T> targetClass, final Map<String, KeysAndAttributes> requestItems) {
        return asyncExecutor.execute(new Callable<Map<String, List<T>>>() {
            @Override
            public Map<String, List<T>> call() throws Exception {
                return dbExecutor.batchGetItem(targetClass, requestItems);
            }
        });
    }

    public <T> CompletableFuture<Map<String, List<T>>> batchGetItem(final Class<T> targetClass, final Map<String, KeysAndAttributes> requestItems,
            final String returnConsumedCapacity) {
        return asyncExecutor.execute(new Callable<Map<String, List<T>>>() {
            @Override
            public Map<String, List<T>> call() throws Exception {
                return dbExecutor.batchGetItem(targetClass, requestItems, returnConsumedCapacity);
            }
        });
    }

    public <T> CompletableFuture<Map<String, List<T>>> batchGetItem(final Class<T> targetClass, final BatchGetItemRequest batchGetItemRequest) {
        return asyncExecutor.execute(new Callable<Map<String, List<T>>>() {
            @Override
            public Map<String, List<T>> call() throws Exception {
                return dbExecutor.batchGetItem(targetClass, batchGetItemRequest);
            }
        });
    }

    public CompletableFuture<PutItemResult> putItem(final String tableName, final Map<String, AttributeValue> item) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, item);
            }
        });
    }

    public CompletableFuture<PutItemResult> putItem(final String tableName, final Map<String, AttributeValue> item, final String returnValues) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, item, returnValues);
            }
        });
    }

    public CompletableFuture<PutItemResult> putItem(final PutItemRequest putItemRequest) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(putItemRequest);
            }
        });
    }

    // There is no too much benefit to add method for "Object entity"
    // And it may cause error because the "Object" is ambiguous to any type. 
    CompletableFuture<PutItemResult> putItem(final String tableName, final Object entity) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, entity);
            }
        });
    }

    // There is no too much benefit to add method for "Object entity"
    // And it may cause error because the "Object" is ambiguous to any type. 
    CompletableFuture<PutItemResult> putItem(final String tableName, final Object entity, final String returnValues) {
        return asyncExecutor.execute(new Callable<PutItemResult>() {
            @Override
            public PutItemResult call() throws Exception {
                return dbExecutor.putItem(tableName, entity, returnValues);
            }
        });
    }

    public CompletableFuture<BatchWriteItemResult> batchWriteItem(final Map<String, List<WriteRequest>> requestItems) {
        return asyncExecutor.execute(new Callable<BatchWriteItemResult>() {
            @Override
            public BatchWriteItemResult call() throws Exception {
                return dbExecutor.batchWriteItem(requestItems);
            }
        });
    }

    public CompletableFuture<BatchWriteItemResult> batchWriteItem(final BatchWriteItemRequest batchWriteItemRequest) {
        return asyncExecutor.execute(new Callable<BatchWriteItemResult>() {
            @Override
            public BatchWriteItemResult call() throws Exception {
                return dbExecutor.batchWriteItem(batchWriteItemRequest);
            }
        });
    }

    public CompletableFuture<UpdateItemResult> updateItem(final String tableName, final Map<String, AttributeValue> key,
            final Map<String, AttributeValueUpdate> attributeUpdates) {
        return asyncExecutor.execute(new Callable<UpdateItemResult>() {
            @Override
            public UpdateItemResult call() throws Exception {
                return dbExecutor.updateItem(tableName, key, attributeUpdates);
            }
        });
    }

    public CompletableFuture<UpdateItemResult> updateItem(final String tableName, final Map<String, AttributeValue> key,
            final Map<String, AttributeValueUpdate> attributeUpdates, final String returnValues) {
        return asyncExecutor.execute(new Callable<UpdateItemResult>() {
            @Override
            public UpdateItemResult call() throws Exception {
                return dbExecutor.updateItem(tableName, key, attributeUpdates, returnValues);
            }
        });
    }

    public CompletableFuture<UpdateItemResult> updateItem(final UpdateItemRequest updateItemRequest) {
        return asyncExecutor.execute(new Callable<UpdateItemResult>() {
            @Override
            public UpdateItemResult call() throws Exception {
                return dbExecutor.updateItem(updateItemRequest);
            }
        });
    }

    public CompletableFuture<DeleteItemResult> deleteItem(final String tableName, final Map<String, AttributeValue> key) {
        return asyncExecutor.execute(new Callable<DeleteItemResult>() {
            @Override
            public DeleteItemResult call() throws Exception {
                return dbExecutor.deleteItem(tableName, key);
            }
        });
    }

    public CompletableFuture<DeleteItemResult> deleteItem(final String tableName, final Map<String, AttributeValue> key, final String returnValues) {
        return asyncExecutor.execute(new Callable<DeleteItemResult>() {
            @Override
            public DeleteItemResult call() throws Exception {
                return dbExecutor.deleteItem(tableName, key, returnValues);
            }
        });
    }

    public CompletableFuture<DeleteItemResult> deleteItem(final DeleteItemRequest deleteItemRequest) {
        return asyncExecutor.execute(new Callable<DeleteItemResult>() {
            @Override
            public DeleteItemResult call() throws Exception {
                return dbExecutor.deleteItem(deleteItemRequest);
            }
        });
    }

    public CompletableFuture<List<Map<String, Object>>> find(final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<List<Map<String, Object>>>() {
            @Override
            public List<Map<String, Object>> call() throws Exception {
                return dbExecutor.find(queryRequest);
            }
        });
    }

    public <T> CompletableFuture<List<T>> find(final Class<T> targetClass, final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.find(targetClass, queryRequest);
            }
        });
    }

    public CompletableFuture<DataSet> query(final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(queryRequest);
            }
        });
    }

    public <T> CompletableFuture<DataSet> query(final Class<T> targetClass, final QueryRequest queryRequest) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.query(targetClass, queryRequest);
            }
        });
    }

    public CompletableFuture<DataSet> scan(final String tableName, final List<String> attributesToGet) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.scan(tableName, attributesToGet);
            }
        });
    }

    public CompletableFuture<DataSet> scan(final String tableName, final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.scan(tableName, scanFilter);
            }
        });
    }

    public CompletableFuture<DataSet> scan(final String tableName, final List<String> attributesToGet, final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.scan(tableName, attributesToGet, scanFilter);
            }
        });
    }

    public CompletableFuture<DataSet> scan(final ScanRequest scanRequest) {
        return asyncExecutor.execute(new Callable<DataSet>() {
            @Override
            public DataSet call() throws Exception {
                return dbExecutor.scan(scanRequest);
            }
        });
    }

    public <T> CompletableFuture<List<T>> scan(final Class<T> targetClass, final String tableName, final List<String> attributesToGet) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.scan(targetClass, tableName, attributesToGet);
            }
        });
    }

    public <T> CompletableFuture<List<T>> scan(final Class<T> targetClass, final String tableName, final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.scan(targetClass, tableName, scanFilter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> scan(final Class<T> targetClass, final String tableName, final List<String> attributesToGet,
            final Map<String, Condition> scanFilter) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.scan(targetClass, tableName, attributesToGet, scanFilter);
            }
        });
    }

    public <T> CompletableFuture<List<T>> scan(final Class<T> targetClass, final ScanRequest scanRequest) {
        return asyncExecutor.execute(new Callable<List<T>>() {
            @Override
            public List<T> call() throws Exception {
                return dbExecutor.scan(targetClass, scanRequest);
            }
        });
    }
}
