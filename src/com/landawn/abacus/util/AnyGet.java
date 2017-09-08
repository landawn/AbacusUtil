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

import static com.landawn.abacus.util.HBaseExecutor.toFamilyQualifierBytes;
import static com.landawn.abacus.util.HBaseExecutor.toRowKeyBytes;
import static com.landawn.abacus.util.HBaseExecutor.toValueBytes;

import java.io.IOException;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;

import org.apache.hadoop.hbase.client.Consistency;
import org.apache.hadoop.hbase.client.Get;
import org.apache.hadoop.hbase.client.IsolationLevel;
import org.apache.hadoop.hbase.exceptions.DeserializationException;
import org.apache.hadoop.hbase.filter.Filter;
import org.apache.hadoop.hbase.io.TimeRange;
import org.apache.hadoop.hbase.security.access.Permission;
import org.apache.hadoop.hbase.security.visibility.Authorizations;

import com.landawn.abacus.exception.UncheckedIOException;

/**
 * It's a wrapper of <code>Get</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Get
 */
public final class AnyGet {
    private final Get get;

    public AnyGet(Object rowKey) {
        this.get = new Get(toRowKeyBytes(rowKey));
    }

    public static AnyGet of(Object rowKey) {
        return new AnyGet(rowKey);
    }

    public Get value() {
        return get;
    }

    public AnyGet addFamily(String family) {
        get.addFamily(toFamilyQualifierBytes(family));

        return this;
    }

    public AnyGet addColumn(String family, String qualifier) {
        get.addColumn(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));

        return this;
    }

    public boolean hasFamilies() {
        return get.hasFamilies();
    }

    public int numFamilies() {
        return get.numFamilies();
    }

    public Set<byte[]> familySet() {
        return get.familySet();
    }

    public Map<byte[], NavigableSet<byte[]>> getFamilyMap() {
        return get.getFamilyMap();
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class.
     * The properties not set by this should be get by the methods in <code>Get</code>
     * 
     * @return
     */
    Map<String, Object> getFingerprint() {
        return get.getFingerprint();
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class
     * The properties not set by this should be get by the methods in <code>Get</code>
     * 
     * @param maxCols
     * @return
     */
    Map<String, Object> toMap(int maxCols) {
        return get.toMap(maxCols);
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class
     * The properties not set by this should be get by the methods in <code>Get</code>
     * 
     * @return
     */
    byte[] getRow() {
        return get.getRow();
    }

    public boolean isCheckExistenceOnly() {
        return get.isCheckExistenceOnly();
    }

    public AnyGet setCheckExistenceOnly(boolean checkExistenceOnly) {
        get.setCheckExistenceOnly(checkExistenceOnly);

        return this;
    }

    public boolean isClosestRowBefore() {
        return get.isClosestRowBefore();
    }

    public AnyGet setClosestRowBefore(boolean closestRowBefore) {
        get.setClosestRowBefore(closestRowBefore);

        return this;
    }

    public TimeRange getTimeRange() {
        return get.getTimeRange();
    }

    public AnyGet setTimeRange(long minStamp, long maxStamp) {
        try {
            get.setTimeRange(minStamp, maxStamp);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        return this;
    }

    public AnyGet setTimeStamp(long timestamp) {
        try {
            get.setTimeStamp(timestamp);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        return this;
    }

    public int getMaxVersions() {
        return get.getMaxVersions();
    }

    public AnyGet setMaxVersions(int maxVersions) {
        try {
            get.setMaxVersions(maxVersions);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        return this;
    }

    /**
     * Get all available versions.
     * @return this for invocation chaining
     */
    public AnyGet setMaxVersions() {
        get.setMaxVersions();

        return this;
    }

    public int getMaxResultsPerColumnFamily() {
        return get.getMaxResultsPerColumnFamily();
    }

    public AnyGet setMaxResultsPerColumnFamily(int limit) {
        get.setMaxResultsPerColumnFamily(limit);

        return this;
    }

    public int getRowOffsetPerColumnFamily() {
        return get.getRowOffsetPerColumnFamily();
    }

    public AnyGet setRowOffsetPerColumnFamily(int offset) {
        get.setRowOffsetPerColumnFamily(offset);

        return this;
    }

    public boolean getCacheBlocks() {
        return get.getCacheBlocks();
    }

    public AnyGet setCacheBlocks(boolean cacheBlocks) {
        get.setCacheBlocks(cacheBlocks);

        return this;
    }

    public Filter getFilter() {
        return get.getFilter();
    }

    public AnyGet setFilter(Filter filter) {
        get.setFilter(filter);

        return this;
    }

    public String getId() {
        return get.getId();
    }

    public AnyGet setId(String id) {
        get.setId(id);

        return this;
    }

    public Authorizations getAuthorizations() throws DeserializationException {
        return get.getAuthorizations();
    }

    public AnyGet setAuthorizations(Authorizations authorizations) {
        get.setAuthorizations(authorizations);

        return this;
    }

    public byte[] getACL() {
        return get.getACL();
    }

    public AnyGet setACL(Map<String, Permission> perms) {
        get.setACL(perms);

        return this;
    }

    public AnyGet setACL(String user, Permission perms) {
        get.setACL(user, perms);

        return this;
    }

    public Consistency getConsistency() {
        return get.getConsistency();
    }

    public AnyGet setConsistency(Consistency consistency) {
        get.setConsistency(consistency);

        return this;
    }

    public int getReplicaId() {
        return get.getReplicaId();
    }

    public AnyGet setReplicaId(int Id) {
        get.setReplicaId(Id);

        return this;
    }

    public IsolationLevel getIsolationLevel() {
        return get.getIsolationLevel();
    }

    public AnyGet setIsolationLevel(IsolationLevel level) {
        get.setIsolationLevel(level);

        return this;
    }

    public Map<String, byte[]> getAttributesMap() {
        return get.getAttributesMap();
    }

    public byte[] getAttribute(String name) {
        return get.getAttribute(name);
    }

    public AnyGet setAttribute(String name, Object value) {
        get.setAttribute(name, toValueBytes(value));

        return this;
    }

    @Override
    public int hashCode() {
        return get.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof AnyGet) {
            AnyGet other = (AnyGet) obj;

            return this.get.equals(other.get);
        }

        return false;
    }

    @Override
    public String toString() {
        return get.toString();
    }
}
