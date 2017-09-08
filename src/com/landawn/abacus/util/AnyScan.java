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

import org.apache.hadoop.hbase.client.Consistency;
import org.apache.hadoop.hbase.client.IsolationLevel;
import org.apache.hadoop.hbase.client.Scan;
import org.apache.hadoop.hbase.client.metrics.ScanMetrics;
import org.apache.hadoop.hbase.exceptions.DeserializationException;
import org.apache.hadoop.hbase.filter.Filter;
import org.apache.hadoop.hbase.io.TimeRange;
import org.apache.hadoop.hbase.security.access.Permission;
import org.apache.hadoop.hbase.security.visibility.Authorizations;

/**
 * It's a wrapper of <code>Scan</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Scan
 */
public final class AnyScan {
    private final Scan scan;

    public AnyScan() {
        scan = new Scan();
    }

    public AnyScan(final Object startRow) {
        scan = new Scan(toRowKeyBytes(startRow));
    }

    public AnyScan(final Object startRow, final Object stopRow) {
        scan = new Scan(toRowKeyBytes(startRow), toRowKeyBytes(stopRow));
    }

    public AnyScan(final Object startRow, final Filter filter) {
        scan = new Scan(toRowKeyBytes(startRow), filter);
    }

    public static AnyScan of(final Object startRow) {
        return new AnyScan(startRow);
    }

    public static AnyScan of(final Object startRow, final Object stopRow) {
        return new AnyScan(startRow, stopRow);
    }

    public static AnyScan of(final Object startRow, final Filter filter) {
        return new AnyScan(startRow, filter);
    }

    public Scan value() {
        return scan;
    }

    public AnyScan addFamily(String family) {
        scan.addFamily(toFamilyQualifierBytes(family));

        return this;
    }

    public AnyScan addColumn(String family, String qualifier) {
        scan.addColumn(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));

        return this;
    }

    public boolean hasFamilies() {
        return scan.hasFamilies();
    }

    public int numFamilies() {
        return scan.numFamilies();
    }

    public Map<byte[], NavigableSet<byte[]>> getFamilyMap() {
        return scan.getFamilyMap();
    }

    public byte[][] getFamilies() {
        return scan.getFamilies();
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class.
     * The properties not set by this should be get by the methods in <code>Get</code>
     * 
     * @return
     */
    Map<String, Object> getFingerprint() {
        return scan.getFingerprint();
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class
     * The properties not set by this should be get by the methods in <code>Get</code>
     * 
     * @param maxCols
     * @return
     */
    Map<String, Object> toMap(int maxCols) {
        return scan.toMap(maxCols);
    }

    public TimeRange getTimeRange() {
        return scan.getTimeRange();
    }

    public AnyScan setTimeRange(long minStamp, long maxStamp) throws IOException {
        scan.setTimeRange(minStamp, maxStamp);

        return this;
    }

    public AnyScan setTimeStamp(long timestamp) throws IOException {
        scan.setTimeStamp(timestamp);

        return this;
    }

    public byte[] getStartRow() {
        return scan.getStartRow();
    }

    public AnyScan setStartRow(final Object startRow) {
        scan.setStartRow(toRowKeyBytes(startRow));

        return this;
    }

    public byte[] getStopRow() {
        return scan.getStopRow();
    }

    public AnyScan setStopRow(final Object stopRow) {
        scan.setStopRow(toRowKeyBytes(stopRow));

        return this;
    }

    public int getMaxVersions() {
        return scan.getMaxVersions();
    }

    public AnyScan setMaxVersions(int maxVersions) throws IOException {
        scan.setMaxVersions(maxVersions);

        return this;
    }

    public AnyScan setMaxVersions() {
        scan.setMaxVersions();

        return this;
    }

    public int getBatch() {
        return scan.getBatch();
    }

    public AnyScan setBatch(int batch) {
        scan.setBatch(batch);

        return this;
    }

    public int getMaxResultsPerColumnFamily() {
        return scan.getMaxResultsPerColumnFamily();
    }

    public AnyScan setMaxResultsPerColumnFamily(int limit) {
        scan.setMaxResultsPerColumnFamily(limit);

        return this;
    }

    public int getRowOffsetPerColumnFamily() {
        return scan.getRowOffsetPerColumnFamily();
    }

    public AnyScan setRowOffsetPerColumnFamily(int offset) {
        scan.setRowOffsetPerColumnFamily(offset);

        return this;
    }

    public AnyScan setRowPrefixFilter(String rowPrefix) {
        scan.setRowPrefixFilter(toRowKeyBytes(rowPrefix));

        return this;
    }

    public int getCaching() {
        return scan.getCaching();
    }

    public AnyScan setCaching(int caching) {
        scan.setCaching(caching);

        return this;
    }

    public boolean getCacheBlocks() {
        return scan.getCacheBlocks();
    }

    public AnyScan setCacheBlocks(boolean cacheBlocks) {
        scan.setCacheBlocks(cacheBlocks);

        return this;
    }

    public Filter getFilter() {
        return scan.getFilter();
    }

    public AnyScan setFilter(Filter filter) {
        scan.setFilter(filter);

        return this;
    }

    public boolean hasFilter() {
        return scan.hasFilter();
    }

    public boolean isReversed() {
        return scan.isReversed();
    }

    public AnyScan setReversed(boolean reversed) {
        scan.setReversed(reversed);

        return this;
    }

    public boolean getAllowPartialResults() {
        return scan.getAllowPartialResults();
    }

    public AnyScan setAllowPartialResults(boolean allowPartialResults) {
        scan.setAllowPartialResults(allowPartialResults);

        return this;
    }

    public Boolean getLoadColumnFamiliesOnDemandValue() {
        return scan.getLoadColumnFamiliesOnDemandValue();
    }

    public AnyScan setLoadColumnFamiliesOnDemand(boolean value) {
        scan.setLoadColumnFamiliesOnDemand(value);

        return this;
    }

    public boolean doLoadColumnFamiliesOnDemand() {
        return scan.doLoadColumnFamiliesOnDemand();
    }

    public boolean isRaw() {
        return scan.isRaw();
    }

    public AnyScan setRaw(boolean raw) {
        scan.setRaw(raw);

        return this;
    }

    public boolean isSmall() {
        return scan.isSmall();
    }

    public AnyScan setSmall(boolean small) {
        scan.setSmall(small);

        return this;
    }

    public boolean isScanMetricsEnabled() {
        return scan.isScanMetricsEnabled();
    }

    public AnyScan setScanMetricsEnabled(final boolean enabled) {
        scan.setScanMetricsEnabled(enabled);

        return this;
    }

    public ScanMetrics getScanMetrics() {
        return scan.getScanMetrics();
    }

    public String getId() {
        return scan.getId();
    }

    public AnyScan setId(String id) {
        scan.setId(id);

        return this;
    }

    public Authorizations getAuthorizations() throws DeserializationException {
        return scan.getAuthorizations();
    }

    public AnyScan setAuthorizations(Authorizations authorizations) {
        scan.setAuthorizations(authorizations);

        return this;
    }

    public byte[] getACL() {
        return scan.getACL();
    }

    public AnyScan setACL(Map<String, Permission> perms) {
        scan.setACL(perms);

        return this;
    }

    public AnyScan setACL(String user, Permission perms) {
        scan.setACL(user, perms);

        return this;
    }

    public Consistency getConsistency() {
        return scan.getConsistency();
    }

    public AnyScan setConsistency(Consistency consistency) {
        scan.setConsistency(consistency);

        return this;
    }

    public int getReplicaId() {
        return scan.getReplicaId();
    }

    public AnyScan setReplicaId(int Id) {
        scan.setReplicaId(Id);

        return this;
    }

    public IsolationLevel getIsolationLevel() {
        return scan.getIsolationLevel();
    }

    public AnyScan setIsolationLevel(IsolationLevel level) {
        scan.setIsolationLevel(level);

        return this;
    }

    public Map<String, byte[]> getAttributesMap() {
        return scan.getAttributesMap();
    }

    public byte[] getAttribute(String name) {
        return scan.getAttribute(name);
    }

    public AnyScan setAttribute(String name, Object value) {
        scan.setAttribute(name, toValueBytes(value));

        return this;
    }

    @Override
    public int hashCode() {
        return scan.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof AnyScan) {
            AnyScan other = (AnyScan) obj;

            return this.scan.equals(other.scan);
        }

        return false;
    }

    @Override
    public String toString() {
        return scan.toString();
    }
}
