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

import java.io.IOException;
import java.util.Map;
import java.util.NavigableSet;

import org.apache.hadoop.hbase.HConstants;
import org.apache.hadoop.hbase.client.Cursor;
import org.apache.hadoop.hbase.client.Get;
import org.apache.hadoop.hbase.client.ResultScanner;
import org.apache.hadoop.hbase.client.Scan;
import org.apache.hadoop.hbase.client.Scan.ReadType;
import org.apache.hadoop.hbase.client.metrics.ScanMetrics;
import org.apache.hadoop.hbase.filter.Filter;
import org.apache.hadoop.hbase.io.TimeRange;

/**
 * It's a wrapper of <code>Scan</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 0.8 
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Scan
 */
public final class AnyScan extends AnyQuery<AnyScan> {
    private final Scan scan;

    public AnyScan() {
        super(new Scan());
        this.scan = (Scan) query;
    }

    /**
     * Create a Scan operation starting at the specified row.
     * <p>
     * If the specified row does not exist, the Scanner will start from the next closest row after the
     * specified row.
     * @param startRow row to start scanner at or after
     * @deprecated use {@code new Scan().withStartRow(startRow)} instead.
     */
    @Deprecated
    public AnyScan(final Object startRow) {
        super(new Scan(toRowKeyBytes(startRow)));
        this.scan = (Scan) query;
    }

    /**
     * Create a Scan operation for the range of rows specified.
     * @param startRow row to start scanner at or after (inclusive)
     * @param stopRow row to stop scanner before (exclusive)
     * @deprecated use {@code new Scan().withStartRow(startRow).withStopRow(stopRow)} instead.
     */
    @Deprecated
    public AnyScan(final Object startRow, final Object stopRow) {
        super(new Scan(toRowKeyBytes(startRow), toRowKeyBytes(stopRow)));
        this.scan = (Scan) query;
    }

    /**
     * @deprecated use {@code new Scan().withStartRow(startRow).setFilter(filter)} instead.
     */
    @Deprecated
    public AnyScan(final Object startRow, final Filter filter) {
        super(new Scan(toRowKeyBytes(startRow), filter));
        this.scan = (Scan) query;
    }

    public AnyScan(final Scan scan) {
        super(scan);
        this.scan = (Scan) query;
    }

    public AnyScan(final Get get) {
        super(get);
        this.scan = (Scan) query;
    }

    public static AnyScan create() {
        return new AnyScan();
    }

    public static AnyScan createScanFromCursor(Cursor cursor) {
        return new AnyScan(Scan.createScanFromCursor(cursor));
    }

    /**
     * Create a Scan operation starting at the specified row.
     * <p>
     * If the specified row does not exist, the Scanner will start from the next closest row after the
     * specified row.
     * @param startRow row to start scanner at or after
     * @deprecated use {@code new Scan().withStartRow(startRow)} instead.
     */
    @Deprecated
    public static AnyScan of(final Object startRow) {
        return new AnyScan(startRow);
    }

    /**
     * Create a Scan operation for the range of rows specified.
     * @param startRow row to start scanner at or after (inclusive)
     * @param stopRow row to stop scanner before (exclusive)
     * @deprecated use {@code new Scan().withStartRow(startRow).withStopRow(stopRow)} instead.
     */
    @Deprecated
    public static AnyScan of(final Object startRow, final Object stopRow) {
        return new AnyScan(startRow, stopRow);
    }

    /**
     * @deprecated use {@code new Scan().withStartRow(startRow).setFilter(filter)} instead.
     */
    @Deprecated
    public static AnyScan of(final Object startRow, final Filter filter) {
        return new AnyScan(startRow, filter);
    }

    public static AnyScan of(final Scan scan) {
        return new AnyScan(scan);
    }

    public static AnyScan of(final Get get) {
        return new AnyScan(get);
    }

    public Scan val() {
        return scan;
    }

    public boolean isGetScan() {
        return scan.isGetScan();
    }

    public boolean hasFamilies() {
        return scan.hasFamilies();
    }

    public int numFamilies() {
        return scan.numFamilies();
    }

    public byte[][] getFamilies() {
        return scan.getFamilies();
    }

    public AnyScan addFamily(String family) {
        scan.addFamily(toFamilyQualifierBytes(family));

        return this;
    }

    public AnyScan addFamily(byte[] family) {
        scan.addFamily(family);

        return this;
    }

    public Map<byte[], NavigableSet<byte[]>> getFamilyMap() {
        return scan.getFamilyMap();
    }

    public AnyScan setFamilyMap(Map<byte[], NavigableSet<byte[]>> familyMap) {
        scan.setFamilyMap(familyMap);

        return this;
    }

    @Override
    public AnyScan setColumnFamilyTimeRange(String family, long minTimestamp, long maxTimestamp) {
        scan.setColumnFamilyTimeRange(toFamilyQualifierBytes(family), minTimestamp, maxTimestamp);

        return this;
    }

    @Override
    public AnyScan setColumnFamilyTimeRange(byte[] family, long minTimestamp, long maxTimestamp) {
        scan.setColumnFamilyTimeRange(family, minTimestamp, maxTimestamp);

        return this;
    }

    public AnyScan addColumn(String family, String qualifier) {
        scan.addColumn(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));

        return this;
    }

    public AnyScan addColumn(byte[] family, byte[] qualifier) {
        scan.addColumn(family, qualifier);

        return this;
    }

    public TimeRange getTimeRange() {
        return scan.getTimeRange();
    }

    public AnyScan setTimeRange(long minStamp, long maxStamp) throws IOException {
        scan.setTimeRange(minStamp, maxStamp);

        return this;
    }

    public AnyScan setTimestamp(long timestamp) throws IOException {
        scan.setTimestamp(timestamp);

        return this;
    }

    /**
     * Get versions of columns with the specified timestamp. Note, default maximum
     * versions to return is 1.  If your time range spans more than one version
     * and you want all versions returned, up the number of versions beyond the
     * defaut.
     * @param timestamp version timestamp
     * @see #setMaxVersions()
     * @see #setMaxVersions(int)
     * @return this
     * @deprecated As of release 2.0.0, this will be removed in HBase 3.0.0.
     *             Use {@link #setTimestamp(long)} instead
     */
    @Deprecated
    public AnyScan setTimeStamp(long timestamp) throws IOException {
        scan.setTimeStamp(timestamp);

        return this;
    }

    public boolean includeStartRow() {
        return scan.includeStartRow();
    }

    public byte[] getStartRow() {
        return scan.getStartRow();
    }

    /**
     * Set the start row of the scan.
     * <p>
     * If the specified row does not exist, the Scanner will start from the next closest row after the
     * specified row.
     * @param startRow row to start scanner at or after
     * @return this
     * @throws IllegalArgumentException if startRow does not meet criteria for a row key (when length
     *           exceeds {@link HConstants#MAX_ROW_LENGTH})
     * @deprecated use {@link #withStartRow(byte[])} instead. This method may change the inclusive of
     *             the stop row to keep compatible with the old behavior.
     */
    @Deprecated
    public AnyScan setStartRow(final Object startRow) {
        scan.setStartRow(toRowKeyBytes(startRow));

        return this;
    }

    public AnyScan withStartRow(final Object startRow) {
        scan.withStartRow(toRowKeyBytes(startRow));

        return this;
    }

    public AnyScan withStartRow(final Object startRow, final boolean inclusive) {
        scan.withStartRow(toRowKeyBytes(startRow), inclusive);

        return this;
    }

    public boolean includeStopRow() {
        return scan.includeStopRow();
    }

    public byte[] getStopRow() {
        return scan.getStopRow();
    }

    /**
     * Set the stop row of the scan.
     * <p>
     * The scan will include rows that are lexicographically less than the provided stopRow.
     * <p>
     * <b>Note:</b> When doing a filter for a rowKey <u>Prefix</u> use
     * {@link #setRowPrefixFilter(byte[])}. The 'trailing 0' will not yield the desired result.
     * </p>
     * @param stopRow row to end at (exclusive)
     * @return this
     * @throws IllegalArgumentException if stopRow does not meet criteria for a row key (when length
     *           exceeds {@link HConstants#MAX_ROW_LENGTH})
     * @deprecated use {@link #withStopRow(byte[])} instead. This method may change the inclusive of
     *             the stop row to keep compatible with the old behavior.
     */
    @Deprecated
    public AnyScan setStopRow(final Object stopRow) {
        scan.setStopRow(toRowKeyBytes(stopRow));

        return this;
    }

    public AnyScan withStopRow(final Object stopRow) {
        scan.withStopRow(toRowKeyBytes(stopRow));

        return this;
    }

    public AnyScan withStopRow(final Object stopRow, final boolean inclusive) {
        scan.withStopRow(toRowKeyBytes(stopRow), inclusive);

        return this;
    }

    public AnyScan setRowPrefixFilter(final Object rowPrefix) {
        scan.setRowPrefixFilter(toRowKeyBytes(rowPrefix));

        return this;
    }

    public int getMaxVersions() {
        return scan.getMaxVersions();
    }

    /**
     * Get all available versions.
     * @return this
     * @deprecated It is easy to misunderstand with column family's max versions, so use
     *             {@link #readAllVersions()} instead.
     */
    @Deprecated
    public AnyScan setMaxVersions(int maxVersions) throws IOException {
        scan.setMaxVersions(maxVersions);

        return this;
    }

    /**
     * Get all available versions.
     * @return this
     * @deprecated It is easy to misunderstand with column family's max versions, so use
     *             {@link #readAllVersions()} instead.
     */
    @Deprecated
    public AnyScan setMaxVersions() {
        scan.setMaxVersions();

        return this;
    }

    public AnyScan readVersions(int maxVersions) throws IOException {
        scan.readVersions(maxVersions);

        return this;
    }

    public AnyScan readAllVersions() {
        scan.readAllVersions();

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

    public long getMaxResultSize() {
        return scan.getMaxResultSize();
    }

    public AnyScan setMaxResultSize(long maxResultSize) {
        scan.setMaxResultSize(maxResultSize);

        return this;
    }

    public int getLimit() {
        return scan.getLimit();
    }

    public AnyScan setLimit(int limit) {
        scan.setLimit(limit);

        return this;
    }

    public AnyScan setOneRowLimit() {
        scan.setOneRowLimit();

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

    public boolean isRaw() {
        return scan.isRaw();
    }

    public AnyScan setRaw(boolean raw) {
        scan.setRaw(raw);

        return this;
    }

    /**
     * Get whether this scan is a small scan
     * @return true if small scan
     * @deprecated since 2.0.0. See the comment of {@link #setSmall(boolean)}
     */
    @Deprecated
    public boolean isSmall() {
        return scan.isSmall();
    }

    /**
     * Set whether this scan is a small scan
     * <p>
     * Small scan should use pread and big scan can use seek + read seek + read is fast but can cause
     * two problem (1) resource contention (2) cause too much network io [89-fb] Using pread for
     * non-compaction read request https://issues.apache.org/jira/browse/HBASE-7266 On the other hand,
     * if setting it true, we would do openScanner,next,closeScanner in one RPC call. It means the
     * better performance for small scan. [HBASE-9488]. Generally, if the scan range is within one
     * data block(64KB), it could be considered as a small scan.
     * @param small
     * @deprecated since 2.0.0. Use {@link #setLimit(int)} and {@link #setReadType(ReadType)} instead.
     *             And for the one rpc optimization, now we will also fetch data when openScanner, and
     *             if the number of rows reaches the limit then we will close the scanner
     *             automatically which means we will fall back to one rpc.
     * @see #setLimit(int)
     * @see #setReadType(ReadType)
     */
    @Deprecated
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

    /**
     * @return Metrics on this Scan, if metrics were enabled.
     * @see #setScanMetricsEnabled(boolean)
     * @deprecated Use {@link ResultScanner#getScanMetrics()} instead. And notice that, please do not
     *             use this method and {@link ResultScanner#getScanMetrics()} together, the metrics
     *             will be messed up.
     */
    @Deprecated
    public ScanMetrics getScanMetrics() {
        return scan.getScanMetrics();
    }

    public Boolean isAsyncPrefetch() {
        return scan.isAsyncPrefetch();
    }

    public AnyScan setAsyncPrefetch(boolean asyncPrefetch) {
        scan.setAsyncPrefetch(asyncPrefetch);

        return this;
    }

    /**
     * @return the read type for this scan
     */
    public ReadType getReadType() {
        return scan.getReadType();
    }

    /**
     * Set the read type for this scan.
     * <p>
     * Notice that we may choose to use pread even if you specific {@link ReadType#STREAM} here. For
     * example, we will always use pread if this is a get scan.
     * @return this
     */
    public AnyScan setReadType(ReadType readType) {
        scan.setReadType(readType);

        return this;
    }

    public boolean isNeedCursorResult() {
        return scan.isNeedCursorResult();
    }

    public AnyScan setNeedCursorResult(boolean needCursorResult) {
        scan.setAllowPartialResults(needCursorResult);

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
