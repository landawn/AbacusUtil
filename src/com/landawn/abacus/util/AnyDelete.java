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
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.apache.hadoop.hbase.Cell;
import org.apache.hadoop.hbase.client.Delete;
import org.apache.hadoop.hbase.exceptions.DeserializationException;
import org.apache.hadoop.hbase.security.access.Permission;
import org.apache.hadoop.hbase.security.visibility.CellVisibility;

/**
 * It's a wrapper of <code>Delete</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Delete
 */
public final class AnyDelete {
    private final Delete delete;

    public AnyDelete(Object rowKey) {
        this.delete = new Delete(toRowKeyBytes(rowKey));
    }

    public AnyDelete(Object rowKey, long timestamp) {
        this.delete = new Delete(toRowKeyBytes(rowKey), timestamp);
    }

    public static AnyDelete of(Object rowKey) {
        return new AnyDelete(rowKey);
    }

    public static AnyDelete of(Object rowKey, long timestamp) {
        return new AnyDelete(rowKey, timestamp);
    }

    public Delete value() {
        return delete;
    }

    public long getTimeStamp() {
        return delete.getTimeStamp();
    }

    public AnyDelete setTimestamp(long timestamp) {
        delete.setTimestamp(timestamp);

        return this;
    }

    public AnyDelete addDeleteMarker(Cell kv) throws IOException {
        delete.addDeleteMarker(kv);

        return this;
    }

    /**
     * Delete all versions of all columns of the specified family.
     * <p>
     * Overrides previous calls to deleteColumn and deleteColumns for the
     * specified family.
     * 
     * @param family
     * @return
     */
    public AnyDelete addFamily(String family) {
        delete.addFamily(toFamilyQualifierBytes(family));

        return this;
    }

    /**
     * Delete all columns of the specified family with a timestamp less than
     * or equal to the specified timestamp.
     * <p>
     * Overrides previous calls to deleteColumn and deleteColumns for the
     * specified family.
     * 
     * @param family
     * @param timestamp
     * @return
     */
    public AnyDelete addFamily(String family, long timestamp) {
        delete.addFamily(toFamilyQualifierBytes(family), timestamp);

        return this;
    }

    /**
     * Delete all columns of the specified family with a timestamp equal to the specified timestamp.
     * 
     * @param family
     * @param timestamp
     * @return
     */
    public AnyDelete addFamilyVersion(String family, final long timestamp) {
        delete.addFamilyVersion(toFamilyQualifierBytes(family), timestamp);

        return this;
    }

    public int numFamilies() {
        return delete.numFamilies();
    }

    /**
     * Delete the latest version of the specified column.
     * This is an expensive call in that on the server-side, it first does a
     * get to find the latest versions timestamp.  Then it adds a delete using
     * the fetched cells timestamp.
     * 
     * @param family
     * @param qualifier
     * @return
     */
    public AnyDelete addColumn(String family, String qualifier) {
        delete.addColumn(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));

        return this;
    }

    /**
     * Delete the specified version of the specified column.
     * 
     * @param family
     * @param qualifier
     * @param timestamp
     * @return
     */
    public AnyDelete addColumn(String family, String qualifier, long timestamp) {
        delete.addColumn(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), timestamp);

        return this;
    }

    /**
     * Delete all versions of the specified column.
     * 
     * @param family
     * @param qualifier
     * @return
     */
    public AnyDelete addColumns(String family, String qualifier) {
        delete.addColumns(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));

        return this;
    }

    /**
     * Delete all versions of the specified column with a timestamp less than or equal to the specified timestamp.
     * 
     * @param family
     * @param qualifier
     * @param timestamp
     * @return
     */
    public AnyDelete addColumns(String family, String qualifier, long timestamp) {
        delete.addColumns(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), timestamp);

        return this;
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class.
     * The properties not set by this should be delete by the methods in <code>Get</code>
     * 
     * @return
     */
    Map<String, Object> getFingerprint() {
        return delete.getFingerprint();
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class
     * The properties not set by this should be delete by the methods in <code>Get</code>
     * 
     * @param maxCols
     * @return
     */
    Map<String, Object> toMap(int maxCols) {
        return delete.toMap(maxCols);
    }

    /**
     * To Keep it simple, there should be no methods for the properties if it's not set by this class
     * The properties not set by this should be delete by the methods in <code>Get</code>
     * 
     * @return
     */
    byte[] getRow() {
        return delete.getRow();
    }

    public String getId() {
        return delete.getId();
    }

    public AnyDelete setId(String id) {
        delete.setId(id);

        return this;
    }

    public List<UUID> getClusterIds() {
        return delete.getClusterIds();
    }

    public AnyDelete setClusterIds(List<UUID> clusterIds) {
        delete.setClusterIds(clusterIds);

        return this;
    }

    public CellVisibility getCellVisibility() throws DeserializationException {
        return delete.getCellVisibility();
    }

    public AnyDelete setCellVisibility(CellVisibility expression) {
        delete.setCellVisibility(expression);

        return this;
    }

    public byte[] getACL() {
        return delete.getACL();
    }

    public AnyDelete setACL(Map<String, Permission> perms) {
        delete.setACL(perms);

        return this;
    }

    public AnyDelete setACL(String user, Permission perms) {
        delete.setACL(user, perms);

        return this;
    }

    public Map<String, byte[]> getAttributesMap() {
        return delete.getAttributesMap();
    }

    public byte[] getAttribute(String name) {
        return delete.getAttribute(name);
    }

    public AnyDelete setAttribute(String name, Object value) {
        delete.setAttribute(name, toValueBytes(value));

        return this;
    }

    @Override
    public int hashCode() {
        return delete.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof AnyDelete) {
            AnyDelete other = (AnyDelete) obj;

            return this.delete.equals(other.delete);
        }

        return false;
    }

    @Override
    public String toString() {
        return delete.toString();
    }
}
