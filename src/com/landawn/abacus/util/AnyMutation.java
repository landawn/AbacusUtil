/*
 * Copyright (C) 2019 HaiYang Li
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

import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.UUID;

import org.apache.hadoop.hbase.Cell;
import org.apache.hadoop.hbase.CellScanner;
import org.apache.hadoop.hbase.client.Durability;
import org.apache.hadoop.hbase.client.Mutation;
import org.apache.hadoop.hbase.client.Row;
import org.apache.hadoop.hbase.exceptions.DeserializationException;
import org.apache.hadoop.hbase.security.access.Permission;
import org.apache.hadoop.hbase.security.visibility.CellVisibility;

/**
 * It's a wrapper of <code>Mutation</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 1.7.13
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Mutation
 */
abstract class AnyMutation<MP extends AnyMutation<?>> extends AnyOperationWithAttributes<MP> implements Comparable<Row> {
    protected final Mutation mutation;

    protected AnyMutation(final Mutation mutation) {
        super(mutation);

        this.mutation = mutation;
    }

    public CellScanner cellScanner() {
        return mutation.cellScanner();
    }

    /**
     * Compile the column family (i.e. schema) information
     * into a Map. Useful for parsing and aggregation by debugging,
     * logging, and administration tools.
     * @return Map
     */
    @Override
    public Map<String, Object> getFingerprint() {
        return mutation.getFingerprint();
    }

    /** Get the current durability */
    public Durability getDurability() {
        return mutation.getDurability();
    }

    /**
     * Set the durability for this mutation
     * @param d
     */
    public MP setDurability(Durability d) {
        mutation.setDurability(d);

        return (MP) this;
    }

    /**
     * Method for retrieving the put's familyMap
     * @return familyMap
     */
    public NavigableMap<byte[], List<Cell>> getFamilyCellMap() {
        return mutation.getFamilyCellMap();
    }

    /**
     * Method for setting the mutation's familyMap
     * @deprecated As of release 2.0.0, this will be removed in HBase 3.0.0.
     *             Use {@link Mutation#Mutation(byte[], long, NavigableMap)} instead
     */
    @Deprecated
    public MP setFamilyCellMap(NavigableMap<byte[], List<Cell>> map) {
        mutation.setFamilyCellMap(map);

        return (MP) this;
    }

    /**
     * Method for retrieving the timestamp
     * @return timestamp
     * @deprecated As of release 2.0.0, this will be removed in HBase 3.0.0.
     *             Use {@link #getTimestamp()} instead
     */
    @Deprecated
    public long getTimeStamp() {
        return mutation.getTimeStamp();
    }

    /**
     * Method for retrieving the timestamp.
     *
     * @return timestamp
     */
    public long getTimestamp() {
        return mutation.getTimestamp();
    }

    /**
     * Set the timestamp of the delete.
     */
    public MP setTimestamp(long timestamp) {
        mutation.setTimestamp(timestamp);

        return (MP) this;
    }

    /**
     * @return the set of clusterIds that have consumed the mutation
     */
    public List<UUID> getClusterIds() {
        return mutation.getClusterIds();
    }

    /**
     * Marks that the clusters with the given clusterIds have consumed the mutation
     * @param clusterIds of the clusters that have consumed the mutation
     */
    public MP setClusterIds(List<UUID> clusterIds) {
        mutation.setClusterIds(clusterIds);

        return (MP) this;
    }

    /**
     * @return CellVisibility associated with cells in this Mutation.
     * @throws DeserializationException
     */
    public CellVisibility getCellVisibility() throws DeserializationException {
        return mutation.getCellVisibility();
    }

    /**
     * Sets the visibility expression associated with cells in this Mutation.
     * @param expression
     */
    public MP setCellVisibility(CellVisibility expression) {
        mutation.setCellVisibility(expression);

        return (MP) this;
    }

    /**
     * @return The serialized ACL for this operation, or null if none
     */
    public byte[] getACL() {
        return mutation.getACL();
    }

    /**
     * @param user User short name
     * @param perms Permissions for the user
     */
    public MP setACL(String user, Permission perms) {
        mutation.setACL(user, perms);

        return (MP) this;
    }

    /**
     * @param perms A map of permissions for a user or users
     */
    public MP setACL(Map<String, Permission> perms) {
        mutation.setACL(perms);

        return (MP) this;
    }

    /**
     * Return the TTL requested for the result of the mutation, in milliseconds.
     * @return the TTL requested for the result of the mutation, in milliseconds,
     * or Long.MAX_VALUE if unset
     */
    public long getTTL() {
        return mutation.getTTL();
    }

    /**
     * Set the TTL desired for the result of the mutation, in milliseconds.
     * @param ttl the TTL desired for the result of the mutation, in milliseconds
     * @return this
     */
    public MP setTTL(long ttl) {
        mutation.setTTL(ttl);

        return (MP) this;
    }

    /**
     * Returns a list of all KeyValue objects with matching column family and qualifier.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @return a list of KeyValue objects with the matching family and qualifier,
     *   returns an empty list if one doesn't exist for the given family.
     */
    public List<Cell> get(final String family, final String qualifier) {
        return mutation.get(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));
    }

    /**
     * Returns a list of all KeyValue objects with matching column family and qualifier.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @return a list of KeyValue objects with the matching family and qualifier,
     *   returns an empty list if one doesn't exist for the given family.
     */
    public List<Cell> get(final byte[] family, final byte[] qualifier) {
        return mutation.get(family, qualifier);
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * a value assigned to the given family &amp; qualifier.
     * Both given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @return returns true if the given family and qualifier already has an
     * existing KeyValue object in the family map.
     */
    public boolean has(final String family, final String qualifier) {
        return mutation.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * a value assigned to the given family, qualifier and timestamp.
     * All 3 given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @param ts timestamp
     * @return returns true if the given family, qualifier and timestamp already has an
     * existing KeyValue object in the family map.
     */
    public boolean has(final String family, final String qualifier, long ts) {
        return mutation.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), ts);
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * a value assigned to the given family, qualifier and timestamp.
     * All 3 given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @param value value to check
     * @return returns true if the given family, qualifier and value already has an
     * existing KeyValue object in the family map.
     */
    public boolean has(final String family, final String qualifier, final Object value) {
        return mutation.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), HBaseExecutor.toValueBytes(value));
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * the given value assigned to the given family, qualifier and timestamp.
     * All 4 given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @param ts timestamp
     * @param value value to check
     * @return returns true if the given family, qualifier timestamp and value
     *   already has an existing KeyValue object in the family map.
     */
    public boolean has(final String family, final String qualifier, long ts, final Object value) {
        return mutation.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), ts, HBaseExecutor.toValueBytes(value));
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * a value assigned to the given family &amp; qualifier.
     * Both given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @return returns true if the given family and qualifier already has an
     * existing KeyValue object in the family map.
     */
    public boolean has(final byte[] family, final byte[] qualifier) {
        return mutation.has(family, qualifier);
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * a value assigned to the given family, qualifier and timestamp.
     * All 3 given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @param ts timestamp
     * @return returns true if the given family, qualifier and timestamp already has an
     * existing KeyValue object in the family map.
     */
    public boolean has(final byte[] family, final byte[] qualifier, long ts) {
        return mutation.has(family, qualifier, ts);
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * a value assigned to the given family, qualifier and timestamp.
     * All 3 given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @param value value to check
     * @return returns true if the given family, qualifier and value already has an
     * existing KeyValue object in the family map.
     */
    public boolean has(final byte[] family, final byte[] qualifier, final byte[] value) {
        return mutation.has(family, qualifier, value);
    }

    /**
     * A convenience method to determine if this object's familyMap contains
     * the given value assigned to the given family, qualifier and timestamp.
     * All 4 given arguments must match the KeyValue object to return true.
     *
     * @param family column family
     * @param qualifier column qualifier
     * @param ts timestamp
     * @param value value to check
     * @return returns true if the given family, qualifier timestamp and value
     *   already has an existing KeyValue object in the family map.
     */
    public boolean has(final byte[] family, final byte[] qualifier, long ts, final byte[] value) {
        return mutation.has(family, qualifier, ts, value);
    }

    /**
     * Method for retrieving the delete's row
     * @return row
     */
    public byte[] getRow() {
        return mutation.getRow();
    }

    /**
     * Method to check if the familyMap is empty
     * @return true if empty, false otherwise
     */
    public boolean isEmpty() {
        return mutation.isEmpty();
    }

    /**
     * Number of KeyValues carried by this Mutation.
     * @return the total number of KeyValues
     */
    public int size() {
        return mutation.size();
    }

    /**
     * @return the number of different families
     */
    public int numFamilies() {
        return mutation.numFamilies();
    }

    /**
     * @return Calculate what Mutation adds to class heap size.
     */
    public long heapSize() {
        return mutation.heapSize();
    }

    /**
     * @deprecated As of release 2.0.0, this will be removed in HBase 3.0.0.
     *             Use {@link Row#COMPARATOR} instead
     */
    @Override
    @Deprecated
    public int compareTo(final Row d) {
        return mutation.compareTo(d);
    }
}
