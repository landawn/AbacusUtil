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
import java.util.NavigableMap;
import java.util.UUID;

import org.apache.hadoop.hbase.Cell;
import org.apache.hadoop.hbase.Tag;
import org.apache.hadoop.hbase.client.Durability;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.exceptions.DeserializationException;
import org.apache.hadoop.hbase.security.access.Permission;
import org.apache.hadoop.hbase.security.visibility.CellVisibility;

import com.landawn.abacus.exception.UncheckedIOException;

/**
 * It's a wrapper of <code>Put</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Put
 */
public final class AnyPut {
    private final Put put;

    public AnyPut(Object rowKey) {
        put = new Put(toRowKeyBytes(rowKey));
    }

    public AnyPut(Object rowKey, long ts) {
        put = new Put(toRowKeyBytes(rowKey), ts);
    }

    public static AnyPut of(Object rowKey) {
        return new AnyPut(rowKey);
    }

    public static AnyPut of(Object rowKey, long timestamp) {
        return new AnyPut(rowKey, timestamp);
    }

    public Put value() {
        return put;
    }

    public long getTimeStamp() {
        return put.getTimeStamp();
    }

    public AnyPut addColumn(String family, String qualifier, Object value) {
        put.addColumn(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), toValueBytes(value));

        return this;
    }

    public AnyPut addColumn(String family, String qualifier, long ts, Object value) {
        put.addColumn(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), ts, toValueBytes(value));

        return this;
    }

    public AnyPut addImmutable(String family, String qualifier, Object value) {
        put.addImmutable(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), toValueBytes(value));

        return this;
    }

    public AnyPut addImmutable(String family, String qualifier, long ts, Object value) {
        put.addImmutable(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), ts, toValueBytes(value));

        return this;
    }

    public AnyPut addImmutable(String family, String qualifier, Object value, Tag[] tag) {
        put.addImmutable(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), toValueBytes(value), tag);

        return this;
    }

    public AnyPut addImmutable(String family, String qualifier, long ts, Object value, Tag[] tag) {
        put.addImmutable(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), ts, toValueBytes(value), tag);

        return this;
    }

    public AnyPut add(Cell kv) throws UncheckedIOException {
        try {
            put.add(kv);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        return this;
    }

    public boolean has(String family, String qualifier) {
        return put.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));
    }

    public boolean has(String family, String qualifier, long ts) {
        return put.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), ts);
    }

    public boolean has(String family, String qualifier, Object value) {
        return put.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), toValueBytes(value));
    }

    public boolean has(String family, String qualifier, long ts, Object value) {
        return put.has(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier), ts, toValueBytes(value));
    }

    public List<Cell> get(String family, String qualifier) {
        return put.get(toFamilyQualifierBytes(family), toFamilyQualifierBytes(qualifier));
    }

    public int numFamilies() {
        return put.numFamilies();
    }

    public NavigableMap<byte[], List<Cell>> getFamilyCellMap() {
        return put.getFamilyCellMap();
    }

    public AnyPut setFamilyCellMap(NavigableMap<byte[], List<Cell>> map) {
        put.setFamilyCellMap(map);

        return this;
    }

    public Durability getDurability() {
        return put.getDurability();
    }

    public AnyPut setDurability(Durability d) {
        put.setDurability(d);

        return this;
    }

    public List<UUID> getClusterIds() {
        return put.getClusterIds();
    }

    public AnyPut setClusterIds(List<UUID> clusterIds) {
        put.setClusterIds(clusterIds);

        return this;
    }

    public CellVisibility getCellVisibility() throws DeserializationException {
        return put.getCellVisibility();
    }

    public AnyPut setCellVisibility(CellVisibility expression) {
        put.setCellVisibility(expression);

        return this;
    }

    public byte[] getACL() {
        return put.getACL();
    }

    public AnyPut setACL(String user, Permission perms) {
        put.setACL(user, perms);

        return this;
    }

    public AnyPut setACL(Map<String, Permission> perms) {
        put.setACL(perms);

        return this;
    }

    public long getTTL() {
        return put.getTTL();
    }

    public AnyPut setTTL(long ttl) {
        put.setTTL(ttl);

        return this;
    }

    public String getId() {
        return put.getId();
    }

    public AnyPut setId(String id) {
        put.setId(id);

        return this;
    }

    public Map<String, byte[]> getAttributesMap() {
        return put.getAttributesMap();
    }

    public byte[] getAttribute(String name) {
        return put.getAttribute(name);
    }

    public AnyPut setAttribute(String name, Object value) {
        put.setAttribute(name, toValueBytes(value));

        return this;
    }

    @Override
    public int hashCode() {
        return put.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof AnyPut) {
            AnyPut other = (AnyPut) obj;

            return this.put.equals(other.put);
        }

        return false;
    }

    @Override
    public String toString() {
        return put.toString();
    }
}
