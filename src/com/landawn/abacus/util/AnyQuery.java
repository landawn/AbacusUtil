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

import java.util.Map;

import org.apache.hadoop.hbase.client.Consistency;
import org.apache.hadoop.hbase.client.IsolationLevel;
import org.apache.hadoop.hbase.client.Query;
import org.apache.hadoop.hbase.exceptions.DeserializationException;
import org.apache.hadoop.hbase.filter.Filter;
import org.apache.hadoop.hbase.io.TimeRange;
import org.apache.hadoop.hbase.security.access.Permission;
import org.apache.hadoop.hbase.security.visibility.Authorizations;

/**
 * It's a wrapper of <code>Query</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 1.7.13
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Query
 */
abstract class AnyQuery<QP extends AnyQuery<?>> extends AnyOperationWithAttributes<QP> {
    protected final Query query;

    protected AnyQuery(final Query query) {
        super(query);
        this.query = query;
    }

    /**
     * @return Filter
     */
    public Filter getFilter() {
        return query.getFilter();
    }

    /**
     * Apply the specified server-side filter when performing the Query. Only
     * {@link Filter#filterCell(org.apache.hadoop.hbase.Cell)} is called AFTER all tests for ttl,
     * column match, deletes and column family's max versions have been run.
     * @param filter filter to run on the server
     * @return this for invocation chaining
     */
    public QP setFilter(final Filter filter) {
        this.query.setFilter(filter);

        return (QP) this;
    }

    /**
     * @return The authorizations this Query is associated with.
     * @throws DeserializationException
     */
    public Authorizations getAuthorizations() throws DeserializationException {
        return this.query.getAuthorizations();
    }

    /**
     * Sets the authorizations to be used by this Query
     * @param authorizations
     */
    public QP setAuthorizations(final Authorizations authorizations) {
        this.query.setAuthorizations(authorizations);

        return (QP) this;
    }

    /**
     * @return The serialized ACL for this operation, or null if none
     */
    public byte[] getACL() {
        return this.query.getACL();
    }

    /**
     * @param user User short name
     * @param perms Permissions for the user
     */
    public QP setACL(final String user, final Permission perms) {
        this.query.setACL(user, perms);

        return (QP) this;
    }

    /**
     * @param perms A map of permissions for a user or users
     */
    public QP setACL(final Map<String, Permission> perms) {
        this.query.setACL(perms);

        return (QP) this;
    }

    /**
     * Returns the consistency level for this operation
     * @return the consistency level
     */
    public Consistency getConsistency() {
        return this.query.getConsistency();
    }

    /**
     * Sets the consistency level for this operation
     * @param consistency the consistency level
     */
    public QP setConsistency(final Consistency consistency) {
        this.query.setConsistency(consistency);

        return (QP) this;
    }

    /**
     * Returns region replica id where Query will fetch data from.
     * @return region replica id or -1 if not set.
     */
    public int getReplicaId() {
        return this.query.getReplicaId();
    }

    /**
     * Specify region replica id where Query will fetch data from. Use this together with
     * {@link #setConsistency(Consistency)} passing {@link Consistency#TIMELINE} to read data from
     * a specific replicaId.
     * <br><b> Expert: </b>This is an advanced API exposed. Only use it if you know what you are doing
     * @param Id
     */
    public QP setReplicaId(final int id) {
        this.query.setReplicaId(id);

        return (QP) this;
    }

    /**
     * @return The isolation level of this query.
     * If no isolation level was set for this query object,
     * then it returns READ_COMMITTED.
     * @return The IsolationLevel for this query
     */
    public IsolationLevel getIsolationLevel() {
        return this.query.getIsolationLevel();
    }

    /**
     * Set the isolation level for this query. If the
     * isolation level is set to READ_UNCOMMITTED, then
     * this query will return data from committed and
     * uncommitted transactions. If the isolation level
     * is set to READ_COMMITTED, then this query will return
     * data from committed transactions only. If a isolation
     * level is not explicitly set on a Query, then it
     * is assumed to be READ_COMMITTED.
     * @param level IsolationLevel for this query
     */
    public QP setIsolationLevel(final IsolationLevel level) {
        this.query.setIsolationLevel(level);

        return (QP) this;
    }

    /**
     * Get the raw loadColumnFamiliesOnDemand setting; if it's not set, can be null.
     */
    public Boolean getLoadColumnFamiliesOnDemandValue() {
        return this.query.getLoadColumnFamiliesOnDemandValue();
    }

    /**
     * Set the value indicating whether loading CFs on demand should be allowed (cluster
     * default is false). On-demand CF loading doesn't load column families until necessary, e.g.
     * if you filter on one column, the other column family data will be loaded only for the rows
     * that are included in result, not all rows like in normal case.
     * With column-specific filters, like SingleColumnValueFilter w/filterIfMissing == true,
     * this can deliver huge perf gains when there's a cf with lots of data; however, it can
     * also lead to some inconsistent results, as follows:
     * - if someone does a concurrent update to both column families in question you may get a row
     *   that never existed, e.g. for { rowKey = 5, { cat_videos =&gt; 1 }, { video =&gt; "my cat" } }
     *   someone puts rowKey 5 with { cat_videos =&gt; 0 }, { video =&gt; "my dog" }, concurrent scan
     *   filtering on "cat_videos == 1" can get { rowKey = 5, { cat_videos =&gt; 1 },
     *   { video =&gt; "my dog" } }.
     * - if there's a concurrent split and you have more than 2 column families, some rows may be
     *   missing some column families.
     */
    public QP setLoadColumnFamiliesOnDemand(final boolean value) {
        this.query.setLoadColumnFamiliesOnDemand(value);

        return (QP) this;
    }

    /**
     * Get the logical value indicating whether on-demand CF loading should be allowed.
     */
    public boolean doLoadColumnFamiliesOnDemand() {
        return this.query.doLoadColumnFamiliesOnDemand();
    }

    /**
     * @return A map of column families to time ranges
     */
    public Map<byte[], TimeRange> getColumnFamilyTimeRange() {
        return this.query.getColumnFamilyTimeRange();
    }

    /**
     * Get versions of columns only within the specified timestamp range,
     * [minStamp, maxStamp) on a per CF bases.  Note, default maximum versions to return is 1.  If
     * your time range spans more than one version and you want all versions
     * returned, up the number of versions beyond the default.
     * Column Family time ranges take precedence over the global time range.
     *
     * @param cf the column family for which you want to restrict
     * @param minStamp minimum timestamp value, inclusive
     * @param maxStamp maximum timestamp value, exclusive
     * @return this
     */
    public QP setColumnFamilyTimeRange(final String cf, final long minStamp, final long maxStamp) {
        this.query.setColumnFamilyTimeRange(toFamilyQualifierBytes(cf), minStamp, maxStamp);

        return (QP) this;
    }

    /**
     * Get versions of columns only within the specified timestamp range,
     * [minStamp, maxStamp) on a per CF bases.  Note, default maximum versions to return is 1.  If
     * your time range spans more than one version and you want all versions
     * returned, up the number of versions beyond the default.
     * Column Family time ranges take precedence over the global time range.
     *
     * @param cf the column family for which you want to restrict
     * @param minStamp minimum timestamp value, inclusive
     * @param maxStamp maximum timestamp value, exclusive
     * @return this
     */
    public QP setColumnFamilyTimeRange(final byte[] cf, final long minStamp, final long maxStamp) {
        this.query.setColumnFamilyTimeRange(cf, minStamp, maxStamp);

        return (QP) this;
    }
}
