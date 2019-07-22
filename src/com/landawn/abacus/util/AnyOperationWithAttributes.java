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

import java.util.Map;

import org.apache.hadoop.hbase.client.OperationWithAttributes;

/**
 * It's a wrapper of <code>OperationWithAttributes</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 1.7.13
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.OperationWithAttributes
 */
abstract class AnyOperationWithAttributes<AP extends AnyOperationWithAttributes<?>> extends AnyOperation<AP> {
    protected final OperationWithAttributes ap;

    protected AnyOperationWithAttributes(final OperationWithAttributes ap) {
        super(ap);
        this.ap = ap;
    }

    public byte[] getAttribute(String name) {
        return ap.getAttribute(name);
    }

    public Map<String, byte[]> getAttributesMap() {
        return ap.getAttributesMap();
    }

    public AP setAttribute(final String name, final Object value) {
        ap.setAttribute(name, HBaseExecutor.toValueBytes(value));

        return (AP) this;
    }

    /**
     * This method allows you to retrieve the identifier for the operation if one
     * was set.
     * @return the id or null if not set
     */
    public String getId() {
        return ap.getId();
    }

    /**
     * This method allows you to set an identifier on an operation. The original
     * motivation for this was to allow the identifier to be used in slow query
     * logging, but this could obviously be useful in other places. One use of
     * this could be to put a class.method identifier in here to see where the
     * slow query is coming from.
     * @param id
     *          id to set for the scan
     */
    public AP setId(final String id) {
        ap.setId(id);

        return (AP) this;
    }

    public int getPriority() {
        return ap.getPriority();
    }

    public AP setPriority(final int priority) {
        ap.setPriority(priority);

        return (AP) this;
    }
}
