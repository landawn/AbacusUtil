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

import java.io.IOException;
import java.util.Map;

import org.apache.hadoop.hbase.client.Operation;

/**
 * It's a wrapper of <code>Operation</code> in HBase to reduce the manual conversion between bytes and String/Object.
 * 
 * @since 1.7.13
 * 
 * @see <a href="http://hbase.apache.org/devapidocs/index.html">http://hbase.apache.org/devapidocs/index.html</a>
 * @see org.apache.hadoop.hbase.client.Operation
 */
abstract class AnyOperation<AP extends AnyOperation<?>> {
    protected final Operation op;

    protected AnyOperation(final Operation op) {
        this.op = op;
    }

    public Map<String, Object> getFingerprint() {
        return op.getFingerprint();
    }

    public Map<String, Object> toMap() {
        return op.toMap();
    }

    public Map<String, Object> toMap(final int maxCols) {
        return op.toMap(maxCols);
    }

    public String toJSON() throws IOException {
        return op.toJSON();
    }

    public String toJSON(final int maxCols) throws IOException {
        return op.toJSON(maxCols);
    }

    @Override
    public String toString() {
        return op.toString();
    }

    public String toString(final int maxCols) {
        return op.toString(maxCols);
    }
}
