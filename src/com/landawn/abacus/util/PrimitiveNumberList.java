/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public abstract class PrimitiveNumberList<C, P, E, A, L extends PrimitiveList<C, P, E, A, L>> extends AbastractPrimitiveList<C, P, E, A, L> {

    public Number sum() {
        return sum(0, size());
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return Long for byte/short/int/long or Double for float/double
     */
    public abstract Number sum(final int fromIndex, final int toIndex);

    public Number avg() {
        return avg(0, size());
    }

    /**
     * 
     * @param fromIndex
     * @param toIndex
     * @return Double
     */
    public abstract Number avg(final int fromIndex, final int toIndex);
}
