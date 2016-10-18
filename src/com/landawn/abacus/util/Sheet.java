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

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public interface Sheet<R, C, E> {

    Set<R> rowKeySet();

    Set<C> columnKeySet();

    E get(R rowKey, C columnKey);

    E put(R rowKey, C columnKey, E value);

    void putAll(Sheet<R, C, ? extends E> source);

    E remove(R rowKey, C columnKey);

    boolean containsValue(Object value);

    List<E> getRow(R rowKey);

    void setRow(R rowKey, Collection<? extends E> row);

    void addRow(R rowKey, Collection<? extends E> row);

    void removeRow(R rowKey);

    boolean containsRow(R rowKey);

    Map<C, E> row(R rowKey);

    Map<R, Map<C, E>> rowMap();

    List<E> getColumn(C columnKey);

    void setColumn(C columnKey, Collection<? extends E> column);

    void addColumn(C columnKey, Collection<? extends E> column);

    void removeColumn(C columnKey);

    boolean containsColumn(C columnKey);

    Map<R, E> column(C columnKey);

    Map<C, Map<R, E>> columnMap();

    /**
     * Returns the size of row key set.
     *
     * @return
     */
    int rowLength();

    /**
     * Returns the size of column key set.
     *
     * @return
     */
    int columnLength();

    void clear();

    void trimToSize();

    <T extends Sheet<R, C, E>> T copy();

    Object[][] toArray();
}
