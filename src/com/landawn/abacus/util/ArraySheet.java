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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public final class ArraySheet<R, C, E> implements Sheet<R, C, E> {
    private final Set<R> rowKeySet;
    private final Set<C> columnKeySet;
    private BiMap<R, Integer> rowKeyIndexMap;
    private BiMap<C, Integer> columnKeyIndexMap;
    private List<List<Object>> columnList;
    private boolean initialized = false;

    public ArraySheet(Collection<R> rowKeySet, Collection<C> columnKeySet) {
        this.rowKeySet = new LinkedHashSet<>(rowKeySet);
        this.columnKeySet = new LinkedHashSet<>(columnKeySet);
    }

    @Override
    public Set<R> rowKeySet() {
        return rowKeySet;
    }

    @Override
    public Set<C> columnKeySet() {
        return columnKeySet;
    }

    @Override
    public E get(R rowKey, C columnKey) {
        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);
            final int columnIndex = getColumnIndex(columnKey);

            return get(rowIndex, columnIndex);
        } else {
            checkRowKey(rowKey);
            checkColumnKey(columnKey);

            return null;
        }
    }

    /**
     *
     * @param rowIndex
     * @param columnIndex
     * @return
     */
    public E get(int rowIndex, int columnIndex) {
        if (initialized) {
            return (E) columnList.get(columnIndex).get(rowIndex);
        } else {
            checkRowIndex(rowIndex);
            checkColumnIndex(columnIndex);

            return null;
        }
    }

    @Override
    public E put(R rowKey, C columnKey, E value) {
        init();

        final int rowIndex = getRowIndex(rowKey);
        final int columnIndex = getColumnIndex(columnKey);

        return put(rowIndex, columnIndex, value);
    }

    public E put(int rowIndex, int columnIndex, E value) {
        init();

        final Object previousValue = columnList.get(columnIndex).get(rowIndex);
        columnList.get(columnIndex).set(rowIndex, value);

        return (E) previousValue;
    }

    @Override
    public void putAll(Sheet<R, C, ? extends E> source) {
        if (!this.rowKeySet().containsAll(source.rowKeySet())) {
            throw new IllegalArgumentException(source.rowKeySet() + " are not all included in this sheet with row key set: " + this.rowKeySet());
        }

        if (!this.columnKeySet().containsAll(source.columnKeySet())) {
            throw new IllegalArgumentException(source.columnKeySet() + " are not all included in this sheet with column key set: " + this.columnKeySet());
        }

        for (R r : source.rowKeySet()) {
            for (C c : source.columnKeySet()) {
                this.put(r, c, source.get(r, c));
            }
        }
    }

    @Override
    public E remove(R rowKey, C columnKey) {
        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);
            final int columnIndex = getColumnIndex(columnKey);

            return remove(rowIndex, columnIndex);
        } else {
            checkRowKey(rowKey);
            checkColumnKey(columnKey);

            return null;
        }
    }

    public E remove(int rowIndex, int columnIndex) {
        if (initialized) {
            final Object previousValue = columnList.get(columnIndex).get(rowIndex);
            columnList.get(columnIndex).set(rowIndex, null);
            return (E) previousValue;
        } else {
            checkRowIndex(rowIndex);
            checkColumnIndex(columnIndex);

            return null;
        }
    }

    @Override
    public boolean containsValue(Object value) {
        if (value == null) {
            for (R r : rowKeySet()) {
                for (C c : columnKeySet()) {
                    if (this.get(r, c) == null) {
                        return true;
                    }
                }
            }
        } else {
            for (R r : rowKeySet()) {
                for (C c : columnKeySet()) {
                    if (value.equals(this.get(r, c))) {
                        return true;
                    }
                }
            }
        }

        return false;
    }

    @Override
    public List<E> getRow(R rowKey) {
        final int columnLength = columnKeySet.size();
        final List<E> row = new ArrayList<E>(columnLength);

        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                row.add((E) columnList.get(columnIndex).get(rowIndex));
            }
        } else {
            checkRowKey(rowKey);

            N.fill(row, 0, columnLength, null);
        }

        return row;
    }

    @Override
    public void setRow(R rowKey, Collection<? extends E> row) {
        init();

        final int rowIndex = getRowIndex(rowKey);
        final int columnLength = columnKeySet.size();

        if (N.notNullOrEmpty(row) && row.size() != columnKeySet.size()) {
            throw new IllegalArgumentException("Row: " + row + " not matches the column key: " + columnKeySet);
        }

        final Iterator<? extends E> it = row.iterator();
        if (N.isNullOrEmpty(row)) {
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).set(rowIndex, null);
            }
        } else {
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).set(rowIndex, it.next());
            }
        }
    }

    @Override
    public void addRow(R rowKey, Collection<? extends E> row) {
        if (rowKeySet.contains(rowKey)) {
            throw new IllegalArgumentException("Row '" + rowKey + "' already existed");
        }

        if (N.notNullOrEmpty(row) && row.size() != columnKeySet.size()) {
            throw new IllegalArgumentException("Row: " + row + " not matches the column key: " + columnKeySet);
        }

        rowKeySet.add(rowKey);

        final int columnLength = columnKeySet.size();
        final int newRowLength = rowKeySet.size();
        final int newRowIndex = newRowLength - 1;

        if (initialized) {
            rowKeyIndexMap.put(rowKey, newRowIndex);
        } else {
            init();
        }

        if (N.isNullOrEmpty(row)) {
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).add(null);
            }
        } else {
            final Iterator<? extends E> it = row.iterator();
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).add(it.next());
            }
        }
    }

    @Override
    public void removeRow(R rowKey) {
        checkRowKey(rowKey);

        rowKeySet.remove(rowKey);

        if (initialized) {
            final int columnLength = columnKeySet.size();
            final int newRowSize = rowKeySet.size();
            final int removedRowIndex = rowKeyIndexMap.remove(rowKey);

            if (removedRowIndex == newRowSize) {
                // removed last row.
            } else {
                for (int i = removedRowIndex; i < newRowSize; i++) {
                    rowKeyIndexMap.put(rowKeyIndexMap.getByValue(i + 1), i);
                }
            }

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).remove(removedRowIndex);
            }
        }
    }

    @Override
    public boolean containsRow(R rowKey) {
        return rowKeySet.contains(rowKey);
    }

    @Override
    public Map<C, E> rowMap(R rowKey) {
        final int columnLength = columnKeySet.size();
        Map<C, E> rowMap = new LinkedHashMap<>(N.initHashCapacity(columnLength));

        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);
            int columnIndex = 0;

            for (C columnKey : this.columnKeySet()) {
                rowMap.put(columnKey, (E) columnList.get(columnIndex++).get(rowIndex));
            }
        } else {
            checkRowKey(rowKey);

            for (C columnKey : this.columnKeySet()) {
                rowMap.put(columnKey, null);
            }
        }

        return rowMap;
    }

    @Override
    public Map<R, Map<C, E>> rowMap() {
        final int columnLength = columnKeySet.size();
        final Map<R, Map<C, E>> result = new LinkedHashMap<>(N.initHashCapacity(this.rowKeySet().size()));

        if (initialized) {
            for (R rowKey : this.rowKeySet()) {
                final int rowIndex = getRowIndex(rowKey);
                final Map<C, E> rowMap = new LinkedHashMap<>(N.initHashCapacity(columnLength));
                int columnIndex = 0;

                for (C columnKey : this.columnKeySet()) {
                    rowMap.put(columnKey, (E) columnList.get(columnIndex++).get(rowIndex));
                }

                result.put(rowKey, rowMap);
            }
        } else {
            for (R rowKey : this.rowKeySet()) {
                final Map<C, E> row = new LinkedHashMap<>(N.initHashCapacity(columnLength));

                for (C columnKey : this.columnKeySet()) {
                    row.put(columnKey, null);
                }

                result.put(rowKey, row);
            }
        }

        return result;
    }

    @Override
    public List<E> getColumn(C columnKey) {
        final int rowLength = rowKeySet.size();
        List<E> column = null;

        if (initialized) {
            column = (List<E>) columnList.get(getColumnIndex(columnKey));
        } else {
            checkColumnKey(columnKey);
            column = new ArrayList<E>(rowLength);
            N.fill(column, 0, rowLength, null);
        }

        return N.asImmutableList(column);
    }

    @Override
    public void setColumn(C columnKey, Collection<? extends E> column) {
        init();

        final int columnIndex = getColumnIndex(columnKey);
        final int rowLength = rowKeySet.size();

        if (N.notNullOrEmpty(column) && column.size() != rowKeySet.size()) {
            throw new IllegalArgumentException("Column: " + column + " not matches the row key: " + rowKeySet);
        }

        if (N.isNullOrEmpty(column)) {
            N.fill(columnList.get(columnIndex), 0, rowLength, null);
        } else {
            final Iterator<? extends E> it = column.iterator();

            for (int rowIndex = 0; rowIndex < rowLength; rowIndex++) {
                columnList.get(columnIndex).set(rowIndex, it.next());
            }
        }
    }

    @Override
    public void addColumn(C columnKey, Collection<? extends E> column) {
        if (columnKeySet.contains(columnKey)) {
            throw new IllegalArgumentException("Column '" + columnKey + "' already existed");
        }

        if (N.notNullOrEmpty(column) && column.size() != rowKeySet.size()) {
            throw new IllegalArgumentException("Column: " + column + " not matches the row key: " + rowKeySet);
        }

        columnKeySet.add(columnKey);

        final int rowLength = rowKeySet.size();
        final int newColumnLength = columnKeySet.size();
        final int newColumnIndex = newColumnLength - 1;

        if (initialized) {
            columnKeyIndexMap.put(columnKey, newColumnIndex);
        } else {
            init();
        }

        if (N.isNullOrEmpty(column)) {
            List<Object> newColumn = new ArrayList<>();
            N.fill(newColumn, 0, rowLength, null);
            columnList.add(newColumn);
        } else {
            columnList.add(new ArrayList<Object>(column));
        }
    }

    @Override
    public void removeColumn(C columnKey) {
        checkColumnKey(columnKey);

        columnKeySet.remove(columnKey);

        if (initialized) {
            final int newColumnLength = columnKeySet.size();
            final int removedColumnIndex = columnKeyIndexMap.remove(columnKey);

            if (removedColumnIndex == newColumnLength) {
                // removed the last column
            } else {
                for (int i = removedColumnIndex; i < newColumnLength; i++) {
                    columnKeyIndexMap.put(columnKeyIndexMap.getByValue(i + 1), i);
                }
            }

            columnList.remove(removedColumnIndex);
        }
    }

    @Override
    public boolean containsColumn(C columnKey) {
        return columnKeySet.contains(columnKey);
    }

    @Override
    public Map<R, E> columnMap(C columnKey) {
        final int rowLength = rowKeySet.size();
        final Map<R, E> columnMap = new LinkedHashMap<>(N.initHashCapacity(rowLength));

        if (initialized) {
            final int columnIndex = getColumnIndex(columnKey);
            final List<E> column = (List<E>) columnList.get(columnIndex);
            int rowIndex = 0;

            for (R rowKey : this.rowKeySet()) {
                columnMap.put(rowKey, column.get(rowIndex++));
            }
        } else {
            checkColumnKey(columnKey);

            for (R rowKey : this.rowKeySet()) {
                columnMap.put(rowKey, null);
            }
        }

        return columnMap;
    }

    @Override
    public Map<C, Map<R, E>> columnMap() {
        final int rowLength = rowKeySet.size();
        final Map<C, Map<R, E>> result = new LinkedHashMap<>(N.initHashCapacity(this.columnKeySet().size()));

        if (initialized) {
            for (C columnKey : this.columnKeySet()) {
                final int columnIndex = getColumnIndex(columnKey);
                final Map<R, E> columnMap = new LinkedHashMap<>(N.initHashCapacity(rowLength));
                final List<E> column = (List<E>) columnList.get(columnIndex);
                int rowIndex = 0;

                for (R rowKey : this.rowKeySet()) {
                    columnMap.put(rowKey, column.get(rowIndex++));
                }

                result.put(columnKey, columnMap);
            }
        } else {
            for (C columnKey : this.columnKeySet()) {
                final Map<R, E> column = new LinkedHashMap<>(N.initHashCapacity(rowLength));

                for (R rowKey : this.rowKeySet()) {
                    column.put(rowKey, null);
                }

                result.put(columnKey, column);
            }
        }

        return result;
    }

    @Override
    public int rowLength() {
        return rowKeySet.size();
    }

    @Override
    public int columnLength() {
        return columnKeySet.size();
    }

    @Override
    public void clear() {
        if (initialized && columnList.size() > 0) {
            for (List<Object> column : columnList) {
                N.fill(column, 0, column.size(), null);
            }
        }
    }

    @Override
    public void trimToSize() {
        if (initialized && columnList.size() > 0) {
            for (List<Object> column : columnList) {
                if (column instanceof ArrayList) {
                    ((ArrayList<?>) column).trimToSize();
                }
            }
        }
    }

    @Override
    public ArraySheet<R, C, E> copy() {
        final ArraySheet<R, C, E> copy = new ArraySheet<R, C, E>(this.rowKeySet, this.columnKeySet);

        if (this.initialized) {
            final int rowLength = copy.rowKeySet.size();
            final int columnLength = copy.columnKeySet.size();

            copy.rowKeyIndexMap = new BiMap<>(N.initHashCapacity(rowLength));
            int index = 0;
            for (R rowKey : copy.rowKeySet) {
                copy.rowKeyIndexMap.put(rowKey, index++);
            }

            copy.columnKeyIndexMap = new BiMap<>(N.initHashCapacity(columnLength));
            index = 0;
            for (C columnKey : copy.columnKeySet) {
                copy.columnKeyIndexMap.put(columnKey, index++);
            }

            copy.columnList = new ArrayList<>(columnList.size());

            for (List<Object> column : columnList) {
                copy.columnList.add(new ArrayList<>(column));
            }

            copy.initialized = this.initialized;
        }

        return copy;
    }

    @Override
    public Object[][] toArray() {
        final int rowLength = rowKeySet.size();
        final int columnLength = columnKeySet.size();
        final Object[][] copy = new Object[rowLength][columnLength];

        if (initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<Object> column = columnList.get(i);

                for (int j = 0; j < rowLength; j++) {
                    copy[j][i] = column.get(j);
                }
            }
        }

        return copy;
    }

    private void init() {
        if (!initialized) {
            final int rowLength = rowKeySet.size();
            rowKeyIndexMap = new BiMap<>(N.initHashCapacity(rowLength));
            int index = 0;
            for (R rowKey : rowKeySet) {
                rowKeyIndexMap.put(rowKey, index++);
            }

            final int columnLength = columnKeySet.size();
            columnKeyIndexMap = new BiMap<>(N.initHashCapacity(columnLength));
            index = 0;
            for (C columnKey : columnKeySet) {
                columnKeyIndexMap.put(columnKey, index++);
            }

            columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                List<Object> column = new ArrayList<>(rowLength);
                N.fill(column, 0, rowLength, null);
                columnList.add(column);
            }

            initialized = true;
        }
    }

    private void checkRowKey(R rowKey) {
        if (!rowKeySet.contains(rowKey)) {
            throw new IllegalArgumentException("No row found by key: " + rowKey);
        }
    }

    private void checkColumnKey(C columnKey) {
        if (!columnKeySet.contains(columnKey)) {
            throw new IllegalArgumentException("No column found by key: " + columnKey);
        }
    }

    private void checkRowIndex(int rowIndex) {
        if (rowIndex < 0 || rowIndex >= rowKeySet.size()) {
            throw new IndexOutOfBoundsException("Row index: " + rowIndex + " can't be negative or equals to or bigger than the row size: " + rowKeySet.size());
        }
    }

    private void checkColumnIndex(int columnIndex) {
        if (columnIndex < 0 || columnIndex >= columnKeySet.size()) {
            throw new IndexOutOfBoundsException(
                    "Column index: " + columnIndex + " can't be negative or equals to or bigger than the column size: " + columnKeySet.size());
        }
    }

    private int getRowIndex(R rowKey) {
        Integer index = rowKeyIndexMap.get(rowKey);

        if (index == null) {
            throw new IllegalArgumentException("No row found by key: " + rowKey);
        }

        return index;
    }

    private int getColumnIndex(C columnKey) {
        Integer index = columnKeyIndexMap.get(columnKey);

        if (index == null) {
            throw new IllegalArgumentException("No column found by key: " + columnKey);
        }

        return index;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((rowKeySet == null) ? 0 : rowKeySet.hashCode());
        result = prime * result + ((columnKeySet == null) ? 0 : columnKeySet.hashCode());
        result = prime * result + (initialized ? columnList.hashCode() : 0);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof ArraySheet) {
            ArraySheet<R, C, E> other = (ArraySheet<R, C, E>) obj;

            return N.equals(other.rowKeySet, rowKeySet) && N.equals(other.columnKeySet, columnKeySet) && N.deepEquals(other.columnList, columnList);
        }

        return false;
    }

    @Override
    public String toString() {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        sb.append("{rowKeySet=");
        sb.append(rowKeySet);
        sb.append(", columnKeySet=");
        sb.append(columnKeySet);
        sb.append(", rowList=");
        sb.append("[");

        if (initialized) {
            for (int i = 0, rowLength = rowKeySet.size(), columnLength = columnKeySet.size(); i < rowLength; i++) {
                if (i > 0) {
                    sb.append(N.ELEMENT_SEPARATOR_CHAR_ARRAY);
                }

                sb.append("[");

                for (int j = 0; j < columnLength; j++) {
                    if (j > 0) {
                        sb.append(N.ELEMENT_SEPARATOR_CHAR_ARRAY);
                    }

                    sb.append(N.toString(columnList.get(j).get(i)));
                }

                sb.append("]");
            }
        }

        sb.append("]");
        sb.append("}");

        String str = sb.toString();

        ObjectFactory.recycle(sb);

        return str;
    }
}
