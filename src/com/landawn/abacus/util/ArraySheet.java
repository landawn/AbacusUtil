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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.exception.AbacusException;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public final class ArraySheet<R, C, E> implements Sheet<R, C, E> {

    private static final NullMask NULL_MASK = new NullMask();

    private final Set<R> rowKeySet;
    private final Set<C> columnKeySet;
    private BiMap<R, Integer> rowKeyIndexMap;
    private BiMap<C, Integer> columnKeyIndexMap;
    private Object[][] values;
    private int count;
    private boolean initialized = false;

    public ArraySheet(Collection<R> rowKeySet, Collection<C> columnKeySet) {
        this.rowKeySet = N.newLinkedHashSet(rowKeySet);
        this.columnKeySet = N.newLinkedHashSet(columnKeySet);
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
            final Object value = values[rowIndex][columnIndex];
            return value == NULL_MASK ? null : (E) value;
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

        final Object previousValue = values[rowIndex][columnIndex];

        values[rowIndex][columnIndex] = value;

        count++;

        return previousValue == NULL_MASK ? null : (E) previousValue;
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
            final Object previousValue = values[rowIndex][columnIndex];

            values[rowIndex][columnIndex] = NULL_MASK;

            if (previousValue != NULL_MASK) {
                count--;
            }

            return previousValue == NULL_MASK ? null : (E) previousValue;
        } else {
            checkRowIndex(rowIndex);
            checkColumnIndex(columnIndex);

            return null;
        }
    }

    @Override
    public boolean contains(R rowKey, C columnKey) {
        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);
            final int columnIndex = getColumnIndex(columnKey);

            return contains(rowIndex, columnIndex);
        } else {
            checkRowKey(rowKey);
            checkColumnKey(columnKey);

            return false;
        }
    }

    public boolean contains(int rowIndex, int columnIndex) {
        if (initialized) {
            return values[rowIndex][columnIndex] != NULL_MASK;
        } else {
            checkRowIndex(rowIndex);
            checkColumnIndex(columnIndex);

            return false;
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
            Object value = null;

            for (int i = 0; i < columnLength; i++) {
                value = values[rowIndex][i];

                row.add(value == NULL_MASK ? null : (E) value);
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

        if (row.size() != columnKeySet.size()) {
            throw new IllegalArgumentException("Row: " + row + " not matches the column key: " + columnKeySet);
        }

        final Iterator<? extends E> it = row.iterator();

        for (int i = 0; i < columnLength; i++) {
            values[rowIndex][i] = it.next();

            count++;
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

            if (values.length >= newRowLength) {
                // ignore
            } else if (values.length == newRowLength - 1) {
                values = Arrays.copyOf(values, newRowLength);
                values[newRowIndex] = new Object[values.length > 1 ? values[0].length : columnLength];

                if (values[newRowIndex].length > 0) {
                    Arrays.fill(values[newRowIndex], NULL_MASK);
                }
            } else {
                // Should never happen.
                throw new AbacusException(
                        "Unknown error happened: the row length is " + newRowLength + ". But the length of value array is just " + values.length);
            }
        }

        if (N.notNullOrEmpty(row)) {
            init();

            final Iterator<? extends E> it = row.iterator();

            for (int i = 0; i < columnLength; i++) {
                values[newRowIndex][i] = it.next();

                count++;
            }
        }

    }

    @Override
    public void clearRow(R rowKey) {
        checkRowKey(rowKey);

        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);
            final int columnLength = columnKeySet.size();

            for (int i = 0; i < columnLength; i++) {
                if (values[rowIndex][i] != NULL_MASK) {
                    values[rowIndex][i] = NULL_MASK;
                    count--;
                }
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

            for (int i = 0; i < columnLength; i++) {
                if (values[removedRowIndex][i] != NULL_MASK) {
                    count--;
                }
            }

            if (removedRowIndex == newRowSize) {
                // removed last row.
            } else {
                for (int i = removedRowIndex; i < newRowSize; i++) {
                    rowKeyIndexMap.put(rowKeyIndexMap.getByValue(i + 1), i);
                }

                // Save the row Object[] and move it last row.
                Object[] removedRow = values[removedRowIndex];

                // the values[newRowSize -1 ] and values[newRowSize] share the same Object array.
                N.copy(values, removedRowIndex + 1, values, removedRowIndex, newRowSize - removedRowIndex);

                // replace the last row Object[].
                values[newRowSize] = removedRow;
            }

            Arrays.fill(values[newRowSize], 0, columnLength, NULL_MASK);
        }
    }

    @Override
    public boolean containsRow(R rowKey) {
        return rowKeySet.contains(rowKey);
    }

    @Override
    public Map<C, E> row(R rowKey) {
        final int columnLength = columnKeySet.size();
        Map<C, E> row = N.newHashMap(N.initHashCapacity(columnLength));

        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);
            Object value = null;

            for (int i = 0; i < columnLength; i++) {
                value = values[rowIndex][i];

                row.put(columnKeyIndexMap.getByValue(i), value == NULL_MASK ? null : (E) value);
            }
        } else {
            checkRowKey(rowKey);

            for (C columnKey : this.columnKeySet()) {
                row.put(columnKey, null);
            }
        }

        return row;
    }

    @Override
    public Map<R, Map<C, E>> rowMap() {
        final int columnLength = columnKeySet.size();
        final Map<R, Map<C, E>> rowMap = N.newHashMap(N.initHashCapacity(this.rowKeySet().size()));

        if (initialized) {
            for (R rowKey : this.rowKeySet()) {
                final int rowIndex = getRowIndex(rowKey);
                final Map<C, E> row = N.newHashMap(N.initHashCapacity(columnLength));
                Object value = null;

                for (int i = 0; i < columnLength; i++) {
                    value = values[rowIndex][i];
                    row.put(columnKeyIndexMap.getByValue(i), value == NULL_MASK ? null : (E) value);
                }

                rowMap.put(rowKey, row);
            }
        } else {
            for (R rowKey : this.rowKeySet()) {
                final Map<C, E> row = N.newHashMap(N.initHashCapacity(columnLength));

                for (C columnKey : this.columnKeySet()) {
                    row.put(columnKey, null);
                }

                rowMap.put(rowKey, row);
            }
        }

        return rowMap;
    }

    @Override
    public List<E> getColumn(C columnKey) {
        final int rowLength = rowKeySet.size();
        final List<E> column = new ArrayList<E>(rowLength);

        if (initialized) {
            final int columnIndex = getColumnIndex(columnKey);

            Object value = null;
            for (int i = 0; i < rowLength; i++) {
                value = values[i][columnIndex];
                column.add(value == NULL_MASK ? null : (E) value);
            }

        } else {
            checkColumnKey(columnKey);

            N.fill(column, 0, rowLength, null);
        }

        return column;
    }

    @Override
    public void setColumn(C columnKey, Collection<? extends E> column) {
        init();

        final int columnIndex = getColumnIndex(columnKey);
        final int rowLength = rowKeySet.size();

        if (column.size() != rowKeySet.size()) {
            throw new IllegalArgumentException("Column: " + column + " not matches the row key: " + rowKeySet);
        }

        final Iterator<? extends E> it = column.iterator();

        for (int i = 0; i < rowLength; i++) {
            values[i][columnIndex] = it.next();

            count++;
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

        if (N.notNullOrEmpty(column)) {
            init();
        }

        columnKeySet.add(columnKey);

        final int rowLength = rowKeySet.size();
        final int newColumnLength = columnKeySet.size();
        final int newColumnIndex = newColumnLength - 1;

        if (initialized) {
            columnKeyIndexMap.put(columnKey, newColumnIndex);

            if (values.length == 0) {
                // ignore if no row key.
            } else if (values[0].length >= newColumnLength) {
                // ignore
            } else if (values[0].length == newColumnLength - 1) {
                for (int i = 0; i < rowLength; i++) {
                    values[i] = Arrays.copyOf(values[i], newColumnLength);
                    values[i][newColumnIndex] = NULL_MASK;
                }
            } else {
                // Should never happen.
                throw new AbacusException(
                        "Unknown error happened: the column length is " + newColumnLength + ". But the width of value array is just " + values[0].length);
            }
        }

        if (N.notNullOrEmpty(column)) {
            init();

            final Iterator<? extends E> it = column.iterator();

            for (int i = 0; i < rowLength; i++) {
                values[i][newColumnIndex] = it.next();

                count++;
            }
        }
    }

    @Override
    public void clearColumn(C columnKey) {
        checkColumnKey(columnKey);

        if (initialized) {
            final int rowLength = rowKeySet.size();
            final int columnIndex = this.getColumnIndex(columnKey);

            for (int i = 0; i < rowLength; i++) {
                if (values[i][columnIndex] != NULL_MASK) {
                    values[i][columnIndex] = NULL_MASK;
                    count--;
                }
            }
        }
    }

    @Override
    public void removeColumn(C columnKey) {
        checkColumnKey(columnKey);

        columnKeySet.remove(columnKey);

        if (initialized) {
            final int rowLength = rowKeySet.size();
            final int newColumnLength = columnKeySet.size();
            final int removedColumnIndex = columnKeyIndexMap.remove(columnKey);

            for (int i = 0; i < rowLength; i++) {
                if (values[i][removedColumnIndex] != NULL_MASK) {
                    count--;
                }
            }

            if (removedColumnIndex == newColumnLength) {
                // removed the last column
            } else {
                for (int i = removedColumnIndex; i < newColumnLength; i++) {
                    columnKeyIndexMap.put(columnKeyIndexMap.getByValue(i + 1), i);
                }

                for (int i = 0; i < rowLength; i++) {
                    N.copy(values[i], removedColumnIndex + 1, values[i], removedColumnIndex, newColumnLength - removedColumnIndex);
                }
            }

            for (int i = 0; i < rowLength; i++) {
                values[i][newColumnLength] = NULL_MASK;
            }
        }
    }

    @Override
    public boolean containsColumn(C columnKey) {
        return columnKeySet.contains(columnKey);
    }

    @Override
    public Map<R, E> column(C columnKey) {
        final int rowLength = rowKeySet.size();
        final Map<R, E> column = N.newHashMap(N.initHashCapacity(rowLength));

        if (initialized) {
            final int columnIndex = getColumnIndex(columnKey);
            Object value = null;
            for (int i = 0; i < rowLength; i++) {
                value = values[i][columnIndex];

                column.put(rowKeyIndexMap.getByValue(i), value == NULL_MASK ? null : (E) value);
            }
        } else {
            checkColumnKey(columnKey);

            for (R rowKey : this.rowKeySet()) {
                column.put(rowKey, null);
            }
        }

        return column;
    }

    @Override
    public Map<C, Map<R, E>> columnMap() {
        final int rowLength = rowKeySet.size();
        final Map<C, Map<R, E>> columnMap = N.newHashMap(N.initHashCapacity(this.columnKeySet().size()));

        if (initialized) {
            for (C columnKey : this.columnKeySet()) {
                final int columnIndex = getColumnIndex(columnKey);
                final Map<R, E> column = N.newHashMap(N.initHashCapacity(rowLength));
                Object value = null;

                for (int i = 0; i < rowLength; i++) {
                    value = values[i][columnIndex];

                    column.put(rowKeyIndexMap.getByValue(i), value == NULL_MASK ? null : (E) value);
                }

                columnMap.put(columnKey, column);
            }
        } else {
            for (C columnKey : this.columnKeySet()) {
                final Map<R, E> column = N.newHashMap(N.initHashCapacity(rowLength));

                for (R rowKey : this.rowKeySet()) {
                    column.put(rowKey, null);
                }

                columnMap.put(columnKey, column);
            }
        }

        return columnMap;
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
    public List<E> values() {
        final List<E> coll = N.newArrayList(count);

        if (initialized) {
            final int rowLength = rowKeySet.size();
            final int columnLength = columnKeySet.size();

            Object value = null;
            for (int i = 0; i < rowLength; i++) {
                for (int j = 0; j < columnLength; j++) {
                    value = values[i][j];

                    if (value != NULL_MASK) {
                        coll.add((E) value);
                    }
                }
            }
        }

        return coll;
    }

    @Override
    public Set<E> valueSet() {
        final Set<E> coll = N.newHashSet();

        if (initialized) {
            final int rowLength = rowKeySet.size();
            final int columnLength = columnKeySet.size();

            Object value = null;
            for (int i = 0; i < rowLength; i++) {
                for (int j = 0; j < columnLength; j++) {
                    value = values[i][j];

                    if (value != NULL_MASK) {
                        coll.add((E) value);
                    }
                }
            }
        }

        return coll;
    }

    @Override
    public int count() {
        return count;
    }

    @Override
    public boolean isEmpty() {
        return count == 0;
    }

    @Override
    public void clear() {
        if (initialized && values.length > 0) {
            for (int i = 0, rowLength = rowKeySet.size(), columnLength = columnKeySet.size(); i < rowLength; i++) {
                Arrays.fill(values[i], 0, columnLength, NULL_MASK);
            }

            count = 0;
        }
    }

    @Override
    public ArraySheet<R, C, E> copy() {
        final ArraySheet<R, C, E> copy = new ArraySheet<R, C, E>(this.rowKeySet, this.columnKeySet);

        if (this.initialized) {
            final int rowLength = copy.rowKeySet.size();
            final int columnLength = copy.columnKeySet.size();

            copy.rowKeyIndexMap = N.newBiMap(N.initHashCapacity(rowLength));
            int index = 0;
            for (R e : copy.rowKeySet) {
                copy.rowKeyIndexMap.put(e, index++);
            }

            copy.columnKeyIndexMap = N.newBiMap(N.initHashCapacity(columnLength));
            index = 0;
            for (C e : copy.columnKeySet) {
                copy.columnKeyIndexMap.put(e, index++);
            }

            copy.values = new Object[rowLength][];
            for (int i = 0; i < rowLength; i++) {
                copy.values[i] = N.copyOf(this.values[i], columnLength);
            }

            copy.count = this.count;
            copy.initialized = this.initialized;
        }

        return copy;
    }

    @Override
    public Object[][] toArray() {
        final int rowLength = rowKeySet.size();
        final int columnLength = columnKeySet.size();
        final Object[][] copy = N.copyOf(values, rowLength);

        for (int i = 0; i < rowLength; i++) {
            copy[i] = N.copyOf(copy[i], columnLength);

            for (int j = 0; j < columnLength; j++) {
                if (copy[i][j] == NULL_MASK) {
                    copy[i][j] = null;
                }
            }
        }

        return copy;
    }

    private void init() {
        if (!initialized) {
            final int rowLength = rowKeySet.size();
            rowKeyIndexMap = N.newBiMap(N.initHashCapacity(rowLength));
            int index = 0;
            for (R e : rowKeySet) {
                rowKeyIndexMap.put(e, index++);
            }

            final int columnLength = columnKeySet.size();
            columnKeyIndexMap = N.newBiMap(N.initHashCapacity(columnLength));
            index = 0;
            for (C e : columnKeySet) {
                columnKeyIndexMap.put(e, index++);
            }

            values = new Object[rowLength][columnLength];

            for (int i = 0; i < rowLength; i++) {
                Arrays.fill(values[i], NULL_MASK);
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

    public void trimToSize() {
        if (initialized && values.length > 0) {
            final int rowLength = rowKeySet.size();
            final int columnLength = columnKeySet.size();

            if (values.length > rowLength) {
                values = N.copyOf(values, rowLength);
            }

            if (values[0].length > columnLength) {
                for (int i = 0; i < rowLength; i++) {
                    values[i] = N.copyOf(values[i], columnLength);
                }
            }
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((rowKeySet == null) ? 0 : rowKeySet.hashCode());
        result = prime * result + ((columnKeySet == null) ? 0 : columnKeySet.hashCode());
        result = prime * result + Arrays.deepHashCode(values);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof ArraySheet) {
            ArraySheet<R, C, E> other = (ArraySheet<R, C, E>) obj;

            return N.equals(other.rowKeySet, rowKeySet) && N.equals(other.columnKeySet, columnKeySet) && N.deepEquals(other.values, values);
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

                    sb.append(values[i][j] == NULL_MASK ? N.NULL_STRING : N.toString(values[i][j]));
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

    private static final class NullMask implements Serializable {
        private static final long serialVersionUID = 5537858632226058561L;

        private NullMask() {
        }

        @Override
        public String toString() {
            return "NULL";
        }

        private Object readResolve() {
            return NULL_MASK;
        }
    }
}
