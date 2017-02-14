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
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.DataSet;
import com.landawn.abacus.core.RowDataSet;
import com.landawn.abacus.parser.KryoParser;
import com.landawn.abacus.parser.ParserFactory;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.stream.ImmutableIterator;
import com.landawn.abacus.util.stream.Stream;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public final class ArraySheet<R, C, E> implements Sheet<R, C, E> {
    private static final KryoParser kryoParser = ParserFactory.isKryoAvailable() ? ParserFactory.createKryoParser() : null;

    private final Set<R> _rowKeySet;
    private final Set<C> _columnKeySet;
    private BiMap<R, Integer> _rowKeyIndexMap;
    private BiMap<C, Integer> _columnKeyIndexMap;
    private List<List<E>> _columnList;
    private boolean _initialized = false;
    private boolean _isFrozen = false;

    // For Kryo.
    ArraySheet() {
        this(N.EMPTY_LIST, N.EMPTY_LIST);
    }

    public ArraySheet(Collection<R> rowKeySet, Collection<C> columnKeySet) {
        this._rowKeySet = new LinkedHashSet<>(rowKeySet);
        this._columnKeySet = new LinkedHashSet<>(columnKeySet);
    }

    public ArraySheet(Collection<R> rowKeySet, Collection<C> columnKeySet, Object[][] rows) {
        this(rowKeySet, columnKeySet);

        final int rowLength = this._rowKeySet.size();
        final int columnLength = this._columnKeySet.size();

        if (N.notNullOrEmpty(rows)) {
            N.checkArgument(rows.length == rowLength, "The length of array is not equal to size of row/column key set");

            for (Object[] e : rows) {
                N.checkArgument(e.length == columnLength, "The length of array is not equal to size of row/column key set");
            }

            initIndexMap();

            _columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                final List<E> column = new ArrayList<>(rowLength);

                for (int j = 0; j < rowLength; j++) {
                    column.add((E) rows[j][i]);
                }

                _columnList.add(column);
            }

            _initialized = true;
        }
    }

    public static <R, C, E> ArraySheet<R, C, E> of(Collection<R> rowKeySet, Collection<C> columnKeySet, Object[][] rows) {
        return new ArraySheet<>(rowKeySet, columnKeySet, rows);
    }

    public static <R, C, E> ArraySheet<R, C, E> of(Collection<R> rowKeySet, Collection<C> columnKeySet, Collection<? extends Collection<? extends E>> rows) {
        final ArraySheet<R, C, E> instance = new ArraySheet<>(rowKeySet, columnKeySet);

        final int rowLength = instance._rowKeySet.size();
        final int columnLength = instance._columnKeySet.size();

        if (N.notNullOrEmpty(rows)) {
            N.checkArgument(rows.size() == rowLength, "The size of collection is not equal to size of row/column key set");

            for (Collection<? extends E> e : rows) {
                N.checkArgument(e.size() == columnLength, "The size of collection is not equal to size of row/column key set");
            }

            instance.initIndexMap();

            instance._columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                instance._columnList.add(new ArrayList<E>(rowLength));
            }

            for (Collection<? extends E> row : rows) {
                final Iterator<? extends E> iter = row.iterator();

                for (int i = 0; i < columnLength; i++) {
                    instance._columnList.get(i).add(iter.next());
                }
            }

            instance._initialized = true;
        }

        return instance;

    }

    public static <R, C, E> ArraySheet<R, C, E> from(Collection<R> rowKeySet, Collection<C> columnKeySet, Object[][] columns) {
        final ArraySheet<R, C, E> instance = new ArraySheet<>(rowKeySet, columnKeySet);

        final int rowLength = instance._rowKeySet.size();
        final int columnLength = instance._columnKeySet.size();

        if (N.notNullOrEmpty(columns)) {
            N.checkArgument(columns.length == columnLength, "The length of array is not equal to size of row/column key set");

            for (Object[] e : columns) {
                N.checkArgument(e.length == rowLength, "The length of array is not equal to size of row/column key set");
            }

            instance.initIndexMap();

            instance._columnList = new ArrayList<>(columnLength);

            for (Object[] column : columns) {
                instance._columnList.add(new ArrayList<>((List<E>) Arrays.asList(column)));
            }

            instance._initialized = true;
        }

        return instance;
    }

    public static <R, C, E> ArraySheet<R, C, E> from(Collection<R> rowKeySet, Collection<C> columnKeySet,
            Collection<? extends Collection<? extends E>> columns) {
        final ArraySheet<R, C, E> instance = new ArraySheet<>(rowKeySet, columnKeySet);

        final int rowLength = instance._rowKeySet.size();
        final int columnLength = instance._columnKeySet.size();

        if (N.notNullOrEmpty(columns)) {
            N.checkArgument(columns.size() == columnLength, "The size of collection is not equal to size of row/column key set");

            for (Collection<? extends E> e : columns) {
                N.checkArgument(e.size() == rowLength, "The size of collection is not equal to size of row/column key set");
            }

            instance.initIndexMap();

            instance._columnList = new ArrayList<>(columnLength);

            for (Collection<? extends E> column : columns) {
                instance._columnList.add(new ArrayList<>(column));
            }

            instance._initialized = true;
        }

        return instance;
    }

    @Override
    public Set<R> rowKeySet() {
        return _rowKeySet;
    }

    @Override
    public Set<C> columnKeySet() {
        return _columnKeySet;
    }

    @Override
    public E get(Object rowKey, Object columnKey) {
        if (_initialized) {
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
        if (_initialized) {
            return _columnList.get(columnIndex).get(rowIndex);
        } else {
            checkRowIndex(rowIndex);
            checkColumnIndex(columnIndex);

            return null;
        }
    }

    @Override
    public E put(R rowKey, C columnKey, E value) {
        checkFrozen();

        init();

        final int rowIndex = getRowIndex(rowKey);
        final int columnIndex = getColumnIndex(columnKey);

        return put(rowIndex, columnIndex, value);
    }

    public E put(int rowIndex, int columnIndex, E value) {
        checkFrozen();

        init();

        final Object previousValue = _columnList.get(columnIndex).get(rowIndex);
        _columnList.get(columnIndex).set(rowIndex, value);

        return (E) previousValue;
    }

    @Override
    public void putAll(Sheet<? extends R, ? extends C, ? extends E> source) {
        checkFrozen();

        if (!this.rowKeySet().containsAll(source.rowKeySet())) {
            throw new IllegalArgumentException(source.rowKeySet() + " are not all included in this sheet with row key set: " + this.rowKeySet());
        }

        if (!this.columnKeySet().containsAll(source.columnKeySet())) {
            throw new IllegalArgumentException(source.columnKeySet() + " are not all included in this sheet with column key set: " + this.columnKeySet());
        }

        final Sheet<R, C, ? extends E> tmp = (Sheet<R, C, ? extends E>) source;
        for (R r : tmp.rowKeySet()) {
            for (C c : tmp.columnKeySet()) {
                this.put(r, c, tmp.get(r, c));
            }
        }
    }

    @Override
    public E remove(Object rowKey, Object columnKey) {
        checkFrozen();

        if (_initialized) {
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
        checkFrozen();

        if (_initialized) {
            return _columnList.get(columnIndex).set(rowIndex, null);
        } else {
            checkRowIndex(rowIndex);
            checkColumnIndex(columnIndex);

            return null;
        }
    }

    @Override
    public boolean contains(Object rowKey, Object columnKey) {
        return get(rowKey, columnKey) != null;
    }

    @Override
    public boolean containsValue(Object value) {
        //        if (value == null) {
        //            for (R r : rowKeySet()) {
        //                for (C c : columnKeySet()) {
        //                    if (this.get(r, c) == null) {
        //                        return true;
        //                    }
        //                }
        //            }
        //        } else {
        //            for (R r : rowKeySet()) {
        //                for (C c : columnKeySet()) {
        //                    if (value.equals(this.get(r, c))) {
        //                        return true;
        //                    }
        //                }
        //            }
        //        }
        //
        //        return false;

        if (this._initialized) {
            for (List<E> column : _columnList) {
                if (column.contains(value)) {
                    return true;
                }
            }

            return false;
        } else {
            return value == null;
        }
    }

    @Override
    public List<E> getRow(Object rowKey) {
        final int columnLength = columnLength();
        final List<E> row = new ArrayList<>(columnLength);

        if (_initialized) {
            final int rowIndex = getRowIndex(rowKey);

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                row.add(_columnList.get(columnIndex).get(rowIndex));
            }
        } else {
            checkRowKey(rowKey);

            N.fill(row, 0, columnLength, null);
        }

        return N.asImmutableList(row);
    }

    @Override
    public void setRow(R rowKey, Collection<? extends E> row) {
        final int columnLength = columnLength();

        if (N.notNullOrEmpty(row) && row.size() != columnLength) {
            throw new IllegalArgumentException("The size of specified row: " + row.size() + " doesn't match the length of column key set: " + columnLength);
        }

        init();

        final int rowIndex = getRowIndex(rowKey);

        if (N.isNullOrEmpty(row)) {
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                _columnList.get(columnIndex).set(rowIndex, null);
            }
        } else {
            final Iterator<? extends E> iter = row.iterator();

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                _columnList.get(columnIndex).set(rowIndex, iter.next());
            }
        }
    }

    @Override
    public void addRow(R rowKey, Collection<? extends E> row) {
        checkFrozen();

        if (_rowKeySet.contains(rowKey)) {
            throw new IllegalArgumentException("Row '" + rowKey + "' already existed");
        }

        final int rowLength = rowLength();
        final int columnLength = columnLength();

        if (N.notNullOrEmpty(row) && row.size() != columnLength) {
            throw new IllegalArgumentException("The size of specified row: " + row.size() + " doesn't match the length of column key set: " + columnLength);
        }

        init();

        _rowKeySet.add(rowKey);
        _rowKeyIndexMap.put(rowKey, rowLength);

        if (N.isNullOrEmpty(row)) {
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                _columnList.get(columnIndex).add(null);
            }
        } else {
            final Iterator<? extends E> iter = row.iterator();

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                _columnList.get(columnIndex).add(iter.next());
            }
        }
    }

    @Override
    public void addRow(final int rowIndex, final R rowKey, final Collection<? extends E> row) {
        checkFrozen();

        final int rowLength = rowLength();
        final int columnLength = columnLength();

        if (rowIndex == rowLength) {
            addRow(rowKey, row);
            return;
        }

        if (rowIndex < 0 || rowIndex > rowLength) {
            throw new IllegalArgumentException("Invalid row index: " + rowIndex + ". It must be: >= 0 and <= " + rowLength);
        }

        if (_rowKeySet.contains(rowKey)) {
            throw new IllegalArgumentException("Row '" + rowKey + "' already existed");
        }

        if (N.notNullOrEmpty(row) && row.size() != columnLength) {
            throw new IllegalArgumentException("The size of specified row: " + row.size() + " doesn't match the length of column key set: " + columnLength);
        }

        init();

        final List<R> tmp = new ArrayList<>(rowLength + 1);
        tmp.addAll(_rowKeySet);
        tmp.add(rowIndex, rowKey);

        _rowKeySet.clear();
        _rowKeySet.addAll(tmp);

        for (Map.Entry<R, Integer> entry : _rowKeyIndexMap.entrySet()) {
            if (entry.getValue().intValue() >= rowIndex) {
                entry.setValue(entry.getValue() + 1);
            }
        }

        _rowKeyIndexMap.put(rowKey, rowIndex);

        if (N.isNullOrEmpty(row)) {
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                _columnList.get(columnIndex).add(rowIndex, null);
            }
        } else {
            final Iterator<? extends E> iter = row.iterator();

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                _columnList.get(columnIndex).add(rowIndex, iter.next());
            }
        }

    }

    @Override
    public void updateRow(R rowKey, Function<? super E, E> func) {
        checkFrozen();

        if (columnLength() > 0) {
            this.init();

            final int rowIndex = this.getRowIndex(rowKey);

            for (List<E> column : _columnList) {
                column.set(rowIndex, func.apply(column.get(rowIndex)));
            }
        }
    }

    @Override
    public void removeRow(Object rowKey) {
        checkFrozen();

        checkRowKey(rowKey);

        _rowKeySet.remove(rowKey);

        if (_rowKeyIndexMap != null) {
            final int columnLength = columnLength();
            final int newRowSize = _rowKeySet.size();
            final int removedRowIndex = _rowKeyIndexMap.remove(rowKey);

            if (removedRowIndex == newRowSize) {
                // removed last row.
            } else {
                for (int i = removedRowIndex; i < newRowSize; i++) {
                    _rowKeyIndexMap.put(_rowKeyIndexMap.getByValue(i + 1), i);
                }
            }

            if (_initialized) {
                for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                    _columnList.get(columnIndex).remove(removedRowIndex);
                }
            }
        }
    }

    @Override
    public void moveRow(Object rowKey, int newRowIndex) {
        checkFrozen();

        this.checkRowIndex(newRowIndex);

        final int rowIndex = this.getRowIndex(rowKey);
        final List<R> tmp = new ArrayList<>(rowLength());
        tmp.addAll(_rowKeySet);
        tmp.add(newRowIndex, tmp.remove(rowIndex));

        _rowKeySet.clear();
        _rowKeySet.addAll(tmp);

        _rowKeyIndexMap = null;

        if (_initialized && _columnList.size() > 0) {
            for (List<E> column : _columnList) {
                column.add(newRowIndex, column.remove(rowIndex));
            }
        }
    }

    @Override
    public void swapRow(Object rowKeyA, Object rowKeyB) {
        checkFrozen();

        final int rowIndexA = this.getRowIndex(rowKeyA);
        final int rowIndexB = this.getRowIndex(rowKeyB);

        final List<R> tmp = new ArrayList<>(rowLength());
        tmp.addAll(_rowKeySet);
        final R tmpRowKeyA = tmp.get(rowIndexA);
        tmp.set(rowIndexA, tmp.get(rowIndexB));
        tmp.set(rowIndexB, tmpRowKeyA);

        _rowKeySet.clear();
        _rowKeySet.addAll(tmp);

        _rowKeyIndexMap.put(tmp.get(rowIndexA), rowIndexA);
        _rowKeyIndexMap.put(tmp.get(rowIndexB), rowIndexB);

        if (_initialized && _columnList.size() > 0) {
            E tmpVal = null;

            for (List<E> column : _columnList) {
                tmpVal = column.get(rowIndexA);
                column.set(rowIndexA, column.get(rowIndexB));
                column.set(rowIndexB, tmpVal);
            }
        }
    }

    @Override
    public void renameRow(R rowKey, R newRowKey) {
        checkFrozen();
        checkRowKey(rowKey);

        if (this._rowKeySet.contains(newRowKey)) {
            throw new IllegalArgumentException("Invalid new row key: " + N.toString(newRowKey) + ". It's already in the row key set.");
        }

        final int rowIndex = this.getRowIndex(rowKey);
        final List<R> tmp = new ArrayList<>(_rowKeySet);
        tmp.set(rowIndex, newRowKey);

        this._rowKeySet.clear();
        this._rowKeySet.addAll(tmp);

        if (N.notNullOrEmpty(this._rowKeyIndexMap)) {
            this._rowKeyIndexMap.put(newRowKey, _rowKeyIndexMap.remove(rowKey));
        }
    }

    @Override
    public boolean containsRow(Object rowKey) {
        return _rowKeySet.contains(rowKey);
    }

    @Override
    public Map<C, E> row(Object rowKey) {
        final int columnLength = columnLength();
        Map<C, E> rowMap = new LinkedHashMap<>(N.initHashCapacity(columnLength));

        if (_initialized) {
            final int rowIndex = getRowIndex(rowKey);
            int columnIndex = 0;

            for (C columnKey : this.columnKeySet()) {
                rowMap.put(columnKey, _columnList.get(columnIndex++).get(rowIndex));
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
        final Map<R, Map<C, E>> result = new LinkedHashMap<>(N.initHashCapacity(this.rowKeySet().size()));

        for (R rowKey : this.rowKeySet()) {
            result.put(rowKey, row(rowKey));
        }

        return result;
    }

    @Override
    public List<E> getColumn(Object columnKey) {
        final int rowLength = rowLength();
        List<E> column = null;

        if (_initialized) {
            column = _columnList.get(getColumnIndex(columnKey));
        } else {
            checkColumnKey(columnKey);
            column = new ArrayList<>(rowLength);
            N.fill(column, 0, rowLength, null);
        }

        return N.asImmutableList(column);
    }

    @Override
    public void setColumn(C columnKey, Collection<? extends E> column) {
        checkFrozen();

        final int rowLength = rowLength();

        if (N.notNullOrEmpty(column) && column.size() != rowLength) {
            throw new IllegalArgumentException("The size of specified column: " + column.size() + " doesn't match the length of row key set: " + rowLength);
        }

        init();

        final int columnIndex = getColumnIndex(columnKey);

        if (N.isNullOrEmpty(column)) {
            N.fill(_columnList.get(columnIndex), 0, rowLength, null);
        } else {
            _columnList.set(columnIndex, new ArrayList<>(column));
        }
    }

    @Override
    public void addColumn(C columnKey, Collection<? extends E> column) {
        checkFrozen();

        if (_columnKeySet.contains(columnKey)) {
            throw new IllegalArgumentException("Column '" + columnKey + "' already existed");
        }

        final int rowLength = rowLength();
        final int columnLength = columnLength();

        if (N.notNullOrEmpty(column) && column.size() != rowLength) {
            throw new IllegalArgumentException("The size of specified column: " + column.size() + " doesn't match the length of row key set: " + rowLength);
        }

        init();

        _columnKeySet.add(columnKey);
        _columnKeyIndexMap.put(columnKey, columnLength);

        if (N.isNullOrEmpty(column)) {
            List<E> newColumn = new ArrayList<>();
            N.fill(newColumn, 0, rowLength, null);
            _columnList.add(newColumn);
        } else {
            _columnList.add(new ArrayList<>(column));
        }
    }

    @Override
    public void addColumn(int columnIndex, C columnKey, Collection<? extends E> column) {
        checkFrozen();

        final int rowLength = rowLength();
        final int columnLength = columnLength();

        if (columnIndex == columnLength) {
            addColumn(columnKey, column);
            return;
        }

        if (columnIndex < 0 || columnIndex > columnLength) {
            throw new IllegalArgumentException("Invalid column index: " + columnIndex + ". It must be: >= 0 and <= " + columnLength);
        }

        if (_columnKeySet.contains(columnKey)) {
            throw new IllegalArgumentException("Column '" + columnKey + "' already existed");
        }

        if (N.notNullOrEmpty(column) && column.size() != rowLength) {
            throw new IllegalArgumentException("The size of specified column: " + column.size() + " doesn't match the length of row key set: " + rowLength);
        }

        init();

        final List<C> tmp = new ArrayList<>(columnLength + 1);
        tmp.addAll(_columnKeySet);
        tmp.add(columnIndex, columnKey);

        _columnKeySet.clear();
        _columnKeySet.addAll(tmp);

        for (Map.Entry<C, Integer> entry : _columnKeyIndexMap.entrySet()) {
            if (entry.getValue().intValue() >= columnIndex) {
                entry.setValue(entry.getValue() + 1);
            }
        }

        _columnKeyIndexMap.put(columnKey, columnIndex);

        if (N.isNullOrEmpty(column)) {
            List<E> newColumn = new ArrayList<>();
            N.fill(newColumn, 0, rowLength, null);
            _columnList.add(columnIndex, newColumn);
        } else {
            _columnList.add(columnIndex, new ArrayList<>(column));
        }
    }

    @Override
    public void updateColumn(C columnKey, Function<? super E, E> func) {
        checkFrozen();

        if (rowLength() > 0) {
            this.init();

            final int rowLength = rowLength();
            final List<E> column = _columnList.get(this.getColumnIndex(columnKey));

            for (int rowIndex = 0; rowIndex < rowLength; rowIndex++) {
                column.set(rowIndex, func.apply(column.get(rowIndex)));
            }
        }
    }

    @Override
    public void removeColumn(Object columnKey) {
        checkFrozen();

        checkColumnKey(columnKey);

        _columnKeySet.remove(columnKey);

        if (_columnKeyIndexMap != null) {
            final int newColumnLength = _columnKeySet.size();
            final int removedColumnIndex = _columnKeyIndexMap.remove(columnKey);

            if (removedColumnIndex == newColumnLength) {
                // removed the last column
            } else {
                for (int i = removedColumnIndex; i < newColumnLength; i++) {
                    _columnKeyIndexMap.put(_columnKeyIndexMap.getByValue(i + 1), i);
                }
            }

            if (_initialized) {
                _columnList.remove(removedColumnIndex);
            }
        }
    }

    @Override
    public void moveColumn(Object columnKey, int newColumnIndex) {
        checkFrozen();

        this.checkColumnIndex(newColumnIndex);

        final int columnIndex = this.getColumnIndex(columnKey);
        final List<C> tmp = new ArrayList<>(columnLength());
        tmp.addAll(_columnKeySet);
        tmp.add(newColumnIndex, tmp.remove(columnIndex));

        _columnKeySet.clear();
        _columnKeySet.addAll(tmp);

        _columnKeyIndexMap = null;

        if (_initialized && _columnList.size() > 0) {
            _columnList.add(newColumnIndex, _columnList.remove(columnIndex));
        }
    }

    @Override
    public void swapColumn(Object columnKeyA, Object columnKeyB) {
        checkFrozen();

        final int columnIndexA = this.getColumnIndex(columnKeyA);
        final int columnIndexB = this.getColumnIndex(columnKeyB);

        final List<C> tmp = new ArrayList<>(rowLength());
        tmp.addAll(_columnKeySet);
        final C tmpColumnKeyA = tmp.get(columnIndexA);
        tmp.set(columnIndexA, tmp.get(columnIndexB));
        tmp.set(columnIndexB, tmpColumnKeyA);

        _columnKeySet.clear();
        _columnKeySet.addAll(tmp);

        _columnKeyIndexMap.put(tmp.get(columnIndexA), columnIndexA);
        _columnKeyIndexMap.put(tmp.get(columnIndexB), columnIndexB);

        if (_initialized && _columnList.size() > 0) {
            final List<E> tmpColumnA = _columnList.get(columnIndexA);

            _columnList.set(columnIndexA, _columnList.get(columnIndexB));
            _columnList.set(columnIndexB, tmpColumnA);
        }
    }

    @Override
    public void renameColumn(C columnKey, C newColumnKey) {
        checkFrozen();

        this.checkColumnKey(columnKey);

        if (this._columnKeySet.contains(newColumnKey)) {
            throw new IllegalArgumentException("Invalid new column key: " + N.toString(newColumnKey) + ". It's already in the column key set.");
        }

        final int columnIndex = this.getColumnIndex(columnKey);
        final List<C> tmp = new ArrayList<>(_columnKeySet);
        tmp.set(columnIndex, newColumnKey);

        this._columnKeySet.clear();
        this._columnKeySet.addAll(tmp);

        if (N.notNullOrEmpty(this._columnKeyIndexMap)) {
            this._columnKeyIndexMap.put(newColumnKey, _columnKeyIndexMap.remove(columnKey));
        }
    }

    @Override
    public boolean containsColumn(Object columnKey) {
        return _columnKeySet.contains(columnKey);
    }

    @Override
    public Map<R, E> column(Object columnKey) {
        final int rowLength = rowLength();
        final Map<R, E> columnMap = new LinkedHashMap<>(N.initHashCapacity(rowLength));

        if (_initialized) {
            final int columnIndex = getColumnIndex(columnKey);
            final List<E> column = _columnList.get(columnIndex);
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
        final Map<C, Map<R, E>> result = new LinkedHashMap<>(N.initHashCapacity(this.columnKeySet().size()));

        for (C columnKey : this.columnKeySet()) {
            result.put(columnKey, column(columnKey));
        }

        return result;
    }

    @Override
    public int rowLength() {
        return _rowKeySet.size();
    }

    @Override
    public int columnLength() {
        return _columnKeySet.size();
    }

    @Override
    public void updateAll(Function<? super E, E> func) {
        checkFrozen();

        if (rowLength() > 0 && columnLength() > 0) {
            this.init();

            final int rowLength = rowLength();

            for (List<E> column : _columnList) {
                for (int rowIndex = 0; rowIndex < rowLength; rowIndex++) {
                    column.set(rowIndex, func.apply(column.get(rowIndex)));
                }
            }
        }
    }

    @Override
    public ArraySheet<R, C, E> copy() {
        final ArraySheet<R, C, E> copy = new ArraySheet<>(this._rowKeySet, this._columnKeySet);

        if (this._initialized) {
            copy.initIndexMap();

            copy._columnList = new ArrayList<>(_columnList.size());

            for (List<E> column : _columnList) {
                copy._columnList.add(new ArrayList<>(column));
            }

            copy._initialized = true;
        }

        return copy;
    }

    @Override
    public ArraySheet<R, C, E> copy(Collection<R> rowKeySet, Collection<C> columnKeySet) {
        if (this._rowKeySet.containsAll(rowKeySet) == false) {
            throw new IllegalArgumentException(
                    "Row keys: " + N.difference(rowKeySet, this._rowKeySet) + " are not included in this sheet row keys: " + this._rowKeySet);
        }

        if (this._columnKeySet.containsAll(columnKeySet) == false) {
            throw new IllegalArgumentException(
                    "Column keys: " + N.difference(columnKeySet, this._columnKeySet) + " are not included in this sheet Column keys: " + this._columnKeySet);
        }

        final ArraySheet<R, C, E> copy = new ArraySheet<>(rowKeySet, columnKeySet);

        if (this._initialized) {
            copy.initIndexMap();

            copy._columnList = new ArrayList<>(copy._columnKeySet.size());

            final int[] rowKeyIndices = new int[copy._rowKeySet.size()];
            int idx = 0;

            for (R rowKey : copy._rowKeySet) {
                rowKeyIndices[idx++] = this.getRowIndex(rowKey);
            }

            for (C columnKey : copy._columnKeySet) {
                final List<E> column = _columnList.get(this.getColumnIndex(columnKey));
                final List<E> newColumn = new ArrayList<>(rowKeyIndices.length);

                for (int rowIndex : rowKeyIndices) {
                    newColumn.add(column.get(rowIndex));
                }

                copy._columnList.add(newColumn);
            }

            copy._initialized = true;
        }

        return copy;
    }

    @Override
    public Sheet<R, C, E> clone() {
        return clone(this._isFrozen);
    }

    @Override
    public Sheet<R, C, E> clone(boolean freeze) {
        if (kryoParser == null) {
            throw new RuntimeException("Kryo is required");
        }

        final ArraySheet<R, C, E> copy = kryoParser.clone(this);

        copy._isFrozen = freeze;

        return copy;
    }

    @Override
    public Sheet<R, C, E> merge(Sheet<? extends R, ? extends C, ? extends E> source, BiFunction<? super E, ? super E, E> mergeFunction) {
        final ArraySheet<R, C, E> result = this.copy();

        for (R rowKey : source.rowKeySet()) {
            if (result.containsRow(rowKey) == false) {
                result.addRow(rowKey, null);
            }
        }

        for (C columnKey : source.columnKeySet()) {
            if (result.containsColumn(columnKey) == false) {
                result.addColumn(columnKey, null);
            }
        }

        final int[] rowKeyIndexes = new int[source.rowLength()];
        final int[] columnKeyIndexes = new int[source.columnLength()];

        int idx = 0;
        for (R rowKey : source.rowKeySet()) {
            rowKeyIndexes[idx++] = result.getRowIndex(rowKey);
        }

        idx = 0;
        for (C columnKey : source.columnKeySet()) {
            columnKeyIndexes[idx++] = result.getColumnIndex(columnKey);
        }

        final Iterator<? extends C> iter = source.columnKeySet().iterator();

        for (int columnIndex = 0, columnLength = columnKeyIndexes.length; columnIndex < columnLength; columnIndex++) {
            final List<? extends E> srcColumn = source.getColumn(iter.next());
            final List<E> targetColumn = _columnList.get(columnKeyIndexes[columnIndex]);

            for (int rowIndex = 0, rowLength = rowKeyIndexes.length; rowIndex < rowLength; rowIndex++) {
                targetColumn.set(rowKeyIndexes[rowIndex], mergeFunction.apply(targetColumn.get(rowKeyIndexes[rowIndex]), srcColumn.get(rowIndex)));
            }
        }

        return result;
    }

    @Override
    public Sheet<C, R, E> transpose() {
        final ArraySheet<C, R, E> copy = new ArraySheet<>(this._columnKeySet, this._rowKeySet);

        if (this._initialized) {
            copy.initIndexMap();

            final int rowLength = copy.rowLength();
            final int columnLength = copy.columnLength();

            copy._columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                final List<E> column = new ArrayList<>(rowLength);

                for (int j = 0; j < rowLength; j++) {
                    column.add(_columnList.get(j).get(i));
                }

                copy._columnList.add(column);
            }

            copy._initialized = true;
        }

        return copy;
    }

    @Override
    public void freeze() {
        _isFrozen = true;
    }

    @Override
    public boolean frozen() {
        return _isFrozen;
    }

    @Override
    public void clear() {
        checkFrozen();

        if (_initialized && _columnList.size() > 0) {
            for (List<E> column : _columnList) {
                N.fill(column, 0, column.size(), null);
            }
        }
    }

    @Override
    public void trimToSize() {
        if (_initialized && _columnList.size() > 0) {
            for (List<E> column : _columnList) {
                if (column instanceof ArrayList) {
                    ((ArrayList<?>) column).trimToSize();
                }
            }
        }
    }

    @Override
    public Stream<Sheet.Cell<R, C, E>> cells() {
        return cells(0, _rowKeySet.size());
    }

    @Override
    public Stream<Sheet.Cell<R, C, E>> cells(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, _rowKeySet.size());

        if (_rowKeySet.size() == 0 || _columnKeySet.size() == 0) {
            return Stream.empty();
        }

        initIndexMap();

        return Stream.of(new ImmutableIterator<Sheet.Cell<R, C, E>>() {
            private final int columnLength = columnLength();
            private final long toIndex = toRowIndex * columnLength * 1L;
            private long cursor = fromRowIndex * columnLength * 1L;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Sheet.Cell<R, C, E> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int rowIndex = (int) (cursor / columnLength);
                final int columnIndex = (int) (cursor++ % columnLength);

                return new Cell0<>(_rowKeyIndexMap.getByValue(rowIndex), _columnKeyIndexMap.getByValue(columnIndex),
                        _initialized ? _columnList.get(columnIndex).get(rowIndex) : null);
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }
        });
    }

    @Override
    public Stream<Sheet.Cell<R, C, E>> cells0() {
        return cells0(0, _columnKeySet.size());
    }

    @Override
    public Stream<Sheet.Cell<R, C, E>> cells0(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, _columnKeySet.size());

        if (_rowKeySet.size() == 0 || _columnKeySet.size() == 0) {
            return Stream.empty();
        }

        initIndexMap();

        return Stream.of(new ImmutableIterator<Sheet.Cell<R, C, E>>() {
            private final int rowLength = rowLength();
            private final long toIndex = toColumnIndex * rowLength * 1L;
            private long cursor = fromColumnIndex * rowLength * 1L;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Sheet.Cell<R, C, E> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                final int rowIndex = (int) (cursor % rowLength);
                final int columnIndex = (int) (cursor++ / rowLength);

                return new Cell0<>(_rowKeyIndexMap.getByValue(rowIndex), _columnKeyIndexMap.getByValue(columnIndex),
                        _initialized ? _columnList.get(columnIndex).get(rowIndex) : null);
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }
        });
    }

    @Override
    public Stream<E> stream() {
        return stream(0, _rowKeySet.size());
    }

    @Override
    public Stream<E> stream(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, _rowKeySet.size());

        if (_rowKeySet.size() == 0 || _columnKeySet.size() == 0) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<E>() {
            private final int columnLength = columnLength();
            private final long toIndex = toRowIndex * columnLength * 1L;
            private long cursor = fromRowIndex * columnLength * 1L;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public E next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                if (_initialized) {
                    return _columnList.get((int) (cursor % columnLength)).get((int) (cursor++ / columnLength));
                } else {
                    cursor++;
                    return null;
                }
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }
        });
    }

    @Override
    public Stream<E> stream0() {
        return stream0(0, _columnKeySet.size());
    }

    @Override
    public Stream<E> stream0(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, _columnKeySet.size());

        if (_rowKeySet.size() == 0 || _columnKeySet.size() == 0) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<E>() {
            private final int rowLength = rowLength();
            private final long toIndex = toColumnIndex * rowLength * 1L;
            private long cursor = fromColumnIndex * rowLength * 1L;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public E next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                if (_initialized) {
                    return _columnList.get((int) (cursor / rowLength)).get((int) (cursor++ % rowLength));
                } else {
                    cursor++;
                    return null;
                }
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }
        });
    }

    @Override
    public Stream<Stream<E>> stream2() {
        return stream2(0, _rowKeySet.size());
    }

    @Override
    public Stream<Stream<E>> stream2(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, _rowKeySet.size());

        if (_rowKeySet.size() == 0 || _columnKeySet.size() == 0) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<Stream<E>>() {
            private final int toIndex = toRowIndex;
            private volatile int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<E> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return Stream.of(new ImmutableIterator<E>() {
                    private final int rowIndex = cursor++;
                    private final int toIndex2 = _columnKeySet.size();
                    private int cursor2 = 0;

                    @Override
                    public boolean hasNext() {
                        return cursor2 < toIndex2;
                    }

                    @Override
                    public E next() {
                        if (cursor2 >= toIndex2) {
                            throw new NoSuchElementException();
                        }

                        if (_initialized) {
                            return _columnList.get(cursor2++).get(rowIndex);
                        } else {
                            cursor2++;
                            return null;
                        }
                    }

                    @Override
                    public void skip(long n) {
                        cursor2 = n < toIndex2 - cursor2 ? cursor2 + (int) n : toIndex2;
                    }

                    @Override
                    public long count() {
                        return toIndex2 - cursor2;
                    }
                });
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }
        });
    }

    @Override
    public Stream<Stream<E>> stream02() {
        return stream02(0, _columnKeySet.size());
    }

    @Override
    public Stream<Stream<E>> stream02(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, _columnKeySet.size());

        if (_rowKeySet.size() == 0 || _columnKeySet.size() == 0) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<Stream<E>>() {
            private final int toIndex = toColumnIndex;
            private int cursor = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<E> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                if (_initialized) {
                    return Stream.of(_columnList.get(cursor++));
                } else {
                    cursor++;
                    return Stream.repeat(null, rowLength());
                }
            }

            @Override
            public void skip(long n) {
                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public long count() {
                return toIndex - cursor;
            }
        });
    }

    @Override
    public DataSet toDataSet() {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final List<String> dataSetColumnNameList = new ArrayList<>(columnLength);

        for (C columnKey : _columnKeySet) {
            dataSetColumnNameList.add(N.toString(columnKey));
        }

        final List<List<Object>> dataSetColumnList = new ArrayList<>(columnLength);

        if (_initialized) {
            for (List<E> column : _columnList) {
                dataSetColumnList.add(new ArrayList<Object>(column));
            }
        } else {
            for (int i = 0; i < columnLength; i++) {
                List<Object> column = new ArrayList<>(rowLength);
                N.fill(column, 0, rowLength, null);
                dataSetColumnList.add(column);
            }
        }

        return new RowDataSet(dataSetColumnNameList, dataSetColumnList);
    }

    @Override
    public DataSet toDataSet0() {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final List<String> dataSetColumnNameList = new ArrayList<>(rowLength);

        for (R rowKey : _rowKeySet) {
            dataSetColumnNameList.add(N.toString(rowKey));
        }

        final List<List<Object>> dataSetColumnList = new ArrayList<>(rowLength);

        if (_initialized) {
            for (int i = 0; i < rowLength; i++) {
                final List<Object> column = new ArrayList<>(columnLength);

                for (int j = 0; j < columnLength; j++) {
                    column.add(_columnList.get(j).get(i));
                }

                dataSetColumnList.add(column);
            }
        } else {
            for (int i = 0; i < rowLength; i++) {
                List<Object> column = new ArrayList<>(columnLength);
                N.fill(column, 0, columnLength, null);
                dataSetColumnList.add(column);
            }
        }

        return new RowDataSet(dataSetColumnNameList, dataSetColumnList);
    }

    @Override
    public Matrix<E> toMatrix(Class<E> cls) {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final E[][] c = N.newArray(N.newArray(cls, 0).getClass(), rowLength);

        for (int i = 0; i < rowLength; i++) {
            c[i] = N.newArray(cls, columnLength);
        }

        if (rowLength == 0 || columnLength == 0 || _initialized == false) {
            return new Matrix<>(c);
        }

        for (int i = 0; i < columnLength; i++) {
            final List<E> column = _columnList.get(i);

            for (int j = 0; j < rowLength; j++) {
                c[j][i] = column.get(j);
            }
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<E> toMatrix0(Class<E> cls) {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final E[][] c = N.newArray(N.newArray(cls, 0).getClass(), columnLength);

        for (int i = 0; i < columnLength; i++) {
            c[i] = N.newArray(cls, rowLength);
        }

        if (rowLength == 0 || columnLength == 0 || _initialized == false) {
            return new Matrix<>(c);
        }

        for (int i = 0; i < columnLength; i++) {
            final List<E> column = _columnList.get(i);

            for (int j = 0; j < rowLength; j++) {
                c[i][j] = column.get(j);
            }
        }

        return new Matrix<>(c);
    }

    @Override
    public Object[][] toArray() {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final Object[][] copy = new Object[rowLength][columnLength];

        if (_initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = _columnList.get(i);

                for (int j = 0; j < rowLength; j++) {
                    copy[j][i] = column.get(j);
                }
            }
        }

        return copy;
    }

    @Override
    public <T> T[][] toArray(Class<T> cls) {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final T[][] copy = N.newArray(N.newArray(cls, 0).getClass(), rowLength);

        for (int i = 0; i < rowLength; i++) {
            copy[i] = N.newArray(cls, columnLength);
        }

        if (_initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = _columnList.get(i);

                for (int j = 0; j < rowLength; j++) {
                    copy[j][i] = (T) column.get(j);
                }
            }
        }

        return copy;
    }

    @Override
    public Object[][] toArray0() {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final Object[][] copy = new Object[columnLength][rowLength];

        if (_initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = _columnList.get(i);

                for (int j = 0; j < rowLength; j++) {
                    copy[i][j] = column.get(j);
                }
            }
        }

        return copy;
    }

    @Override
    public <T> T[][] toArray0(Class<T> cls) {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final T[][] copy = N.newArray(N.newArray(cls, 0).getClass(), columnLength);

        for (int i = 0; i < columnLength; i++) {
            copy[i] = N.newArray(cls, rowLength);
        }

        if (_initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = _columnList.get(i);

                for (int j = 0; j < rowLength; j++) {
                    copy[i][j] = (T) column.get(j);
                }
            }
        }

        return copy;
    }

    @Override
    public void println() {
        final int columnLength = columnLength();
        N.println(Joiner.with(", ", "       ", "").join(_columnKeySet).toString());

        int i = 0;
        for (R rowKey : _rowKeySet) {
            final Joiner joiner = Joiner.with(", ");
            joiner.add(rowKey);

            if (this._initialized) {
                for (int j = 0; j < columnLength; j++) {
                    joiner.add(_columnList.get(j).get(i));
                }
            } else {
                for (int j = 0; j < columnLength; j++) {
                    joiner.add(N.NULL_STRING);
                }
            }

            i++;

            N.println(joiner.toString());
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((_rowKeySet == null) ? 0 : _rowKeySet.hashCode());
        result = prime * result + ((_columnKeySet == null) ? 0 : _columnKeySet.hashCode());
        result = prime * result + (_initialized ? _columnList.hashCode() : 0);
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof ArraySheet) {
            ArraySheet<R, C, E> other = (ArraySheet<R, C, E>) obj;

            return N.equals(other._rowKeySet, _rowKeySet) && N.equals(other._columnKeySet, _columnKeySet) && N.deepEquals(other._columnList, _columnList);
        }

        return false;
    }

    @Override
    public String toString() {
        final StringBuilder sb = ObjectFactory.createStringBuilder();

        sb.append("{rowKeySet=");
        sb.append(_rowKeySet);
        sb.append(", columnKeySet=");
        sb.append(_columnKeySet);
        sb.append(", rowList=");
        sb.append("[");

        if (_initialized) {
            for (int i = 0, rowLength = _rowKeySet.size(), columnLength = _columnKeySet.size(); i < rowLength; i++) {
                if (i > 0) {
                    sb.append(N.ELEMENT_SEPARATOR_CHAR_ARRAY);
                }

                sb.append("[");

                for (int j = 0; j < columnLength; j++) {
                    if (j > 0) {
                        sb.append(N.ELEMENT_SEPARATOR_CHAR_ARRAY);
                    }

                    sb.append(N.toString(_columnList.get(j).get(i)));
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

    private void init() {
        if (!_initialized) {
            initIndexMap();

            final int rowLength = rowLength();
            final int columnLength = columnLength();
            _columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                final List<E> column = new ArrayList<>(rowLength);
                N.fill(column, 0, rowLength, null);
                _columnList.add(column);
            }

            _initialized = true;
        }
    }

    private void initIndexMap() {
        if (_rowKeyIndexMap == null || _columnKeyIndexMap == null) {
            final int rowLength = rowLength();
            _rowKeyIndexMap = new BiMap<>(N.initHashCapacity(rowLength));
            int index = 0;
            for (R rowKey : _rowKeySet) {
                _rowKeyIndexMap.put(rowKey, index++);
            }

            final int columnLength = columnLength();
            _columnKeyIndexMap = new BiMap<>(N.initHashCapacity(columnLength));
            index = 0;
            for (C columnKey : _columnKeySet) {
                _columnKeyIndexMap.put(columnKey, index++);
            }
        }
    }

    private void checkRowKey(Object rowKey) {
        if (!_rowKeySet.contains(rowKey)) {
            throw new IllegalArgumentException("No row found by key: " + rowKey);
        }
    }

    private void checkColumnKey(Object columnKey) {
        if (!_columnKeySet.contains(columnKey)) {
            throw new IllegalArgumentException("No column found by key: " + columnKey);
        }
    }

    private void checkRowIndex(int rowIndex) {
        if (rowIndex < 0 || rowIndex >= _rowKeySet.size()) {
            throw new IndexOutOfBoundsException("Row index: " + rowIndex + " can't be negative or equals to or bigger than the row size: " + _rowKeySet.size());
        }
    }

    private void checkColumnIndex(int columnIndex) {
        if (columnIndex < 0 || columnIndex >= _columnKeySet.size()) {
            throw new IndexOutOfBoundsException(
                    "Column index: " + columnIndex + " can't be negative or equals to or bigger than the column size: " + _columnKeySet.size());
        }
    }

    private int getRowIndex(Object rowKey) {
        if (_rowKeyIndexMap == null) {
            this.initIndexMap();
        }

        Integer index = _rowKeyIndexMap.get(rowKey);

        if (index == null) {
            throw new IllegalArgumentException("No row found by key: " + rowKey);
        }

        return index;
    }

    private int getColumnIndex(Object columnKey) {
        if (_columnKeyIndexMap == null) {
            this.initIndexMap();
        }

        Integer index = _columnKeyIndexMap.get(columnKey);

        if (index == null) {
            throw new IllegalArgumentException("No column found by key: " + columnKey);
        }

        return index;
    }

    private void checkFrozen() {
        if (_isFrozen) {
            throw new IllegalStateException("This DataSet is frozen, can't modify it.");
        }
    }

    static class Cell0<R, C, E> implements Sheet.Cell<R, C, E> {
        private final R rowKey;
        private final C columnKey;
        private final E value;

        public Cell0(R rowKey, C columnKey, E value) {
            this.rowKey = rowKey;
            this.columnKey = columnKey;
            this.value = value;
        }

        @Override
        public R rowKey() {
            return rowKey;
        }

        @Override
        public C columnKey() {
            return columnKey;
        }

        @Override
        public E value() {
            return value;
        }

        @Override
        public int hashCode() {
            int result = N.hashCode(rowKey);
            result = result * 31 + N.hashCode(columnKey);
            result = result * 31 + N.hashCode(value);
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj instanceof Cell0) {
                final Cell0<R, C, E> other = (Cell0<R, C, E>) obj;

                return N.equals(rowKey, other.rowKey) && N.equals(columnKey, other.columnKey) && N.equals(value, other.value);
            }

            return false;
        }

        @Override
        public String toString() {
            return "{rowKey=" + N.toString(rowKey) + ", columnKey=" + N.toString(columnKey) + ", value=" + N.toString(value) + "}";
        }
    }
}
