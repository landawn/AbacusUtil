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
import com.landawn.abacus.util.stream.ImmutableIterator;
import com.landawn.abacus.util.stream.Stream;

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
    private List<List<E>> columnList;
    private boolean initialized = false;

    public ArraySheet(Collection<R> rowKeySet, Collection<C> columnKeySet) {
        this.rowKeySet = new LinkedHashSet<>(rowKeySet);
        this.columnKeySet = new LinkedHashSet<>(columnKeySet);
    }

    public ArraySheet(Collection<R> rowKeySet, Collection<C> columnKeySet, Object[][] rows) {
        this(rowKeySet, columnKeySet);

        final int rowLength = this.rowKeySet.size();
        final int columnLength = this.columnKeySet.size();

        if (N.notNullOrEmpty(rows)) {
            N.checkArgument(rows.length == rowLength, "The length of array is not equal to size of row/column key set");

            for (Object[] e : rows) {
                N.checkArgument(e.length == columnLength, "The length of array is not equal to size of row/column key set");
            }

            initIndexMap();

            this.columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                final List<E> column = new ArrayList<>(rowLength);

                for (int j = 0; j < rowLength; j++) {
                    column.add((E) rows[j][i]);
                }

                this.columnList.add(column);
            }

            initialized = true;
        }
    }

    public static <R, C, E> ArraySheet<R, C, E> of(Collection<R> rowKeySet, Collection<C> columnKeySet, Object[][] rows) {
        return new ArraySheet<>(rowKeySet, columnKeySet, rows);
    }

    public static <R, C, E> ArraySheet<R, C, E> of(Collection<R> rowKeySet, Collection<C> columnKeySet, Collection<? extends Collection<? extends E>> rows) {
        final ArraySheet<R, C, E> instance = new ArraySheet<>(rowKeySet, columnKeySet);

        final int rowLength = instance.rowKeySet.size();
        final int columnLength = instance.columnKeySet.size();

        if (N.notNullOrEmpty(rows)) {
            N.checkArgument(rows.size() == rowLength, "The size of collection is not equal to size of row/column key set");

            for (Collection<? extends E> e : rows) {
                N.checkArgument(e.size() == columnLength, "The size of collection is not equal to size of row/column key set");
            }

            instance.initIndexMap();

            instance.columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                instance.columnList.add(new ArrayList<E>(rowLength));
            }

            for (Collection<? extends E> row : rows) {
                final Iterator<? extends E> iter = row.iterator();

                for (int i = 0; i < columnLength; i++) {
                    instance.columnList.get(i).add(iter.next());
                }
            }

            instance.initialized = true;
        }

        return instance;

    }

    public static <R, C, E> ArraySheet<R, C, E> from(Collection<R> rowKeySet, Collection<C> columnKeySet, Object[][] columns) {
        final ArraySheet<R, C, E> instance = new ArraySheet<>(rowKeySet, columnKeySet);

        final int rowLength = instance.rowKeySet.size();
        final int columnLength = instance.columnKeySet.size();

        if (N.notNullOrEmpty(columns)) {
            N.checkArgument(columns.length == columnLength, "The length of array is not equal to size of row/column key set");

            for (Object[] e : columns) {
                N.checkArgument(e.length == rowLength, "The length of array is not equal to size of row/column key set");
            }

            instance.initIndexMap();

            instance.columnList = new ArrayList<>(columnLength);

            for (Object[] e : columns) {
                instance.columnList.add(new ArrayList<>((List<E>) Arrays.asList(e)));
            }

            instance.initialized = true;
        }

        return instance;

    }

    public static <R, C, E> ArraySheet<R, C, E> from(Collection<R> rowKeySet, Collection<C> columnKeySet,
            Collection<? extends Collection<? extends E>> columns) {
        final ArraySheet<R, C, E> instance = new ArraySheet<>(rowKeySet, columnKeySet);

        final int rowLength = instance.rowKeySet.size();
        final int columnLength = instance.columnKeySet.size();

        if (N.notNullOrEmpty(columns)) {
            N.checkArgument(columns.size() == columnLength, "The size of collection is not equal to size of row/column key set");

            for (Collection<? extends E> e : columns) {
                N.checkArgument(e.size() == rowLength, "The size of collection is not equal to size of row/column key set");
            }

            instance.initIndexMap();

            instance.columnList = new ArrayList<>(columnLength);

            for (Collection<? extends E> e : columns) {
                instance.columnList.add(new ArrayList<>(e));
            }

            instance.initialized = true;
        }

        return instance;
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
    public E get(Object rowKey, Object columnKey) {
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
            return columnList.get(columnIndex).get(rowIndex);
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
    public void putAll(Sheet<? extends R, ? extends C, ? extends E> source) {
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
            return columnList.get(columnIndex).set(rowIndex, null);
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

        if (this.initialized) {
            for (List<E> column : this.columnList) {
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
        final List<E> row = new ArrayList<E>(columnLength);

        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                row.add(columnList.get(columnIndex).get(rowIndex));
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
                columnList.get(columnIndex).set(rowIndex, null);
            }
        } else {
            final Iterator<? extends E> iter = row.iterator();

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).set(rowIndex, iter.next());
            }
        }
    }

    @Override
    public void addRow(R rowKey, Collection<? extends E> row) {
        if (rowKeySet.contains(rowKey)) {
            throw new IllegalArgumentException("Row '" + rowKey + "' already existed");
        }

        final int columnLength = columnLength();

        if (N.notNullOrEmpty(row) && row.size() != columnLength) {
            throw new IllegalArgumentException("The size of specified row: " + row.size() + " doesn't match the length of column key set: " + columnLength);
        }

        init();

        rowKeySet.add(rowKey);

        final int newRowLength = rowKeySet.size();
        final int newRowIndex = newRowLength - 1;
        rowKeyIndexMap.put(rowKey, newRowIndex);

        if (N.isNullOrEmpty(row)) {
            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).add(null);
            }
        } else {
            final Iterator<? extends E> iter = row.iterator();

            for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                columnList.get(columnIndex).add(iter.next());
            }
        }
    }

    @Override
    public void removeRow(Object rowKey) {
        checkRowKey(rowKey);

        rowKeySet.remove(rowKey);

        if (rowKeyIndexMap != null) {
            final int columnLength = columnLength();
            final int newRowSize = rowKeySet.size();
            final int removedRowIndex = rowKeyIndexMap.remove(rowKey);

            if (removedRowIndex == newRowSize) {
                // removed last row.
            } else {
                for (int i = removedRowIndex; i < newRowSize; i++) {
                    rowKeyIndexMap.put(rowKeyIndexMap.getByValue(i + 1), i);
                }
            }

            if (initialized) {
                for (int columnIndex = 0; columnIndex < columnLength; columnIndex++) {
                    columnList.get(columnIndex).remove(removedRowIndex);
                }
            }
        }
    }

    @Override
    public boolean containsRow(Object rowKey) {
        return rowKeySet.contains(rowKey);
    }

    @Override
    public Map<C, E> row(Object rowKey) {
        final int columnLength = columnLength();
        Map<C, E> rowMap = new LinkedHashMap<>(N.initHashCapacity(columnLength));

        if (initialized) {
            final int rowIndex = getRowIndex(rowKey);
            int columnIndex = 0;

            for (C columnKey : this.columnKeySet()) {
                rowMap.put(columnKey, columnList.get(columnIndex++).get(rowIndex));
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

        if (initialized) {
            column = columnList.get(getColumnIndex(columnKey));
        } else {
            checkColumnKey(columnKey);
            column = new ArrayList<E>(rowLength);
            N.fill(column, 0, rowLength, null);
        }

        return N.asImmutableList(column);
    }

    @Override
    public void setColumn(C columnKey, Collection<? extends E> column) {
        final int rowLength = rowLength();

        if (N.notNullOrEmpty(column) && column.size() != rowLength) {
            throw new IllegalArgumentException("The size of specified column: " + column.size() + " doesn't match the length of row key set: " + rowLength);
        }

        init();

        final int columnIndex = getColumnIndex(columnKey);

        if (N.isNullOrEmpty(column)) {
            N.fill(columnList.get(columnIndex), 0, rowLength, null);
        } else {
            columnList.set(columnIndex, new ArrayList<E>(column));
        }
    }

    @Override
    public void addColumn(C columnKey, Collection<? extends E> column) {
        if (columnKeySet.contains(columnKey)) {
            throw new IllegalArgumentException("Column '" + columnKey + "' already existed");
        }

        final int rowLength = rowLength();

        if (N.notNullOrEmpty(column) && column.size() != rowLength) {
            throw new IllegalArgumentException("The size of specified column: " + column.size() + " doesn't match the length of row key set: " + rowLength);
        }

        init();

        columnKeySet.add(columnKey);

        final int newColumnLength = columnKeySet.size();
        final int newColumnIndex = newColumnLength - 1;
        columnKeyIndexMap.put(columnKey, newColumnIndex);

        if (N.isNullOrEmpty(column)) {
            List<E> newColumn = new ArrayList<>();
            N.fill(newColumn, 0, rowLength, null);
            columnList.add(newColumn);
        } else {
            columnList.add(new ArrayList<>(column));
        }
    }

    @Override
    public void removeColumn(Object columnKey) {
        checkColumnKey(columnKey);

        columnKeySet.remove(columnKey);

        if (columnKeyIndexMap != null) {
            final int newColumnLength = columnKeySet.size();
            final int removedColumnIndex = columnKeyIndexMap.remove(columnKey);

            if (removedColumnIndex == newColumnLength) {
                // removed the last column
            } else {
                for (int i = removedColumnIndex; i < newColumnLength; i++) {
                    columnKeyIndexMap.put(columnKeyIndexMap.getByValue(i + 1), i);
                }
            }

            if (initialized) {
                columnList.remove(removedColumnIndex);
            }
        }
    }

    @Override
    public boolean containsColumn(Object columnKey) {
        return columnKeySet.contains(columnKey);
    }

    @Override
    public Map<R, E> column(Object columnKey) {
        final int rowLength = rowLength();
        final Map<R, E> columnMap = new LinkedHashMap<>(N.initHashCapacity(rowLength));

        if (initialized) {
            final int columnIndex = getColumnIndex(columnKey);
            final List<E> column = columnList.get(columnIndex);
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
        return rowKeySet.size();
    }

    @Override
    public int columnLength() {
        return columnKeySet.size();
    }

    @Override
    public void clear() {
        if (initialized && columnList.size() > 0) {
            for (List<E> column : columnList) {
                N.fill(column, 0, column.size(), null);
            }
        }
    }

    @Override
    public void trimToSize() {
        if (initialized && columnList.size() > 0) {
            for (List<E> column : columnList) {
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
            copy.initIndexMap();

            copy.columnList = new ArrayList<>(this.columnList.size());

            for (List<E> column : this.columnList) {
                copy.columnList.add(new ArrayList<>(column));
            }

            copy.initialized = true;
        }

        return copy;
    }

    @Override
    public Sheet<C, R, E> transpose() {
        final ArraySheet<C, R, E> copy = new ArraySheet<C, R, E>(this.columnKeySet, this.rowKeySet);

        if (this.initialized) {
            copy.initIndexMap();

            final int rowLength = copy.rowLength();
            final int columnLength = copy.columnLength();

            copy.columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                final List<E> column = new ArrayList<>(rowLength);

                for (int j = 0; j < rowLength; j++) {
                    column.add(this.columnList.get(j).get(i));
                }

                copy.columnList.add(column);
            }

            copy.initialized = true;
        }

        return copy;
    }

    @Override
    public Stream<Sheet.Cell<R, C, E>> cells() {
        return cells(0, rowKeySet.size());
    }

    @Override
    public Stream<Sheet.Cell<R, C, E>> cells(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, rowKeySet.size());

        if (rowKeySet.size() == 0 || columnKeySet.size() == 0) {
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

                return new Cell0<R, C, E>(rowKeyIndexMap.getByValue(rowIndex), columnKeyIndexMap.getByValue(columnIndex),
                        initialized ? columnList.get(columnIndex).get(rowIndex) : null);
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
        return cells0(0, columnKeySet.size());
    }

    @Override
    public Stream<Sheet.Cell<R, C, E>> cells0(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, columnKeySet.size());

        if (rowKeySet.size() == 0 || columnKeySet.size() == 0) {
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

                return new Cell0<R, C, E>(rowKeyIndexMap.getByValue(rowIndex), columnKeyIndexMap.getByValue(columnIndex),
                        initialized ? columnList.get(columnIndex).get(rowIndex) : null);
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
        return stream(0, rowKeySet.size());
    }

    @Override
    public Stream<E> stream(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, rowKeySet.size());

        if (rowKeySet.size() == 0 || columnKeySet.size() == 0) {
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

                if (initialized) {
                    return columnList.get((int) (cursor % columnLength)).get((int) (cursor++ / columnLength));
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
        return stream0(0, columnKeySet.size());
    }

    @Override
    public Stream<E> stream0(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, columnKeySet.size());

        if (rowKeySet.size() == 0 || columnKeySet.size() == 0) {
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

                if (initialized) {
                    return columnList.get((int) (cursor / rowLength)).get((int) (cursor++ % rowLength));
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
        return stream2(0, rowKeySet.size());
    }

    @Override
    public Stream<Stream<E>> stream2(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, rowKeySet.size());

        if (rowKeySet.size() == 0 || columnKeySet.size() == 0) {
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
                    private final int toIndex2 = columnKeySet.size();
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

                        if (initialized) {
                            return columnList.get(cursor2++).get(rowIndex);
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
        return stream02(0, columnKeySet.size());
    }

    @Override
    public Stream<Stream<E>> stream02(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, columnKeySet.size());

        if (rowKeySet.size() == 0 || columnKeySet.size() == 0) {
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

                if (initialized) {
                    return Stream.of(columnList.get(cursor++));
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
        final List<String> _columnNameList = new ArrayList<>(columnLength);

        for (C columnKey : columnKeySet) {
            _columnNameList.add(N.toString(columnKey));
        }

        final List<List<Object>> _columnList = new ArrayList<>(columnLength);

        if (initialized) {
            for (List<E> column : columnList) {
                _columnList.add(new ArrayList<Object>(column));
            }
        } else {
            for (int i = 0; i < columnLength; i++) {
                List<Object> column = new ArrayList<Object>(rowLength);
                N.fill(column, 0, rowLength, null);
                _columnList.add(column);
            }
        }

        return new RowDataSet(_columnNameList, _columnList);
    }

    @Override
    public DataSet toDataSet0() {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final List<String> _columnNameList = new ArrayList<>(rowLength);

        for (R rowKey : rowKeySet) {
            _columnNameList.add(N.toString(rowKey));
        }

        final List<List<Object>> _columnList = new ArrayList<>(rowLength);

        if (initialized) {
            for (int i = 0; i < rowLength; i++) {
                final List<Object> column = new ArrayList<>(columnLength);

                for (int j = 0; j < columnLength; j++) {
                    column.add(columnList.get(j).get(i));
                }

                _columnList.add(column);
            }
        } else {
            for (int i = 0; i < rowLength; i++) {
                List<Object> column = new ArrayList<>(columnLength);
                N.fill(column, 0, columnLength, null);
                _columnList.add(column);
            }
        }

        return new RowDataSet(_columnNameList, _columnList);
    }

    @Override
    public Matrix<E> toMatrix(Class<E> cls) {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final E[][] c = N.newArray(N.newArray(cls, 0).getClass(), rowLength);

        for (int i = 0; i < rowLength; i++) {
            c[i] = N.newArray(cls, columnLength);
        }

        if (rowLength == 0 || columnLength == 0 || initialized == false) {
            return new Matrix<E>(c);
        }

        for (int i = 0; i < columnLength; i++) {
            final List<E> column = columnList.get(i);

            for (int j = 0; j < rowLength; j++) {
                c[j][i] = column.get(j);
            }
        }

        return new Matrix<E>(c);
    }

    @Override
    public Matrix<E> toMatrix0(Class<E> cls) {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final E[][] c = N.newArray(N.newArray(cls, 0).getClass(), columnLength);

        for (int i = 0; i < columnLength; i++) {
            c[i] = N.newArray(cls, rowLength);
        }

        if (rowLength == 0 || columnLength == 0 || initialized == false) {
            return new Matrix<E>(c);
        }

        for (int i = 0; i < columnLength; i++) {
            final List<E> column = columnList.get(i);

            for (int j = 0; j < rowLength; j++) {
                c[i][j] = column.get(j);
            }
        }

        return new Matrix<E>(c);
    }

    @Override
    public Object[][] toArray() {
        final int rowLength = rowLength();
        final int columnLength = columnLength();
        final Object[][] copy = new Object[rowLength][columnLength];

        if (initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = columnList.get(i);

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

        if (initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = columnList.get(i);

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

        if (initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = columnList.get(i);

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

        if (initialized) {
            for (int i = 0; i < columnLength; i++) {
                final List<E> column = columnList.get(i);

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
        N.println(Joiner.with(", ", "       ", "").join(columnKeySet).toString());

        int i = 0;
        for (R rowKey : rowKeySet) {
            final Joiner joiner = Joiner.with(", ");
            joiner.add(rowKey);

            if (this.initialized) {
                for (int j = 0; j < columnLength; j++) {
                    joiner.add(columnList.get(j).get(i));
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

    private void init() {
        if (!initialized) {
            initIndexMap();

            final int rowLength = rowLength();
            final int columnLength = columnLength();
            columnList = new ArrayList<>(columnLength);

            for (int i = 0; i < columnLength; i++) {
                final List<E> column = new ArrayList<>(rowLength);
                N.fill(column, 0, rowLength, null);
                columnList.add(column);
            }

            initialized = true;
        }
    }

    private void initIndexMap() {
        if (rowKeyIndexMap == null || columnKeyIndexMap == null) {
            final int rowLength = rowLength();
            rowKeyIndexMap = new BiMap<>(N.initHashCapacity(rowLength));
            int index = 0;
            for (R rowKey : rowKeySet) {
                rowKeyIndexMap.put(rowKey, index++);
            }

            final int columnLength = columnLength();
            columnKeyIndexMap = new BiMap<>(N.initHashCapacity(columnLength));
            index = 0;
            for (C columnKey : columnKeySet) {
                columnKeyIndexMap.put(columnKey, index++);
            }
        }
    }

    private void checkRowKey(Object rowKey) {
        if (!rowKeySet.contains(rowKey)) {
            throw new IllegalArgumentException("No row found by key: " + rowKey);
        }
    }

    private void checkColumnKey(Object columnKey) {
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

    private int getRowIndex(Object rowKey) {
        Integer index = rowKeyIndexMap.get(rowKey);

        if (index == null) {
            throw new IllegalArgumentException("No row found by key: " + rowKey);
        }

        return index;
    }

    private int getColumnIndex(Object columnKey) {
        Integer index = columnKeyIndexMap.get(columnKey);

        if (index == null) {
            throw new IllegalArgumentException("No column found by key: " + columnKey);
        }

        return index;
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
