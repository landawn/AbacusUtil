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

package com.landawn.abacus;

import java.io.File;
import java.io.OutputStream;
import java.io.Writer;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.util.Multimap;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.ObjectList;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.OptionalNullable;
import com.landawn.abacus.util.Properties;
import com.landawn.abacus.util.Sheet;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Stream;

/**
 * Uses <code>IntFunction rowSupplier</code> to identity row for generic types.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public interface DataSet extends Iterable<Object[]> {

    @SuppressWarnings("rawtypes")
    public static final IntFunction<ObjectList<Object>> OBJECT_LIST_FACTORY = (IntFunction) IntFunction.OBJECT_LIST_FACTORY;

    @SuppressWarnings("rawtypes")
    public static final IntFunction<List<Object>> LIST_FACTORY = (IntFunction) IntFunction.LIST_FACTORY;

    @SuppressWarnings("rawtypes")
    public static final IntFunction<LinkedList<Object>> LINKED_LIST_FACTORY = (IntFunction) IntFunction.LINKED_LIST_FACTORY;

    @SuppressWarnings("rawtypes")
    public static final IntFunction<Set<Object>> SET_FACTORY = (IntFunction) IntFunction.SET_FACTORY;

    @SuppressWarnings("rawtypes")
    public static final IntFunction<LinkedHashSet<Object>> LINKED_HASH_SET_FACTORY = (IntFunction) IntFunction.LINKED_HASH_SET_FACTORY;

    @SuppressWarnings("rawtypes")
    public static final IntFunction<Map<String, Object>> MAP_FACTORY = (IntFunction) IntFunction.MAP_FACTORY;

    @SuppressWarnings("rawtypes")
    public static final IntFunction<LinkedHashMap<String, Object>> LINKED_HASH_MAP_FACTORY = (IntFunction) IntFunction.LINKED_HASH_MAP_FACTORY;

    //    /**
    //     * Returns the entity name associated with the query.
    //     *
    //     * @return
    //     */
    //    String entityName();
    //
    //    /**
    //     * Returns the target entity class associated with the query.
    //     *
    //     * @return
    //     */
    //    <T> Class<T> entityClass();

    /**
     * Return the column name list in this DataSet.
     */
    List<String> columnNameList();

    /**
     * Return the column name list filtered by specified <code>filter</code>.
     * 
     * @param filter
     */
    List<String> columnNames(Predicate<String> filter);

    /**
     * Method getColumnName.
     *
     * @param columnIndex
     * @return
     */
    String getColumnName(int columnIndex);

    /**
     * Method getColumnIndex.
     *
     * @param columnName
     * @return -1 if the specified <code>columnName</code> is not found
     */
    int getColumnIndex(String columnName);

    /**
     * -1 is set to the element in the returned array if the mapping column name is not included in this <code>DataSet</code>
     *
     * @param columnNames
     * @return
     */
    int[] getColumnIndex(Collection<String> columnNames);

    /**
     *
     * @param columnName
     * @return
     */
    boolean containsColumn(String columnName);

    /**
     * Check if this <code>DataSet</code> contains all the specified columns.
     *
     * @param columnNames
     * @return <code>true</code> if all the specified columns are included in the this <code>DataSet</code>
     */
    boolean containsColumnAll(Collection<String> columnNames);

    /**
     *
     * @param columnName
     * @param newColumnName
     */
    void renameColumn(String columnName, String newColumnName);

    /**
     * 
     * @param oldNewNames
     */
    void renameColumn(Map<String, String> oldNewNames);

    /**
     *
     * @param columnName
     * @param func
     */
    void renameColumn(String columnName, Function<String, String> func);

    /**
     *
     * @param columnNames
     * @param func
     */
    void renameColumn(Collection<String> columnNames, Function<String, String> func);

    void moveColumn(String columnName, int newPosition);

    void moveColumn(Map<String, Integer> columnNameNewPositionMap);

    /**
     * Swap the positions of the two specified columns.
     * 
     * @param columnNameA
     * @param columnNameB
     */
    void swapColumn(String columnNameA, String columnNameB);

    /**
     * Move the specified row to the new position.
     * 
     * @param rowIndex
     * @param newRowIndex
     */
    void moveRow(int rowIndex, int newRowIndex);

    /**
     * Swap the positions of the two specified rows.
     * 
     * @param columnNameA
     * @param columnNameB
     */
    void swapRow(int rowIndexA, int rowIndexB);

    /**
     *
     * @param rowIndex
     * @param columnIndex
     * @return
     */
    <T> T get(int rowIndex, int columnIndex);

    /**
     * @param targetClass
     * @param rowIndex
     * @param columnIndex
     * @return
     */
    <T> T get(Class<T> targetClass, int rowIndex, int columnIndex);

    /**
     *
     * @param rowIndex
     * @param columnIndex
     * @param element
     */
    void set(int rowIndex, int columnIndex, Object element);

    /**
     * 
     * @param rowIndex
     * @param columnIndex
     * @return
     */
    boolean isNull(int rowIndex, int columnIndex);

    /**
     *
     * @param columnIndex
     * @return
     */
    <T> T get(int columnIndex);

    /**
     *
     * @param targetClass
     * @param columnIndex
     * @return
     */
    <T> T get(Class<T> targetClass, int columnIndex);

    /**
     *
     * @param columnName
     * @return
     */
    <T> T get(String columnName);

    /**
     *
     * @param targetClass
     * @param columnName
     * @return
     */
    <T> T get(Class<T> targetClass, String columnName);

    /**
     * Return default value (false) if the property is null.
     *
     * @param columnIndex
     * @return
     */
    boolean getBoolean(int columnIndex);

    /**
     * Return default value (false) if the property is null.
     *
     * @param columnName
     * @return
     */
    boolean getBoolean(String columnName);

    /**
     * Return default value (0) if the property is null.
     *
     * @param columnIndex
     * @return
     */
    char getChar(int columnIndex);

    /**
     * Return default value (0) if the property is null.
     *
     * @param columnName
     * @return
     */
    char getChar(String columnName);

    /**
     * Return default value (0) if the property is null. Return Number.byteValue() otherwise.
     *
     * @param columnIndex
     * @return
     */
    byte getByte(int columnIndex);

    /**
     * Return default value (0) if the property is null. Return Number.byteValue() otherwise.
     *
     * @param columnName
     * @return
     */
    byte getByte(String columnName);

    /**
     * Return default value (0) if the property is null. Return Number.shortValue() otherwise.
     *
     * @param columnIndex
     * @return
     */
    short getShort(int columnIndex);

    /**
     * Return default value (0) if the property is null. Return Number.shortValue() otherwise.
     *
     * @param columnName
     * @return
     */
    short getShort(String columnName);

    /**
     * Return default value (0) if the property is null. Return Number.intValue() otherwise.
     *
     * @param columnIndex
     * @return
     */
    int getInt(int columnIndex);

    /**
     * Return default value (0) if the property is null. Return Number.intValue() otherwise.
     *
     * @param columnName
     * @return
     */
    int getInt(String columnName);

    /**
     * Return default value (0) if the property is null. Return Number.longValue() otherwise.
     *
     * @param columnIndex
     * @return
     */
    long getLong(int columnIndex);

    /**
     * Return default value (0) if the property is null. Return Number.longValue() otherwise.
     *
     * @param columnName
     * @return
     */
    long getLong(String columnName);

    /**
     * Return default value (0f) if the property is null. Return Number.floatValue() otherwise.
     *
     * @param columnIndex
     * @return
     */
    float getFloat(int columnIndex);

    /**
     * Return default value (0f) if the property is null. Return Number.floatValue() otherwise.
     *
     * @param columnName
     * @return
     */
    float getFloat(String columnName);

    /**
     * Return default value (0d) if the property is null. Return Number.doubleValue() otherwise.
     *
     * @param columnIndex
     * @return
     */
    double getDouble(int columnIndex);

    /**
     * Return default value (0d) if the property is null. Return Number.doubleValue() otherwise.
     *
     * @param columnName
     * @return
     */
    double getDouble(String columnName);

    /**
     *
     * @param columnIndex
     * @return
     */
    boolean isNull(int columnIndex);

    /**
     *
     * @param columnName
     * @return
     */
    boolean isNull(String columnName);

    /**
     * Method set.
     *
     * @param columnIndex
     * @param value
     */
    void set(int columnIndex, Object value);

    /**
     * Method set.
     *
     * @param columnName
     * @param value
     */
    void set(String columnName, Object value);

    /**
     * Must NOT modify the returned list.
     *
     * @param columnIndex
     * @return
     */
    <T> List<T> getColumn(int columnIndex);

    /**
     * Must NOT modify the returned list.
     *
     * @param columnName
     * @return
     */
    <T> List<T> getColumn(String columnName);

    //    /**
    //     * Must NOT modify the returned list.
    //     * 
    //     * @param columnType it's only used to identify the type.
    //     * @param columnIndex
    //     * @return
    //     */
    //    <T> List<T> getColumn(Class<? extends T> columnType, int columnIndex);
    //
    //    /**
    //     * Must NOT modify the returned list.
    //     *
    //     * @param columnType it's only used to identify the type.
    //     * @param columnName
    //     * @return
    //     */
    //    <T> List<T> getColumn(Class<? extends T> columnType, String columnName);

    /**
     * Method addColumn.
     *
     * @param columnName
     * @param column
     */
    void addColumn(String columnName, List<?> column);

    /**
     * Method addColumn.
     *
     * @param columnIndex position to add.
     * @param columnName
     * @param column
     */
    void addColumn(int columnIndex, String columnName, List<?> column);

    /**
     * Generate the new column values from the specified column by the specified <code>Function</code>.
     * 
     * @param fromColumnName
     * @param newColumnName
     * @param func
     */
    void addColumn(String fromColumnName, String newColumnName, Function<?, ?> func);

    /**
     * Generate the new column values from the specified column by the specified <code>Function</code>.
     * 
     * @param columnIndex
     * @param fromColumnName
     * @param newColumnName
     * @param func
     */
    void addColumn(int columnIndex, String fromColumnName, String newColumnName, Function<?, ?> func);

    /**
     * Generate the new column values from the specified columns by the specified <code>Function</code>.
     * 
     * @param fromColumnNames
     * @param newColumnName
     * @param func
     */
    void addColumn(Collection<String> fromColumnNames, String newColumnName, Function<? super Object[], ?> func);

    /**
     * Generate the new column values from the specified columns by the specified <code>Function</code>.
     * 
     * @param columnIndex
     * @param fromColumnNames
     * @param newColumnName
     * @param func
     */
    void addColumn(int columnIndex, Collection<String> fromColumnNames, String newColumnName, Function<? super Object[], ?> func);

    /**
     * Remove the column with the specified columnName from this DataSet.
     *
     * @param columnName
     */
    void removeColumn(String columnName);

    /**
     * Remove the column(s) with the specified columnNames from this DataSet.
     *
     * @param columnNames
     */
    void removeColumnAll(Collection<String> columnNames);

    /**
     * convert the specified column to target type.
     *
     * @param columnName
     * @param targetType
     */
    void convertColumn(String columnName, Class<?> targetType);

    /**
     * convert the specified columns to target types.
     *
     * @param columnTargetTypes
     */
    void convertColumn(Map<String, Class<?>> columnTargetTypes);

    /**
     * convert values of the specified column by the specified function.
     *
     * @param columnName
     * @param func
     */
    void updateColumn(String columnName, Function<?, ?> func);

    /**
     * convert values of the specified columns by the specified function.
     *
     * @param columnNames
     * @param func
     */
    void updateColumn(Collection<String> columnNames, Function<?, ?> func);

    //
    //    /**
    //     * convert the specified columns to target types.
    //     *
    //     * @param targetColumnTypes fill the element with <code>null</code> if don't wan to convert the target column.
    //     */
    //    void convertColumn(Class<?>[] targetColumnTypes);
    //
    /**
     *
     * @param columnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    void combineColumn(Collection<String> columnNames, String newColumnName, Class<?> newColumnClass);

    void combineColumn(Collection<String> columnNames, String newColumnName, Function<? super Object[], ?> combineFunc);

    void combineColumn(Predicate<String> columnNameFilter, String newColumnName, Class<?> newColumnClass);

    void combineColumn(Predicate<String> columnNameFilter, String newColumnName, Function<? super Object[], ?> combineFunc);

    void divideColumn(String columnName, Collection<String> newColumnNames, Function<?, ? extends List<?>> divideFunc);

    /**
     * 
     * @param row can be Object[]/List/Map/Entity with getter/setter methods
     */
    void addRow(Object row);

    /**
     * 
     * @param row can be Object[]/List/Map/Entity with getter/setter methods
     */
    void addRow(int rowIndex, Object row);

    /**
     * 
     * @param rowIndex
     */
    void removeRow(int rowIndex);

    /**
     * 
     * @param indices
     */
    void removeRowAll(int... indices);

    void updateRow(int rowIndex, Function<?, ?> func);

    void updateRow(int[] indices, Function<?, ?> func);

    void updateAll(Function<?, ?> func);

    /**
     * Returns the current row number.
     *
     * @return
     */
    int currentRowNum();

    /**
     * Move the cursor to the specified row.
     *
     * @param rowNum
     * @return this object itself.
     */
    DataSet absolute(int rowNum);

    /**
     *
     * @param rowNum
     * @return
     */
    Object[] getRow(int rowNum);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param rowNum
     * @return
     */
    <T> T getRow(Class<? extends T> rowClass, int rowNum);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param rowNum
     * @return
     */
    <T> T getRow(Class<? extends T> rowClass, Collection<String> columnNames, int rowNum);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param rowNum
     * @return
     */
    <T> T getRow(IntFunction<? extends T> rowSupplier, int rowNum);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param rowNum
     * @return
     */
    <T> T getRow(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int rowNum);

    /**
     *
     * @return {@code Optional<Object[]>}
     */
    Optional<Object[]> firstRow();

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return {@code Optional<E>}
     */
    <T> Optional<T> firstRow(Class<? extends T> rowClass);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @return {@code Optional<E>}
     */
    <T> Optional<T> firstRow(Class<? extends T> rowClass, Collection<String> columnNames);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return {@code Optional<T>}
     */
    <T> Optional<T> firstRow(IntFunction<? extends T> rowSupplier);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @return {@code Optional<T>}
     */
    <T> Optional<T> firstRow(IntFunction<? extends T> rowSupplier, Collection<String> columnNames);

    /**
     *
     * @return {@code Optional<Object[]>}
     */
    Optional<Object[]> lastRow();

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return {@code Optional<E>}
     */
    <T> Optional<T> lastRow(Class<? extends T> rowClass);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     *            which can be object array/list/set/map/entity.
     * @param columnNames
     * @return {@code Optional<E>}
     */
    <T> Optional<T> lastRow(Class<? extends T> rowClass, Collection<String> columnNames);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return {@code Optional<T>}
     */
    <T> Optional<T> lastRow(IntFunction<? extends T> rowSupplier);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @return {@code Optional<T>}
     */
    <T> Optional<T> lastRow(IntFunction<? extends T> rowSupplier, Collection<String> columnNames);

    //    /**
    //     * It's for faster iteration without creating new row object for each row.
    //     *
    //     * @param output
    //     *            which can be an instance of object array/list/set/map/entity.
    //     * @param rowNum
    //     */
    //    void row(Object output, int rowNum);
    //
    //    /**
    //     * It's for faster iteration without creating new row object for each row.
    //     *
    //     * @param output
    //     *            which can be an instance of object array/list/set/map/entity.
    //     * @param columnIndexes
    //     * @param rowNum
    //     */
    //    void row(Object output, int[] columnIndexes, int rowNum);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * 
     * @param action
     */
    void forEach(Consumer<? super Object[]> action);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * @param action
     * @param shareRowArray the same object array will be reset for each row during the iteration if it's <code>true</code>. 
     * It can be set to <code>true</code> to improve the performance if the <code>action</code> only read each row object array once, don't modify it or save it in collection.
     * The default value is false.
     */
    void forEach(Consumer<? super Object[]> action, boolean shareRowArray);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * 
     * @param columnNames
     * @param action
     */
    void forEach(Collection<String> columnNames, Consumer<? super Object[]> action);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * 
     * @param columnNames
     * @param action
     * @param shareRowArray the same object array will be reset for each row during the iteration if it's <code>true</code>. 
     * It can be set to <code>true</code> to improve the performance if the <code>action</code> only read each row object array once, don't modify it or save it in collection.
     * The default value is false.
     */
    void forEach(Collection<String> columnNames, Consumer<? super Object[]> action, boolean shareRowArray);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @param action
     */
    void forEach(int fromRowIndex, int toRowIndex, Consumer<? super Object[]> action);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @param action
     * @param shareRowArray the same object array will be reset for each row during the iteration if it's <code>true</code>. 
     * It can be set to <code>true</code> to improve the performance if the <code>action</code> only read each row object array once, don't modify it or save it in collection.
     * The default value is false.
     */
    void forEach(int fromRowIndex, int toRowIndex, Consumer<? super Object[]> action, boolean shareRowArray);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param action
     */
    void forEach(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Consumer<? super Object[]> action);

    /**
     * Performs the given action for each row of the {@code DataSet}
     * until all rows have been processed or the action throws an
     * exception.
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param action
     * @param shareRowArray the same object array will be reset for each row during the iteration if it's <code>true</code>. 
     * It can be set to <code>true</code> to improve the performance if the <code>action</code> only read each row object array once, don't modify it or save it in collection.
     * The default value is false.
     */
    void forEach(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Consumer<? super Object[]> action, boolean shareRowArray);

    <R> R forEach(R seed, BiFunction<R, ? super Object[], R> accumulator, BiPredicate<? super Object[], ? super R> conditionToBreak);

    <R> R forEach(R seed, BiFunction<R, ? super Object[], R> accumulator, BiPredicate<? super Object[], ? super R> conditionToBreak, boolean shareRowArray);

    <R> R forEach(Collection<String> columnNames, R seed, BiFunction<R, ? super Object[], R> accumulator,
            BiPredicate<? super Object[], ? super R> conditionToBreak);

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param columnNames
     * @param seed
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @param shareRowArray
     * @return
     */
    <R> R forEach(Collection<String> columnNames, R seed, BiFunction<R, ? super Object[], R> accumulator,
            BiPredicate<? super Object[], ? super R> conditionToBreak, boolean shareRowArray);

    <R> R forEach(int fromRowIndex, int toRowIndex, R seed, BiFunction<R, ? super Object[], R> accumulator,
            BiPredicate<? super Object[], ? super R> conditionToBreak);

    <R> R forEach(int fromRowIndex, int toRowIndex, R seed, BiFunction<R, ? super Object[], R> accumulator,
            BiPredicate<? super Object[], ? super R> conditionToBreak, boolean shareRowArray);

    <R> R forEach(Collection<String> columnNames, int fromRowIndex, int toRowIndex, R seed, BiFunction<R, ? super Object[], R> accumulator,
            BiPredicate<? super Object[], ? super R> conditionToBreak);

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param seed
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @param shareRowArray
     * @return
     */
    <R> R forEach(Collection<String> columnNames, int fromRowIndex, int toRowIndex, R seed, BiFunction<R, ? super Object[], R> accumulator,
            BiPredicate<? super Object[], ? super R> conditionToBreak, boolean shareRowArray);

    /**
     *
     * @return
     */
    Object[][] toArray();

    /**
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    Object[][] toArray(int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    <T> T[] toArray(Class<? extends T> rowClass);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity the class for the row value.
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> T[] toArray(Class<? extends T> rowClass, int fromRowIndex, int toRowIndex);

    /**
    *
    * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity the class for the row value.
    * @param columnNames
    * @param fromRowIndex
    * @param toRowIndex
    * @return
    */
    <T> T[] toArray(Class<? extends T> rowClass, Collection<String> columnNames);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity the class for the row value.
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> T[] toArray(Class<? extends T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity the class for the row value.
     * @return
     */
    <T> T[] toArray(IntFunction<? extends T> rowSupplier);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity the class for the row value.
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> T[] toArray(IntFunction<? extends T> rowSupplier, int fromRowIndex, int toRowIndex);

    /**
    *
    * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity the class for the row value.
    * @param columnNames
    * @param fromRowIndex
    * @param toRowIndex
    * @return
    */
    <T> T[] toArray(IntFunction<? extends T> rowSupplier, Collection<String> columnNames);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity the class for the row value.
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> T[] toArray(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @return
     */
    List<Object[]> toList();

    /**
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    List<Object[]> toList(int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    <T> List<T> toList(Class<? extends T> rowClass);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> List<T> toList(Class<? extends T> rowClass, int fromRowIndex, int toRowIndex);

    /**
    *
    * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    * @param fromRowIndex
    * @param toRowIndex
    * @return
    */
    <T> List<T> toList(Class<? extends T> rowClass, Collection<String> columnNames);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> List<T> toList(Class<? extends T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    <T> List<T> toList(IntFunction<? extends T> rowSupplier);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> List<T> toList(IntFunction<? extends T> rowSupplier, int fromRowIndex, int toRowIndex);

    /**
    *
    * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
    * @param fromRowIndex
    * @param toRowIndex
    * @return
    */
    <T> List<T> toList(IntFunction<? extends T> rowSupplier, Collection<String> columnNames);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> List<T> toList(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param keyColumnName
     * @param valueColumnName
     * @return
     */
    <K, V> Map<K, V> toMap(String keyColumnName, String valueColumnName);

    /**
     *
     * @param keyColumnName
     * @param valueColumnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <K, V> Map<K, V> toMap(String keyColumnName, String valueColumnName, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param keyColumnName
     * @param valueColumnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier
     * @return
     */
    <K, V, M extends Map<K, V>> M toMap(String keyColumnName, String valueColumnName, int fromRowIndex, int toRowIndex, IntFunction<M> supplier);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @return
     */
    <K, V> Map<K, V> toMap(Class<? extends V> rowClass, String keyColumnName, Collection<String> valueColumnNames);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <K, V> Map<K, V> toMap(Class<? extends V> rowClass, String keyColumnName, Collection<String> valueColumnNames, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier
     * @return
     */
    <K, V, M extends Map<K, V>> M toMap(Class<? extends V> rowClass, String keyColumnName, Collection<String> valueColumnNames, int fromRowIndex,
            int toRowIndex, IntFunction<M> supplier);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @return
     */
    <K, V> Map<K, V> toMap(IntFunction<? extends V> rowSupplier, String keyColumnName, Collection<String> valueColumnNames);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <K, V> Map<K, V> toMap(IntFunction<? extends V> rowSupplier, String keyColumnName, Collection<String> valueColumnNames, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier
     * @return
     */
    <K, V, M extends Map<K, V>> M toMap(IntFunction<? extends V> rowSupplier, String keyColumnName, Collection<String> valueColumnNames, int fromRowIndex,
            int toRowIndex, IntFunction<M> supplier);

    /**
     *
     * @param keyColumnName
     * @param valueColumnName
     * @return
     */
    <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(String keyColumnName, String valueColumnName);

    /**
     *
     * @param keyColumnName
     * @param valueColumnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(String keyColumnName, String valueColumnName, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param keyColumnName
     * @param valueColumnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier
     * @return
     */
    <K, E, V extends Collection<E>> Multimap<K, E, V> toMultimap(String keyColumnName, String valueColumnName, int fromRowIndex, int toRowIndex,
            IntFunction<Multimap<K, E, V>> supplier);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @return
     */
    <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(Class<? extends E> rowClass, String keyColumnName, Collection<String> valueColumnNames);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(Class<? extends E> rowClass, String keyColumnName, Collection<String> valueColumnNames,
            int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier
     * @return
     */
    <K, E, V extends Collection<E>> Multimap<K, E, V> toMultimap(Class<? extends E> rowClass, String keyColumnName, Collection<String> valueColumnNames,
            int fromRowIndex, int toRowIndex, IntFunction<Multimap<K, E, V>> supplier);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @return
     */
    <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(IntFunction<? extends E> rowSupplier, String keyColumnName, Collection<String> valueColumnNames);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(IntFunction<? extends E> rowSupplier, String keyColumnName, Collection<String> valueColumnNames,
            int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param keyColumnName
     * @param valueColumnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier
     * @return
     */
    <K, E, V extends Collection<E>> Multimap<K, E, V> toMultimap(IntFunction<? extends E> rowSupplier, String keyColumnName,
            Collection<String> valueColumnNames, int fromRowIndex, int toRowIndex, IntFunction<Multimap<K, E, V>> supplier);

    /**
     *
     * @param columnName
     * @return
     */
    <T> Multiset<T> toMultiset(String columnName);

    /**
     *
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Multiset<T> toMultiset(String columnName, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier create <code>Multiset</code> by new <code>Multiset(ArrayHashMap.class)</code> or <code>Multiset(LinkedArrayHashMap.class)</code> if the element is array.
     * @return
     */
    <T> Multiset<T> toMultiset(String columnName, int fromRowIndex, int toRowIndex, IntFunction<Multiset<T>> supplier);

    /**
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @return
     */
    <T> Multiset<T> toMultiset(Class<? extends T> rowClass, Collection<String> columnNames);

    /**
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Multiset<T> toMultiset(Class<? extends T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier create <code>Multiset</code> by new <code>Multiset(ArrayHashMap.class)</code> or <code>Multiset(LinkedArrayHashMap.class)</code> if <code>rowClass</code> is array.
     * @return
     */
    <T> Multiset<T> toMultiset(Class<? extends T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex,
            IntFunction<Multiset<T>> supplier);

    /**
     * 
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @return
     */
    <T> Multiset<T> toMultiset(IntFunction<? extends T> rowSupplier, Collection<String> columnNames);

    /**
     * 
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Multiset<T> toMultiset(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param supplier create <code>Multiset</code> by new <code>Multiset(ArrayHashMap.class)</code> or <code>Multiset(LinkedArrayHashMap.class)</code> if <code>rowClass</code> is array.
     * @return
     */
    <T> Multiset<T> toMultiset(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int fromRowIndex, int toRowIndex,
            IntFunction<Multiset<T>> supplier);

    /**
     *
     * @return
     */
    String toJSON();

    /**
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    String toJSON(int fromRowIndex, int toRowIndex);

    /**
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    String toJSON(Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
    *
    * @param out
    */
    void toJSON(File out);

    /**
    *
    * @param out
    * @param fromRowIndex
    * @param toRowIndex
    */
    void toJSON(File out, int fromRowIndex, int toRowIndex);

    /**
    *
    * @param out
    * @param columnNames
    * @param fromRowIndex
    * @param toRowIndex
    */
    void toJSON(File out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param os
     */
    void toJSON(OutputStream out);

    /**
     *
     * @param os
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toJSON(OutputStream out, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param os
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toJSON(OutputStream out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param os
     */
    void toJSON(Writer out);

    /**
     *
     * @param os
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toJSON(Writer out, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param os
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toJSON(Writer out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @return
     */
    String toXML();

    /**
     * @param rowElementName
     * @return
     */
    String toXML(String rowElementName);

    /**
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    String toXML(int fromRowIndex, int toRowIndex);

    /**
     * @param rowElementName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    String toXML(String rowElementName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    String toXML(Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * @param rowElementName
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    String toXML(String rowElementName, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     */
    void toXML(File out);

    /**
     * @param out
     * @param rowElementName
     */
    void toXML(File out, String rowElementName);

    /**
     *
     * @param out
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(File out, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     * @param rowElementName
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(File out, String rowElementName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(File out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     * @param rowElementName
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(File out, String rowElementName, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     */
    void toXML(OutputStream out);

    /**
     * @param out
     * @param rowElementName
     */
    void toXML(OutputStream out, String rowElementName);

    /**
     *
     * @param out
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(OutputStream out, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     * @param rowElementName
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(OutputStream out, String rowElementName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(OutputStream out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     * @param rowElementName
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(OutputStream out, String rowElementName, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     */
    void toXML(Writer out);

    /**
     * @param out
     * @param rowElementName
     */
    void toXML(Writer out, String rowElementName);

    /**
     *
     * @param out
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(Writer out, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     * @param rowElementName
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(Writer out, String rowElementName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(Writer out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * @param out
     * @param rowElementName
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toXML(Writer out, String rowElementName, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     */
    String toCSV();

    /**
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    String toCSV(Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param writeTitle
     * @param quoteValue
     * @return
     */
    String toCSV(boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param writeTitle
     * @param quoteValue
     * @return
     */
    String toCSV(Collection<String> columnNames, int fromRowIndex, int toRowIndex, boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param out
     */
    void toCSV(File out);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toCSV(File out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param out
     * @param writeTitle
     * @param quoteValue
     */
    void toCSV(File out, boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param writeTitle
     * @param quoteValue
     */
    void toCSV(File out, Collection<String> columnNames, int fromRowIndex, int toRowIndex, boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param out
     */
    void toCSV(OutputStream out);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toCSV(OutputStream out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param out
     * @param writeTitle
     * @param quoteValue
     */
    void toCSV(OutputStream out, boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param writeTitle
     * @param quoteValue
     */
    void toCSV(OutputStream out, Collection<String> columnNames, int fromRowIndex, int toRowIndex, boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param out
     */
    void toCSV(Writer out);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    void toCSV(Writer out, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param out
     * @param writeTitle
     * @param quoteValue
     */
    void toCSV(Writer out, boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param out
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param writeTitle
     * @param quoteValue
     */
    void toCSV(Writer out, Collection<String> columnNames, int fromRowIndex, int toRowIndex, boolean writeTitle, boolean quoteValue);

    /**
     *
     * @param columnName
     */
    void sortBy(String columnName);

    /**
     *
     * @param columnName
     * @param cmp
     */
    void sortBy(String columnName, Comparator<?> cmp);

    /**
     *
     * @param columnNames
     */
    void sortBy(Collection<String> columnNames);

    /**
     *
     * @param columnNames
     * @param cmp
     */
    void sortBy(Collection<String> columnNames, Comparator<? super Object[]> cmp);

    /**
     *
     * @param columnName
     */
    void parallelSortBy(String columnName);

    /**
     *
     * @param columnName
     * @param cmp
     */
    void parallelSortBy(String columnName, Comparator<?> cmp);

    /**
     *
     * @param columnNames
     */
    void parallelSortBy(Collection<String> columnNames);

    /**
     *
     * @param columnNames
     * @param cmp
     */
    void parallelSortBy(Collection<String> columnNames, Comparator<? super Object[]> cmp);

    /**
     *
     * @param columnName specifying the column to group by.
     * @return
     */
    DataSet groupBy(String columnName);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    DataSet groupBy(String columnName, Function<?, ?> keyMapper);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, String aggregateResultColumnName, String aggregateOnColumnName, Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, String aggregateResultColumnName, Collection<String> aggregateOnColumnNames,
            Collector<? super Object[], ?, ?> collector);

    /**
     * 
     * @param columnName specifying the column to group by. 
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, Function<?, ?> keyMapper, String aggregateResultColumnName, String aggregateOnColumnName,
            Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the columns to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, Function<?, ?> keyMapper, String aggregateResultColumnName, Collection<String> aggregateOnColumnNames,
            Collector<? super Object[], ?, ?> collector);

    /**
     *
     * @param columnName specifying the column to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    DataSet groupBy(String columnName, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    DataSet groupBy(String columnName, int fromRowIndex, int toRowIndex, Function<?, ?> keyMapper);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, int fromRowIndex, int toRowIndex, String aggregateResultColumnName, String aggregateOnColumnName,
            Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the columns to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, int fromRowIndex, int toRowIndex, String aggregateResultColumnName, Collection<String> aggregateOnColumnNames,
            Collector<? super Object[], ?, ?> collector);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, int fromRowIndex, int toRowIndex, Function<?, ?> keyMapper, String aggregateResultColumnName,
            String aggregateOnColumnName, Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnName specifying the column to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the columns to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(String columnName, int fromRowIndex, int toRowIndex, Function<?, ?> keyMapper, String aggregateResultColumnName,
            Collection<String> aggregateOnColumnNames, Collector<? super Object[], ?, ?> collector);

    /**
     *
     * @param columnNames specifying the columns to group by.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames);

    /**
     * 
     * @param columnNames specifying the columns to group by. 
     * @param keyMapper don't change value of the input parameter. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, Function<? super Object[], ?> keyMapper);

    /**
     * 
     * @param columnNames specifying the columns to group by.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, String aggregateResultColumnName, String aggregateOnColumnName, Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnNames specifying the columns to group by.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, String aggregateResultColumnName, Collection<String> aggregateOnColumnNames,
            Collector<? super Object[], ?, ?> collector);

    /**
     * 
     * @param columnNames specifying the columns to group by. 
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, Function<? super Object[], ?> keyMapper, String aggregateResultColumnName, String aggregateOnColumnName,
            Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnNames specifying the columns to group by. 
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the columns to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, Function<? super Object[], ?> keyMapper, String aggregateResultColumnName,
            Collection<String> aggregateOnColumnNames, Collector<? super Object[], ?, ?> collector);

    /**
     *
     * @param columnNames specifying the columns to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param columnNames specifying the columns to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Function<? super Object[], ?> keyMapper);

    /**
     * 
     * @param columnNames specifying the columns to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, int fromRowIndex, int toRowIndex, String aggregateResultColumnName, String aggregateOnColumnName,
            Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnNames specifying the columns to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the columns to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, int fromRowIndex, int toRowIndex, String aggregateResultColumnName,
            Collection<String> aggregateOnColumnNames, Collector<? super Object[], ?, ?> collector);

    /**
     * 
     * @param columnNames specifying the columns to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnName specifying the column to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Function<? super Object[], ?> keyMapper, String aggregateResultColumnName,
            String aggregateOnColumnName, Collector<Object, ?, ?> collector);

    /**
     * 
     * @param columnNames specifying the columns to group by.
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @param aggregateResultColumnName
     * @param aggregateOnColumnNames specifying the columns to apply the collector.
     * @param collector refer to {@link com.landawn.abacus.util.stream.Collectors#groupingBy(Function, Collector)}. 
     * For example, set collector to {@link com.landawn.abacus.util.stream.Collectors#counting()} to count the row number.
     * @return
     */
    DataSet groupBy(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Function<? super Object[], ?> keyMapper, String aggregateResultColumnName,
            Collection<String> aggregateOnColumnNames, Collector<? super Object[], ?, ?> collector);

    /**
     *
     * @param columnName
     * @param n
     * @return
     */
    DataSet top(String columnName, int n);

    /**
     *
     * @param columnName
     * @param n
     * @param cmp
     * @return
     */
    DataSet top(String columnName, int n, Comparator<?> cmp);

    /**
     *
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param n
     * @param cmp
     * @return
     */
    DataSet top(String columnName, int fromRowIndex, int toRowIndex, int n, Comparator<?> cmp);

    /**
    *
    * @param columnNames
    * @param n
    * @return
    */
    DataSet top(Collection<String> columnNames, int n);

    /**
     *
     * @param columnNames
     * @param n
     * @param cmp
     * @return
     */
    DataSet top(Collection<String> columnNames, int n, Comparator<? super Object[]> cmp);

    /**
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param n
     * @param cmp
     * @return
     */
    DataSet top(Collection<String> columnNames, int fromRowIndex, int toRowIndex, int n, Comparator<? super Object[]> cmp);

    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param n
    //     * @return
    //     */
    //    <T> List<T> top(Class<T> rowClass, String columnName, int n);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param n
    //     * @param cmp
    //     * @return
    //     */
    //    <T> List<T> top(Class<T> rowClass, String columnName, int n, Comparator<?> cmp);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param n
    //     * @param cmp
    //     * @return
    //     */
    //    <T> List<T> top(Class<T> rowClass, String columnName, int fromRowIndex, int toRowIndex, int n, Comparator<?> cmp);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnNames
    //     * @param n
    //     * @return
    //     */
    //    <T> List<T> top(Class<T> rowClass, Collection<String> columnNames, int n);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnNames
    //     * @param n
    //     * @param cmp
    //     * @return
    //     */
    //    <T> List<T> top(Class<T> rowClass, Collection<String> columnNames, int n, Comparator<? super Object[]> cmp);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnNames
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param n
    //     * @param cmp
    //     * @return
    //     */
    //    <T> List<T> top(Class<T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex, int n, Comparator<? super Object[]> cmp);

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the values in all columns
     *
     * @return a new DataSet
     */
    DataSet distinct();

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the value in the specified column
     *
     * @param columnName
     * @return a new DataSet
     */
    DataSet distinct(String columnName);

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the value in the specified column from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>
     * 
     * @param columnName
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    DataSet distinct(String columnName, Function<?, ?> keyMapper);

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the value in the specified column from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>
     *
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return a new DataSet
     */
    DataSet distinct(String columnName, int fromRowIndex, int toRowIndex);

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the value in the specified column from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    DataSet distinct(String columnName, int fromRowIndex, int toRowIndex, Function<?, ?> keyMapper);

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the values in the specified columns
     *
     * @param columnNames
     * @return a new DataSet
     */
    DataSet distinct(Collection<String> columnNames);

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the values in the specified columns from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    DataSet distinct(Collection<String> columnNames, Function<? super Object[], ?> keyMapper);

    /**
     *Returns a new <code>DataSet</code> with the rows de-duplicated by the values in the specified columns from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    DataSet distinct(Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * Returns a new <code>DataSet</code> with the rows de-duplicated by the values in the specified columns from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    DataSet distinct(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Function<? super Object[], ?> keyMapper);

    /**
     *
     * @param filter
     * @return
     */
    DataSet filter(Predicate<? super Object[]> filter);

    /**
     * 
     * @param filter
     * @param max
     * @return
     */
    DataSet filter(Predicate<? super Object[]> filter, int max);

    /**
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @return
     */
    DataSet filter(int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter);

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @param max
     * @return
     */
    DataSet filter(int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter, int max);

    /**
     *
     * @param columnName
     * @param filter
     * @return
     */
    DataSet filter(String columnName, Predicate<?> filter);

    /**
     * 
     * @param columnName
     * @param filter
     * @param max
     * @return
     */
    DataSet filter(String columnName, Predicate<?> filter, int max);

    /**
     *
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @return
     */
    DataSet filter(String columnName, int fromRowIndex, int toRowIndex, Predicate<?> filter);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @param max
     * @return
     */
    DataSet filter(String columnName, int fromRowIndex, int toRowIndex, Predicate<?> filter, int max);

    //    /**
    //     *
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @param offset
    //     * @param count
    //     * @return
    //     */
    //    <C> DataSet filter(String columnName, int fromRowIndex, int toRowIndex, Predicate<C> filter, int offset, int count);

    /**
     *
     * @param columnNames
     * @param filter
     * @return
     */
    DataSet filter(Collection<String> columnNames, Predicate<? super Object[]> filter);

    /**
     * 
     * @param columnNames
     * @param filter
     * @param max
     * @return
     */
    DataSet filter(Collection<String> columnNames, Predicate<? super Object[]> filter, int max);

    /**
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @return
     */
    DataSet filter(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter);

    /**
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @param max
     * @return
     */
    DataSet filter(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter, int max);

    //    /**
    //     * Filter the result by the specified columns {@code columnNames} with the specified {@code filter}.
    //     * @param columnNames
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @param offset
    //     * @param count
    //     * @return
    //     */
    //    DataSet filter(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter, int offset, int count);

    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param filter
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, Predicate<? super Object[]> filter);
    //
    //    /**
    //     * 
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param filter
    //     * @param max
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, Predicate<? super Object[]> filter, int max);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter);
    //
    //    /**
    //     * 
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @param max
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter, int max);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param filter
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, String columnName, Predicate<?> filter);
    //
    //    /**
    //     * 
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param filter
    //     * @param max
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, String columnName, Predicate<?> filter, int max);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, String columnName, int fromRowIndex, int toRowIndex, Predicate<?> filter);
    //
    //    /**
    //     * 
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @param max
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, String columnName, int fromRowIndex, int toRowIndex, Predicate<?> filter, int max);

    //    /**
    //     * 
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @param offset
    //     * @param count
    //     * @return
    //     */
    //    <T, C> List<T> filter(Class<T> rowClass, String columnName, int fromRowIndex, int toRowIndex, Predicate<C> filter, int offset, int count);

    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnNames
    //     * @param filter
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, Collection<String> columnNames, Predicate<? super Object[]> filter);
    //
    //    /**
    //     * 
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnNames
    //     * @param filter
    //     * @param max
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, Collection<String> columnNames, Predicate<? super Object[]> filter, int max);
    //
    //    /**
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnNames
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter);
    //
    //    /**
    //     * 
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     * @param columnNames
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @param max
    //     * @return
    //     */
    //    <T> List<T> filter(Class<T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter, int max);

    //    /**
    //     * Filter the result by the specified columns {@code columnNames} with the specified {@code filter}.
    //     *
    //     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
    //     *            which can be list/set/map/entity with getter/setter methods
    //     * @param columnNames
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param filter
    //     * @param offset
    //     * @param count
    //     * @return the a list of row
    //     */
    //    <T> List<T> filter(Class<T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter, int offset, int count);

    /**
     *
     * @param filter
     * @return
     */
    int count(Predicate<? super Object[]> filter);

    /**
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @return
     */
    int count(int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter);

    /**
    *
    * @param columnName
    * @param filter
    * @return
    */
    int count(String columnName, Predicate<?> filter);

    /**
    * count the result by the specified columns {@code columnName} with the specified {@code filter}.
    *
    * @param columnName
    * @param fromRowIndex
    * @param toRowIndex
    * @param filter
    * @return
    */
    int count(String columnName, int fromRowIndex, int toRowIndex, Predicate<?> filter);

    /**
     *
     * @param columnNames
     * @param filter
     * @return
     */
    int count(Collection<String> columnNames, Predicate<? super Object[]> filter);

    /**
     * count the result by the specified columns {@code columnNames} with the specified {@code filter}.
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param filter
     * @return
     */
    int count(Collection<String> columnNames, int fromRowIndex, int toRowIndex, Predicate<? super Object[]> filter);

    /**
     * @param columnName
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> min(String columnName);

    /**
     *
     * @param columnName
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> min(String columnName, Comparator<? super T> comparator);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> min(String columnName, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> min(String columnName, int fromRowIndex, int toRowIndex, Comparator<? super T> comparator);

    /**
     * @param columnName
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> max(String columnName);

    /**
     *
     * @param columnName
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> max(String columnName, Comparator<? super T> comparator);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> max(String columnName, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> max(String columnName, int fromRowIndex, int toRowIndex, Comparator<? super T> comparator);

    /**
     * @param columnName
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> median(String columnName);

    /**
     *
     * @param columnName
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> median(String columnName, Comparator<? super T> comparator);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> median(String columnName, int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> median(String columnName, int fromRowIndex, int toRowIndex, Comparator<? super T> comparator);

    /**
     * @param columnName
     * @param k
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> kthLargest(String columnName, int k);

    /**
     *
     * @param columnName
     * @param k
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> kthLargest(String columnName, int k, Comparator<? super T> comparator);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param k
     * @return
     */
    <T extends Comparable<? super T>> OptionalNullable<T> kthLargest(String columnName, int fromRowIndex, int toRowIndex, int k);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param k
     * @param comparator
     * @return
     */
    <T> OptionalNullable<T> kthLargest(String columnName, int fromRowIndex, int toRowIndex, int k, Comparator<? super T> comparator);

    //    /**
    //     *
    //     * @param columnName
    //     * @param mapper
    //     * @return
    //     */
    //    Long sum(String columnName, ToLongFunction<?> mapper);
    //
    //    /**
    //     * 
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param mapper
    //     * @return
    //     */
    //    Long sum(String columnName, int fromRowIndex, int toRowIndex, ToLongFunction<?> mapper);

    /**
     * @param columnName
     * @return
     */
    Long sumInt(String columnName);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    Long sumInt(String columnName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param columnName
     * @param mapper
     * @return
     */
    Long sumInt(String columnName, ToIntFunction<?> mapper);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param mapper
     * @return
     */
    Long sumInt(String columnName, int fromRowIndex, int toRowIndex, ToIntFunction<?> mapper);

    /**
     * @param columnName
     * @return
     */
    Long sumLong(String columnName);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    Long sumLong(String columnName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param columnName
     * @param mapper
     * @return
     */
    Long sumLong(String columnName, ToLongFunction<?> mapper);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param mapper
     * @return
     */
    Long sumLong(String columnName, int fromRowIndex, int toRowIndex, ToLongFunction<?> mapper);

    /**
     * @param columnName
     * @return
     */
    Double sumDouble(String columnName);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    Double sumDouble(String columnName, int fromRowIndex, int toRowIndex);

    //    /**
    //     *
    //     * @param columnName
    //     * @param mapper
    //     * @return
    //     */
    //    Long sum(String columnName, ToLongFunction<?> mapper);
    //
    //    /**
    //     * 
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param mapper
    //     * @return
    //     */
    //    Long sum(String columnName, int fromRowIndex, int toRowIndex, ToLongFunction<?> mapper);

    /**
     *
     * @param columnName
     * @param mapper
     * @return
     */
    Double sumDouble(String columnName, ToDoubleFunction<?> mapper);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param mapper
     * @return
     */
    Double sumDouble(String columnName, int fromRowIndex, int toRowIndex, ToDoubleFunction<?> mapper);

    /**
     * @param columnName
     * @return
     */
    OptionalDouble averageInt(String columnName);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    OptionalDouble averageInt(String columnName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param columnName
     * @param mapper
     * @return
     */
    OptionalDouble averageInt(String columnName, ToIntFunction<?> mapper);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param mapper
     * @return
     */
    OptionalDouble averageInt(String columnName, int fromRowIndex, int toRowIndex, ToIntFunction<?> mapper);

    /**
     * @param columnName
     * @return
     */
    OptionalDouble averageLong(String columnName);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    OptionalDouble averageLong(String columnName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param columnName
     * @param mapper
     * @return
     */
    OptionalDouble averageLong(String columnName, ToLongFunction<?> mapper);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param mapper
     * @return
     */
    OptionalDouble averageLong(String columnName, int fromRowIndex, int toRowIndex, ToLongFunction<?> mapper);

    /**
     * @param columnName
     * @return
     */
    OptionalDouble averageDouble(String columnName);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    OptionalDouble averageDouble(String columnName, int fromRowIndex, int toRowIndex);

    //    /**
    //     *
    //     * @param columnName
    //     * @param mapper
    //     * @return
    //     */
    //    OptionalDouble average(String columnName, ToLongFunction<?> mapper);
    //
    //    /**
    //     * 
    //     * @param columnName
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @param mapper
    //     * @return
    //     */
    //    OptionalDouble average(String columnName, int fromRowIndex, int toRowIndex, ToLongFunction<?> mapper);

    /**
     *
     * @param columnName
     * @param mapper
     * @return
     */
    OptionalDouble averageDouble(String columnName, ToDoubleFunction<?> mapper);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @param mapper
     * @return
     */
    OptionalDouble averageDouble(String columnName, int fromRowIndex, int toRowIndex, ToDoubleFunction<?> mapper);

    /**
     * Returns the copy of this <code>DataSet</code>.
     * The frozen status of the copy will always be false, even the original <code>DataSet</code> is frozen.
     *
     * @return a copy of this DataSet
     */
    DataSet copy();

    /**
     * Returns the copy of this <code>DataSet</code> with specified column name list.
     * The frozen status of the copy will always be false, even the original <code>DataSet</code> is frozen.
     *
     * @param columnNames
     * @return a copy of this DataSet
     */
    DataSet copy(Collection<String> columnNames);

    /**
     * Returns the copy of this <code>DataSet</code> from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>.
     * The frozen status of the copy will always be false, even the original <code>DataSet</code> is frozen.
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @return a copy of this DataSet
     */
    DataSet copy(int fromRowIndex, int toRowIndex);

    /**
     * Returns the copy of this <code>DataSet</code> with specified column name list from the specified <code>fromRowIndex</code> to <code>toRowIndex</code>.
     * The frozen status of the copy will always be false, even the original <code>DataSet</code> is frozen.
     *
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return a copy of this DataSet
     */
    DataSet copy(Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * Deeply copy each element in this <code>DataSet</code> by Serialization/Deserialization.
     * 
     * @return
     */
    DataSet clone();

    /**
     * Deeply copy each element in this <code>DataSet</code> by Serialization/Deserialization.
     * 
     * @return
     */
    DataSet clone(boolean freeze);

    /**
     * Returns a new <code>DataSet</code> that is limited to the rows where there is a match in both <code>this DataSet</code> and <code>right DataSet</code>.
     *
     * @param right
     * @param columnName
     * @param refColumnName
     * @return a new DataSet
     */
    DataSet join(DataSet right, String columnName, String refColumnName);

    /**
     * Returns a new <code>DataSet</code> that is limited to the rows where there is a match in both <code>this DataSet</code> and <code>right DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @return a new DataSet
     */
    DataSet join(DataSet right, Map<String, String> onColumnNames);

    /**
     * Returns a new <code>DataSet</code> that is limited to the rows where there is a match in both <code>this DataSet</code> and <code>right DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return a new DataSet
     */
    DataSet join(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass);

    /**
     * Returns a new <code>DataSet</code> that is limited to the rows where there is a match in both <code>this DataSet</code> and <code>right DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param collClass it's for one-to-many join
     * @return a new DataSet
     */
    @SuppressWarnings("rawtypes")
    DataSet join(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass, Class<? extends Collection> collClass);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the rows from the specified <code>right DataSet</code> if they have a match with the rows from the this <code>DataSet</code>.
     *
     * @param right
     * @param columnName
     * @param refColumnName
     * @return a new DataSet
     */
    DataSet leftJoin(DataSet right, String columnName, String refColumnName);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the rows from the specified <code>right DataSet</code> if they have a match with the rows from the this <code>DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @return a new DataSet
     */
    DataSet leftJoin(DataSet right, Map<String, String> onColumnNames);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the rows from the specified <code>right DataSet</code> if they have a match with the rows from the this <code>DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return a new DataSet
     */
    DataSet leftJoin(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the rows from the specified <code>right DataSet</code> if they have a match with the rows from the this <code>DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param collClass it's for one-to-many join
     * @return a new DataSet
     */
    @SuppressWarnings("rawtypes")
    DataSet leftJoin(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass, Class<? extends Collection> collClass);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from the specified right <code>DataSet</code> and the rows from <code>this DataSet</code> if they have a match with the rows from the right <code>DataSet</code>.
     *
     * @param right
     * @param columnName
     * @param refColumnName
     * @return a new DataSet
     */
    DataSet rightJoin(DataSet right, String columnName, String refColumnName);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from the specified right <code>DataSet</code> and the rows from <code>this DataSet</code> if they have a match with the rows from the right <code>DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @return a new DataSet
     */
    DataSet rightJoin(DataSet right, Map<String, String> onColumnNames);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from the specified right <code>DataSet</code> and the rows from <code>this DataSet</code> if they have a match with the rows from the right <code>DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return a new DataSet
     */
    DataSet rightJoin(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from the specified right <code>DataSet</code> and the rows from <code>this DataSet</code> if they have a match with the rows from the right <code>DataSet</code>.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param collClass it's for one-to-many join
     * @return a new DataSet
     */
    @SuppressWarnings("rawtypes")
    DataSet rightJoin(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass, Class<? extends Collection> collClass);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the specified <code>right DataSet</code>, regardless of whether there are any matches.
     *
     * @param right
     * @param columnName
     * @param refColumnName
     * @return a new DataSet
     */
    DataSet fullJoin(DataSet right, String columnName, String refColumnName);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the specified <code>right DataSet</code>, regardless of whether there are any matches.
     *
     * @param right
     * @param onColumnNames
     * @return a new DataSet
     */
    DataSet fullJoin(DataSet right, Map<String, String> onColumnNames);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the specified <code>right DataSet</code>, regardless of whether there are any matches.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return a new DataSet
     */
    DataSet fullJoin(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass);

    /**
     * Returns a new <code>DataSet</code> that has all the rows from this <code>DataSet</code> and the specified <code>right DataSet</code>, regardless of whether there are any matches.
     *
     * @param right
     * @param onColumnNames
     * @param newColumnName
     * @param newColumnClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param collClass it's for one-to-many join
     * @return a new DataSet
     */
    @SuppressWarnings("rawtypes")
    DataSet fullJoin(DataSet right, Map<String, String> onColumnNames, String newColumnName, Class<?> newColumnClass, Class<? extends Collection> collClass);

    /**
     * Returns a new <code>DataSet</code>. Duplicated row in the specified {@code DataSet} will be eliminated.
     *
     * @param dataSet
     * @return a new DataSet
     */
    DataSet union(DataSet dataSet);

    /**
     * Returns a new <code>DataSet</code>. Duplicated row in the specified {@code DataSet} will not be eliminated.
     *
     * @param dataSet
     * @return a new DataSet
     */
    DataSet unionAll(DataSet dataSet);

    /**
     * Returns a new <code>DataSet</code>.
     *
     * @param dataSet
     * @return a new DataSet
     * @see com.landawn.abacus.util.IntList#intersection(com.landawn.abacus.util.IntList)
     */
    DataSet intersection(DataSet dataSet);

    /**
     * Returns a new <code>DataSet</code>.
     *
     * @param dataSet
     * @return a new DataSet
     * @see com.landawn.abacus.util.IntList#difference(com.landawn.abacus.util.IntList)
     */
    DataSet difference(DataSet dataSet);

    /**
     * 
     * @param dataSet
     * @return
     * @see com.landawn.abacus.util.IntList#symmetricDifference(com.landawn.abacus.util.IntList)
     */
    DataSet symmetricDifference(DataSet dataSet);

    /**
     * 
     * @param dataSet
     * @return
     * @see java.util.Collection#retainAll(Collection)
     */
    DataSet retainAll(DataSet dataSet);

    /**
     * 
     * @param dataSet
     * @return
     * @see java.util.Collection#removeAll(Collection)
     */
    DataSet removeAll(DataSet dataSet);

    /**
     * Returns a new <code>DataSet</code> by appending the specified <code>from</code> <code>DataSet</code> into this <code>DataSet</code>.
     *
     * @param from
     */
    DataSet merge(DataSet from);

    /**
     * Returns a new <code>DataSet</code> by appending the specified <code>from</code> <code>DataSet</code> into this <code>DataSet</code>.
     *
     * @param from
     * @param columnNames
     */
    DataSet merge(DataSet from, Collection<String> columnNames);

    /**
     * Returns a new <code>DataSet</code> by appending the specified <code>from</code> <code>DataSet</code> from <code>fromRowIndex</code> to <code>toRowIndex</code> into this <code>DataSet</code>.
     *
     * @param from
     * @param fromRowIndex
     * @param toRowIndex
     */
    DataSet merge(DataSet from, int fromRowIndex, int toRowIndex);

    /**
     * Returns a new <code>DataSet</code> by appending the specified <code>columnNames</code> in <code>from</code> <code>DataSet</code> from <code>fromRowIndex</code> to <code>toRowIndex</code> into this <code>DataSet</code>.
     *
     * @param from
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     */
    DataSet merge(DataSet from, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param size
     * @return
     */
    List<DataSet> split(int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param size
     * @param columnNames
     * @return
     */
    List<DataSet> split(Collection<String> columnNames, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @param size
     * @return
     */
    List<DataSet> split(int fromRowIndex, int toRowIndex, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param size
     * @return
     */
    List<DataSet> split(Collection<String> columnNames, int fromRowIndex, int toRowIndex, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param size
     * @return
     */
    <T> List<List<T>> split(Class<? extends T> rowClass, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param size
     * @param columnNames
     * @return
     */
    <T> List<List<T>> split(Class<? extends T> rowClass, Collection<String> columnNames, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @param size
     * @return
     */
    <T> List<List<T>> split(Class<? extends T> rowClass, int fromRowIndex, int toRowIndex, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param size
     * @return
     */
    <T> List<List<T>> split(Class<? extends T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param size
     * @return
     */
    <T> List<List<T>> split(IntFunction<? extends T> rowSupplier, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param size
     * @param columnNames
     * @return
     */
    <T> List<List<T>> split(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @param size
     * @return
     */
    <T> List<List<T>> split(IntFunction<? extends T> rowSupplier, int fromRowIndex, int toRowIndex, int size);

    /**
     * Returns consecutive sub lists of this DataSet, each of the same size (the list may be smaller), or an empty List if this DataSet is empty.
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @param size
     * @return
     */
    <T> List<List<T>> split(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int fromRowIndex, int toRowIndex, int size);

    /**
     * Method paginate.
     *
     * @param pageSize
     * @return
     */
    PaginatedDataSet paginate(int pageSize);

    //    /**
    //     * Returns the elements at: <code>0.01%, 0.1%, 1%, 10%, 20%, 30%, 50%, 70%, 80%, 90%, 99%, 99.9%, 99.99%</code> * length of the specified column after it's sorted.
    //     * The the value of column must implements the <code>Comparable</code> interface.
    //     * 
    //     * @param columnName
    //     * @return
    //     */
    //    <T extends Comparable<T>> Map<String, T> distribution(String columnName);
    //
    //    /**
    //     * Returns the elements at: <code>0.01%, 0.1%, 1%, 10%, 20%, 30%, 50%, 70%, 80%, 90%, 99%, 99.9%, 99.99%</code> * length of the specified column after it's sorted.
    //     * 
    //     * @param columnName
    //     * @param comparator
    //     * @return
    //     */
    //    <T> Map<String, T> distribution(String columnName, Comparator<? super T> comparator);

    /**
     * 
     * @param columnName
     * @return
     */
    <T> Stream<T> stream(String columnName);

    /**
     * 
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(String columnName, int fromRowIndex, int toRowIndex);

    /**
     * @param columnType it's only used to identify the type.
     * @param columnName
     * @return
     */
    <T> Stream<T> stream(Class<? extends T> columnType, String columnName);

    /**
     * 
     * @param columnType it's only used to identify the type.
     * @param columnName
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(Class<? extends T> columnType, String columnName, int fromRowIndex, int toRowIndex);

    /**
     *
     * @return
     */
    Stream<Object[]> stream();

    /**
     *
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    Stream<Object[]> stream(int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param columnNames
     * @return
     */
    Stream<Object[]> stream(Collection<String> columnNames);

    /**
     * 
     * @param columnNames
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    Stream<Object[]> stream(Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    <T> Stream<T> stream(Class<? extends T> rowClass);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(Class<? extends T> rowClass, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(Class<? extends T> rowClass, Collection<String> columnNames);

    /**
     *
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(Class<? extends T> rowClass, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    <T> Stream<T> stream(IntFunction<? extends T> rowSupplier);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(IntFunction<? extends T> rowSupplier, int fromRowIndex, int toRowIndex);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(IntFunction<? extends T> rowSupplier, Collection<String> columnNames);

    /**
     *
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @param fromRowIndex
     * @param toRowIndex
     * @return
     */
    <T> Stream<T> stream(IntFunction<? extends T> rowSupplier, Collection<String> columnNames, int fromRowIndex, int toRowIndex);

    /**
     * Method freeze
     */
    void freeze();

    /**
     * Method frozen
     *
     * @return
     */
    boolean frozen();

    /**
     * Method clear.
     */
    void clear();

    /**
     *
     * @return
     */
    boolean isEmpty();

    /**
     * 
     */
    void trimToSize();

    /**
     * Returns the size of this {@code DataSet}.
     *
     * @return
     */
    int size();

    /**
     *
     * @return
     */
    Properties<String, Object> getProperties();

    //    /**
    //     *
    //     * @param propName
    //     * @return
    //     */
    //    <T> T getProperty(String propName);
    //
    //    /**
    //     * Returns the old value associated with the property by the {@code propName}, {@code null} if it doesn't exist.
    //     *
    //     * @param propName
    //     * @param propValue
    //     * @return
    //     */
    //    <T> T setProperty(String propName, Object propValue);
    //
    //    /**
    //     * Returns value of the property which is to be removed, {@code null} if it doesn't exist.
    //     *
    //     * @param propName
    //     * @return
    //     */
    //    <T> T removeProperty(String propName);

    /**
     * Specify the concrete row type for operations: stream\split\toList\toArray\...
     * @return
     */
    __<Object[]> __();

    /**
     * Specify the concrete row type for operations: stream\split\toList\toArray\... with specified <code>rowClass</code>
     * 
     * @param rowClass it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    <T> __<T> __(Class<? extends T> rowClass);

    /**
     * Specify the concrete row type for operations: stream\split\toList\toArray\... with specified <code>rowClass</code>
     * 
     * @param rowSupplier it can be Object[]/ObjectList/List/Set/Map/Entity
     * @return
     */
    <T> __<T> __(IntFunction<? extends T> rowSupplier);

    // Builder<DataSet> __(Consumer<DataSet> func);

    // <E> __<List<E>> _2();

    // <E> __<List<E>> _2(@SuppressWarnings("rawtypes") Class<? extends List> rowClass);

    // <E, R extends Collection<E>> __<R> shift2(Class<? extends R> rowClass);

    // <E, R extends Collection<E>> __<R> shift2(Class<? extends R> rowClass, Class<? extends E> elementType);

    // <V> __<Map<String, V>> _3();

    // @SuppressWarnings("rawtypes")
    // <V> __<Map<String, V>> _3(Class<? extends Map> rowClass);

    // <V, R extends Map<String, V>> __<R> shift3(Class<? extends R> rowClass);

    // <V, R extends Map<String, V>> __<R> shift3(Class<? extends R> rowClass, Class<? extends V> valueType);

    <T> Sheet<Integer, String, T> toSheet();

    /**
     * it's same as: N.println(toString());
     */
    void println();

    /**
     *
     * @param <T> it could be Object[]/ObjectList/List/Set/Map/Entity
     */
    static interface __<T> {
        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnIndex
         * @return
         */
        <E> List<E> getColumn(Class<? extends E> columnType, int columnIndex);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @return
         */
        <E> List<E> getColumn(Class<? extends E> columnType, String columnName);

        /**
         * 
         * @param keyType it's only used to identify the type.
         * @param valueType
         * @param keyColumnName
         * @param valueColumnName
         * @return
         */
        <K, V> Map<K, V> toMap(Class<? extends K> keyType, Class<? extends V> valueType, String keyColumnName, String valueColumnName);

        /**
         *
         * @param keyType it's only used to identify the type.
         * @param valueType
         * @param keyColumnName
         * @param valueColumnName
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <K, V> Map<K, V> toMap(Class<? extends K> keyType, Class<? extends V> valueType, String keyColumnName, String valueColumnName, int fromRowIndex,
                int toRowIndex);

        /**
         * 
         * @param keyType it's only used to identify the type.
         * @param valueType
         * @param keyColumnName
         * @param valueColumnName
         * @param fromRowIndex
         * @param toRowIndex
         * @param supplier
         * @return
         */
        <K, V, M extends Map<K, V>> M toMap(Class<? extends K> keyType, Class<? extends V> valueType, String keyColumnName, String valueColumnName,
                int fromRowIndex, int toRowIndex, IntFunction<M> supplier);

        /**
         * @param keyType it's only used to identify the type.
         * @param keyColumnName
         * @param valueColumnNames
         * @return
         */
        <K> Map<K, T> toMap(Class<? extends K> keyType, String keyColumnName, Collection<String> valueColumnNames);

        /**
         *
         * @param keyType it's only used to identify the type.
         * @param keyColumnName
         * @param valueColumnNames
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <K> Map<K, T> toMap(Class<? extends K> keyType, String keyColumnName, Collection<String> valueColumnNames, int fromRowIndex, int toRowIndex);

        /**
         * 
         * @param keyType it's only used to identify the type.
         * @param keyColumnName
         * @param valueColumnNames
         * @param fromRowIndex
         * @param toRowIndex
         * @param supplier
         * @return
         */
        <K, M extends Map<K, T>> M toMap(Class<? extends K> keyType, String keyColumnName, Collection<String> valueColumnNames, int fromRowIndex,
                int toRowIndex, IntFunction<M> supplier);

        /**
        *
        * @param keyColumnName
        * @param valueColumnName
        * @return
        */
        <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(Class<? extends K> keyType, Class<? extends E> valueType, String keyColumnName,
                String valueColumnName);

        /**
        *
        * @param keyColumnName
        * @param valueColumnName
        * @param fromRowIndex
        * @param toRowIndex
        * @return
        */
        <K, E, V extends List<E>> Multimap<K, E, V> toMultimap(Class<? extends K> keyType, Class<? extends E> valueType, String keyColumnName,
                String valueColumnName, int fromRowIndex, int toRowIndex);

        /**
        * 
        * @param keyColumnName
        * @param valueColumnName
        * @param fromRowIndex
        * @param toRowIndex
        * @param supplier
        * @return
        */
        <K, E, V extends Collection<E>> Multimap<K, E, V> toMultimap(Class<? extends K> keyType, Class<? extends E> valueType, String keyColumnName,
                String valueColumnName, int fromRowIndex, int toRowIndex, IntFunction<Multimap<K, E, V>> supplier);

        /**
         *
         * @param keyType it's only used to identify the type.
         * @param keyColumnName
         * @param valueColumnNames
         * @return
         */
        <K> Multimap<K, T, List<T>> toMultimap(Class<? extends K> keyType, String keyColumnName, Collection<String> valueColumnNames);

        /**
         *
         * @param keyType it's only used to identify the type.
         * @param keyColumnName
         * @param valueColumnNames
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <K> Multimap<K, T, List<T>> toMultimap(Class<? extends K> keyType, String keyColumnName, Collection<String> valueColumnNames, int fromRowIndex,
                int toRowIndex);

        /**
         * 
         * @param keyType it's only used to identify the type.
         * @param keyColumnName
         * @param valueColumnNames
         * @param fromRowIndex
         * @param toRowIndex
         * @param supplier
         * @return
         */
        <K, V extends Collection<T>> Multimap<K, T, V> toMultimap(Class<? extends K> keyType, String keyColumnName, Collection<String> valueColumnNames,
                int fromRowIndex, int toRowIndex, IntFunction<Multimap<K, T, V>> supplier);

        /**
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @return
         */
        <E> Multiset<E> toMultiset(Class<? extends E> columnType, String columnName);

        /**
         *
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <E> Multiset<E> toMultiset(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @param supplier create <code>Multiset</code> by new <code>Multiset(ArrayHashMap.class)</code> or <code>Multiset(LinkedArrayHashMap.class)</code> if the element is array.
         * @return
         */
        <E> Multiset<E> toMultiset(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex, IntFunction<Multiset<E>> supplier);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> min(Class<? extends E> columnType, String columnName);

        /**
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> min(Class<? extends E> columnType, String columnName, Comparator<? super E> comparator);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> min(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> min(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex, Comparator<? super E> comparator);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> max(Class<? extends E> columnType, String columnName);

        /**
         *
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> max(Class<? extends E> columnType, String columnName, Comparator<? super E> comparator);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> max(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> max(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex, Comparator<? super E> comparator);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> median(Class<? extends E> columnType, String columnName);

        /**
         *
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> median(Class<? extends E> columnType, String columnName, Comparator<? super E> comparator);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> median(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> median(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex, Comparator<? super E> comparator);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param k
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> kthLargest(Class<? extends E> columnType, String columnName, int k);

        /**
         *
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param k
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> kthLargest(Class<? extends E> columnType, String columnName, int k, Comparator<? super E> comparator);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @param k
         * @return
         */
        <E extends Comparable<? super E>> OptionalNullable<E> kthLargest(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex,
                int k);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @param k
         * @param comparator
         * @return
         */
        <E> OptionalNullable<E> kthLargest(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex, int k,
                Comparator<? super E> comparator);

        /**
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @return
         */
        <E> Stream<E> stream(Class<? extends E> columnType, String columnName);

        /**
         * 
         * @param columnType it's only used to identify the type.
         * @param columnName
         * @param fromRowIndex
         * @param toRowIndex
         * @return
         */
        <E> Stream<E> stream(Class<? extends E> columnType, String columnName, int fromRowIndex, int toRowIndex);
    }
}
