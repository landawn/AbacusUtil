/*
 * Copyright (C) 2016 HaiYang Li
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

import java.util.List;
import java.util.NoSuchElementException;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.Pair.IntPair;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntBiFunction;
import com.landawn.abacus.util.function.IntBiPredicate;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.UnaryOperator;
import com.landawn.abacus.util.stream.ExIterator;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Matrix<T> extends AbstractMatrix<T[], List<T>, Stream<T>, Stream<Stream<T>>, Matrix<T>> {
    private final Class<T[]> arrayType;
    private final Class<T> componentType;

    public Matrix(final T[][] a) {
        super(a);
        this.arrayType = (Class<T[]>) this.a.getClass().getComponentType();
        this.componentType = (Class<T>) this.arrayType.getComponentType();
    }

    @SafeVarargs
    public static <T> Matrix<T> of(final T[]... a) {
        return new Matrix<>(a);
    }

    public static <T> Matrix<T> repeat(final T val, final int len) {
        final T[][] c = N.newArray(N.newArray(val.getClass(), 0).getClass(), 1);
        c[0] = Array.repeat(val, len);
        return new Matrix<>(c);
    }

    public static <T> Matrix<T> diagonalLU2RD(final T[] leftUp2RighDownDiagonal) {
        return diagonal(leftUp2RighDownDiagonal, null);
    }

    public static <T> Matrix<T> diagonalRU2LD(final T[] rightUp2LeftDownDiagonal) {
        return diagonal(null, rightUp2LeftDownDiagonal);
    }

    public static <T> Matrix<T> diagonal(final T[] leftUp2RighDownDiagonal, T[] rightUp2LeftDownDiagonal) {
        N.checkArgument(
                N.isNullOrEmpty(leftUp2RighDownDiagonal) || N.isNullOrEmpty(rightUp2LeftDownDiagonal)
                        || leftUp2RighDownDiagonal.length == rightUp2LeftDownDiagonal.length,
                "The length of 'leftUp2RighDownDiagonal' and 'rightUp2LeftDownDiagonal' must be same");

        final Class<?> arrayClass = leftUp2RighDownDiagonal != null ? leftUp2RighDownDiagonal.getClass() : rightUp2LeftDownDiagonal.getClass();
        final Class<?> componentClass = arrayClass.getComponentType();
        final int len = leftUp2RighDownDiagonal != null ? leftUp2RighDownDiagonal.length : rightUp2LeftDownDiagonal.length;

        final T[][] c = N.newArray(arrayClass, len);

        for (int i = 0; i < len; i++) {
            c[i] = N.newArray(componentClass, len);
        }

        if (N.isNullOrEmpty(leftUp2RighDownDiagonal)) {
            if (N.isNullOrEmpty(rightUp2LeftDownDiagonal)) {
                return new Matrix<>(c);
            } else {
                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }

                return new Matrix<>(c);
            }
        } else {
            for (int i = 0; i < len; i++) {
                c[i][i] = leftUp2RighDownDiagonal[i];
            }

            if (N.notNullOrEmpty(rightUp2LeftDownDiagonal)) {
                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }
            }

            return new Matrix<>(c);
        }
    }

    public T[][] array() {
        return a;
    }

    public T get(final int i, final int j) {
        return a[i][j];
    }

    public T get(final IntPair point) {
        return a[point._1][point._2];
    }

    public void set(final int i, final int j, final T val) {
        a[i][j] = val;
    }

    public void set(final IntPair point, final T val) {
        a[point._1][point._2] = val;
    }

    public NullabLe<T> upOf(final int i, final int j) {
        return i == 0 ? NullabLe.<T> empty() : NullabLe.of(a[i - 1][j]);
    }

    public NullabLe<T> downOf(final int i, final int j) {
        return i == rows - 1 ? NullabLe.<T> empty() : NullabLe.of(a[i + 1][j]);
    }

    public NullabLe<T> leftOf(final int i, final int j) {
        return j == 0 ? NullabLe.<T> empty() : NullabLe.of(a[i][j - 1]);
    }

    public NullabLe<T> rightOf(final int i, final int j) {
        return j == cols - 1 ? NullabLe.<T> empty() : NullabLe.of(a[i][j + 1]);
    }

    /**
     * Returns the four adjacencies with order: up, right, down, left. <code>null</code> is set if the adjacency doesn't exist.
     * 
     * @param i
     * @param j
     * @return
     */
    public Stream<IntPair> adjacent4Points(final int i, final int j) {
        final IntPair up = i == 0 ? null : IntPair.of(i - 1, j);
        final IntPair right = j == cols - 1 ? null : IntPair.of(i, j + 1);
        final IntPair down = i == rows - 1 ? null : IntPair.of(i + 1, j);
        final IntPair left = j == 0 ? null : IntPair.of(i, j - 1);

        return Stream.of(up, right, down, left);
    }

    /**
     * Returns the eight adjacencies with order: left-up, up, right-up, right, right-down, down, left-down, left. <code>null</code> is set if the adjacency doesn't exist.
     * 
     * @param i
     * @param j
     * @return
     */
    public Stream<IntPair> adjacent8Points(final int i, final int j) {
        final IntPair up = i == 0 ? null : IntPair.of(i - 1, j);
        final IntPair right = j == cols - 1 ? null : IntPair.of(i, j + 1);
        final IntPair down = i == rows - 1 ? null : IntPair.of(i + 1, j);
        final IntPair left = j == 0 ? null : IntPair.of(i, j - 1);

        final IntPair leftUp = i > 0 && j > 0 ? IntPair.of(i - 1, j - 1) : null;
        final IntPair rightUp = i > 0 && j < cols - 1 ? IntPair.of(i - 1, j + 1) : null;
        final IntPair rightDown = i < rows - 1 && j < cols - 1 ? IntPair.of(j + 1, j + 1) : null;
        final IntPair leftDown = i < rows - 1 && j > 0 ? IntPair.of(i + 1, j - 1) : null;

        return Stream.of(leftUp, up, rightUp, right, rightDown, down, leftDown, left);
    }

    public T[] row(final int rowIndex) {
        N.checkArgument(rowIndex >= 0 && rowIndex < rows, "Invalid row Index: %s", rowIndex);

        return a[rowIndex];
    }

    public T[] column(final int columnIndex) {
        N.checkArgument(columnIndex >= 0 && columnIndex < cols, "Invalid column Index: %s", columnIndex);

        final T[] c = N.newArray(componentType, rows);

        for (int i = 0; i < rows; i++) {
            c[i] = a[i][columnIndex];
        }

        return c;
    }

    public void setRow(int rowIndex, T[] row) {
        N.checkArgument(row.length == cols, "The size of the specified row doesn't match the length of column");

        N.copy(row, 0, a[rowIndex], 0, cols);
    }

    public void setColumn(int columnIndex, T[] column) {
        N.checkArgument(column.length == rows, "The size of the specified column doesn't match the length of row");

        for (int i = 0; i < rows; i++) {
            a[i][columnIndex] = column[i];
        }
    }

    public void updateRow(int rowIndex, UnaryOperator<T> func) {
        for (int i = 0; i < cols; i++) {
            a[rowIndex][i] = func.apply(a[rowIndex][i]);
        }
    }

    public void updateColumn(int columnIndex, UnaryOperator<T> func) {
        for (int i = 0; i < rows; i++) {
            a[i][columnIndex] = func.apply(a[i][columnIndex]);
        }
    }

    public T[] getLU2RD() {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);

        final T[] res = N.newArray(componentType, rows);

        for (int i = 0; i < rows; i++) {
            res[i] = a[i][i];
        }

        return res;
    }

    public void setLU2RD(final T[] diagonal) {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);
        N.checkArgument(diagonal.length >= rows, "The length of specified array is less than n=%s", rows);

        for (int i = 0; i < rows; i++) {
            a[i][i] = diagonal[i];
        }
    }

    public void updateLU2RD(final UnaryOperator<T> func) {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);

        for (int i = 0; i < rows; i++) {
            a[i][i] = func.apply(a[i][i]);
        }
    }

    public T[] getRU2LD() {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);

        final T[] res = N.newArray(componentType, rows);

        for (int i = 0; i < rows; i++) {
            res[i] = a[i][cols - i - 1];
        }

        return res;
    }

    public void setRU2LD(final T[] diagonal) {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);
        N.checkArgument(diagonal.length >= rows, "The length of specified array is less than n=%s", rows);

        for (int i = 0; i < rows; i++) {
            a[i][cols - i - 1] = diagonal[i];
        }
    }

    public void updateRU2LD(final UnaryOperator<T> func) {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);

        for (int i = 0; i < rows; i++) {
            a[i][cols - i - 1] = func.apply(a[i][cols - i - 1]);
        }
    }

    public void updateAll(final UnaryOperator<T> func) {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            a[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        a[i][j] = func.apply(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        a[i][j] = func.apply(a[i][j]);
                    }
                }
            }
        }
    }

    /**
     * Update all elements based on points
     * 
     * @param func
     */
    public void updateAll(final IntBiFunction<T> func) {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = func.apply(i, j);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            a[i][j] = func.apply(i, j);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        a[i][j] = func.apply(i, j);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        a[i][j] = func.apply(i, j);
                    }
                }
            }
        }
    }

    public void replaceIf(final Predicate<? super T> predicate, final T newValue) {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                    }
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    public NullabLe<T> min(final Comparator<? super T> cmp) {
    //        if (isEmpty()) {
    //            return NullabLe.empty();
    //        }
    //
    //        final Comparator<? super T> comparator = cmp == null ? N.NULL_MIN_COMPARATOR : cmp;
    //        T candicate = a[0][0];
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (comparator.compare(a[i][j], candicate) < 0) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return NullabLe.of(candicate);
    //    }
    //
    //    public NullabLe<T> max(final Comparator<? super T> cmp) {
    //        if (isEmpty()) {
    //            return NullabLe.empty();
    //        }
    //
    //        final Comparator<? super T> comparator = cmp == null ? N.NULL_MIN_COMPARATOR : cmp;
    //        T candicate = a[0][0];
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (comparator.compare(a[i][j], candicate) > 0) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return NullabLe.of(candicate);
    //    }
    //
    //    @Override
    //    public List<T> row(final int i) {
    //        return List.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public List<T> column(final int j) {
    //        return List.of(column2(j));
    //    }

    /**
     * Replace elements by <code>Predicate.test(i, j)</code> based on points
     * 
     * @param predicate
     * @param newValue
     */
    public void replaceIf(final IntBiPredicate predicate, final T newValue) {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = predicate.test(i, j) ? newValue : a[i][j];
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            a[i][j] = predicate.test(i, j) ? newValue : a[i][j];
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        a[i][j] = predicate.test(i, j) ? newValue : a[i][j];
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        a[i][j] = predicate.test(i, j) ? newValue : a[i][j];
                    }
                }
            }
        }
    }

    public Matrix<T> map(final Function<? super T, T> func) {
        return map(this.componentType, func);
    }

    public <R> Matrix<R> map(final Class<R> cls, final Function<? super T, R> func) {
        final R[][] c = N.newArray(N.newArray(cls, 0).getClass(), rows);

        for (int i = 0; i < rows; i++) {
            c[i] = N.newArray(cls, cols);
        }

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.apply(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.apply(a[i][j]);
                    }
                }
            }
        }

        return Matrix.of(c);
    }

    public BooleanMatrix mapToBoolean(final ToBooleanFunction<? super T> func) {
        final boolean[][] c = new boolean[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsBoolean(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsBoolean(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsBoolean(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsBoolean(a[i][j]);
                    }
                }
            }
        }

        return BooleanMatrix.of(c);
    }

    public ByteMatrix mapToByte(final ToByteFunction<? super T> func) {
        final byte[][] c = new byte[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsByte(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsByte(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsByte(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsByte(a[i][j]);
                    }
                }
            }
        }

        return ByteMatrix.of(c);
    }

    public CharMatrix mapToChar(final ToCharFunction<? super T> func) {
        final char[][] c = new char[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsChar(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsChar(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsChar(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsChar(a[i][j]);
                    }
                }
            }
        }

        return CharMatrix.of(c);
    }

    public ShortMatrix mapToShort(final ToShortFunction<? super T> func) {
        final short[][] c = new short[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            }
        }

        return ShortMatrix.of(c);
    }

    public IntMatrix mapToInt(final ToIntFunction<? super T> func) {
        final int[][] c = new int[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsInt(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsInt(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsInt(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsInt(a[i][j]);
                    }
                }
            }
        }

        return IntMatrix.of(c);
    }

    public LongMatrix mapToLong(final ToLongFunction<? super T> func) {
        final long[][] c = new long[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsLong(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsLong(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsLong(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsLong(a[i][j]);
                    }
                }
            }
        }

        return LongMatrix.of(c);
    }

    public FloatMatrix mapToFloat(final ToFloatFunction<? super T> func) {
        final float[][] c = new float[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsFloat(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsFloat(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsFloat(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsFloat(a[i][j]);
                    }
                }
            }
        }

        return FloatMatrix.of(c);
    }

    public DoubleMatrix mapToDouble(final ToDoubleFunction<? super T> func) {
        final double[][] c = new double[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsDouble(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsDouble(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = func.applyAsDouble(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = func.applyAsDouble(a[i][j]);
                    }
                }
            }
        }

        return DoubleMatrix.of(c);
    }

    public void fill(final T val) {
        for (int i = 0; i < rows; i++) {
            N.fill(a[i], val);
        }
    }

    public void fill(final T[][] b) {
        fill(0, 0, b);
    }

    public void fill(final int fromRowIndex, final int fromColumnIndex, final T[][] b) {
        N.checkFromToIndex(fromRowIndex, rows, rows);
        N.checkFromToIndex(fromColumnIndex, cols, cols);

        for (int i = 0, minLen = N.min(rows - fromRowIndex, b.length); i < minLen; i++) {
            N.copy(b[i], 0, a[i + fromRowIndex], fromColumnIndex, N.min(b[i].length, cols - fromColumnIndex));
        }
    }

    @Override
    public Matrix<T> copy() {
        final T[][] c = N.newArray(arrayType, rows);

        for (int i = 0; i < rows; i++) {
            c[i] = a[i].clone();
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> copy(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        final T[][] c = N.newArray(arrayType, toRowIndex - fromRowIndex);

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        final T[][] c = N.newArray(arrayType, toRowIndex - fromRowIndex);

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> rotate90() {
        final T[][] c = N.newArray(arrayType, cols);

        for (int i = 0; i < cols; i++) {
            c[i] = N.newArray(this.componentType, rows);
        }

        if (rows <= cols) {
            for (int j = 0; j < rows; j++) {
                for (int i = 0; i < cols; i++) {
                    c[i][j] = a[rows - j - 1][i];
                }
            }
        } else {
            for (int i = 0; i < cols; i++) {
                for (int j = 0; j < rows; j++) {
                    c[i][j] = a[rows - j - 1][i];
                }
            }
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> rotate180() {
        final T[][] c = N.newArray(arrayType, rows);

        for (int i = 0; i < rows; i++) {
            c[i] = a[rows - i - 1].clone();
            N.reverse(c[i]);
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> rotate270() {
        final T[][] c = N.newArray(arrayType, cols);

        for (int i = 0; i < cols; i++) {
            c[i] = N.newArray(this.componentType, rows);
        }

        if (rows <= cols) {
            for (int j = 0; j < rows; j++) {
                for (int i = 0; i < cols; i++) {
                    c[i][j] = a[j][cols - i - 1];
                }
            }
        } else {
            for (int i = 0; i < cols; i++) {
                for (int j = 0; j < rows; j++) {
                    c[i][j] = a[j][cols - i - 1];
                }
            }
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> transpose() {
        final T[][] c = N.newArray(arrayType, cols);

        for (int i = 0; i < cols; i++) {
            c[i] = N.newArray(componentType, rows);
        }

        if (rows <= cols) {
            for (int j = 0; j < rows; j++) {
                for (int i = 0; i < cols; i++) {
                    c[i][j] = a[j][i];
                }
            }
        } else {
            for (int i = 0; i < cols; i++) {
                for (int j = 0; j < rows; j++) {
                    c[i][j] = a[j][i];
                }
            }
        }
        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> reshape(final int newRows, final int newCols) {
        final T[][] c = N.newArray(arrayType, newRows);

        for (int i = 0; i < newRows; i++) {
            c[i] = N.newArray(componentType, newCols);
        }

        if (newRows == 0 || newCols == 0 || N.isNullOrEmpty(a)) {
            return new Matrix<>(c);
        }

        if (a.length == 1) {
            final T[] a0 = a[0];

            for (int i = 0, len = (int) N.min(newRows, count % newCols == 0 ? count / newCols : count / newCols + 1); i < len; i++) {
                N.copy(a0, i * newCols, c[i], 0, (int) N.min(newCols, count - i * newCols));
            }
        } else {
            long cnt = 0;

            for (int i = 0, len = (int) N.min(newRows, count % newCols == 0 ? count / newCols : count / newCols + 1); i < len; i++) {
                for (int j = 0, col = (int) N.min(newCols, count - i * newCols); j < col; j++, cnt++) {
                    c[i][j] = a[(int) (cnt / this.cols)][(int) (cnt % this.cols)];
                }
            }
        }

        return new Matrix<>(c);
    }

    @Override
    public Matrix<T> repmat(final int rowRepeats, final int colRepeats) {
        N.checkArgument(rowRepeats > 0 && colRepeats > 0, "rowRepeats=%s and colRepeats=%s must be bigger than 0", rowRepeats, colRepeats);

        final T[][] c = N.newArray(arrayType, rows * rowRepeats);

        for (int i = 0, len = c.length; i < len; i++) {
            c[i] = N.newArray(componentType, cols * colRepeats);
        }

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < colRepeats; j++) {
                N.copy(a[i], 0, c[i], j * cols, cols);
            }
        }

        for (int i = 1; i < rowRepeats; i++) {
            for (int j = 0; j < rows; j++) {
                N.copy(c[j], 0, c[i * rows + j], 0, c[j].length);
            }
        }

        return new Matrix<T>(c);
    }

    @Override
    public List<T> flatten() {
        final T[] c = N.newArray(componentType, rows * cols);

        for (int i = 0; i < rows; i++) {
            N.copy(a[i], 0, c, i * cols, cols);
        }

        return N.asList(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#vstack(IntMatrix)
     */
    public Matrix<T> vstack(final Matrix<? extends T> b) {
        N.checkArgument(this.cols == b.cols, "The count of column in this matrix and the specified matrix are not equals");

        final T[][] c = N.newArray(arrayType, this.rows + b.rows);
        int j = 0;

        for (int i = 0; i < rows; i++) {
            c[j++] = a[i].clone();
        }

        for (int i = 0; i < b.rows; i++) {
            c[j++] = b.a[i].clone();
        }

        return Matrix.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#hstack(IntMatrix)
     */
    public Matrix<T> hstack(final Matrix<T> b) {
        N.checkArgument(this.rows == b.rows, "The count of row in this matrix and the specified matrix are not equals");

        final T[][] c = N.newArray(arrayType, rows);

        for (int i = 0; i < rows; i++) {
            c[i] = N.copyOf(a[i], cols + b.cols);
            N.copy(b.a[i], 0, c[i], cols, b.cols);
        }

        return Matrix.of(c);
    }

    public <B> Matrix<T> zipWith(final Matrix<B> matrixB, final BiFunction<? super T, ? super B, T> zipFunction) {
        return zipWith(componentType, matrixB, zipFunction);
    }

    public <B, R> Matrix<R> zipWith(final Class<R> cls, final Matrix<B> matrixB, final BiFunction<? super T, ? super B, R> zipFunction) {
        N.checkArgument(rows == matrixB.rows && cols == matrixB.cols, "Can't zip two matrices which have different shape.");

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), rows);

        for (int i = 0; i < rows; i++) {
            result[i] = N.newArray(cls, cols);
        }

        final B[][] b = matrixB.a;

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                    }
                }
            }
        }

        return new Matrix<>(result);
    }

    public <B, C> Matrix<T> zipWith(final Matrix<B> matrixB, final Matrix<C> matrixC, final TriFunction<? super T, ? super B, ? super C, T> zipFunction) {
        return zipWith(componentType, matrixB, matrixC, zipFunction);
    }

    public <B, C, R> Matrix<R> zipWith(final Class<R> cls, final Matrix<B> matrixB, final Matrix<C> matrixC,
            final TriFunction<? super T, ? super B, ? super C, R> zipFunction) {
        N.checkArgument(rows == matrixB.rows && cols == matrixB.cols, "Can't zip two matrices which have different shape.");

        final R[][] result = N.newArray(N.newArray(cls, 0).getClass(), rows);

        for (int i = 0; i < rows; i++) {
            result[i] = N.newArray(cls, cols);
        }

        final B[][] b = matrixB.a;
        final C[][] c = matrixC.a;

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                    }
                }
            }
        }

        return new Matrix<>(result);
    }

    /**
     * 
     * @return a stream composed by elements on the diagonal line from left up to right down.
     */
    @Override
    public Stream<T> streamLU2RD() {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<T>() {
            private final int toIndex = rows;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor][cursor++];
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

    /**
     * 
     * @return a stream composed by elements on the diagonal line from right up to left down.
     */
    @Override
    public Stream<T> streamRU2LD() {
        N.checkState(rows == cols, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", rows, cols);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<T>() {
            private final int toIndex = rows;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public T next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor][rows - ++cursor];
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

    /**
     * 
     * @return a stream based on the order of row.
     */
    @Override
    public Stream<T> streamH() {
        return streamH(0, rows);
    }

    @Override
    public Stream<T> streamH(final int rowIndex) {
        return streamH(rowIndex, rowIndex + 1);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a stream based on the order of row.
     */
    @Override
    public Stream<T> streamH(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<T>() {
            private int i = fromRowIndex;
            private int j = 0;

            @Override
            public boolean hasNext() {
                return i < toRowIndex;
            }

            @Override
            public T next() {
                if (i >= toRowIndex) {
                    throw new NoSuchElementException();
                }

                final T result = a[i][j++];

                if (j >= cols) {
                    i++;
                    j = 0;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toRowIndex - i) * cols * 1L - j) {
                    i = toRowIndex;
                    j = 0;
                } else {
                    i += (n + j) / cols;
                    j += (n + j) % cols;
                }
            }

            @Override
            public long count() {
                return (toRowIndex - i) * cols * 1L - j;
            }

            @Override
            public <A> A[] toArray(A[] c) {
                final int len = (int) count();

                if (c.length < len) {
                    c = N.copyOf(c, len);
                }

                for (int k = 0; k < len; k++) {
                    c[k] = (A) a[i][j++];

                    if (j >= cols) {
                        i++;
                        j = 0;
                    }
                }

                return c;
            }
        });
    }

    /**
     * 
     * @return a stream based on the order of column.
     */
    @Override
    @Beta
    public Stream<T> streamV() {
        return streamV(0, cols);
    }

    @Override
    public Stream<T> streamV(final int columnIndex) {
        return streamV(columnIndex, columnIndex + 1);
    }

    /**
     * 
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a stream based on the order of column.
     */
    @Beta
    @Override
    public Stream<T> streamV(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<T>() {
            private int i = 0;
            private int j = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return j < toColumnIndex;
            }

            @Override
            public T next() {
                if (j >= toColumnIndex) {
                    throw new NoSuchElementException();
                }

                final T result = a[i++][j];

                if (i >= rows) {
                    i = 0;
                    j++;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toColumnIndex - j) * Matrix.this.rows * 1L - i) {
                    i = 0;
                    j = toColumnIndex;
                } else {
                    i += (n + i) % Matrix.this.rows;
                    j += (n + i) / Matrix.this.rows;
                }
            }

            @Override
            public long count() {
                return (toColumnIndex - j) * rows - i;
            }

            @Override
            public <A> A[] toArray(A[] c) {
                final int len = (int) count();

                if (c.length < len) {
                    c = N.copyOf(c, len);
                }

                for (int k = 0; k < len; k++) {
                    c[k] = (A) a[i++][j];

                    if (i >= rows) {
                        i = 0;
                        j++;
                    }
                }

                return c;
            }
        });
    }

    /**
     * 
     * @return a row stream based on the order of row.
     */
    @Override
    public Stream<Stream<T>> streamR() {
        return streamR(0, rows);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    @Override
    public Stream<Stream<T>> streamR(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Stream<T>>() {
            private final int toIndex = toRowIndex;
            private int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return Stream.of(a[cursor++]);
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

    /**
     * 
     * @return a column stream based on the order of column.
     */
    @Override
    @Beta
    public Stream<Stream<T>> streamC() {
        return streamC(0, cols);
    }

    /**
     * 
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a column stream based on the order of column.
     */
    @Override
    @Beta
    public Stream<Stream<T>> streamC(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Stream<T>>() {
            private final int toIndex = toColumnIndex;
            private volatile int cursor = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<T> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return Stream.of(new ExIterator<T>() {
                    private final int columnIndex = cursor++;
                    private final int toIndex2 = rows;
                    private int cursor2 = 0;

                    @Override
                    public boolean hasNext() {
                        return cursor2 < toIndex2;
                    }

                    @Override
                    public T next() {
                        if (cursor2 >= toIndex2) {
                            throw new NoSuchElementException();
                        }

                        return a[cursor2++][columnIndex];
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
    protected int length(T[] a) {
        return a == null ? 0 : a.length;
    }

    @Override
    public int hashCode() {
        return N.deepHashCode(a);
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof Matrix) {
            final Matrix<T> another = (Matrix<T>) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
