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

import java.util.NoSuchElementException;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.ObjIteratorEx;
import com.landawn.abacus.util.stream.ShortIteratorEx;
import com.landawn.abacus.util.stream.ShortStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ShortMatrix extends AbstractMatrix<short[], ShortList, ShortStream, Stream<ShortStream>, ShortMatrix> {
    static final ShortMatrix EMPTY_SHORT_MATRIX = new ShortMatrix(new short[0][0]);

    public ShortMatrix(final short[][] a) {
        super(a == null ? new short[0][0] : a);
    }

    public static ShortMatrix empty() {
        return EMPTY_SHORT_MATRIX;
    }

    @SafeVarargs
    public static ShortMatrix of(final short[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_SHORT_MATRIX : new ShortMatrix(a);
    }

    public static ShortMatrix random(final int len) {
        return new ShortMatrix(new short[][] { ShortList.random(len).array() });
    }

    public static ShortMatrix repeat(final short val, final int len) {
        return new ShortMatrix(new short[][] { Array.repeat(val, len) });
    }

    public static ShortMatrix range(short startInclusive, final short endExclusive) {
        return new ShortMatrix(new short[][] { Array.range(startInclusive, endExclusive) });
    }

    public static ShortMatrix range(short startInclusive, final short endExclusive, final short by) {
        return new ShortMatrix(new short[][] { Array.range(startInclusive, endExclusive, by) });
    }

    public static ShortMatrix rangeClosed(short startInclusive, final short endInclusive) {
        return new ShortMatrix(new short[][] { Array.rangeClosed(startInclusive, endInclusive) });
    }

    public static ShortMatrix rangeClosed(short startInclusive, final short endInclusive, final short by) {
        return new ShortMatrix(new short[][] { Array.rangeClosed(startInclusive, endInclusive, by) });
    }

    public static ShortMatrix diagonalLU2RD(final short[] leftUp2RighDownDiagonal) {
        return diagonal(leftUp2RighDownDiagonal, null);
    }

    public static ShortMatrix diagonalRU2LD(final short[] rightUp2LeftDownDiagonal) {
        return diagonal(null, rightUp2LeftDownDiagonal);
    }

    public static ShortMatrix diagonal(final short[] leftUp2RighDownDiagonal, short[] rightUp2LeftDownDiagonal) {
        N.checkArgument(
                N.isNullOrEmpty(leftUp2RighDownDiagonal) || N.isNullOrEmpty(rightUp2LeftDownDiagonal)
                        || leftUp2RighDownDiagonal.length == rightUp2LeftDownDiagonal.length,
                "The length of 'leftUp2RighDownDiagonal' and 'rightUp2LeftDownDiagonal' must be same");

        if (N.isNullOrEmpty(leftUp2RighDownDiagonal)) {
            if (N.isNullOrEmpty(rightUp2LeftDownDiagonal)) {
                return empty();
            } else {
                final int len = rightUp2LeftDownDiagonal.length;
                final short[][] c = new short[len][len];

                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }

                return new ShortMatrix(c);
            }
        } else {
            final int len = leftUp2RighDownDiagonal.length;
            final short[][] c = new short[len][len];

            for (int i = 0; i < len; i++) {
                c[i][i] = leftUp2RighDownDiagonal[i];
            }

            if (N.notNullOrEmpty(rightUp2LeftDownDiagonal)) {
                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }
            }

            return new ShortMatrix(c);
        }
    }

    public short get(final int i, final int j) {
        return a[i][j];
    }

    public short get(final IntPair point) {
        return a[point._1][point._2];
    }

    public void set(final int i, final int j, final short val) {
        a[i][j] = val;
    }

    public void set(final IntPair point, final short val) {
        a[point._1][point._2] = val;
    }

    public OptionalShort upOf(final int i, final int j) {
        return i == 0 ? OptionalShort.empty() : OptionalShort.of(a[i - 1][j]);
    }

    public OptionalShort downOf(final int i, final int j) {
        return i == rows - 1 ? OptionalShort.empty() : OptionalShort.of(a[i + 1][j]);
    }

    public OptionalShort leftOf(final int i, final int j) {
        return j == 0 ? OptionalShort.empty() : OptionalShort.of(a[i][j - 1]);
    }

    public OptionalShort rightOf(final int i, final int j) {
        return j == cols - 1 ? OptionalShort.empty() : OptionalShort.of(a[i][j + 1]);
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

    public short[] row(final int rowIndex) {
        N.checkArgument(rowIndex >= 0 && rowIndex < rows, "Invalid row Index: %s", rowIndex);

        return a[rowIndex];
    }

    public short[] column(final int columnIndex) {
        N.checkArgument(columnIndex >= 0 && columnIndex < cols, "Invalid column Index: %s", columnIndex);

        final short[] c = new short[rows];

        for (int i = 0; i < rows; i++) {
            c[i] = a[i][columnIndex];
        }

        return c;
    }

    public void setRow(int rowIndex, short[] row) {
        N.checkArgument(row.length == cols, "The size of the specified row doesn't match the length of column");

        N.copy(row, 0, a[rowIndex], 0, cols);
    }

    public void setColumn(int columnIndex, short[] column) {
        N.checkArgument(column.length == rows, "The size of the specified column doesn't match the length of row");

        for (int i = 0; i < rows; i++) {
            a[i][columnIndex] = column[i];
        }
    }

    public <E extends Exception> void updateRow(int rowIndex, Try.ShortUnaryOperator<E> func) throws E {
        for (int i = 0; i < cols; i++) {
            a[rowIndex][i] = func.applyAsShort(a[rowIndex][i]);
        }
    }

    public <E extends Exception> void updateColumn(int columnIndex, Try.ShortUnaryOperator<E> func) throws E {
        for (int i = 0; i < rows; i++) {
            a[i][columnIndex] = func.applyAsShort(a[i][columnIndex]);
        }
    }

    public short[] getLU2RD() {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        final short[] res = new short[rows];

        for (int i = 0; i < rows; i++) {
            res[i] = a[i][i];
        }

        return res;
    }

    public void setLU2RD(final short[] diagonal) {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);
        N.checkArgument(diagonal.length >= rows, "The length of specified array is less than rows=%s", rows);

        for (int i = 0; i < rows; i++) {
            a[i][i] = diagonal[i];
        }
    }

    public <E extends Exception> void updateLU2RD(final Try.ShortUnaryOperator<E> func) throws E {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        for (int i = 0; i < rows; i++) {
            a[i][i] = func.applyAsShort(a[i][i]);
        }
    }

    public short[] getRU2LD() {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        final short[] res = new short[rows];

        for (int i = 0; i < rows; i++) {
            res[i] = a[i][cols - i - 1];
        }

        return res;
    }

    public void setRU2LD(final short[] diagonal) {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);
        N.checkArgument(diagonal.length >= rows, "The length of specified array is less than rows=%s", rows);

        for (int i = 0; i < rows; i++) {
            a[i][cols - i - 1] = diagonal[i];
        }
    }

    public <E extends Exception> void updateRU2LD(final Try.ShortUnaryOperator<E> func) throws E {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        for (int i = 0; i < rows; i++) {
            a[i][cols - i - 1] = func.applyAsShort(a[i][cols - i - 1]);
        }
    }

    public <E extends Exception> void updateAll(final Try.ShortUnaryOperator<E> func) throws E {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {

                    @Override
                    public void accept(final int j) throws E {
                        for (int i = 0; i < rows; i++) {
                            a[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (

                        int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        a[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        a[i][j] = func.applyAsShort(a[i][j]);
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
    public <E extends Exception> void updateAll(final Try.IntBiFunction<Short, E> func) throws E {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = func.apply(i, j);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int j) throws E {
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

    public <E extends Exception> void replaceIf(final Try.ShortPredicate<E> predicate, final short newValue) throws E {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int j) throws E {
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

    /**
     * Replace elements by <code>Predicate.test(i, j)</code> based on points
     * 
     * @param predicate
     * @param newValue
     */
    public <E extends Exception> void replaceIf(final Try.IntBiPredicate<E> predicate, final short newValue) throws E {
        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            a[i][j] = predicate.test(i, j) ? newValue : a[i][j];
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int j) throws E {
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

    public <E extends Exception> ShortMatrix map(final Try.ShortUnaryOperator<E> func) throws E {
        final short[][] c = new short[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {

                    @Override
                    public void accept(final int j) throws E {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (

                        int i = 0; i < rows; i++) {
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

    public <T, E extends Exception> Matrix<T> mapToObj(final Class<T> cls, final Try.ShortFunction<? extends T, E> func) throws E {
        final T[][] c = N.newArray(N.newArray(cls, 0).getClass(), rows);

        for (int i = 0; i < rows; i++) {
            c[i] = N.newArray(cls, cols);
        }

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {

                    @Override
                    public void accept(final int j) throws E {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (

                        int i = 0; i < rows; i++) {
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

    public void fill(final short val) {
        for (int i = 0; i < rows; i++) {
            N.fill(a[i], val);
        }
    }

    public void fill(final short[][] b) {
        fill(0, 0, b);
    }

    public void fill(final int fromRowIndex, final int fromColumnIndex, final short[][] b) {
        N.checkFromToIndex(fromRowIndex, rows, rows);
        N.checkFromToIndex(fromColumnIndex, cols, cols);

        for (int i = 0, minLen = N.min(rows - fromRowIndex, b.length); i < minLen; i++) {
            N.copy(b[i], 0, a[i + fromRowIndex], fromColumnIndex, N.min(b[i].length, cols - fromColumnIndex));
        }
    }

    @Override
    public ShortMatrix copy() {
        final short[][] c = new short[rows][];

        for (int i = 0; i < rows; i++) {
            c[i] = a[i].clone();
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        final short[][] c = new short[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        final short[][] c = new short[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new ShortMatrix(c);
    }

    public ShortMatrix extend(final int newRows, final int newCols) {
        return extend(newRows, newCols, f.SHORT_0);
    }

    public ShortMatrix extend(final int newRows, final int newCols, final short defaultValueForNewCell) {
        N.checkArgument(newRows >= 0, "The 'newRows' can't be negative %s", newRows);
        N.checkArgument(newCols >= 0, "The 'newCols' can't be negative %s", newCols);

        if (newRows <= rows && newCols <= cols) {
            return copy(0, newRows, 0, newCols);
        } else {
            final boolean fillDefaultValue = defaultValueForNewCell != f.SHORT_0;
            final short[][] b = new short[newRows][];

            for (int i = 0; i < newRows; i++) {
                b[i] = i < rows ? N.copyOf(a[i], newCols) : new short[newCols];

                if (fillDefaultValue) {
                    if (i >= rows) {
                        N.fill(b[i], defaultValueForNewCell);
                    } else if (cols < newCols) {
                        N.fill(b[i], cols, newCols, defaultValueForNewCell);
                    }
                }
            }

            return new ShortMatrix(b);
        }
    }

    public ShortMatrix extend(final int toUp, final int toDown, final int toLeft, final int toRight) {
        return extend(toUp, toDown, toLeft, toRight, f.SHORT_0);
    }

    public ShortMatrix extend(final int toUp, final int toDown, final int toLeft, final int toRight, final short defaultValueForNewCell) {
        N.checkArgument(toUp >= 0, "The 'toUp' can't be negative %s", toUp);
        N.checkArgument(toDown >= 0, "The 'toDown' can't be negative %s", toDown);
        N.checkArgument(toLeft >= 0, "The 'toLeft' can't be negative %s", toLeft);
        N.checkArgument(toRight >= 0, "The 'toRight' can't be negative %s", toRight);

        if (toUp == 0 && toDown == 0 && toLeft == 0 && toRight == 0) {
            return copy();
        } else {
            final int newRows = toUp + rows + toDown;
            final int newCols = toLeft + cols + toRight;
            final boolean fillDefaultValue = defaultValueForNewCell != f.SHORT_0;
            final short[][] b = new short[newRows][newCols];

            for (int i = 0; i < newRows; i++) {
                if (i >= toUp && i < toUp + rows) {
                    N.copy(a[i - toUp], 0, b[i], toLeft, cols);
                }

                if (fillDefaultValue) {
                    if (i < toUp || i >= toUp + rows) {
                        N.fill(b[i], defaultValueForNewCell);
                    } else if (cols < newCols) {
                        if (toLeft > 0) {
                            N.fill(b[i], 0, toLeft, defaultValueForNewCell);
                        }

                        if (toRight > 0) {
                            N.fill(b[i], cols + toLeft, newCols, defaultValueForNewCell);
                        }
                    }
                }
            }

            return new ShortMatrix(b);
        }
    }

    public void reverseH() {
        for (int i = 0; i < rows; i++) {
            N.reverse(a[i]);
        }
    }

    public void reverseV() {
        for (int j = 0; j < cols; j++) {
            short tmp = 0;
            for (int l = 0, h = rows - 1; l < h;) {
                tmp = a[l][j];
                a[l++][j] = a[h][j];
                a[h--][j] = tmp;
            }
        }
    }

    /**
     * 
     * @return
     * @see IntMatrix#flipH()
     */
    public ShortMatrix flipH() {
        final ShortMatrix res = this.copy();
        res.reverseH();
        return res;
    }

    /**
     * 
     * @return
     * @see IntMatrix#flipV()
     */
    public ShortMatrix flipV() {
        final ShortMatrix res = this.copy();
        res.reverseV();
        return res;
    }

    @Override
    public ShortMatrix rotate90() {
        final short[][] c = new short[cols][rows];

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

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix rotate180() {
        final short[][] c = new short[rows][];

        for (int i = 0; i < rows; i++) {
            c[i] = a[rows - i - 1].clone();
            N.reverse(c[i]);
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix rotate270() {
        final short[][] c = new short[cols][rows];

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

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix transpose() {
        final short[][] c = new short[cols][rows];

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

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix reshape(final int newRows, final int newCols) {
        final short[][] c = new short[newRows][newCols];

        if (newRows == 0 || newCols == 0 || N.isNullOrEmpty(a)) {
            return new ShortMatrix(c);
        }

        if (a.length == 1) {
            final short[] a0 = a[0];

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

        return new ShortMatrix(c);
    }

    /**
     * Repeat elements <code>rowRepeats</code> times in row direction and <code>colRepeats</code> times in column direction.
     * 
     * @param rowRepeats
     * @param colRepeats
     * @return a new matrix
     * @see IntMatrix#repelem(int, int)
     */
    @Override
    public ShortMatrix repelem(final int rowRepeats, final int colRepeats) {
        N.checkArgument(rowRepeats > 0 && colRepeats > 0, "rowRepeats=%s and colRepeats=%s must be bigger than 0", rowRepeats, colRepeats);

        final short[][] c = new short[rows * rowRepeats][cols * colRepeats];

        for (int i = 0; i < rows; i++) {
            final short[] fr = c[i * rowRepeats];

            for (int j = 0; j < cols; j++) {
                N.copy(Array.repeat(a[i][j], colRepeats), 0, fr, j * colRepeats, colRepeats);
            }

            for (int k = 1; k < rowRepeats; k++) {
                N.copy(fr, 0, c[i * rowRepeats + k], 0, fr.length);
            }
        }

        return new ShortMatrix(c);
    }

    /**
     * Repeat this matrix <code>rowRepeats</code> times in row direction and <code>colRepeats</code> times in column direction.
     * 
     * @param rowRepeats
     * @param colRepeats
     * @return a new matrix
     * @see IntMatrix#repmat(int, int)
     */
    @Override
    public ShortMatrix repmat(final int rowRepeats, final int colRepeats) {
        N.checkArgument(rowRepeats > 0 && colRepeats > 0, "rowRepeats=%s and colRepeats=%s must be bigger than 0", rowRepeats, colRepeats);

        final short[][] c = new short[rows * rowRepeats][cols * colRepeats];

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

        return new ShortMatrix(c);
    }

    @Override
    public ShortList flatten() {
        final short[] c = new short[rows * cols];

        for (int i = 0; i < rows; i++) {
            N.copy(a[i], 0, c, i * cols, cols);
        }

        return ShortList.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#vstack(IntMatrix)
     */
    public ShortMatrix vstack(final ShortMatrix b) {
        N.checkArgument(this.cols == b.cols, "The count of column in this matrix and the specified matrix are not equals");

        final short[][] c = new short[this.rows + b.rows][];
        int j = 0;

        for (int i = 0; i < rows; i++) {
            c[j++] = a[i].clone();
        }

        for (int i = 0; i < b.rows; i++) {
            c[j++] = b.a[i].clone();
        }

        return ShortMatrix.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#hstack(IntMatrix)
     */
    public ShortMatrix hstack(final ShortMatrix b) {
        N.checkArgument(this.rows == b.rows, "The count of row in this matrix and the specified matrix are not equals");

        final short[][] c = new short[rows][cols + b.cols];

        for (int i = 0; i < rows; i++) {
            N.copy(a[i], 0, c[i], 0, cols);
            N.copy(b.a[i], 0, c[i], cols, b.cols);
        }

        return ShortMatrix.of(c);
    }

    public ShortMatrix add(final ShortMatrix b) {
        N.checkArgument(this.rows == b.rows && this.cols == b.cols, "The 'n' and length are not equal");

        final short[][] c = new short[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = (short) (a[i][j] + b.a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = (short) (a[i][j] + b.a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (

                        int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = (short) (a[i][j] + b.a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = (short) (a[i][j] + b.a[i][j]);
                    }
                }
            }
        }

        return new ShortMatrix(c);
    }

    public ShortMatrix subtract(final ShortMatrix b) {
        N.checkArgument(this.rows == b.rows && this.cols == b.cols, "The 'n' and length are not equal");

        final short[][] c = new short[rows][cols];

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < cols; j++) {
                            c[i][j] = (short) (a[i][j] - b.a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < rows; i++) {
                            c[i][j] = (short) (a[i][j] - b.a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (

                        int i = 0; i < rows; i++) {
                    for (int j = 0; j < cols; j++) {
                        c[i][j] = (short) (a[i][j] - b.a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < cols; j++) {
                    for (int i = 0; i < rows; i++) {
                        c[i][j] = (short) (a[i][j] - b.a[i][j]);
                    }
                }
            }
        }

        return new ShortMatrix(c);
    }

    public ShortMatrix multiply(final ShortMatrix b) {
        N.checkArgument(this.cols == b.rows, "Illegal matrix dimensions");

        final short[][] c = new short[rows][b.cols];
        final short[][] a2 = b.a;

        if (isParallelable(b.cols)) {
            if (N.min(rows, cols, b.cols) == rows) {
                if (N.min(cols, b.cols) == cols) {
                    IntStream.range(0, rows).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int i) {
                            for (int k = 0; k < cols; k++) {
                                for (int j = 0; j < b.cols; j++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                } else {
                    IntStream.range(0, rows).parallel().forEach(new IntConsumer() {

                        @Override
                        public void accept(final int i) {
                            for (int j = 0; j < b.cols; j++) {
                                for (int k = 0; k < cols; k++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                }
            } else if (N.min(rows, cols, b.cols) == cols) {
                if (N.min(rows, b.cols) == rows) {
                    IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int k) {
                            for (int i = 0; i < rows; i++) {
                                for (int j = 0; j < b.cols; j++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                } else {
                    IntStream.range(0, cols).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int k) {
                            for (int j = 0; j < b.cols; j++) {
                                for (int i = 0; i < rows; i++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                }
            } else {
                if (N.min(rows, cols) == rows) {
                    IntStream.range(0, b.cols).parallel().forEach(new IntConsumer() {

                        @Override
                        public void accept(final int j) {
                            for (int i = 0; i < rows; i++) {
                                for (int k = 0; k < cols; k++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                } else {
                    IntStream.range(0, b.cols).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int j) {
                            for (int k = 0; k < cols; k++) {
                                for (int i = 0; i < rows; i++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                }
            }
        } else {
            if (N.min(rows, cols, b.cols) == rows) {
                if (N.min(cols, b.cols) == cols) {
                    for (

                            int i = 0; i < rows; i++) {
                        for (int k = 0; k < cols; k++) {
                            for (int j = 0; j < b.cols; j++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                } else {
                    for (int i = 0; i < rows; i++) {
                        for (int j = 0; j < b.cols; j++) {
                            for (int k = 0; k < cols; k++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                }
            } else if (N.min(rows, cols, b.cols) == cols) {
                if (N.min(rows, b.cols) == rows) {
                    for (int k = 0; k < cols; k++) {
                        for (int i = 0; i < rows; i++) {
                            for (int j = 0; j < b.cols; j++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                } else {
                    for (int k = 0; k < cols; k++) {
                        for (int j = 0; j < b.cols; j++) {
                            for (int i = 0; i < rows; i++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                }
            } else {
                if (N.min(rows, cols) == rows) {
                    for (int j = 0; j < b.cols; j++) {
                        for (int i = 0; i < rows; i++) {
                            for (int k = 0; k < cols; k++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                } else {
                    for (int j = 0; j < b.cols; j++) {
                        for (int k = 0; k < cols; k++) {
                            for (int i = 0; i < rows; i++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                }
            }
        }

        return new ShortMatrix(c);
    }

    public Matrix<Short> boxed() {
        final Short[][] c = new Short[rows][cols];

        if (rows <= cols) {
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < cols; j++) {
                for (int i = 0; i < rows; i++) {
                    c[i][j] = a[i][j];
                }
            }
        }

        return new Matrix<>(c);
    }

    public IntMatrix toIntMatrix() {
        return IntMatrix.from(a);
    }

    public LongMatrix toLongMatrix() {
        final long[][] c = new long[rows][cols];

        if (rows <= cols) {
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < cols; j++) {
                for (int i = 0; i < rows; i++) {
                    c[i][j] = a[i][j];
                }
            }
        }

        return new LongMatrix(c);
    }

    public FloatMatrix toFloatMatrix() {
        final float[][] c = new float[rows][cols];

        if (rows <= cols) {
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < cols; j++) {
                for (int i = 0; i < rows; i++) {
                    c[i][j] = a[i][j];
                }
            }
        }

        return new FloatMatrix(c);
    }

    public DoubleMatrix toDoubleMatrix() {
        final double[][] c = new double[rows][cols];

        if (rows <= cols) {
            for (int i = 0; i < rows; i++) {
                for (int j = 0; j < cols; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < cols; j++) {
                for (int i = 0; i < rows; i++) {
                    c[i][j] = a[i][j];
                }
            }
        }

        return new DoubleMatrix(c);
    }

    public <E extends Exception> ShortMatrix zipWith(final ShortMatrix matrixB, final Try.ShortBiFunction<Short, E> zipFunction) throws E {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final short[][] result = new short[rows][cols];
        final short[][] b = matrixB.a;

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {

                    @Override
                    public void accept(final int j) throws E {
                        for (int i = 0; i < rows; i++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (

                        int i = 0; i < rows; i++) {
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

        return new ShortMatrix(result);
    }

    public <E extends Exception> ShortMatrix zipWith(final ShortMatrix matrixB, final ShortMatrix matrixC, final Try.ShortTriFunction<Short, E> zipFunction)
            throws E {
        N.checkArgument(isSameShape(matrixB) && isSameShape(matrixC), "Can't zip three matrices which have different shape.");

        final short[][] result = new short[rows][cols];
        final short[][] b = matrixB.a;
        final short[][] c = matrixC.a;

        if (isParallelable()) {
            if (rows <= cols) {
                IntStream.range(0, rows).parallel().forEach(new Try.IntConsumer<E>() {
                    @Override
                    public void accept(final int i) throws E {
                        for (int j = 0; j < cols; j++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, cols).parallel().forEach(new Try.IntConsumer<E>() {

                    @Override
                    public void accept(final int j) throws E {
                        for (int i = 0; i < rows; i++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                        }
                    }
                });
            }
        } else {
            if (rows <= cols) {
                for (

                        int i = 0; i < rows; i++) {
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

        return new ShortMatrix(result);
    }

    /**
     * 
     * @return a stream composed by elements on the diagonal line from left up to right down.
     */
    @Override
    public ShortStream streamLU2RD() {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ShortIteratorEx() {
            private final int toIndex = rows;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short nextShort() {
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
    public ShortStream streamRU2LD() {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ShortIteratorEx() {
            private final int toIndex = rows;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public short nextShort() {
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
    public ShortStream streamH() {
        return streamH(0, rows);
    }

    @Override
    public ShortStream streamH(final int rowIndex) {
        return streamH(rowIndex, rowIndex + 1);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a stream based on the order of row.
     */
    @Override
    public ShortStream streamH(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ShortIteratorEx() {
            private int i = fromRowIndex;
            private int j = 0;

            @Override
            public boolean hasNext() {
                return i < toRowIndex;
            }

            @Override
            public short nextShort() {
                if (i >= toRowIndex) {
                    throw new NoSuchElementException();
                }

                final short result = a[i][j++];

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
            public short[] toArray() {
                final int len = (int) count();
                final short[] c = new short[len];

                for (int k = 0; k < len; k++) {
                    c[k] = a[i][j++];

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
    public ShortStream streamV() {
        return streamV(0, cols);
    }

    @Override
    public ShortStream streamV(final int columnIndex) {
        return streamV(columnIndex, columnIndex + 1);
    }

    /**
     * 
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a stream based on the order of column.
     */
    @Override
    @Beta
    public ShortStream streamV(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ShortIteratorEx() {
            private int i = 0;
            private int j = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return j < toColumnIndex;
            }

            @Override
            public short nextShort() {
                if (j >= toColumnIndex) {
                    throw new NoSuchElementException();
                }

                final short result = a[i++][j];

                if (i >= rows) {
                    i = 0;
                    j++;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toColumnIndex - j) * ShortMatrix.this.rows * 1L - i) {
                    i = 0;
                    j = toColumnIndex;
                } else {
                    i += (n + i) % ShortMatrix.this.rows;
                    j += (n + i) / ShortMatrix.this.rows;
                }
            }

            @Override
            public long count() {
                return (toColumnIndex - j) * rows - i;
            }

            @Override
            public short[] toArray() {
                final int len = (int) count();
                final short[] c = new short[len];

                for (int k = 0; k < len; k++) {
                    c[k] = a[i++][j];

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
    public Stream<ShortStream> streamR() {
        return streamR(0, rows);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    @Override
    public Stream<ShortStream> streamR(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ObjIteratorEx<ShortStream>() {
            private final int toIndex = toRowIndex;
            private int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return ShortStream.of(a[cursor++]);
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
    public Stream<ShortStream> streamC() {
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
    public Stream<ShortStream> streamC(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ObjIteratorEx<ShortStream>() {
            private final int toIndex = toColumnIndex;
            private volatile int cursor = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public ShortStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return ShortStream.of(new ShortIteratorEx() {
                    private final int columnIndex = cursor++;
                    private final int toIndex2 = rows;
                    private int cursor2 = 0;

                    @Override
                    public boolean hasNext() {
                        return cursor2 < toIndex2;
                    }

                    @Override
                    public short nextShort() {
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
    protected int length(short[] a) {
        return a == null ? 0 : a.length;
    }

    public <E extends Exception> void forEach(final Try.ShortConsumer<E> action) throws E {
        forEach(0, rows, 0, cols, action);
    }

    public <E extends Exception> void forEach(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex,
            final Try.ShortConsumer<E> action) throws E {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            for (int j = fromColumnIndex; j < toColumnIndex; j++) {
                action.accept(a[i][j]);
            }
        }
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

        if (obj instanceof ShortMatrix) {
            final ShortMatrix another = (ShortMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
