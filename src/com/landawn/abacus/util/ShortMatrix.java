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
import com.landawn.abacus.util.Pair.IntPair;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.ShortBiFunction;
import com.landawn.abacus.util.function.ShortFunction;
import com.landawn.abacus.util.function.ShortPredicate;
import com.landawn.abacus.util.function.ShortTriFunction;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.stream.ExIterator;
import com.landawn.abacus.util.stream.ExShortIterator;
import com.landawn.abacus.util.stream.IntStream;
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

    public short[][] array() {
        return a;
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
        return i == n - 1 ? OptionalShort.empty() : OptionalShort.of(a[i + 1][j]);
    }

    public OptionalShort leftOf(final int i, final int j) {
        return j == 0 ? OptionalShort.empty() : OptionalShort.of(a[i][j - 1]);
    }

    public OptionalShort rightOf(final int i, final int j) {
        return j == m - 1 ? OptionalShort.empty() : OptionalShort.of(a[i][j + 1]);
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
        final IntPair right = j == m - 1 ? null : IntPair.of(i, j + 1);
        final IntPair down = i == n - 1 ? null : IntPair.of(i + 1, j);
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
        final IntPair right = j == m - 1 ? null : IntPair.of(i, j + 1);
        final IntPair down = i == n - 1 ? null : IntPair.of(i + 1, j);
        final IntPair left = j == 0 ? null : IntPair.of(i, j - 1);

        final IntPair leftUp = i > 0 && j > 0 ? IntPair.of(i - 1, j - 1) : null;
        final IntPair rightUp = i > 0 && j < m - 1 ? IntPair.of(i - 1, j + 1) : null;
        final IntPair rightDown = i < n - 1 && j < m - 1 ? IntPair.of(j + 1, j + 1) : null;
        final IntPair leftDown = i < n - 1 && j > 0 ? IntPair.of(i + 1, j - 1) : null;

        return Stream.of(leftUp, up, rightUp, right, rightDown, down, leftDown, left);
    }

    public short[] row(final int rowIndex) {
        N.checkArgument(rowIndex >= 0 && rowIndex < n, "Invalid row Index: %s", rowIndex);

        return a[rowIndex];
    }

    public short[] column(final int columnIndex) {
        N.checkArgument(columnIndex >= 0 && columnIndex < m, "Invalid column Index: %s", columnIndex);

        final short[] c = new short[n];

        for (int i = 0; i < n; i++) {
            c[i] = a[i][columnIndex];
        }

        return c;
    }

    public void setRow(int rowIndex, short[] row) {
        N.checkArgument(row.length == m, "The size of the specified row doesn't match the length of column");

        N.copy(row, 0, a[rowIndex], 0, m);
    }

    public void setColumn(int columnIndex, short[] column) {
        N.checkArgument(column.length == n, "The size of the specified column doesn't match the length of row");

        for (int i = 0; i < n; i++) {
            a[i][columnIndex] = column[i];
        }
    }

    public void updateRow(int rowIndex, ShortUnaryOperator func) {
        for (int i = 0; i < m; i++) {
            a[rowIndex][i] = func.applyAsShort(a[rowIndex][i]);
        }
    }

    public void updateColumn(int columnIndex, ShortUnaryOperator func) {
        for (int i = 0; i < n; i++) {
            a[i][columnIndex] = func.applyAsShort(a[i][columnIndex]);
        }
    }

    public short[] getLU2RD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        final short[] res = new short[n];

        for (int i = 0; i < n; i++) {
            res[i] = a[i][i];
        }

        return res;
    }

    public void setLU2RD(final short[] diagonal) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);
        N.checkArgument(diagonal.length >= n, "The length of specified array is less than n=%s", n);

        for (int i = 0; i < n; i++) {
            a[i][i] = diagonal[i];
        }
    }

    public void updateLU2RD(final ShortUnaryOperator func) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        for (int i = 0; i < n; i++) {
            a[i][i] = func.applyAsShort(a[i][i]);
        }
    }

    public short[] getRU2LD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        final short[] res = new short[n];

        for (int i = 0; i < n; i++) {
            res[i] = a[i][m - i - 1];
        }

        return res;
    }

    public void setRU2LD(final short[] diagonal) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);
        N.checkArgument(diagonal.length >= n, "The length of specified array is less than n=%s", n);

        for (int i = 0; i < n; i++) {
            a[i][m - i - 1] = diagonal[i];
        }
    }

    public void updateRU2LD(final ShortUnaryOperator func) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        for (int i = 0; i < n; i++) {
            a[i][m - i - 1] = func.applyAsShort(a[i][m - i - 1]);
        }
    }

    public void updateAll(final ShortUnaryOperator func) {
        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            a[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            a[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        a[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            }
        }
    }

    public void replaceIf(final ShortPredicate predicate, final short newValue) {
        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        a[i][j] = predicate.test(a[i][j]) ? newValue : a[i][j];
                    }
                }
            }
        }
    }

    public ShortMatrix map(final ShortUnaryOperator func) {
        final short[][] c = new short[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = func.applyAsShort(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            }
        }

        return ShortMatrix.of(c);
    }

    public <T> Matrix<T> mapToObj(final Class<T> cls, final ShortFunction<? extends T> func) {
        final T[][] c = N.newArray(N.newArray(cls, 0).getClass(), n);

        for (int i = 0; i < n; i++) {
            c[i] = N.newArray(cls, m);
        }

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = func.apply(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.apply(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = func.apply(a[i][j]);
                    }
                }
            }
        }

        return Matrix.of(c);
    }

    public void fill(final short val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void fill(final short[][] b) {
        fill(0, 0, b);
    }

    public void fill(final int fromRowIndex, final int fromColumnIndex, final short[][] b) {
        N.checkFromToIndex(fromRowIndex, n, n);
        N.checkFromToIndex(fromColumnIndex, m, m);

        for (int i = 0, minLen = N.min(n - fromRowIndex, b.length); i < minLen; i++) {
            N.copy(b[i], 0, a[i + fromRowIndex], fromColumnIndex, N.min(b[i].length, m - fromColumnIndex));
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalShort min() {
    //        if (isEmpty()) {
    //            return OptionalShort.empty();
    //        }
    //
    //        short candicate = Short.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalShort.of(candicate);
    //    }
    //
    //    public OptionalShort max() {
    //        if (isEmpty()) {
    //            return OptionalShort.empty();
    //        }
    //
    //        short candicate = Short.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalShort.of(candicate);
    //    }
    //
    //    public Long sum() {
    //        long sum = 0;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                sum += a[i][j];
    //            }
    //        }
    //
    //        return sum;
    //    }
    //
    //    public OptionalDouble average() {
    //        if (isEmpty()) {
    //            return OptionalDouble.empty();
    //        }
    //
    //        return OptionalDouble.of(sum() / count);
    //    }
    //
    //    @Override
    //    public ShortList row(final int i) {
    //        return ShortList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public ShortList column(final int j) {
    //        return ShortList.of(column2(j));
    //    }

    @Override
    public ShortMatrix copy() {
        final short[][] c = new short[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        final short[][] c = new short[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        final short[][] c = new short[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix rotate90() {
        final short[][] c = new short[m][n];

        if (n <= m) {
            for (int j = 0; j < n; j++) {
                for (int i = 0; i < m; i++) {
                    c[i][j] = a[n - j - 1][i];
                }
            }
        } else {
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i][j] = a[n - j - 1][i];
                }
            }
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix rotate180() {
        final short[][] c = new short[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix rotate270() {
        final short[][] c = new short[m][n];

        if (n <= m) {
            for (int j = 0; j < n; j++) {
                for (int i = 0; i < m; i++) {
                    c[i][j] = a[j][m - i - 1];
                }
            }
        } else {
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i][j] = a[j][m - i - 1];
                }
            }
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix transpose() {
        final short[][] c = new short[m][n];

        if (n <= m) {
            for (int j = 0; j < n; j++) {
                for (int i = 0; i < m; i++) {
                    c[i][j] = a[j][i];
                }
            }
        } else {
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < n; j++) {
                    c[i][j] = a[j][i];
                }
            }
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix reshape(final int n, final int m) {
        final short[][] c = new short[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new ShortMatrix(c);
        }

        if (a.length == 1) {
            final short[] a0 = a[0];

            for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
                N.copy(a0, i * m, c[i], 0, (int) N.min(m, count - i * m));
            }
        } else {
            long cnt = 0;

            for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
                for (int j = 0, col = (int) N.min(m, count - i * m); j < col; j++, cnt++) {
                    c[i][j] = a[(int) (cnt / this.m)][(int) (cnt % this.m)];
                }
            }
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortList flatten() {
        final short[] c = new short[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
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
        N.checkArgument(this.m == b.m, "The count of column in this matrix and the specified matrix are not equals");

        final short[][] c = new short[this.n + b.n][];
        int j = 0;

        for (int i = 0; i < n; i++) {
            c[j++] = a[i].clone();
        }

        for (int i = 0; i < b.n; i++) {
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
        N.checkArgument(this.n == b.n, "The count of row in this matrix and the specified matrix are not equals");

        final short[][] c = new short[n][m + b.m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c[i], 0, m);
            N.copy(b.a[i], 0, c[i], m, b.m);
        }

        return ShortMatrix.of(c);
    }

    public ShortMatrix add(final ShortMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final short[][] c = new short[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = (short) (a[i][j] + b.a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = (short) (a[i][j] + b.a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (short) (a[i][j] + b.a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = (short) (a[i][j] + b.a[i][j]);
                    }
                }
            }
        }

        return new ShortMatrix(c);
    }

    public ShortMatrix subtract(final ShortMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final short[][] c = new short[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = (short) (a[i][j] - b.a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = (short) (a[i][j] - b.a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (short) (a[i][j] - b.a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = (short) (a[i][j] - b.a[i][j]);
                    }
                }
            }
        }

        return new ShortMatrix(c);
    }

    public ShortMatrix multiply(final ShortMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final short[][] c = new short[n][b.m];
        final short[][] a2 = b.a;

        if (isParallelable(b.m)) {
            if (N.min(n, m, b.m) == n) {
                if (N.min(m, b.m) == m) {
                    IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int i) {
                            for (int k = 0; k < m; k++) {
                                for (int j = 0; j < b.m; j++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                } else {
                    IntStream.range(0, n).parallel().forEach(new IntConsumer() {

                        @Override
                        public void accept(final int i) {
                            for (int j = 0; j < b.m; j++) {
                                for (int k = 0; k < m; k++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                }
            } else if (N.min(n, m, b.m) == m) {
                if (N.min(n, b.m) == n) {
                    IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int k) {
                            for (int i = 0; i < n; i++) {
                                for (int j = 0; j < b.m; j++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                } else {
                    IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int k) {
                            for (int j = 0; j < b.m; j++) {
                                for (int i = 0; i < n; i++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                }
            } else {
                if (N.min(n, m) == n) {
                    IntStream.range(0, b.m).parallel().forEach(new IntConsumer() {

                        @Override
                        public void accept(final int j) {
                            for (int i = 0; i < n; i++) {
                                for (int k = 0; k < m; k++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                } else {
                    IntStream.range(0, b.m).parallel().forEach(new IntConsumer() {
                        @Override
                        public void accept(final int j) {
                            for (int k = 0; k < m; k++) {
                                for (int i = 0; i < n; i++) {
                                    c[i][j] += a[i][k] * a2[k][j];
                                }
                            }
                        }
                    });
                }
            }
        } else {
            if (N.min(n, m, b.m) == n) {
                if (N.min(m, b.m) == m) {
                    for (

                            int i = 0; i < n; i++) {
                        for (int k = 0; k < m; k++) {
                            for (int j = 0; j < b.m; j++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                } else {
                    for (int i = 0; i < n; i++) {
                        for (int j = 0; j < b.m; j++) {
                            for (int k = 0; k < m; k++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                }
            } else if (N.min(n, m, b.m) == m) {
                if (N.min(n, b.m) == n) {
                    for (int k = 0; k < m; k++) {
                        for (int i = 0; i < n; i++) {
                            for (int j = 0; j < b.m; j++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                } else {
                    for (int k = 0; k < m; k++) {
                        for (int j = 0; j < b.m; j++) {
                            for (int i = 0; i < n; i++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                }
            } else {
                if (N.min(n, m) == n) {
                    for (int j = 0; j < b.m; j++) {
                        for (int i = 0; i < n; i++) {
                            for (int k = 0; k < m; k++) {
                                c[i][j] += a[i][k] * a2[k][j];
                            }
                        }
                    }
                } else {
                    for (int j = 0; j < b.m; j++) {
                        for (int k = 0; k < m; k++) {
                            for (int i = 0; i < n; i++) {
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
        final Short[][] c = new Short[n][m];

        if (n <= m) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < m; j++) {
                for (int i = 0; i < n; i++) {
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
        final long[][] c = new long[n][m];

        if (n <= m) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < m; j++) {
                for (int i = 0; i < n; i++) {
                    c[i][j] = a[i][j];
                }
            }
        }

        return new LongMatrix(c);
    }

    public FloatMatrix toFloatMatrix() {
        final float[][] c = new float[n][m];

        if (n <= m) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < m; j++) {
                for (int i = 0; i < n; i++) {
                    c[i][j] = a[i][j];
                }
            }
        }

        return new FloatMatrix(c);
    }

    public DoubleMatrix toDoubleMatrix() {
        final double[][] c = new double[n][m];

        if (n <= m) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j];
                }
            }
        } else {
            for (int j = 0; j < m; j++) {
                for (int i = 0; i < n; i++) {
                    c[i][j] = a[i][j];
                }
            }
        }

        return new DoubleMatrix(c);
    }

    public ShortMatrix zipWith(final ShortMatrix matrixB, final ShortBiFunction<Short> zipFunction) {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final short[][] result = new short[n][m];
        final short[][] b = matrixB.a;

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        result[i][j] = zipFunction.apply(a[i][j], b[i][j]);
                    }
                }
            }
        }

        return new ShortMatrix(result);
    }

    public ShortMatrix zipWith(final ShortMatrix matrixB, final ShortMatrix matrixC, final ShortTriFunction<Short> zipFunction) {
        N.checkArgument(isSameShape(matrixB) && isSameShape(matrixC), "Can't zip three matrices which have different shape.");

        final short[][] result = new short[n][m];
        final short[][] b = matrixB.a;
        final short[][] c = matrixC.a;

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        result[i][j] = zipFunction.apply(a[i][j], b[i][j], c[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
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
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ExShortIterator() {
            private final int toIndex = n;
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
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ExShortIterator() {
            private final int toIndex = n;
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

                return a[cursor][n - ++cursor];
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
        return streamH(0, n);
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
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ExShortIterator() {
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

                if (j >= m) {
                    i++;
                    j = 0;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toRowIndex - i) * m * 1L - j) {
                    i = toRowIndex;
                    j = 0;
                } else {
                    i += (n + j) / m;
                    j += (n + j) % m;
                }
            }

            @Override
            public long count() {
                return (toRowIndex - i) * m * 1L - j;
            }

            @Override
            public short[] toArray() {
                final int len = (int) count();
                final short[] c = new short[len];

                for (int k = 0; k < len; k++) {
                    c[k] = a[i][j++];

                    if (j >= m) {
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
        return streamV(0, m);
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
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return ShortStream.empty();
        }

        return ShortStream.of(new ExShortIterator() {
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

                if (i >= n) {
                    i = 0;
                    j++;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toColumnIndex - j) * ShortMatrix.this.n * 1L - i) {
                    i = 0;
                    j = toColumnIndex;
                } else {
                    i += (n + i) % ShortMatrix.this.n;
                    j += (n + i) / ShortMatrix.this.n;
                }
            }

            @Override
            public long count() {
                return (toColumnIndex - j) * n - i;
            }

            @Override
            public short[] toArray() {
                final int len = (int) count();
                final short[] c = new short[len];

                for (int k = 0; k < len; k++) {
                    c[k] = a[i++][j];

                    if (i >= n) {
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
        return streamR(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    @Override
    public Stream<ShortStream> streamR(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<ShortStream>() {
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
        return streamC(0, m);
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
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<ShortStream>() {
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

                return ShortStream.of(new ExShortIterator() {
                    private final int columnIndex = cursor++;
                    private final int toIndex2 = n;
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
