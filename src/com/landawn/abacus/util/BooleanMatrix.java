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
import com.landawn.abacus.util.function.BooleanBiFunction;
import com.landawn.abacus.util.function.BooleanFunction;
import com.landawn.abacus.util.function.BooleanTriFunction;
import com.landawn.abacus.util.function.BooleanUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.ExIterator;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class BooleanMatrix extends AbstractMatrix<boolean[], BooleanList, Stream<Boolean>, Stream<Stream<Boolean>>, BooleanMatrix> {
    static final BooleanMatrix EMPTY_BOOLEAN_MATRIX = new BooleanMatrix(new boolean[0][0]);

    public BooleanMatrix(final boolean[][] a) {
        super(a == null ? new boolean[0][0] : a);
    }

    public static BooleanMatrix empty() {
        return EMPTY_BOOLEAN_MATRIX;
    }

    @SafeVarargs
    public static BooleanMatrix of(final boolean[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_BOOLEAN_MATRIX : new BooleanMatrix(a);
    }

    public static BooleanMatrix random(final int len) {
        return new BooleanMatrix(new boolean[][] { BooleanList.random(len).array() });
    }

    public static BooleanMatrix repeat(final boolean val, final int len) {
        return new BooleanMatrix(new boolean[][] { Array.repeat(val, len) });
    }

    public static BooleanMatrix diagonalLU2RD(final boolean[] leftUp2RighDownDiagonal) {
        return diagonal(leftUp2RighDownDiagonal, null);
    }

    public static BooleanMatrix diagonalRU2LD(final boolean[] rightUp2LeftDownDiagonal) {
        return diagonal(null, rightUp2LeftDownDiagonal);
    }

    public static BooleanMatrix diagonal(final boolean[] leftUp2RighDownDiagonal, boolean[] rightUp2LeftDownDiagonal) {
        N.checkArgument(
                N.isNullOrEmpty(leftUp2RighDownDiagonal) || N.isNullOrEmpty(rightUp2LeftDownDiagonal)
                        || leftUp2RighDownDiagonal.length == rightUp2LeftDownDiagonal.length,
                "The length of 'leftUp2RighDownDiagonal' and 'rightUp2LeftDownDiagonal' must be same");

        if (N.isNullOrEmpty(leftUp2RighDownDiagonal)) {
            if (N.isNullOrEmpty(rightUp2LeftDownDiagonal)) {
                return empty();
            } else {
                final int len = rightUp2LeftDownDiagonal.length;
                final boolean[][] c = new boolean[len][len];

                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }

                return new BooleanMatrix(c);
            }
        } else {
            final int len = leftUp2RighDownDiagonal.length;
            final boolean[][] c = new boolean[len][len];

            for (int i = 0; i < len; i++) {
                c[i][i] = leftUp2RighDownDiagonal[i];
            }

            if (N.notNullOrEmpty(rightUp2LeftDownDiagonal)) {
                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }
            }

            return new BooleanMatrix(c);
        }
    }

    public boolean[][] array() {
        return a;
    }

    public boolean get(final int i, final int j) {
        return a[i][j];
    }

    public boolean get(final IntPair point) {
        return a[point._1][point._2];
    }

    public void set(final int i, final int j, final boolean val) {
        a[i][j] = val;
    }

    public void set(final IntPair point, final boolean val) {
        a[point._1][point._2] = val;
    }

    public OptionalBoolean upOf(final int i, final int j) {
        return i == 0 ? OptionalBoolean.empty() : OptionalBoolean.of(a[i - 1][j]);
    }

    public OptionalBoolean downOf(final int i, final int j) {
        return i == n - 1 ? OptionalBoolean.empty() : OptionalBoolean.of(a[i + 1][j]);
    }

    public OptionalBoolean leftOf(final int i, final int j) {
        return j == 0 ? OptionalBoolean.empty() : OptionalBoolean.of(a[i][j - 1]);
    }

    public OptionalBoolean rightOf(final int i, final int j) {
        return j == m - 1 ? OptionalBoolean.empty() : OptionalBoolean.of(a[i][j + 1]);
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

    public boolean[] row(final int rowIndex) {
        N.checkArgument(rowIndex >= 0 && rowIndex < n, "Invalid row Index: %s", rowIndex);

        return a[rowIndex];
    }

    public boolean[] column(final int columnIndex) {
        N.checkArgument(columnIndex >= 0 && columnIndex < m, "Invalid column Index: %s", columnIndex);

        final boolean[] c = new boolean[n];

        for (int i = 0; i < n; i++) {
            c[i] = a[i][columnIndex];
        }

        return c;
    }

    public void setRow(int rowIndex, boolean[] row) {
        N.checkArgument(row.length == m, "The size of the specified row doesn't match the length of column");

        N.copy(row, 0, a[rowIndex], 0, m);
    }

    public void setColumn(int columnIndex, boolean[] column) {
        N.checkArgument(column.length == n, "The size of the specified column doesn't match the length of row");

        for (int i = 0; i < n; i++) {
            a[i][columnIndex] = column[i];
        }
    }

    public void updateRow(int rowIndex, BooleanUnaryOperator func) {
        for (int i = 0; i < m; i++) {
            a[rowIndex][i] = func.applyAsBoolean(a[rowIndex][i]);
        }
    }

    public void updateColumn(int columnIndex, BooleanUnaryOperator func) {
        for (int i = 0; i < n; i++) {
            a[i][columnIndex] = func.applyAsBoolean(a[i][columnIndex]);
        }
    }

    public boolean[] getLU2RD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        final boolean[] res = new boolean[n];

        for (int i = 0; i < n; i++) {
            res[i] = a[i][i];
        }

        return res;
    }

    public void setLU2RD(final boolean[] diagonal) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);
        N.checkArgument(diagonal.length >= n, "The length of specified array is less than n=%s", n);

        for (int i = 0; i < n; i++) {
            a[i][i] = diagonal[i];
        }
    }

    public void updateLU2RD(final BooleanUnaryOperator func) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        for (int i = 0; i < n; i++) {
            a[i][i] = func.applyAsBoolean(a[i][i]);
        }
    }

    public boolean[] getRU2LD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        final boolean[] res = new boolean[n];

        for (int i = 0; i < n; i++) {
            res[i] = a[i][m - i - 1];
        }

        return res;
    }

    public void setRU2LD(final boolean[] diagonal) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);
        N.checkArgument(diagonal.length >= n, "The length of specified array is less than n=%s", n);

        for (int i = 0; i < n; i++) {
            a[i][m - i - 1] = diagonal[i];
        }
    }

    public void updateRU2LD(final BooleanUnaryOperator func) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        for (int i = 0; i < n; i++) {
            a[i][m - i - 1] = func.applyAsBoolean(a[i][m - i - 1]);
        }
    }

    public void updateAll(final BooleanUnaryOperator func) {
        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            a[i][j] = func.applyAsBoolean(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {

                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            a[i][j] = func.applyAsBoolean(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (

                        int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = func.applyAsBoolean(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        a[i][j] = func.applyAsBoolean(a[i][j]);
                    }
                }
            }
        }
    }

    public BooleanMatrix map(final BooleanUnaryOperator func) {
        final boolean[][] c = new boolean[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = func.applyAsBoolean(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = func.applyAsBoolean(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsBoolean(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = func.applyAsBoolean(a[i][j]);
                    }
                }
            }
        }

        return BooleanMatrix.of(c);
    }

    public <T> Matrix<T> mapToObj(final Class<T> cls, final BooleanFunction<? extends T> func) {
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
                for (int i = 0; i < n; i++) {
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

    public void fill(final boolean val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void fill(final boolean[][] b) {
        fill(0, 0, b);
    }

    public void fill(final int fromRowIndex, final int fromColumnIndex, final boolean[][] b) {
        N.checkFromToIndex(fromRowIndex, n, n);
        N.checkFromToIndex(fromColumnIndex, m, m);

        for (int i = 0, minLen = N.min(n - fromRowIndex, b.length); i < minLen; i++) {
            N.copy(b[i], 0, a[i + fromRowIndex], fromColumnIndex, N.min(b[i].length, m - fromColumnIndex));
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalBoolean min() {
    //        if (isEmpty()) {
    //            return OptionalBoolean.empty();
    //        }
    //
    //        boolean candicate = Boolean.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalBoolean.of(candicate);
    //    }
    //
    //    public OptionalBoolean max() {
    //        if (isEmpty()) {
    //            return OptionalBoolean.empty();
    //        }
    //
    //        boolean candicate = Boolean.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalBoolean.of(candicate);
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
    //    public BooleanList row(final int i) {
    //        return BooleanList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public BooleanList column(final int j) {
    //        return BooleanList.of(column2(j));
    //    }

    @Override
    public BooleanMatrix copy() {
        final boolean[][] c = new boolean[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        final boolean[][] c = new boolean[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        final boolean[][] c = new boolean[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix rotate90() {
        final boolean[][] c = new boolean[m][n];

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

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix rotate180() {
        final boolean[][] c = new boolean[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix rotate270() {
        final boolean[][] c = new boolean[m][n];

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

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix transpose() {
        final boolean[][] c = new boolean[m][n];

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

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix reshape(final int n, final int m) {
        final boolean[][] c = new boolean[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new BooleanMatrix(c);
        }

        if (a.length == 1) {
            final boolean[] a0 = a[0];

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

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanList flatten() {
        final boolean[] c = new boolean[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return BooleanList.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#vstack(IntMatrix)
     */
    public BooleanMatrix vstack(final BooleanMatrix b) {
        N.checkArgument(this.m == b.m, "The count of column in this matrix and the specified matrix are not equals");

        final boolean[][] c = new boolean[this.n + b.n][];
        int j = 0;

        for (int i = 0; i < n; i++) {
            c[j++] = a[i].clone();
        }

        for (int i = 0; i < b.n; i++) {
            c[j++] = b.a[i].clone();
        }

        return BooleanMatrix.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#hstack(IntMatrix)
     */
    public BooleanMatrix hstack(final BooleanMatrix b) {
        N.checkArgument(this.n == b.n, "The count of row in this matrix and the specified matrix are not equals");

        final boolean[][] c = new boolean[n][m + b.m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c[i], 0, m);
            N.copy(b.a[i], 0, c[i], m, b.m);
        }

        return BooleanMatrix.of(c);
    }

    public Matrix<Boolean> boxed() {
        final Boolean[][] c = new Boolean[n][m];

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

    public BooleanMatrix zipWith(final BooleanMatrix matrixB, final BooleanBiFunction<Boolean> zipFunction) {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final boolean[][] result = new boolean[n][m];
        final boolean[][] b = matrixB.a;

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

        return new BooleanMatrix(result);
    }

    public BooleanMatrix zipWith(final BooleanMatrix matrixB, final BooleanMatrix matrixC, final BooleanTriFunction<Boolean> zipFunction) {
        N.checkArgument(isSameShape(matrixB) && isSameShape(matrixC), "Can't zip three matrices which have different shape.");

        final boolean[][] result = new boolean[n][m];
        final boolean[][] b = matrixB.a;
        final boolean[][] c = matrixC.a;

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

        return new BooleanMatrix(result);
    }

    /**
     * 
     * @return a stream composed by elements on the diagonal line from left up to right down.
     */
    @Override
    public Stream<Boolean> streamLU2RD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Boolean>() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Boolean next() {
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
    public Stream<Boolean> streamRU2LD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Boolean>() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Boolean next() {
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
    public Stream<Boolean> streamH() {
        return streamH(0, n);
    }

    @Override
    public Stream<Boolean> streamH(final int rowIndex) {
        return streamH(rowIndex, rowIndex + 1);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a stream based on the order of row.
     */
    @Override
    public Stream<Boolean> streamH(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Boolean>() {
            private int i = fromRowIndex;
            private int j = 0;

            @Override
            public boolean hasNext() {
                return i < toRowIndex;
            }

            @Override
            public Boolean next() {
                if (i >= toRowIndex) {
                    throw new NoSuchElementException();
                }

                final boolean result = a[i][j++];

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
            public <A> A[] toArray(A[] c) {
                final int len = (int) count();

                if (c.length < len) {
                    c = N.copyOf(c, len);
                }

                for (int k = 0; k < len; k++) {
                    c[k] = (A) (Boolean) a[i][j++];

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
    public Stream<Boolean> streamV() {
        return streamV(0, m);
    }

    @Override
    public Stream<Boolean> streamV(final int columnIndex) {
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
    public Stream<Boolean> streamV(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Boolean>() {
            private int i = 0;
            private int j = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return j < toColumnIndex;
            }

            @Override
            public Boolean next() {
                if (j >= toColumnIndex) {
                    throw new NoSuchElementException();
                }

                final boolean result = a[i++][j];

                if (i >= n) {
                    i = 0;
                    j++;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toColumnIndex - j) * BooleanMatrix.this.n * 1L - i) {
                    i = 0;
                    j = toColumnIndex;
                } else {
                    i += (n + i) % BooleanMatrix.this.n;
                    j += (n + i) / BooleanMatrix.this.n;
                }
            }

            @Override
            public long count() {
                return (toColumnIndex - j) * n - i;
            }

            @Override
            public <A> A[] toArray(A[] c) {
                final int len = (int) count();

                if (c.length < len) {
                    c = N.copyOf(c, len);
                }

                for (int k = 0; k < len; k++) {
                    c[k] = (A) (Boolean) a[i++][j];

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
    public Stream<Stream<Boolean>> streamR() {
        return streamR(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    @Override
    public Stream<Stream<Boolean>> streamR(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Stream<Boolean>>() {
            private final int toIndex = toRowIndex;
            private int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<Boolean> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return Stream.from(a[cursor++]);
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
    public Stream<Stream<Boolean>> streamC() {
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
    public Stream<Stream<Boolean>> streamC(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<Stream<Boolean>>() {
            private final int toIndex = toColumnIndex;
            private volatile int cursor = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public Stream<Boolean> next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return Stream.of(new ExIterator<Boolean>() {
                    private final int columnIndex = cursor++;
                    private final int toIndex2 = n;
                    private int cursor2 = 0;

                    @Override
                    public boolean hasNext() {
                        return cursor2 < toIndex2;
                    }

                    @Override
                    public Boolean next() {
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
    protected int length(boolean[] a) {
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

        if (obj instanceof BooleanMatrix) {
            final BooleanMatrix another = (BooleanMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
