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
import com.landawn.abacus.util.function.DoubleBiFunction;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleTriFunction;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.DoubleStream;
import com.landawn.abacus.util.stream.ExDoubleIterator;
import com.landawn.abacus.util.stream.ExIterator;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class DoubleMatrix extends AbstractMatrix<double[], DoubleList, DoubleStream, Stream<DoubleStream>, DoubleMatrix> {
    static final DoubleMatrix EMPTY_DOUBLE_MATRIX = new DoubleMatrix(new double[0][0]);

    public DoubleMatrix(final double[][] a) {
        super(a == null ? new double[0][0] : a);
    }

    public static DoubleMatrix empty() {
        return EMPTY_DOUBLE_MATRIX;
    }

    @SafeVarargs
    public static DoubleMatrix of(final double[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_DOUBLE_MATRIX : new DoubleMatrix(a);
    }

    @SafeVarargs
    public static DoubleMatrix from(final int[]... a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_DOUBLE_MATRIX;
        }

        final double[][] c = new double[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new DoubleMatrix(c);
    }

    @SafeVarargs
    public static DoubleMatrix from(final long[]... a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_DOUBLE_MATRIX;
        }

        final double[][] c = new double[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new DoubleMatrix(c);
    }

    @SafeVarargs
    public static DoubleMatrix from(final float[]... a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_DOUBLE_MATRIX;
        }

        final double[][] c = new double[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new DoubleMatrix(c);
    }

    public static DoubleMatrix random(final int len) {
        return new DoubleMatrix(new double[][] { DoubleList.random(len).array() });
    }

    public static DoubleMatrix repeat(final double val, final int len) {
        return new DoubleMatrix(new double[][] { Array.repeat(val, len) });
    }

    public static DoubleMatrix diagonalLU2RD(final double[] leftUp2RighDownDiagonal) {
        return diagonal(leftUp2RighDownDiagonal, null);
    }

    public static DoubleMatrix diagonalRU2LD(final double[] rightUp2LeftDownDiagonal) {
        return diagonal(null, rightUp2LeftDownDiagonal);
    }

    public static DoubleMatrix diagonal(final double[] leftUp2RighDownDiagonal, double[] rightUp2LeftDownDiagonal) {
        N.checkArgument(
                N.isNullOrEmpty(leftUp2RighDownDiagonal) || N.isNullOrEmpty(rightUp2LeftDownDiagonal)
                        || leftUp2RighDownDiagonal.length == rightUp2LeftDownDiagonal.length,
                "The length of 'leftUp2RighDownDiagonal' and 'rightUp2LeftDownDiagonal' must be same");

        if (N.isNullOrEmpty(leftUp2RighDownDiagonal)) {
            if (N.isNullOrEmpty(rightUp2LeftDownDiagonal)) {
                return empty();
            } else {
                final int len = rightUp2LeftDownDiagonal.length;
                final double[][] c = new double[len][len];

                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }

                return new DoubleMatrix(c);
            }
        } else {
            final int len = leftUp2RighDownDiagonal.length;
            final double[][] c = new double[len][len];

            for (int i = 0; i < len; i++) {
                c[i][i] = leftUp2RighDownDiagonal[i];
            }

            if (N.notNullOrEmpty(rightUp2LeftDownDiagonal)) {
                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftDownDiagonal[i];
                }
            }

            return new DoubleMatrix(c);
        }
    }

    public double[][] array() {
        return a;
    }

    public double get(final int i, final int j) {
        return a[i][j];
    }

    public double get(final IntPair point) {
        return a[point._1][point._2];
    }

    public void set(final int i, final int j, final double val) {
        a[i][j] = val;
    }

    public void set(final IntPair point, final double val) {
        a[point._1][point._2] = val;
    }

    public OptionalDouble upOf(final int i, final int j) {
        return i == 0 ? OptionalDouble.empty() : OptionalDouble.of(a[i - 1][j]);
    }

    public OptionalDouble downOf(final int i, final int j) {
        return i == n - 1 ? OptionalDouble.empty() : OptionalDouble.of(a[i + 1][j]);
    }

    public OptionalDouble leftOf(final int i, final int j) {
        return j == 0 ? OptionalDouble.empty() : OptionalDouble.of(a[i][j - 1]);
    }

    public OptionalDouble rightOf(final int i, final int j) {
        return j == m - 1 ? OptionalDouble.empty() : OptionalDouble.of(a[i][j + 1]);
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

    public double[] row(final int rowIndex) {
        N.checkArgument(rowIndex >= 0 && rowIndex < n, "Invalid row Index: %s", rowIndex);

        return a[rowIndex];
    }

    public double[] column(final int columnIndex) {
        N.checkArgument(columnIndex >= 0 && columnIndex < m, "Invalid column Index: %s", columnIndex);

        final double[] c = new double[n];

        for (int i = 0; i < n; i++) {
            c[i] = a[i][columnIndex];
        }

        return c;
    }

    public void setRow(int rowIndex, double[] row) {
        N.checkArgument(row.length == m, "The size of the specified row doesn't match the length of column");

        N.copy(row, 0, a[rowIndex], 0, m);
    }

    public void setColumn(int columnIndex, double[] column) {
        N.checkArgument(column.length == n, "The size of the specified column doesn't match the length of row");

        for (int i = 0; i < n; i++) {
            a[i][columnIndex] = column[i];
        }
    }

    public void updateRow(int rowIndex, DoubleUnaryOperator func) {
        for (int i = 0; i < m; i++) {
            a[rowIndex][i] = func.applyAsDouble(a[rowIndex][i]);
        }
    }

    public void updateColumn(int columnIndex, DoubleUnaryOperator func) {
        for (int i = 0; i < n; i++) {
            a[i][columnIndex] = func.applyAsDouble(a[i][columnIndex]);
        }
    }

    public double[] getLU2RD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        final double[] res = new double[n];

        for (int i = 0; i < n; i++) {
            res[i] = a[i][i];
        }

        return res;
    }

    public void setLU2RD(final double[] diagonal) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);
        N.checkArgument(diagonal.length >= n, "The length of specified array is less than n=%s", n);

        for (int i = 0; i < n; i++) {
            a[i][i] = diagonal[i];
        }
    }

    public void updateLU2RD(final DoubleUnaryOperator func) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        for (int i = 0; i < n; i++) {
            a[i][i] = func.applyAsDouble(a[i][i]);
        }
    }

    public double[] getRU2LD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        final double[] res = new double[n];

        for (int i = 0; i < n; i++) {
            res[i] = a[i][m - i - 1];
        }

        return res;
    }

    public void setRU2LD(final double[] diagonal) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);
        N.checkArgument(diagonal.length >= n, "The length of specified array is less than n=%s", n);

        for (int i = 0; i < n; i++) {
            a[i][m - i - 1] = diagonal[i];
        }
    }

    public void updateRU2LD(final DoubleUnaryOperator func) {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        for (int i = 0; i < n; i++) {
            a[i][m - i - 1] = func.applyAsDouble(a[i][m - i - 1]);
        }
    }

    public void updateAll(final DoubleUnaryOperator func) {
        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            a[i][j] = func.applyAsDouble(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            a[i][j] = func.applyAsDouble(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = func.applyAsDouble(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        a[i][j] = func.applyAsDouble(a[i][j]);
                    }
                }
            }
        }
    }

    public void replaceIf(final DoublePredicate predicate, final double newValue) {
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

    public DoubleMatrix map(final DoubleUnaryOperator func) {
        final double[][] c = new double[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = func.applyAsDouble(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = func.applyAsDouble(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsDouble(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = func.applyAsDouble(a[i][j]);
                    }
                }
            }
        }

        return DoubleMatrix.of(c);
    }

    public <T> Matrix<T> mapToObj(final Class<T> cls, final DoubleFunction<? extends T> func) {
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

    public void fill(final double val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void fill(final double[][] b) {
        fill(0, 0, b);
    }

    public void fill(final int fromRowIndex, final int fromColumnIndex, final double[][] b) {
        N.checkFromToIndex(fromRowIndex, n, n);
        N.checkFromToIndex(fromColumnIndex, m, m);

        for (int i = 0, minLen = N.min(n - fromRowIndex, b.length); i < minLen; i++) {
            N.copy(b[i], 0, a[i + fromRowIndex], fromColumnIndex, N.min(b[i].length, m - fromColumnIndex));
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalDouble min() {
    //        if (isEmpty()) {
    //            return OptionalDouble.empty();
    //        }
    //
    //        double candicate = Double.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalDouble.of(candicate);
    //    }
    //
    //    public OptionalDouble max() {
    //        if (isEmpty()) {
    //            return OptionalDouble.empty();
    //        }
    //
    //        double candicate = Double.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalDouble.of(candicate);
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
    //    public DoubleList row(final int i) {
    //        return DoubleList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public DoubleList column(final int j) {
    //        return DoubleList.of(column2(j));
    //    }

    @Override
    public DoubleMatrix copy() {
        final double[][] c = new double[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        final double[][] c = new double[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        final double[][] c = new double[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix rotate90() {
        final double[][] c = new double[m][n];

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

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix rotate180() {
        final double[][] c = new double[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix rotate270() {
        final double[][] c = new double[m][n];

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

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix transpose() {
        final double[][] c = new double[m][n];

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

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix reshape(final int n, final int m) {
        final double[][] c = new double[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new DoubleMatrix(c);
        }

        if (a.length == 1) {
            final double[] a0 = a[0];

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

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleList flatten() {
        final double[] c = new double[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return DoubleList.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#vstack(IntMatrix)
     */
    public DoubleMatrix vstack(final DoubleMatrix b) {
        N.checkArgument(this.m == b.m, "The count of column in this matrix and the specified matrix are not equals");

        final double[][] c = new double[this.n + b.n][];
        int j = 0;

        for (int i = 0; i < n; i++) {
            c[j++] = a[i].clone();
        }

        for (int i = 0; i < b.n; i++) {
            c[j++] = b.a[i].clone();
        }

        return DoubleMatrix.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#hstack(IntMatrix)
     */
    public DoubleMatrix hstack(final DoubleMatrix b) {
        N.checkArgument(this.n == b.n, "The count of row in this matrix and the specified matrix are not equals");

        final double[][] c = new double[n][m + b.m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c[i], 0, m);
            N.copy(b.a[i], 0, c[i], m, b.m);
        }

        return DoubleMatrix.of(c);
    }

    public DoubleMatrix add(final DoubleMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final double[][] c = new double[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = a[i][j] + b.a[i][j];
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = a[i][j] + b.a[i][j];
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = a[i][j] + b.a[i][j];
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = a[i][j] + b.a[i][j];
                    }
                }
            }
        }

        return new DoubleMatrix(c);
    }

    public DoubleMatrix subtract(final DoubleMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final double[][] c = new double[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = a[i][j] - b.a[i][j];
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = a[i][j] - b.a[i][j];
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = a[i][j] - b.a[i][j];
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = a[i][j] - b.a[i][j];
                    }
                }
            }
        }

        return new DoubleMatrix(c);
    }

    public DoubleMatrix multiply(final DoubleMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final double[][] c = new double[n][b.m];
        final double[][] a2 = b.a;

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
                    for (int i = 0; i < n; i++) {
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

        return new DoubleMatrix(c);
    }

    public Matrix<Double> boxed() {
        final Double[][] c = new Double[n][m];

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

    public DoubleMatrix zipWith(final DoubleMatrix matrixB, final DoubleBiFunction<Double> zipFunction) {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final double[][] result = new double[n][m];
        final double[][] b = matrixB.a;

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
                for (int i = 0; i < n; i++) {
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

        return new DoubleMatrix(result);
    }

    public DoubleMatrix zipWith(final DoubleMatrix matrixB, final DoubleMatrix matrixC, final DoubleTriFunction<Double> zipFunction) {
        N.checkArgument(isSameShape(matrixB) && isSameShape(matrixC), "Can't zip three matrices which have different shape.");

        final double[][] result = new double[n][m];
        final double[][] b = matrixB.a;
        final double[][] c = matrixC.a;

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
                for (int i = 0; i < n; i++) {
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

        return new DoubleMatrix(result);
    }

    /**
     * 
     * @return a stream based on the order of row.
     */
    @Override
    public DoubleStream streamH() {
        return streamH(0, n);
    }

    /**
     * 
     * @return a stream composed by elements on the diagonal line from left up to right down.
     */
    @Override
    public DoubleStream streamLU2RD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return DoubleStream.empty();
        }

        return DoubleStream.of(new ExDoubleIterator() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double nextDouble() {
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
    public DoubleStream streamRU2LD() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return DoubleStream.empty();
        }

        return DoubleStream.of(new ExDoubleIterator() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public double nextDouble() {
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

    @Override
    public DoubleStream streamH(final int rowIndex) {
        return streamH(rowIndex, rowIndex + 1);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a stream based on the order of row.
     */
    @Override
    public DoubleStream streamH(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return DoubleStream.empty();
        }

        return DoubleStream.of(new ExDoubleIterator() {
            private int i = fromRowIndex;
            private int j = 0;

            @Override
            public boolean hasNext() {
                return i < toRowIndex;
            }

            @Override
            public double nextDouble() {
                if (i >= toRowIndex) {
                    throw new NoSuchElementException();
                }

                final double result = a[i][j++];

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
            public double[] toArray() {
                final int len = (int) count();
                final double[] c = new double[len];

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
    public DoubleStream streamV() {
        return streamV(0, m);
    }

    @Override
    public DoubleStream streamV(final int columnIndex) {
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
    public DoubleStream streamV(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return DoubleStream.empty();
        }

        return DoubleStream.of(new ExDoubleIterator() {
            private int i = 0;
            private int j = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return j < toColumnIndex;
            }

            @Override
            public double nextDouble() {
                if (j >= toColumnIndex) {
                    throw new NoSuchElementException();
                }

                final double result = a[i++][j];

                if (i >= n) {
                    i = 0;
                    j++;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toColumnIndex - j) * DoubleMatrix.this.n * 1L - i) {
                    i = 0;
                    j = toColumnIndex;
                } else {
                    i += (n + i) % DoubleMatrix.this.n;
                    j += (n + i) / DoubleMatrix.this.n;
                }
            }

            @Override
            public long count() {
                return (toColumnIndex - j) * n - i;
            }

            @Override
            public double[] toArray() {
                final int len = (int) count();
                final double[] c = new double[len];

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
    public Stream<DoubleStream> streamR() {
        return streamR(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    @Override
    public Stream<DoubleStream> streamR(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<DoubleStream>() {
            private final int toIndex = toRowIndex;
            private int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return DoubleStream.of(a[cursor++]);
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
    public Stream<DoubleStream> streamC() {
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
    public Stream<DoubleStream> streamC(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ExIterator<DoubleStream>() {
            private final int toIndex = toColumnIndex;
            private volatile int cursor = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public DoubleStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return DoubleStream.of(new ExDoubleIterator() {
                    private final int columnIndex = cursor++;
                    private final int toIndex2 = n;
                    private int cursor2 = 0;

                    @Override
                    public boolean hasNext() {
                        return cursor2 < toIndex2;
                    }

                    @Override
                    public double nextDouble() {
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
    protected int length(double[] a) {
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

        if (obj instanceof DoubleMatrix) {
            final DoubleMatrix another = (DoubleMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
