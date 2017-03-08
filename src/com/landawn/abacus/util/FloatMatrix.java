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
import com.landawn.abacus.util.function.FloatBiFunction;
import com.landawn.abacus.util.function.FloatTriFunction;
import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.FloatStream;
import com.landawn.abacus.util.stream.ImmutableFloatIterator;
import com.landawn.abacus.util.stream.ImmutableIterator;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class FloatMatrix extends AbstractMatrix<float[], FloatList, FloatMatrix> {
    static final FloatMatrix EMPTY_FLOAT_MATRIX = new FloatMatrix(new float[0][0]);

    public FloatMatrix(final float[][] a) {
        super(a == null ? new float[0][0] : a);
    }

    public static FloatMatrix empty() {
        return EMPTY_FLOAT_MATRIX;
    }

    public static FloatMatrix of(final float[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_FLOAT_MATRIX : new FloatMatrix(a);
    }

    public static FloatMatrix from(final int[]... a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_FLOAT_MATRIX;
        }

        final float[][] c = new float[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new FloatMatrix(c);
    }

    public static FloatMatrix random(final int len) {
        return new FloatMatrix(new float[][] { FloatList.random(len).array() });
    }

    public static FloatMatrix repeat(final float val, final int len) {
        return new FloatMatrix(new float[][] { Array.repeat(val, len) });
    }

    public static FloatMatrix diagonal(final float[] leftUp2RightLowDiagonal) {
        return diagonal(leftUp2RightLowDiagonal, null);
    }

    public static FloatMatrix diagonal(final float[] leftUp2RightLowDiagonal, float[] rightUp2LeftLowDiagonal) {
        N.checkArgument(
                N.isNullOrEmpty(leftUp2RightLowDiagonal) || N.isNullOrEmpty(rightUp2LeftLowDiagonal)
                        || leftUp2RightLowDiagonal.length == rightUp2LeftLowDiagonal.length,
                "The length of 'leftUp2RightLowDiagonal' and 'rightUp2LeftLowDiagonal' must be same");

        if (N.isNullOrEmpty(leftUp2RightLowDiagonal)) {
            if (N.isNullOrEmpty(rightUp2LeftLowDiagonal)) {
                return empty();
            } else {
                final int len = rightUp2LeftLowDiagonal.length;
                final float[][] c = new float[len][len];

                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftLowDiagonal[i];
                }

                return new FloatMatrix(c);
            }
        } else {
            final int len = leftUp2RightLowDiagonal.length;
            final float[][] c = new float[len][len];

            for (int i = 0; i < len; i++) {
                c[i][i] = leftUp2RightLowDiagonal[i];
            }

            if (N.notNullOrEmpty(rightUp2LeftLowDiagonal)) {
                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftLowDiagonal[i];
                }
            }

            return new FloatMatrix(c);
        }
    }

    public float[][] array() {
        return a;
    }

    public float get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final float val) {
        a[i][j] = val;
    }

    public float[] row(final int rowIndex) {
        N.checkArgument(rowIndex >= 0 && rowIndex < n, "Invalid row Index: %s", rowIndex);

        return a[rowIndex];
    }

    public float[] column(final int columnIndex) {
        N.checkArgument(columnIndex >= 0 && columnIndex < m, "Invalid column Index: %s", columnIndex);

        final float[] c = new float[n];

        for (int i = 0; i < n; i++) {
            c[i] = a[i][columnIndex];
        }

        return c;
    }

    public void setRow(int rowIndex, float[] row) {
        N.checkArgument(row.length == m, "The size of the specified row doesn't match the length of column");

        N.copy(row, 0, a[rowIndex], 0, m);
    }

    public void setColumn(int columnIndex, float[] column) {
        N.checkArgument(column.length == n, "The size of the specified column doesn't match the length of row");

        for (int i = 0; i < n; i++) {
            a[i][columnIndex] = column[i];
        }
    }

    public void fill(final float val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void fill(final float[][] b) {
        fill(0, 0, b);
    }

    public void fill(final int fromRowIndex, final int fromColumnIndex, final float[][] b) {
        N.checkFromToIndex(fromRowIndex, n, n);
        N.checkFromToIndex(fromColumnIndex, m, m);

        for (int i = 0, minLen = N.min(n - fromRowIndex, b.length); i < minLen; i++) {
            N.copy(b[i], 0, a[i + fromRowIndex], fromColumnIndex, N.min(b[i].length, m - fromColumnIndex));
        }
    }

    public void replaceAll(final FloatUnaryOperator operator) {
        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            a[i][j] = operator.applyAsFloat(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            a[i][j] = operator.applyAsFloat(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsFloat(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        a[i][j] = operator.applyAsFloat(a[i][j]);
                    }
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalFloat min() {
    //        if (isEmpty()) {
    //            return OptionalFloat.empty();
    //        }
    //
    //        float candicate = Float.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalFloat.of(candicate);
    //    }
    //
    //    public OptionalFloat max() {
    //        if (isEmpty()) {
    //            return OptionalFloat.empty();
    //        }
    //
    //        float candicate = Float.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalFloat.of(candicate);
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
    //    public FloatList row(final int i) {
    //        return FloatList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public FloatList column(final int j) {
    //        return FloatList.of(column2(j));
    //    }

    @Override
    public FloatMatrix copy() {
        final float[][] c = new float[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        final float[][] c = new float[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        final float[][] c = new float[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix rotate90() {
        final float[][] c = new float[m][n];

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

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix rotate180() {
        final float[][] c = new float[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix rotate270() {
        final float[][] c = new float[m][n];

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

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix transpose() {
        final float[][] c = new float[m][n];

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

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix reshape(final int n, final int m) {
        final float[][] c = new float[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new FloatMatrix(c);
        }

        if (a.length == 1) {
            final float[] a0 = a[0];

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

        return new FloatMatrix(c);
    }

    @Override
    public FloatList flatten() {
        final float[] c = new float[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return FloatList.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#vstack(IntMatrix)
     */
    public FloatMatrix vstack(final FloatMatrix b) {
        N.checkArgument(this.m == b.m, "The count of column in this matrix and the specified matrix are not equals");

        final float[][] c = new float[this.n + b.n][];
        int j = 0;

        for (int i = 0; i < n; i++) {
            c[j++] = a[i].clone();
        }

        for (int i = 0; i < b.n; i++) {
            c[j++] = b.a[i].clone();
        }

        return FloatMatrix.of(c);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntMatrix#hstack(IntMatrix)
     */
    public FloatMatrix hstack(final FloatMatrix b) {
        N.checkArgument(this.n == b.n, "The count of row in this matrix and the specified matrix are not equals");

        final float[][] c = new float[n][m + b.m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c[i], 0, m);
            N.copy(b.a[i], 0, c[i], m, b.m);
        }

        return FloatMatrix.of(c);
    }

    public FloatMatrix add(final FloatMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final float[][] c = new float[n][m];

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

        return new FloatMatrix(c);
    }

    public FloatMatrix subtract(final FloatMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final float[][] c = new float[n][m];

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

        return new FloatMatrix(c);
    }

    public FloatMatrix multiply(final FloatMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final float[][] c = new float[n][b.m];
        final float[][] a2 = b.a;

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

        return new FloatMatrix(c);
    }

    public Matrix<Float> boxed() {
        final Float[][] c = new Float[n][m];

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

    public DoubleMatrix toDoubleMatrix() {
        return DoubleMatrix.from(a);
    }

    /**
     * 
     * @return a stream composed by elements on the diagonal line from left up to right down.
     */
    public FloatStream diagonal() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return FloatStream.empty();
        }

        return FloatStream.of(new ImmutableFloatIterator() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float next() {
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
    public FloatStream diagonal2() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return FloatStream.empty();
        }

        return FloatStream.of(new ImmutableFloatIterator() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public float next() {
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

    public FloatMatrix zipWith(final FloatMatrix matrixB, final FloatBiFunction<Float> zipFunction) {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final float[][] result = new float[n][m];
        final float[][] b = matrixB.a;

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

        return new FloatMatrix(result);
    }

    public FloatMatrix zipWith(final FloatMatrix matrixB, final FloatMatrix matrixC, final FloatTriFunction<Float> zipFunction) {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final float[][] result = new float[n][m];
        final float[][] b = matrixB.a;
        final float[][] c = matrixC.a;

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

        return new FloatMatrix(result);
    }

    /**
     * 
     * @return a stream based on the order of row.
     */
    public FloatStream stream() {
        return stream(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a stream based on the order of row.
     */
    public FloatStream stream(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return FloatStream.empty();
        }

        return FloatStream.of(new ImmutableFloatIterator() {
            private int i = fromRowIndex;
            private int j = 0;

            @Override
            public boolean hasNext() {
                return i < toRowIndex;
            }

            @Override
            public float next() {
                if (i >= toRowIndex) {
                    throw new NoSuchElementException();
                }

                final float result = a[i][j++];

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
            public float[] toArray() {
                final int len = (int) count();
                final float[] c = new float[len];

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
    @Beta
    public FloatStream stream0() {
        return stream0(0, m);
    }

    /**
     * 
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a stream based on the order of column.
     */
    @Beta
    public FloatStream stream0(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return FloatStream.empty();
        }

        return FloatStream.of(new ImmutableFloatIterator() {
            private int i = 0;
            private int j = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return j < toColumnIndex;
            }

            @Override
            public float next() {
                if (j >= toColumnIndex) {
                    throw new NoSuchElementException();
                }

                final float result = a[i++][j];

                if (i >= n) {
                    i = 0;
                    j++;
                }

                return result;
            }

            @Override
            public void skip(long n) {
                if (n >= (toColumnIndex - j) * FloatMatrix.this.n * 1L - i) {
                    i = 0;
                    j = toColumnIndex;
                } else {
                    i += (n + i) % FloatMatrix.this.n;
                    j += (n + i) / FloatMatrix.this.n;
                }
            }

            @Override
            public long count() {
                return (toColumnIndex - j) * n - i;
            }

            @Override
            public float[] toArray() {
                final int len = (int) count();
                final float[] c = new float[len];

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
    public Stream<FloatStream> stream2() {
        return stream2(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    public Stream<FloatStream> stream2(final int fromRowIndex, final int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<FloatStream>() {
            private final int toIndex = toRowIndex;
            private int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public FloatStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return FloatStream.of(a[cursor++]);
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
    @Beta
    public Stream<FloatStream> stream02() {
        return stream02(0, m);
    }

    /**
     * 
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a column stream based on the order of column.
     */
    @Beta
    public Stream<FloatStream> stream02(final int fromColumnIndex, final int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<FloatStream>() {
            private final int toIndex = toColumnIndex;
            private volatile int cursor = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public FloatStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return FloatStream.of(new ImmutableFloatIterator() {
                    private final int columnIndex = cursor++;
                    private final int toIndex2 = n;
                    private int cursor2 = 0;

                    @Override
                    public boolean hasNext() {
                        return cursor2 < toIndex2;
                    }

                    @Override
                    public float next() {
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
    protected int length(float[] a) {
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

        if (obj instanceof FloatMatrix) {
            final FloatMatrix another = (FloatMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
