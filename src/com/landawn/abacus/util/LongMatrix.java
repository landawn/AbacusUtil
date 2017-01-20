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
import com.landawn.abacus.util.function.LongBiFunction;
import com.landawn.abacus.util.function.LongTriFunction;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.stream.ImmutableIterator;
import com.landawn.abacus.util.stream.ImmutableLongIterator;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.LongStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class LongMatrix extends AbstractMatrix<long[], LongList, LongMatrix> {
    static final LongMatrix EMPTY_LONG_MATRIX = new LongMatrix(new long[0][0]);

    public LongMatrix(final long[][] a) {
        super(a == null ? new long[0][0] : a);
    }

    public static LongMatrix empty() {
        return EMPTY_LONG_MATRIX;
    }

    public static LongMatrix of(final long[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_LONG_MATRIX : new LongMatrix(a);
    }

    public static LongMatrix from(final int[]... a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_LONG_MATRIX;
        }

        final long[][] c = new long[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new LongMatrix(c);
    }

    public static LongMatrix random(final int len) {
        return new LongMatrix(new long[][] { LongList.random(len).array() });
    }

    public static LongMatrix repeat(final long val, final int len) {
        return new LongMatrix(new long[][] { Array.repeat(val, len) });
    }

    public static LongMatrix range(long startInclusive, final long endExclusive) {
        return new LongMatrix(new long[][] { Array.range(startInclusive, endExclusive) });
    }

    public static LongMatrix range(long startInclusive, final long endExclusive, final long by) {
        return new LongMatrix(new long[][] { Array.range(startInclusive, endExclusive, by) });
    }

    public static LongMatrix rangeClosed(long startInclusive, final long endInclusive) {
        return new LongMatrix(new long[][] { Array.rangeClosed(startInclusive, endInclusive) });
    }

    public static LongMatrix rangeClosed(long startInclusive, final long endInclusive, final long by) {
        return new LongMatrix(new long[][] { Array.rangeClosed(startInclusive, endInclusive, by) });
    }

    public static LongMatrix diagonal(final long[] leftUp2RightLowDiagonal) {
        return diagonal(leftUp2RightLowDiagonal, null);
    }

    public static LongMatrix diagonal(final long[] leftUp2RightLowDiagonal, long[] rightUp2LeftLowDiagonal) {
        N.checkArgument(
                N.isNullOrEmpty(leftUp2RightLowDiagonal) || N.isNullOrEmpty(rightUp2LeftLowDiagonal)
                        || leftUp2RightLowDiagonal.length == rightUp2LeftLowDiagonal.length,
                "The length of 'leftUp2RightLowDiagonal' and 'rightUp2LeftLowDiagonal' must be same");

        if (N.isNullOrEmpty(leftUp2RightLowDiagonal)) {
            if (N.isNullOrEmpty(rightUp2LeftLowDiagonal)) {
                return empty();
            } else {
                final int len = rightUp2LeftLowDiagonal.length;
                final long[][] c = new long[len][len];

                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftLowDiagonal[i];
                }

                return new LongMatrix(c);
            }
        } else {
            final int len = leftUp2RightLowDiagonal.length;
            final long[][] c = new long[len][len];

            for (int i = 0; i < len; i++) {
                c[i][i] = leftUp2RightLowDiagonal[i];
            }

            if (N.notNullOrEmpty(rightUp2LeftLowDiagonal)) {
                for (int i = 0, j = len - 1; i < len; i++, j--) {
                    c[i][j] = rightUp2LeftLowDiagonal[i];
                }
            }

            return new LongMatrix(c);
        }
    }

    public long[][] array() {
        return a;
    }

    public long get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final long val) {
        a[i][j] = val;
    }

    public long[] row(final int rowIndex) {
        N.checkArgument(rowIndex >= 0 && rowIndex < n, "Invalid row Index: %s", rowIndex);

        return a[rowIndex];
    }

    public long[] col(final int columnIndex) {
        N.checkArgument(columnIndex >= 0 && columnIndex < m, "Invalid column Index: %s", columnIndex);

        final long[] c = new long[n];

        for (int i = 0; i < n; i++) {
            c[i] = a[i][columnIndex];
        }

        return c;
    }

    public void fill(final long val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final LongUnaryOperator operator) {
        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            a[i][j] = operator.applyAsLong(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            a[i][j] = operator.applyAsLong(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsLong(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        a[i][j] = operator.applyAsLong(a[i][j]);
                    }
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalLong min() {
    //        if (isEmpty()) {
    //            return OptionalLong.empty();
    //        }
    //
    //        long candicate = Long.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalLong.of(candicate);
    //    }
    //
    //    public OptionalLong max() {
    //        if (isEmpty()) {
    //            return OptionalLong.empty();
    //        }
    //
    //        long candicate = Long.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalLong.of(candicate);
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
    //    public LongList row(final int i) {
    //        return LongList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public LongList column(final int j) {
    //        return LongList.of(column2(j));
    //    }

    @Override
    public LongMatrix copy() {
        final long[][] c = new long[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final long[][] c = new long[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final long[][] c = new long[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix rotate90() {
        final long[][] c = new long[m][n];

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

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix rotate180() {
        final long[][] c = new long[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix rotate270() {
        final long[][] c = new long[m][n];

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

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix transpose() {
        final long[][] c = new long[m][n];

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

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix reshape(final int n, final int m) {
        final long[][] c = new long[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new LongMatrix(c);
        }

        if (a.length == 1) {
            final long[] a0 = a[0];

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

        return new LongMatrix(c);
    }

    @Override
    public LongList flatten() {
        final long[] c = new long[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return LongList.of(c);
    }

    public LongMatrix add(final LongMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final long[][] c = new long[n][m];

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

        return new LongMatrix(c);
    }

    public LongMatrix subtract(final LongMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final long[][] c = new long[n][m];

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

        return new LongMatrix(c);
    }

    public LongMatrix multiply(final LongMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final long[][] c = new long[n][b.m];
        final long[][] a2 = b.a;

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

        return new LongMatrix(c);
    }

    public Matrix<Long> boxed() {
        final Long[][] c = new Long[n][m];

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
        return DoubleMatrix.from(a);
    }

    /**
     * 
     * @return a stream composed by elements on the diagonal line from left up to right down.
     */
    public LongStream diagonal() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return LongStream.empty();
        }

        return LongStream.of(new ImmutableLongIterator() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
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
    public LongStream diagonal2() {
        N.checkState(n == m, "'n' and 'm' must be same to get diagonals: n=%s, m=%s", n, m);

        if (isEmpty()) {
            return LongStream.empty();
        }

        return LongStream.of(new ImmutableLongIterator() {
            private final int toIndex = n;
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
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

    public LongMatrix zipWith(final LongMatrix matrixB, final LongBiFunction<Long> zipFunction) {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final long[][] result = new long[n][m];
        final long[][] b = matrixB.a;

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

        return new LongMatrix(result);
    }

    public LongMatrix zipWith(final LongMatrix matrixB, final LongMatrix matrixC, final LongTriFunction<Long> zipFunction) {
        N.checkArgument(isSameShape(matrixB), "Can't zip two matrices which have different shape.");

        final long[][] result = new long[n][m];
        final long[][] b = matrixB.a;
        final long[][] c = matrixC.a;

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

        return new LongMatrix(result);
    }

    /**
     * 
     * @return a stream based on the order of row.
     */
    public LongStream stream() {
        return stream(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a stream based on the order of row.
     */
    public LongStream stream(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return LongStream.empty();
        }

        return LongStream.of(new ImmutableLongIterator() {
            private final long toIndex = toRowIndex * m * 1L;
            private long cursor = fromRowIndex * m * 1L;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[(int) (cursor / m)][(int) (cursor++ % m)];
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

    /**
     * 
     * @return a stream based on the order of column.
     */
    @Beta
    public LongStream stream0() {
        return stream0(0, m);
    }

    /**
     * 
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a stream based on the order of column.
     */
    @Beta
    public LongStream stream0(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return LongStream.empty();
        }

        return LongStream.of(new ImmutableLongIterator() {
            private final long toIndex = toColumnIndex * n * 1L;
            private long cursor = fromColumnIndex * n * 1L;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public long next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[(int) (cursor % n)][(int) (cursor++ / n)];
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

    /**
     * 
     * @return a row stream based on the order of row.
     */
    public Stream<LongStream> stream2() {
        return stream2(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    public Stream<LongStream> stream2(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<LongStream>() {
            private final int toIndex = toRowIndex;
            private int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public LongStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return LongStream.of(a[cursor++]);
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
    public Stream<LongStream> stream02() {
        return stream02(0, m);
    }

    /**
     * 
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a column stream based on the order of column.
     */
    @Beta
    public Stream<LongStream> stream02(final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<LongStream>() {
            private final int toIndex = toColumnIndex;
            private volatile int cursor = fromColumnIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public LongStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return LongStream.of(new ImmutableLongIterator() {
                    private final int columnIndex = cursor++;
                    private final int toIndex2 = n;
                    private int cursor2 = 0;

                    @Override
                    public boolean hasNext() {
                        return cursor2 < toIndex2;
                    }

                    @Override
                    public long next() {
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
    protected int length(long[] a) {
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

        if (obj instanceof LongMatrix) {
            final LongMatrix another = (LongMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
