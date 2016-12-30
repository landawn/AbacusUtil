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

import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class DoubleMatrix extends AbstractMatrix<double[], DoubleList, DoubleMatrix> {
    static final DoubleMatrix EMPTY_DOUBLE_MATRIX = new DoubleMatrix(new double[0][0]);

    public DoubleMatrix(final double[][] a) {
        super(a == null ? new double[0][0] : a);
    }

    public static DoubleMatrix empty() {
        return EMPTY_DOUBLE_MATRIX;
    }

    public static DoubleMatrix of(final double[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_DOUBLE_MATRIX : new DoubleMatrix(a);
    }

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

    public double get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final double val) {
        a[i][j] = val;
    }

    public void fill(final double val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final DoubleUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsDouble(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsDouble(a[i][j]);
                }
            }
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
    //                if (N.compare(a[i][j], candicate) < 0) {
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
    //                if (N.compare(a[i][j], candicate) > 0) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalDouble.of(candicate);
    //    }
    //
    //    public Double sum() {
    //        double sum = 0;
    //
    //        for (int i = 0; i < n; i++) {
    //            sum += N.sum(a[i]);
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
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final double[][] c = new double[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final double[][] c = new double[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix rotate90() {
        final double[][] c = new double[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
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

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new DoubleMatrix(c);
    }

    @Override
    public DoubleMatrix transpose() {
        final double[][] c = new double[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][i];
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

        long cnt = 0;

        for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
            for (int j = 0, col = (int) N.min(m, count - i * m); j < col; j++, cnt++) {
                c[i][j] = a[(int) (cnt / this.m)][(int) (cnt % this.m)];
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

    public DoubleMatrix add(final DoubleMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final double[][] c = new double[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = a[i][j] + b.a[i][j];
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j] + b.a[i][j];
                }
            }
        }

        return new DoubleMatrix(c);
    }

    public DoubleMatrix subtract(final DoubleMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final double[][] c = new double[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = a[i][j] - b.a[i][j];
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j] - b.a[i][j];
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
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < b.m; j++) {
                    for (int k = 0; k < m; k++) {
                        c[i][j] += a[i][k] * a2[k][j];
                    }
                }
            }
        }

        return new DoubleMatrix(c);
    }

    @Override
    double[] column2(final int j) {
        final double[] c = new double[n];

        for (int i = 0; i < n; i++) {
            c[i] = a[i][j];
        }

        return c;
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
