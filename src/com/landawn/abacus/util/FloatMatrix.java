package com.landawn.abacus.util;

import com.landawn.abacus.util.function.FloatUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;

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

    public static FloatMatrix from(final int n, final int m, final float[] a) {
        final float[][] c = new float[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new FloatMatrix(c);
        }

        for (int i = 0, len = N.min(n, a.length % m == 0 ? a.length / m : a.length / m + 1); i < len; i++) {
            for (int j = 0, col = N.min(m, a.length - i * m), off = i * m; j < col; j++) {
                c[i][j] = a[off + j];
            }
        }

        return new FloatMatrix(c);
    }

    public static FloatMatrix random(final int n, final int m) {
        final float[][] a = new float[n][m];

        if (n == 0 || m == 0) {
            return new FloatMatrix(a);
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = RAND.nextInt();
            }
        }

        return new FloatMatrix(a);
    }

    public static FloatMatrix repeat(final int n, final int m, final float val) {
        final float[][] c = new float[n][m];

        if (n == 0 || m == 0) {
            return new FloatMatrix(c);
        }

        for (int i = 0; i < n; i++) {
            N.fill(c[i], val);
        }

        return new FloatMatrix(c);
    }

    public float get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final float val) {
        a[i][j] = val;
    }

    public void fill(final float val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final FloatUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsFloat(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsFloat(a[i][j]);
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
    //                if (N.compare(a[i][j], candicate) < 0) {
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
    //                if (N.compare(a[i][j], candicate) > 0) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalFloat.of(candicate);
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
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final float[][] c = new float[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final float[][] c = new float[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix rotate90() {
        final float[][] c = new float[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
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

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new FloatMatrix(c);
    }

    @Override
    public FloatMatrix reshape(final int n, final int m) {
        return from(n, m, this.flatten());
    }

    @Override
    public float[] flatten() {
        final float[] c = new float[n * m];
    
        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }
    
        return c;
    }

    public FloatMatrix add(final FloatMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final float[][] c = new float[n][m];

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

        return new FloatMatrix(c);
    }

    public FloatMatrix subtract(final FloatMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final float[][] c = new float[n][m];

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

        return new FloatMatrix(c);
    }

    public FloatMatrix multiply(final FloatMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final float[][] c = new float[n][b.m];
        final float[][] a2 = b.a;

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

        return new FloatMatrix(c);
    }

    public DoubleMatrix toDoubleMatrix() {
        return DoubleMatrix.from(a);
    }

    @Override
    float[] column2(final int j) {
        final float[] c = new float[n];

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