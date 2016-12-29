package com.landawn.abacus.util;

import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.LongUnaryOperator;
import com.landawn.abacus.util.stream.IntStream;

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

    public static LongMatrix from(final int n, final int m, final long[] a) {
        final long[][] c = new long[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new LongMatrix(c);
        }

        for (int i = 0, len = N.min(n, a.length % m == 0 ? a.length / m : a.length / m + 1); i < len; i++) {
            for (int j = 0, col = N.min(m, a.length - i * m), off = i * m; j < col; j++) {
                c[i][j] = a[off + j];
            }
        }

        return new LongMatrix(c);
    }

    public static LongMatrix from(final int[][] a) {
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

    public static LongMatrix random(final int n, final int m) {
        final long[][] a = new long[n][m];

        if (n == 0 || m == 0) {
            return new LongMatrix(a);
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = RAND.nextInt();
            }
        }

        return new LongMatrix(a);
    }

    public static LongMatrix repeat(final int n, final int m, final long val) {
        final long[][] c = new long[n][m];

        if (n == 0 || m == 0) {
            return new LongMatrix(c);
        }

        for (int i = 0; i < n; i++) {
            N.fill(c[i], val);
        }

        return new LongMatrix(c);
    }

    public long get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final long val) {
        a[i][j] = val;
    }

    public void fill(final long val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final LongUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsLong(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsLong(a[i][j]);
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

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
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

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new LongMatrix(c);
    }

    @Override
    public LongMatrix reshape(final int n, final int m) {
        return from(n, m, this.flatten());
    }

    @Override
    public long[] flatten() {
        final long[] c = new long[n * m];
    
        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }
    
        return c;
    }

    public LongMatrix add(final LongMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final long[][] c = new long[n][m];

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

        return new LongMatrix(c);
    }

    public LongMatrix subtract(final LongMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final long[][] c = new long[n][m];

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

        return new LongMatrix(c);
    }

    public LongMatrix multiply(final LongMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final long[][] c = new long[n][b.m];
        final long[][] a2 = b.a;

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

        return new LongMatrix(c);
    }

    public DoubleMatrix toDoubleMatrix() {
        return DoubleMatrix.from(a);
    }

    @Override
    long[] column2(final int j) {
        final long[] c = new long[n];

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