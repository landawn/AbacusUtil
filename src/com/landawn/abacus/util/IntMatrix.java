package com.landawn.abacus.util;

import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.stream.IntStream;

public final class IntMatrix extends AbstractMatrix<int[], IntList, IntMatrix> {
    static final IntMatrix EMPTY_INT_MATRIX = new IntMatrix(new int[0][0]);

    public IntMatrix(final int[][] a) {
        super(a == null ? new int[0][0] : a);
    }

    public static IntMatrix empty() {
        return EMPTY_INT_MATRIX;
    }

    public static IntMatrix of(final int[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_INT_MATRIX : new IntMatrix(a);
    }

    public static IntMatrix from(final int n, final int m, final int[] a) {
        final int[][] c = new int[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new IntMatrix(c);
        }

        for (int i = 0, len = N.min(n, a.length % m == 0 ? a.length / m : a.length / m + 1); i < len; i++) {
            for (int j = 0, col = N.min(m, a.length - i * m), off = i * m; j < col; j++) {
                c[i][j] = a[off + j];
            }
        }

        return new IntMatrix(c);
    }

    public static IntMatrix from(final char[][] a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_INT_MATRIX;
        }

        final int[][] c = new int[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new IntMatrix(c);
    }

    public static IntMatrix from(final byte[][] a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_INT_MATRIX;
        }

        final int[][] c = new int[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new IntMatrix(c);
    }

    public static IntMatrix from(final short[][] a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY_INT_MATRIX;
        }

        final int[][] c = new int[a.length][a[0].length];

        for (int i = 0, len = a.length; i < len; i++) {
            for (int j = 0, col = a[0].length; j < col; j++) {
                c[i][j] = a[i][j];
            }
        }

        return new IntMatrix(c);
    }

    public static IntMatrix random(final int n, final int m) {
        final int[][] a = new int[n][m];

        if (n == 0 || m == 0) {
            return new IntMatrix(a);
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = RAND.nextInt();
            }
        }

        return new IntMatrix(a);
    }

    public static IntMatrix repeat(final int n, final int m, final int val) {
        final int[][] c = new int[n][m];

        if (n == 0 || m == 0) {
            return new IntMatrix(c);
        }

        for (int i = 0; i < n; i++) {
            N.fill(c[i], val);
        }

        return new IntMatrix(c);
    }

    public int get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final int val) {
        a[i][j] = val;
    }

    public void fill(final int val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final IntUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsInt(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsInt(a[i][j]);
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalInt min() {
    //        if (isEmpty()) {
    //            return OptionalInt.empty();
    //        }
    //
    //        int candicate = Integer.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalInt.of(candicate);
    //    }
    //
    //    public OptionalInt max() {
    //        if (isEmpty()) {
    //            return OptionalInt.empty();
    //        }
    //
    //        int candicate = Integer.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalInt.of(candicate);
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
    //    public IntList row(final int i) {
    //        return IntList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public IntList column(final int j) {
    //        return IntList.of(column2(j));
    //    }

    @Override
    public IntMatrix copy() {
        final int[][] c = new int[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new IntMatrix(c);
    }

    @Override
    public IntMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final int[][] c = new int[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new IntMatrix(c);
    }

    @Override
    public IntMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final int[][] c = new int[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new IntMatrix(c);
    }

    @Override
    public IntMatrix rotate90() {
        final int[][] c = new int[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
            }
        }

        return new IntMatrix(c);
    }

    @Override
    public IntMatrix rotate180() {
        final int[][] c = new int[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new IntMatrix(c);
    }

    @Override
    public IntMatrix rotate270() {
        final int[][] c = new int[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new IntMatrix(c);
    }

    @Override
    public IntMatrix reshape(final int n, final int m) {
        return from(n, m, this.flatten());
    }

    @Override
    public int[] flatten() {
        final int[] c = new int[n * m];
    
        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }
    
        return c;
    }

    public IntMatrix add(final IntMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final int[][] c = new int[n][m];

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

        return new IntMatrix(c);
    }

    public IntMatrix subtract(final IntMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final int[][] c = new int[n][m];

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

        return new IntMatrix(c);
    }

    public IntMatrix multiply(final IntMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final int[][] c = new int[n][b.m];
        final int[][] a2 = b.a;

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

        return new IntMatrix(c);
    }

    public LongMatrix toLongMatrix() {
        return LongMatrix.from(a);
    }

    public DoubleMatrix toDoubleMatrix() {
        return DoubleMatrix.from(a);
    }

    @Override
    int[] column2(final int j) {
        final int[] c = new int[n];

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

        if (obj instanceof IntMatrix) {
            final IntMatrix another = (IntMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}