package com.landawn.abacus.util;

import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.ShortUnaryOperator;
import com.landawn.abacus.util.stream.IntStream;

public final class ShortMatrix extends AbstractMatrix<short[], ShortList, ShortMatrix> {
    static final ShortMatrix EMPTY_SHORT_MATRIX = new ShortMatrix(new short[0][0]);

    public ShortMatrix(final short[][] a) {
        super(a == null ? new short[0][0] : a);
    }

    public static ShortMatrix empty() {
        return EMPTY_SHORT_MATRIX;
    }

    public static ShortMatrix of(final short[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_SHORT_MATRIX : new ShortMatrix(a);
    }

    public static ShortMatrix from(final int n, final int m, final short[] a) {
        final short[][] c = new short[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new ShortMatrix(c);
        }

        for (int i = 0, len = N.min(n, a.length % m == 0 ? a.length / m : a.length / m + 1); i < len; i++) {
            for (int j = 0, col = N.min(m, a.length - i * m), off = i * m; j < col; j++) {
                c[i][j] = a[off + j];
            }
        }

        return new ShortMatrix(c);
    }

    public static ShortMatrix random(final int n, final int m) {
        final short[][] a = new short[n][m];

        if (n == 0 || m == 0) {
            return new ShortMatrix(a);
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = (short) RAND.nextInt();
            }
        }

        return new ShortMatrix(a);
    }

    public static ShortMatrix repeat(final int n, final int m, final short val) {
        final short[][] c = new short[n][m];

        if (n == 0 || m == 0) {
            return new ShortMatrix(c);
        }

        for (int i = 0; i < n; i++) {
            N.fill(c[i], val);
        }

        return new ShortMatrix(c);
    }

    public short get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final short val) {
        a[i][j] = val;
    }

    public void fill(final short val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final ShortUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsShort(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsShort(a[i][j]);
                }
            }
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
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final short[][] c = new short[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final short[][] c = new short[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix rotate90() {
        final short[][] c = new short[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
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

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new ShortMatrix(c);
    }

    @Override
    public ShortMatrix reshape(final int n, final int m) {
        return from(n, m, this.flatten());
    }

    @Override
    public short[] flatten() {
        final short[] c = new short[n * m];
    
        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }
    
        return c;
    }

    public ShortMatrix add(final ShortMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final short[][] c = new short[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (short) (a[i][j] + b.a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = (short) (a[i][j] + b.a[i][j]);
                }
            }
        }

        return new ShortMatrix(c);
    }

    public ShortMatrix subtract(final ShortMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final short[][] c = new short[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (short) (a[i][j] - b.a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = (short) (a[i][j] - b.a[i][j]);
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

        return new ShortMatrix(c);
    }

    public IntMatrix toIntMatrix() {
        return IntMatrix.from(a);
    }

    @Override
    short[] column2(final int j) {
        final short[] c = new short[n];

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