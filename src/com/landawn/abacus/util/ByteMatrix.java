package com.landawn.abacus.util;

import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;

public final class ByteMatrix extends AbstractMatrix<byte[], ByteList, ByteMatrix> {
    static final ByteMatrix EMPTY_BYTE_MATRIX = new ByteMatrix(new byte[0][0]);

    public ByteMatrix(final byte[][] a) {
        super(a == null ? new byte[0][0] : a);
    }

    public static ByteMatrix empty() {
        return EMPTY_BYTE_MATRIX;
    }

    public static ByteMatrix of(final byte[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_BYTE_MATRIX : new ByteMatrix(a);
    }

    public static ByteMatrix from(final int n, final int m, final byte[] a) {
        final byte[][] c = new byte[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new ByteMatrix(c);
        }

        for (int i = 0, len = N.min(n, a.length % m == 0 ? a.length / m : a.length / m + 1); i < len; i++) {
            for (int j = 0, col = N.min(m, a.length - i * m), off = i * m; j < col; j++) {
                c[i][j] = a[off + j];
            }
        }

        return new ByteMatrix(c);
    }

    public static ByteMatrix random(final int n, final int m) {
        final byte[][] a = new byte[n][m];

        if (n == 0 || m == 0) {
            return new ByteMatrix(a);
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = (byte) RAND.nextInt();
            }
        }

        return new ByteMatrix(a);
    }

    public static ByteMatrix repeat(final int n, final int m, final byte val) {
        final byte[][] c = new byte[n][m];

        if (n == 0 || m == 0) {
            return new ByteMatrix(c);
        }

        for (int i = 0; i < n; i++) {
            N.fill(c[i], val);
        }

        return new ByteMatrix(c);
    }

    public byte get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final byte val) {
        a[i][j] = val;
    }

    public void fill(final byte val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final ByteUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsByte(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsByte(a[i][j]);
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalByte min() {
    //        if (isEmpty()) {
    //            return OptionalByte.empty();
    //        }
    //
    //        byte candicate = Byte.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalByte.of(candicate);
    //    }
    //
    //    public OptionalByte max() {
    //        if (isEmpty()) {
    //            return OptionalByte.empty();
    //        }
    //
    //        byte candicate = Byte.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalByte.of(candicate);
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
    //    public ByteList row(final int i) {
    //        return ByteList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public ByteList column(final int j) {
    //        return ByteList.of(column2(j));
    //    }

    @Override
    public ByteMatrix copy() {
        final byte[][] c = new byte[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new ByteMatrix(c);
    }

    @Override
    public ByteMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final byte[][] c = new byte[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new ByteMatrix(c);
    }

    @Override
    public ByteMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final byte[][] c = new byte[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new ByteMatrix(c);
    }

    @Override
    public ByteMatrix rotate90() {
        final byte[][] c = new byte[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
            }
        }

        return new ByteMatrix(c);
    }

    @Override
    public ByteMatrix rotate180() {
        final byte[][] c = new byte[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new ByteMatrix(c);
    }

    @Override
    public ByteMatrix rotate270() {
        final byte[][] c = new byte[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new ByteMatrix(c);
    }

    @Override
    public ByteMatrix reshape(final int n, final int m) {
        return from(n, m, this.flatten());
    }

    @Override
    public byte[] flatten() {
        final byte[] c = new byte[n * m];
    
        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }
    
        return c;
    }

    public ByteMatrix add(final ByteMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final byte[][] c = new byte[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (byte) (a[i][j] + b.a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = (byte) (a[i][j] + b.a[i][j]);
                }
            }
        }

        return new ByteMatrix(c);
    }

    public ByteMatrix subtract(final ByteMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final byte[][] c = new byte[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (byte) (a[i][j] - b.a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = (byte) (a[i][j] - b.a[i][j]);
                }
            }
        }

        return new ByteMatrix(c);
    }

    public ByteMatrix multiply(final ByteMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final byte[][] c = new byte[n][b.m];
        final byte[][] a2 = b.a;

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

        return new ByteMatrix(c);
    }

    public IntMatrix toIntMatrix() {
        return IntMatrix.from(a);
    }

    @Override
    byte[] column2(final int j) {
        final byte[] c = new byte[n];

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

        if (obj instanceof ByteMatrix) {
            final ByteMatrix another = (ByteMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}