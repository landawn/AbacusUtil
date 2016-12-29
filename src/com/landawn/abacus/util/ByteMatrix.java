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

import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
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

    public static ByteMatrix random(final int len) {
        return new ByteMatrix(new byte[][] { ByteList.random(len).array() });
    }

    public static ByteMatrix repeat(final byte val, final int len) {
        return new ByteMatrix(new byte[][] { Array.repeat(val, len) });
    }

    public static ByteMatrix range(byte startInclusive, final byte endExclusive) {
        return new ByteMatrix(new byte[][] { Array.range(startInclusive, endExclusive) });
    }

    public static ByteMatrix range(byte startInclusive, final byte endExclusive, final byte by) {
        return new ByteMatrix(new byte[][] { Array.range(startInclusive, endExclusive, by) });
    }

    public static ByteMatrix rangeClosed(byte startInclusive, final byte endInclusive) {
        return new ByteMatrix(new byte[][] { Array.rangeClosed(startInclusive, endInclusive) });
    }

    public static ByteMatrix rangeClosed(byte startInclusive, final byte endInclusive, final byte by) {
        return new ByteMatrix(new byte[][] { Array.rangeClosed(startInclusive, endInclusive, by) });
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
    public ByteMatrix transpose() {
        final byte[][] c = new byte[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][i];
            }
        }

        return new ByteMatrix(c);
    }

    @Override
    public ByteMatrix reshape(final int n, final int m) {
        final byte[][] c = new byte[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new ByteMatrix(c);
        }

        long cnt = 0;

        for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
            for (int j = 0, col = (int) N.min(m, count - i * m); j < col; j++, cnt++) {
                c[i][j] = a[(int) (cnt / this.m)][(int) (cnt % this.m)];
            }
        }

        return new ByteMatrix(c);
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
