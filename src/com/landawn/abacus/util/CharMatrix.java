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

import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class CharMatrix extends AbstractMatrix<char[], CharList, CharMatrix> {
    static final CharMatrix EMPTY_CHAR_MATRIX = new CharMatrix(new char[0][0]);

    public CharMatrix(final char[][] a) {
        super(a == null ? new char[0][0] : a);
    }

    public static CharMatrix empty() {
        return EMPTY_CHAR_MATRIX;
    }

    public static CharMatrix of(final char[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_CHAR_MATRIX : new CharMatrix(a);
    }

    public static CharMatrix random(final int len) {
        return new CharMatrix(new char[][] { CharList.random(len).array() });
    }

    public static CharMatrix repeat(final char val, final int len) {
        return new CharMatrix(new char[][] { Array.repeat(val, len) });
    }

    public static CharMatrix range(char startInclusive, final char endExclusive) {
        return new CharMatrix(new char[][] { Array.range(startInclusive, endExclusive) });
    }

    public static CharMatrix range(char startInclusive, final char endExclusive, final int by) {
        return new CharMatrix(new char[][] { Array.range(startInclusive, endExclusive, by) });
    }

    public static CharMatrix rangeClosed(char startInclusive, final char endInclusive) {
        return new CharMatrix(new char[][] { Array.rangeClosed(startInclusive, endInclusive) });
    }

    public static CharMatrix rangeClosed(char startInclusive, final char endInclusive, final int by) {
        return new CharMatrix(new char[][] { Array.rangeClosed(startInclusive, endInclusive, by) });
    }

    public char get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final char val) {
        a[i][j] = val;
    }

    public void fill(final char val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final CharUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsChar(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsChar(a[i][j]);
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalChar min() {
    //        if (isEmpty()) {
    //            return OptionalChar.empty();
    //        }
    //
    //        char candicate = Character.MAX_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] < candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalChar.of(candicate);
    //    }
    //
    //    public OptionalChar max() {
    //        if (isEmpty()) {
    //            return OptionalChar.empty();
    //        }
    //
    //        char candicate = Character.MIN_VALUE;
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (a[i][j] > candicate) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalChar.of(candicate);
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
    //    public CharList row(final int i) {
    //        return CharList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public CharList column(final int j) {
    //        return CharList.of(column2(j));
    //    }

    @Override
    public CharMatrix copy() {
        final char[][] c = new char[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final char[][] c = new char[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final char[][] c = new char[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix rotate90() {
        final char[][] c = new char[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
            }
        }

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix rotate180() {
        final char[][] c = new char[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix rotate270() {
        final char[][] c = new char[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix transpose() {
        final char[][] c = new char[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][i];
            }
        }

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix reshape(final int n, final int m) {
        final char[][] c = new char[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new CharMatrix(c);
        }

        long cnt = 0;

        for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
            for (int j = 0, col = (int) N.min(m, count - i * m); j < col; j++, cnt++) {
                c[i][j] = a[(int) (cnt / this.m)][(int) (cnt % this.m)];
            }
        }

        return new CharMatrix(c);
    }

    @Override
    public char[] flatten() {
        final char[] c = new char[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return c;
    }

    public CharMatrix add(final CharMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final char[][] c = new char[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (char) (a[i][j] + b.a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = (char) (a[i][j] + b.a[i][j]);
                }
            }
        }

        return new CharMatrix(c);
    }

    public CharMatrix subtract(final CharMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final char[][] c = new char[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (char) (a[i][j] - b.a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = (char) (a[i][j] - b.a[i][j]);
                }
            }
        }

        return new CharMatrix(c);
    }

    public CharMatrix multiply(final CharMatrix b) {
        N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

        final char[][] c = new char[n][b.m];
        final char[][] a2 = b.a;

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

        return new CharMatrix(c);
    }

    public IntMatrix toIntMatrix() {
        return IntMatrix.from(a);
    }

    @Override
    char[] column2(final int j) {
        final char[] c = new char[n];

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

        if (obj instanceof CharMatrix) {
            final CharMatrix another = (CharMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
