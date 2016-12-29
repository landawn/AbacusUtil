package com.landawn.abacus.util;

import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;

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

    public static CharMatrix from(final int n, final int m, final char[] a) {
        final char[][] c = new char[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new CharMatrix(c);
        }

        for (int i = 0, len = N.min(n, a.length % m == 0 ? a.length / m : a.length / m + 1); i < len; i++) {
            for (int j = 0, col = N.min(m, a.length - i * m), off = i * m; j < col; j++) {
                c[i][j] = a[off + j];
            }
        }

        return new CharMatrix(c);
    }

    public static CharMatrix random(final int n, final int m) {
        final char[][] a = new char[n][m];

        if (n == 0 || m == 0) {
            return new CharMatrix(a);
        }

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                a[i][j] = (char) RAND.nextInt();
            }
        }

        return new CharMatrix(a);
    }

    public static CharMatrix repeat(final int n, final int m, final char val) {
        final char[][] c = new char[n][m];

        if (n == 0 || m == 0) {
            return new CharMatrix(c);
        }

        for (int i = 0; i < n; i++) {
            N.fill(c[i], val);
        }

        return new CharMatrix(c);
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
    public CharMatrix reshape(final int n, final int m) {
        return from(n, m, this.flatten());
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