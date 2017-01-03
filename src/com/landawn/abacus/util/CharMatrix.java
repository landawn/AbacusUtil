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

import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.CharStream;
import com.landawn.abacus.util.stream.ImmutableCharIterator;
import com.landawn.abacus.util.stream.ImmutableIterator;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.Stream;

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
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            a[i][j] = operator.applyAsChar(a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            a[i][j] = operator.applyAsChar(a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsChar(a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        a[i][j] = operator.applyAsChar(a[i][j]);
                    }
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
    //        char candicate = Char.MAX_VALUE;
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
    //        char candicate = Char.MIN_VALUE;
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

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix transpose() {
        final char[][] c = new char[m][n];

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

        return new CharMatrix(c);
    }

    @Override
    public CharMatrix reshape(final int n, final int m) {
        final char[][] c = new char[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new CharMatrix(c);
        }

        if (a.length == 1) {
            final char[] a0 = a[0];

            if (m < 8) {
                for (int cnt = 0, i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
                    for (int j = 0, col = (int) N.min(m, count - i * m); j < col; j++) {
                        c[i][j] = a0[cnt++];
                    }
                }
            } else {
                for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
                    N.copy(a0, i * m, c[i], 0, (int) N.min(m, count - i * m));
                }
            }
        } else {
            long cnt = 0;

            for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
                for (int j = 0, col = (int) N.min(m, count - i * m); j < col; j++, cnt++) {
                    c[i][j] = a[(int) (cnt / this.m)][(int) (cnt % this.m)];
                }
            }
        }

        return new CharMatrix(c);
    }

    @Override
    public CharList flatten() {
        final char[] c = new char[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return CharList.of(c);
    }

    public CharMatrix add(final CharMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final char[][] c = new char[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = (char) (a[i][j] + b.a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = (char) (a[i][j] + b.a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (char) (a[i][j] + b.a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = (char) (a[i][j] + b.a[i][j]);
                    }
                }
            }
        }

        return new CharMatrix(c);
    }

    public CharMatrix subtract(final CharMatrix b) {
        N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

        final char[][] c = new char[n][m];

        if (isParallelable()) {
            if (n <= m) {
                IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int i) {
                        for (int j = 0; j < m; j++) {
                            c[i][j] = (char) (a[i][j] - b.a[i][j]);
                        }
                    }
                });
            } else {
                IntStream.range(0, m).parallel().forEach(new IntConsumer() {
                    @Override
                    public void accept(final int j) {
                        for (int i = 0; i < n; i++) {
                            c[i][j] = (char) (a[i][j] - b.a[i][j]);
                        }
                    }
                });
            }
        } else {
            if (n <= m) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = (char) (a[i][j] - b.a[i][j]);
                    }
                }
            } else {
                for (int j = 0; j < m; j++) {
                    for (int i = 0; i < n; i++) {
                        c[i][j] = (char) (a[i][j] - b.a[i][j]);
                    }
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

        return new CharMatrix(c);
    }

    public IntMatrix toIntMatrix() {
        return IntMatrix.from(a);
    }

    /**
     * 
     * @return a stream based on the order of row.
     */
    public CharStream stream() {
        return stream(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a stream based on the order of row.
     */
    public CharStream stream(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return CharStream.empty();
        }

        return CharStream.of(new ImmutableCharIterator() {
            private final long toIndex = toRowIndex * m * 1L;
            private long cursor = fromRowIndex * m * 1L;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public char next() {
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

    // TODO undecided.
    //    /**
    //     * 
    //     * @return a stream based on the order of column.
    //     */
    //    public CharStream stream0() {
    //        return stream0(0, m);
    //    }
    //
    //    /**
    //     * 
    //     * @param fromColumnIndex
    //     * @param toColumnIndex
    //     * @return a stream based on the order of column.
    //     */
    //    public CharStream stream0(final int fromColumnIndex, final int toColumnIndex) {
    //        N.checkIndex(fromColumnIndex, toColumnIndex, m);
    //
    //        if (isEmpty()) {
    //            return CharStream.empty();
    //        }
    //
    //        return CharStream.of(new ImmutableCharIterator() {
    //            private final long toIndex = toColumnIndex * n * 1L;
    //            private long cursor = fromColumnIndex * n * 1L;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return cursor < toIndex;
    //            }
    //
    //            @Override
    //            public char next() {
    //                if (cursor >= toIndex) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                return a[(int) (cursor % n)][(int) (cursor++ / n)];
    //            }
    //
    //            @Override
    //            public void skip(long n) {
    //                cursor = n < toIndex - cursor ? cursor + n : toIndex;
    //            }
    //
    //            @Override
    //            public long count() {
    //                return toIndex - cursor;
    //            }
    //        });
    //    }

    /**
     * 
     * @return a row stream based on the order of row.
     */
    public Stream<CharStream> stream2() {
        return stream2(0, n);
    }

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a row stream based on the order of row.
     */
    public Stream<CharStream> stream2(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        if (isEmpty()) {
            return Stream.empty();
        }

        return Stream.of(new ImmutableIterator<CharStream>() {
            private final int toIndex = toRowIndex;
            private int cursor = fromRowIndex;

            @Override
            public boolean hasNext() {
                return cursor < toIndex;
            }

            @Override
            public CharStream next() {
                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return CharStream.of(a[cursor++]);
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

    // TODO undecided.
    //    /**
    //     * 
    //     * @return a column stream based on the order of column.
    //     */
    //    public Stream<CharStream> stream02() {
    //        return stream02(0, m);
    //    }
    //
    //    /**
    //     * 
    //     * @param fromColumnIndex
    //     * @param toColumnIndex
    //     * @return a column stream based on the order of column.
    //     */
    //    public Stream<CharStream> stream02(final int fromColumnIndex, final int toColumnIndex) {
    //        N.checkIndex(fromColumnIndex, toColumnIndex, m);
    //
    //        if (isEmpty()) {
    //            return Stream.empty();
    //        }
    //
    //        return Stream.of(new ImmutableIterator<CharStream>() {
    //            private final int toIndex = toColumnIndex;
    //            private volatile int cursor = fromColumnIndex;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return cursor < toIndex;
    //            }
    //
    //            @Override
    //            public CharStream next() {
    //                if (cursor >= toIndex) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                return CharStream.of(new ImmutableCharIterator() {
    //                    private final int columnIndex = cursor++;
    //                    private final int toIndex2 = n;
    //                    private int cursor2 = 0;
    //
    //                    @Override
    //                    public boolean hasNext() {
    //                        return cursor2 < toIndex2;
    //                    }
    //
    //                    @Override
    //                    public char next() {
    //                        if (cursor2 >= toIndex2) {
    //                            throw new NoSuchElementException();
    //                        }
    //
    //                        return a[cursor2++][columnIndex];
    //                    }
    //
    //                    @Override
    //                    public void skip(long n) {
    //                        cursor2 = n < toIndex2 - cursor2 ? cursor2 + (int) n : toIndex2;
    //                    }
    //
    //                    @Override
    //                    public long count() {
    //                        return toIndex2 - cursor2;
    //                    }
    //                });
    //            }
    //
    //            @Override
    //            public void skip(long n) {
    //                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
    //            }
    //
    //            @Override
    //            public long count() {
    //                return toIndex - cursor;
    //            }
    //        });
    //    }

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
