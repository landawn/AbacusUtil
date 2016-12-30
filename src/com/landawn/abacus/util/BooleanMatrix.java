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

import com.landawn.abacus.util.function.BooleanUnaryOperator;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.stream.IntStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class BooleanMatrix extends AbstractMatrix<boolean[], BooleanList, BooleanMatrix> {
    static final BooleanMatrix EMPTY_BOOLEAN_MATRIX = new BooleanMatrix(new boolean[0][0]);

    public BooleanMatrix(final boolean[][] a) {
        super(a == null ? new boolean[0][0] : a);
    }

    public static BooleanMatrix empty() {
        return EMPTY_BOOLEAN_MATRIX;
    }

    public static BooleanMatrix of(final boolean[]... a) {
        return N.isNullOrEmpty(a) ? EMPTY_BOOLEAN_MATRIX : new BooleanMatrix(a);
    }

    public static BooleanMatrix random(final int len) {
        return new BooleanMatrix(new boolean[][] { BooleanList.random(len).array() });
    }

    public static BooleanMatrix repeat(final boolean val, final int len) {
        return new BooleanMatrix(new boolean[][] { Array.repeat(val, len) });
    }

    public boolean get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final boolean val) {
        a[i][j] = val;
    }

    public void fill(final boolean val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final BooleanUnaryOperator operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.applyAsBoolean(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.applyAsBoolean(a[i][j]);
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    @Override
    //    public BooleanList row(final int i) {
    //        return BooleanList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public BooleanList column(final int j) {
    //        return BooleanList.of(column2(j));
    //    }

    @Override
    public BooleanMatrix copy() {
        final boolean[][] c = new boolean[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix copy(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final boolean[][] c = new boolean[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final boolean[][] b = new boolean[toRowIndex - fromRowIndex][];

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            b[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new BooleanMatrix(b);
    }

    @Override
    public BooleanMatrix rotate90() {
        final boolean[][] c = new boolean[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
            }
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix rotate180() {
        final boolean[][] c = new boolean[n][];

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix rotate270() {
        final boolean[][] c = new boolean[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix transpose() {
        final boolean[][] c = new boolean[m][n];

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][i];
            }
        }

        return new BooleanMatrix(c);
    }

    @Override
    public BooleanMatrix reshape(final int n, final int m) {
        final boolean[][] c = new boolean[n][m];

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new BooleanMatrix(c);
        }

        long cnt = 0;

        for (int i = 0, len = (int) N.min(n, count % m == 0 ? count / m : count / m + 1); i < len; i++) {
            for (int j = 0, col = (int) N.min(m, count - i * m); j < col; j++, cnt++) {
                c[i][j] = a[(int) (cnt / this.m)][(int) (cnt % this.m)];
            }
        }

        return new BooleanMatrix(c);
    }

    @Override
    boolean[] column2(final int j) {
        final boolean[] c = new boolean[n];

        for (int i = 0; i < n; i++) {
            c[i] = a[i][j];
        }

        return c;
    }

    @Override
    public BooleanList flatten() {
        final boolean[] c = new boolean[n * m];

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return BooleanList.of(c);
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

        if (obj instanceof BooleanMatrix) {
            final BooleanMatrix another = (BooleanMatrix) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
