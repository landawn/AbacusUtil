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

import java.security.SecureRandom;
import java.util.Random;

import com.landawn.abacus.util.Pair.IntPair;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class AbstractMatrix<A, PL, HS, RS, X extends AbstractMatrix<A, PL, HS, RS, X>> {
    static final Random RAND = new SecureRandom();

    static final boolean isParallelStreamSupported;
    static {
        boolean tmp = false;

        try {
            if (ClassUtil.forClass("com.landawn.abacus.util.stream.ParallelArrayIntStream") != null
                    && ClassUtil.forClass("com.landawn.abacus.util.stream.ParallelIteratorIntStream") != null) {
                tmp = true;
            }
        } catch (Throwable e) {
            // ignore.
        }

        isParallelStreamSupported = tmp;
    }

    /**
     * Row length.
     */
    public final int rows;

    /**
     * Column length.
     */
    public final int cols;

    public final long count;

    final A[] a;

    protected AbstractMatrix(A[] a) {
        this.a = a;
        this.rows = a.length;
        this.cols = a.length == 0 ? 0 : length(a[0]);

        if (a.length > 1) {
            for (int i = 1, len = a.length; i < len; i++) {
                if (length(a[i]) != this.cols) {
                    throw new IllegalArgumentException("The length of sub arrays must be same");
                }
            }
        }

        this.count = this.cols * this.rows * 1L;
    }

    public void println() {
        for (A e : a) {
            N.println(e);
        }
    }

    public boolean isEmpty() {
        return count == 0;
    }

    // Replaced by stream and stream2.
    //    public abstract PL row(int i);
    //
    //    public abstract PL column(int j);

    /**
     * 
     * @return a new Matrix
     */
    public abstract X copy();

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @return a new Matrix
     */
    public abstract X copy(int fromRowIndex, int toRowIndex);

    /**
     * 
     * @param fromRowIndex
     * @param toRowIndex
     * @param fromColumnIndex
     * @param toColumnIndex
     * @return a new Matrix
     */
    public abstract X copy(int fromRowIndex, int toRowIndex, int fromColumnIndex, int toColumnIndex);

    /**
     * Rotate this matrix clockwise 90
     * 
     * @return a new Matrix
     */
    public abstract X rotate90();

    /**
     * Rotate this matrix clockwise 180
     * 
     * @return a new Matrix
     */
    public abstract X rotate180();

    /**
     * Rotate this matrix clockwise 270
     * 
     * @return a new Matrix
     */
    public abstract X rotate270();

    public abstract X transpose();

    public X reshape(int newCols) {
        return reshape((int) (count % newCols == 0 ? count / newCols : count / newCols + 1), newCols);
    }

    public abstract X reshape(int newRows, int newCols);

    public boolean isSameShape(X x) {
        return this.rows == x.rows && this.cols == x.cols;
    }

    /**
     * Repeat elements <code>rowRepeats</code> times in row direction and <code>colRepeats</code> times in column direction.
     * 
     * @param rowRepeats
     * @param colRepeats
     * @return a new matrix
     */
    public abstract X repelem(int rowRepeats, int colRepeats);

    /**
     * Repeat this matrix <code>rowRepeats</code> times in row direction and <code>colRepeats</code> times in column direction.
     * 
     * @param rowRepeats
     * @param colRepeats
     * @return a new matrix
     */
    public abstract X repmat(int rowRepeats, int colRepeats);

    public abstract PL flatten();

    public Stream<IntPair> pointsLU2RD() {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        return IntStream.range(0, rows).mapToObj(new IntFunction<IntPair>() {
            @Override
            public IntPair apply(int i) {
                return IntPair.of(i, i);
            }
        });
    }

    public Stream<IntPair> pointsRU2LD() {
        N.checkState(rows == cols, "'rows' and 'cols' must be same to get diagonals: rows=%s, cols=%s", rows, cols);

        return IntStream.range(0, rows).mapToObj(new IntFunction<IntPair>() {
            @Override
            public IntPair apply(int i) {
                return IntPair.of(i, cols - i - 1);
            }
        });
    }

    public Stream<IntPair> pointsH() {
        return pointsH(0, rows);
    }

    public Stream<IntPair> pointsH(int rowIndex) {
        return pointsH(rowIndex, rowIndex + 1);
    }

    public Stream<IntPair> pointsH(int fromRowIndex, int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        return IntStream.range(fromRowIndex, toRowIndex).flatMapToObj(new IntFunction<Stream<IntPair>>() {
            @Override
            public Stream<IntPair> apply(final int rowIndex) {
                return IntStream.range(0, cols).mapToObj(new IntFunction<IntPair>() {
                    @Override
                    public IntPair apply(final int columnIndex) {
                        return IntPair.of(rowIndex, columnIndex);
                    }
                });
            }
        });
    }

    public Stream<IntPair> pointsV() {
        return pointsV(0, cols);
    }

    public Stream<IntPair> pointsV(int columnIndex) {
        return pointsV(columnIndex, columnIndex + 1);
    }

    public Stream<IntPair> pointsV(int fromColumnIndex, int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        return IntStream.range(fromColumnIndex, toColumnIndex).flatMapToObj(new IntFunction<Stream<IntPair>>() {
            @Override
            public Stream<IntPair> apply(final int columnIndex) {
                return IntStream.range(0, rows).mapToObj(new IntFunction<IntPair>() {
                    @Override
                    public IntPair apply(final int rowIndex) {
                        return IntPair.of(rowIndex, columnIndex);
                    }
                });
            }
        });
    }

    public Stream<Stream<IntPair>> pointsR() {
        return pointsR(0, rows);
    }

    public Stream<Stream<IntPair>> pointsR(int fromRowIndex, int toRowIndex) {
        N.checkFromToIndex(fromRowIndex, toRowIndex, rows);

        return IntStream.range(fromRowIndex, toRowIndex).mapToObj(new IntFunction<Stream<IntPair>>() {
            @Override
            public Stream<IntPair> apply(final int rowIndex) {
                return IntStream.range(0, cols).mapToObj(new IntFunction<IntPair>() {
                    @Override
                    public IntPair apply(final int columnIndex) {
                        return IntPair.of(rowIndex, columnIndex);
                    }
                });
            }
        });
    }

    public Stream<Stream<IntPair>> pointsC() {
        return pointsR(0, cols);
    }

    public Stream<Stream<IntPair>> pointsC(int fromColumnIndex, int toColumnIndex) {
        N.checkFromToIndex(fromColumnIndex, toColumnIndex, cols);

        return IntStream.range(fromColumnIndex, toColumnIndex).mapToObj(new IntFunction<Stream<IntPair>>() {
            @Override
            public Stream<IntPair> apply(final int columnIndex) {
                return IntStream.range(0, rows).mapToObj(new IntFunction<IntPair>() {
                    @Override
                    public IntPair apply(final int rowIndex) {
                        return IntPair.of(rowIndex, columnIndex);
                    }
                });
            }
        });
    }

    public abstract HS streamLU2RD();

    public abstract HS streamRU2LD();

    public abstract HS streamH();

    public abstract HS streamH(final int rowIndex);

    public abstract HS streamH(final int fromRowIndex, final int toRowIndex);

    public abstract HS streamV();

    public abstract HS streamV(final int columnIndex);

    public abstract HS streamV(final int fromColumnIndex, final int toColumnIndex);

    public abstract RS streamR();

    public abstract RS streamR(final int fromRowIndex, final int toRowIndex);

    public abstract RS streamC();

    public abstract RS streamC(final int fromColumnIndex, final int toColumnIndex);

    protected abstract int length(A a);

    boolean isParallelable() {
        return isParallelStreamSupported && count > 8192;
    }

    boolean isParallelable(final int bm) {
        return isParallelStreamSupported && count * bm > 8192;
    }

    void checkSameShape(X x) {
        N.checkArgument(this.isSameShape(x), "Must be same shape");
    }
}
