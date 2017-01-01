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

import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.UnaryOperator;
import com.landawn.abacus.util.stream.IntStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Matrix<T> extends AbstractMatrix<T[], ObjectList<T>, Matrix<T>> {
    private Class<T[]> arrayType;
    private Class<T> componentType;

    public Matrix(final T[][] a) {
        super(a);
        this.arrayType = (Class<T[]>) a.getClass().getComponentType();
        this.componentType = (Class<T>) arrayType.getComponentType();
    }

    public static <T> Matrix<T> of(final T[]... a) {
        return new Matrix<T>(a);
    }

    public static <T> Matrix<T> repeat(final T val, final int len) {
        final T[][] c = N.newArray(N.newArray(val.getClass(), 0).getClass(), 1);
        c[0] = Array.repeat(val, len);
        return new Matrix<T>(c);
    }

    public T get(final int i, final int j) {
        return a[i][j];
    }

    public void set(final int i, final int j, final T val) {
        a[i][j] = val;
    }

    public void fill(final T val) {
        for (int i = 0; i < n; i++) {
            N.fill(a[i], val);
        }
    }

    public void replaceAll(final UnaryOperator<T> operator) {
        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        a[i][j] = operator.apply(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    a[i][j] = operator.apply(a[i][j]);
                }
            }
        }
    }

    // Replaced by stream and stream2.
    //    public OptionalNullable<T> min(final Comparator<? super T> cmp) {
    //        if (isEmpty()) {
    //            return OptionalNullable.empty();
    //        }
    //
    //        final Comparator<? super T> comparator = cmp == null ? N.NULL_MIN_COMPARATOR : cmp;
    //        T candicate = a[0][0];
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (comparator.compare(a[i][j], candicate) < 0) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalNullable.of(candicate);
    //    }
    //
    //    public OptionalNullable<T> max(final Comparator<? super T> cmp) {
    //        if (isEmpty()) {
    //            return OptionalNullable.empty();
    //        }
    //
    //        final Comparator<? super T> comparator = cmp == null ? N.NULL_MIN_COMPARATOR : cmp;
    //        T candicate = a[0][0];
    //
    //        for (int i = 0; i < n; i++) {
    //            for (int j = 0; j < m; j++) {
    //                if (comparator.compare(a[i][j], candicate) > 0) {
    //                    candicate = a[i][j];
    //                }
    //            }
    //        }
    //
    //        return OptionalNullable.of(candicate);
    //    }
    //
    //    @Override
    //    public ObjectList<T> row(final int i) {
    //        return ObjectList.of(a[i].clone());
    //    }
    //
    //    @Override
    //    public ObjectList<T> column(final int j) {
    //        return ObjectList.of(column2(j));
    //    }

    public <R> Matrix<R> map(final Class<R> cls, final Function<? super T, R> func) {
        final R[][] c = N.newArray(N.newArray(cls, 0).getClass(), n);

        for (int i = 0; i < n; i++) {
            c[i] = N.newArray(cls, m);
        }

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.apply(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.apply(a[i][j]);
                }
            }
        }

        return Matrix.of(c);
    }

    public BooleanMatrix mapToBoolean(final ToBooleanFunction<? super T> func) {
        final boolean[][] c = new boolean[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsBoolean(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsBoolean(a[i][j]);
                }
            }
        }

        return BooleanMatrix.of(c);
    }

    public ByteMatrix mapToByte(final ToByteFunction<? super T> func) {
        final byte[][] c = new byte[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsByte(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsByte(a[i][j]);
                }
            }
        }

        return ByteMatrix.of(c);
    }

    public CharMatrix mapToChar(final ToCharFunction<? super T> func) {
        final char[][] c = new char[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsChar(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsChar(a[i][j]);
                }
            }
        }

        return CharMatrix.of(c);
    }

    public ShortMatrix mapToShort(final ToShortFunction<? super T> func) {
        final short[][] c = new short[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsShort(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsShort(a[i][j]);
                }
            }
        }

        return ShortMatrix.of(c);
    }

    public IntMatrix mapToInt(final ToIntFunction<? super T> func) {
        final int[][] c = new int[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsInt(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsInt(a[i][j]);
                }
            }
        }

        return IntMatrix.of(c);
    }

    public LongMatrix mapToLong(final ToLongFunction<? super T> func) {
        final long[][] c = new long[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsLong(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsLong(a[i][j]);
                }
            }
        }

        return LongMatrix.of(c);
    }

    public FloatMatrix mapToFloat(final ToFloatFunction<? super T> func) {
        final float[][] c = new float[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsFloat(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsFloat(a[i][j]);
                }
            }
        }

        return FloatMatrix.of(c);
    }

    public DoubleMatrix mapToDouble(final ToDoubleFunction<? super T> func) {
        final double[][] c = new double[n][m];

        if (isParallelable()) {
            IntStream.range(0, n).parallel().forEach(new IntConsumer() {
                @Override
                public void accept(final int i) {
                    for (int j = 0; j < m; j++) {
                        c[i][j] = func.applyAsDouble(a[i][j]);
                    }
                }
            });
        } else {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = func.applyAsDouble(a[i][j]);
                }
            }
        }

        return DoubleMatrix.of(c);
    }

    @Override
    public Matrix<T> copy() {
        final T[][] c = N.newArray(arrayType, n);

        for (int i = 0; i < n; i++) {
            c[i] = a[i].clone();
        }

        return new Matrix<T>(c);
    }

    @Override
    public Matrix<T> copy(final int fromRowIndex, final int toRowIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);

        final T[][] c = N.newArray(arrayType, toRowIndex - fromRowIndex);

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = a[i].clone();
        }

        return new Matrix<T>(c);
    }

    @Override
    public Matrix<T> copy(final int fromRowIndex, final int toRowIndex, final int fromColumnIndex, final int toColumnIndex) {
        N.checkIndex(fromRowIndex, toRowIndex, n);
        N.checkIndex(fromColumnIndex, toColumnIndex, m);

        final T[][] c = N.newArray(arrayType, toRowIndex - fromRowIndex);

        for (int i = fromRowIndex; i < toRowIndex; i++) {
            c[i - fromRowIndex] = N.copyOfRange(a[i], fromColumnIndex, toColumnIndex);
        }

        return new Matrix<T>(c);
    }

    @Override
    public Matrix<T> rotate90() {
        final T[][] c = N.newArray(arrayType, m);

        for (int i = 0; i < m; i++) {
            c[i] = N.newArray(this.componentType, n);
        }

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[n - j - 1][i];
            }
        }

        return new Matrix<T>(c);
    }

    @Override
    public Matrix<T> rotate180() {
        final T[][] c = N.newArray(arrayType, n);

        for (int i = 0; i < n; i++) {
            c[i] = a[n - i - 1].clone();
            N.reverse(c[i]);
        }

        return new Matrix<T>(c);
    }

    @Override
    public Matrix<T> rotate270() {
        final T[][] c = N.newArray(arrayType, m);

        for (int i = 0; i < m; i++) {
            c[i] = N.newArray(this.componentType, n);
        }

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][m - i - 1];
            }
        }

        return new Matrix<T>(c);
    }

    @Override
    public Matrix<T> transpose() {
        final T[][] c = N.newArray(arrayType, m);

        for (int i = 0; i < m; i++) {
            c[i] = N.newArray(componentType, n);
        }

        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                c[i][j] = a[j][i];
            }
        }

        return new Matrix<T>(c);
    }

    @Override
    public Matrix<T> reshape(final int n, final int m) {
        final T[][] c = N.newArray(arrayType, n);

        for (int i = 0; i < n; i++) {
            c[i] = N.newArray(componentType, m);
        }

        if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
            return new Matrix<T>(c);
        }

        if (a.length == 1) {
            final T[] a0 = a[0];

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

        return new Matrix<T>(c);
    }

    @Override
    T[] column2(final int j) {
        final T[] c = N.newArray(componentType, n);

        for (int i = 0; i < n; i++) {
            c[i] = a[i][j];
        }

        return c;
    }

    @Override
    public ObjectList<T> flatten() {
        final T[] c = N.newArray(componentType, n * m);

        for (int i = 0; i < n; i++) {
            N.copy(a[i], 0, c, i * m, m);
        }

        return ObjectList.of(c);
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

        if (obj instanceof Matrix) {
            final Matrix<T> another = (Matrix<T>) obj;

            return N.deepEquals(this.a, another.a);
        }

        return false;
    }

    @Override
    public String toString() {
        return N.deepToString(a);
    }
}
