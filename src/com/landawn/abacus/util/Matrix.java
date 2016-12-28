package com.landawn.abacus.util;

import java.security.SecureRandom;
import java.util.Map;
import java.util.Random;

import com.landawn.abacus.util.stream.Stream;

public abstract class Matrix<T> {
    static final Map<Class<?>, Integer> numArrayClasses = ImmutableMap.of(byte[].class, 0, short[].class, 1, int[].class, 2, long[].class, 3, float[].class, 4,
            double[].class, 5);

    static final Random RAND = new SecureRandom();

    public final int n;
    public final int m;
    final T[] a;
    final long count;

    public Matrix(T[] a) {
        this.a = a;
        this.m = a.length == 0 ? 0 : Array.getLength(a[0]);
        this.n = a.length;

        for (int i = 0, len = a.length; i < len; i++) {
            if (Array.getLength(a[i]) != this.m) {
                throw new IllegalArgumentException("The length of sub arrays must be same");
            }
        }

        this.count = this.m * this.n * 1L;
    }

    public T[] array() {
        return a;
    }

    public long count() {
        return count;
    }

    public void println() {
        for (T e : a) {
            N.println(e);
        }
    }

    public boolean isEmpty() {
        return m == 0 || n == 0;
    }

    /**
     * @param b
     * @return a new Matrix
     */
    public abstract Matrix<T> add(Matrix<T> b);

    /**
     * 
     * @param b
     * @return a new Matrix
     */
    public abstract Matrix<T> subtract(Matrix<T> b);

    public abstract Matrix<T> multiply(Matrix<T> b);

    public abstract Matrix<T> copy();

    public abstract Matrix<T> copy(int fromRow, int toRow);

    public abstract Matrix<T> copy(int fromRow, int toRow, int fromColumn, int toColumn);

    public abstract T flatten();

    public abstract Matrix<T> reshape(int n, int m);

    public Stream<T> stream() {
        return Stream.of(a);
    }

    public Stream<T> stream(int fromIndex, int toIndex) {
        return Stream.of(a, fromIndex, toIndex);
    }

    public static final class IntMatrix extends Matrix<int[]> {
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

        public static IntMatrix of(final int n, final int m, final int[] a) {
            final int[][] c = new int[n][m];

            if (n == 0 || m == 0 || N.isNullOrEmpty(a)) {
                return new IntMatrix(c);
            }

            for (int i = 0, len = N.min(n, a.length % m == 0 ? a.length / m : a.length / m + 1); i < len; i++) {
                for (int j = 0, col = N.min(m, a.length - i * m); j < col; j++) {
                    c[i][j] = a[i * m + j];
                }
            }

            return new IntMatrix(c);
        }

        public static IntMatrix from(final char[]... a) {
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

        public static IntMatrix from(final byte[]... a) {
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

        public static IntMatrix from(final short[]... a) {
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

            for (int i = 0, len = a.length; i < len; i++) {
                for (int j = 0, col = a[0].length; j < col; j++) {
                    a[i][j] = RAND.nextInt();
                }
            }

            return new IntMatrix(a);
        }

        public static IntMatrix repeat(final int n, final int m, final int val) {
            final int[][] a = new int[n][m];

            if (n == 0 || m == 0) {
                return new IntMatrix(a);
            }

            for (int i = 0, len = a.length; i < len; i++) {
                N.fill(a[i], val);
            }

            return new IntMatrix(a);
        }

        public int get(int i, int j) {
            return a[i][j];
        }

        public void set(int i, int j, int val) {
            a[i][j] = val;
        }

        public void fill(int val) {
            for (int i = 0; i < n; i++) {
                N.fill(a[i], val);
            }
        }

        /**
         * 
         * @param val
         * @return a new Matrix
         */
        public IntMatrix add(int val) {
            final int[][] c = new int[n][m];

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j] + val;
                }
            }

            return new IntMatrix(c);
        }

        @Override
        public IntMatrix add(Matrix<int[]> b) {
            N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

            final int[][] c = new int[n][m];

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j] + b.a[i][j];
                }
            }

            return new IntMatrix(c);
        }

        @Override
        public IntMatrix subtract(Matrix<int[]> b) {
            N.checkArgument(this.n == b.n && this.m == b.m, "The 'n' and length are not equal");

            final int[][] c = new int[n][m];

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    c[i][j] = a[i][j] - b.a[i][j];
                }
            }

            return new IntMatrix(c);
        }

        @Override
        public IntMatrix multiply(Matrix<int[]> b) {
            N.checkArgument(this.m == b.n, "Illegal matrix dimensions");

            final int[][] c = new int[n][b.m];
            final int[][] a2 = b.a;

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < b.m; j++) {
                    for (int k = 0; k < m; k++) {
                        c[i][j] += a[i][k] * a2[k][j];
                    }
                }
            }

            return new IntMatrix(c);
        }

        public OptionalInt min() {
            if (isEmpty()) {
                return OptionalInt.empty();
            }

            int candicate = Integer.MAX_VALUE;

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    if (a[i][j] < candicate) {
                        candicate = a[i][j];
                    }
                }
            }

            return OptionalInt.of(candicate);
        }

        public OptionalInt max() {
            if (isEmpty()) {
                return OptionalInt.empty();
            }

            int candicate = Integer.MIN_VALUE;

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    if (a[i][j] > candicate) {
                        candicate = a[i][j];
                    }
                }
            }

            return OptionalInt.of(candicate);
        }

        public Long sum() {
            long sum = 0;

            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    sum += a[i][j];
                }
            }

            return sum;
        }

        public OptionalDouble average() {
            if (isEmpty()) {
                return OptionalDouble.empty();
            }

            return OptionalDouble.of(sum() / (m * n));
        }

        @Override
        public IntMatrix copy() {
            final int[][] b = new int[n][];

            for (int i = 0; i < n; i++) {
                b[i] = a[i].clone();
            }

            return new IntMatrix(b);
        }

        @Override
        public IntMatrix copy(int fromRow, int toRow) {
            N.checkIndex(fromRow, toRow, n);

            final int[][] b = new int[toRow - fromRow][];

            for (int i = fromRow; i < toRow; i++) {
                b[i - fromRow] = a[i].clone();
            }

            return new IntMatrix(b);
        }

        @Override
        public IntMatrix copy(int fromRow, int toRow, int fromColumn, int toColumn) {
            N.checkIndex(fromRow, toRow, n);
            N.checkIndex(fromColumn, toColumn, m);

            final int[][] b = new int[toRow - fromRow][];

            for (int i = fromRow; i < toRow; i++) {
                b[i - fromRow] = N.copyOfRange(a[i], fromColumn, toColumn);
            }

            return new IntMatrix(b);
        }

        @Override
        public int[] flatten() {
            final int[] result = new int[n * m];

            for (int i = 0; i < n; i++) {
                N.copy(a[i], 0, result, i * m, m);
            }

            return result;
        }

        @Override
        public IntMatrix reshape(int n, int m) {
            return of(n, m, this.flatten());
        }

        public IntList row(int i) {
            return IntList.of(a[i].clone());
        }

        public IntList column(int j) {
            final int[] c = new int[n];

            for (int i = 0; i < n; i++) {
                c[i] = a[i][j];
            }

            return IntList.of(c);
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

    public static abstract class LongMaxtrix extends Matrix<long[]> {
        public LongMaxtrix(long[][] a) {
            super(a);
        }
    }

    public static abstract class DoubleMatrix extends Matrix<double[]> {
        public DoubleMatrix(double[][] a) {
            super(a);
        }
    }
}
