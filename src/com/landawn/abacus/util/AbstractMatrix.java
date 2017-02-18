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

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public abstract class AbstractMatrix<A, PL, X extends AbstractMatrix<A, PL, X>> {
    static final Random RAND = new SecureRandom();

    static final boolean isParallelStreamSupported;
    static {
        boolean tmp = false;

        try {
            if (N.forClass("com.landawn.abacus.util.stream.ParallelArrayIntStream") != null
                    && N.forClass("com.landawn.abacus.util.stream.ParallelIteratorIntStream") != null) {
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
    public final int n;

    /**
     * Column length.
     */
    public final int m;

    final A[] a;
    final long count;

    protected AbstractMatrix(A[] a) {
        this.a = a;
        this.n = a.length;
        this.m = a.length == 0 ? 0 : length(a[0]);

        if (a.length > 1) {
            for (int i = 1, len = a.length; i < len; i++) {
                if (length(a[i]) != this.m) {
                    throw new IllegalArgumentException("The length of sub arrays must be same");
                }
            }
        }

        this.count = this.m * this.n * 1L;
    }

    public long count() {
        return count;
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

    public X reshape(int m) {
        return reshape((int) (count % m == 0 ? count / m : count / m + 1), m);
    }

    public abstract X reshape(int n, int m);

    public boolean isSameShape(X x) {
        return this.n == x.n && this.m == x.m;
    }

    public abstract PL flatten();

    //    /**
    //     * 
    //     * @return the stream with each element based on row.
    //     */
    //    public Stream<A> stream() {
    //        return stream(0, n);
    //    }
    //
    //    /**
    //     * 
    //     * @param fromRowIndex
    //     * @param toRowIndex
    //     * @return the stream with each element based on row.
    //     */
    //    public Stream<A> stream(final int fromRowIndex, final int toRowIndex) {
    //        N.checkIndex(fromRowIndex, toRowIndex, n);
    //
    //        return Stream.of(a, fromRowIndex, toRowIndex);
    //    }
    //
    //    /**
    //     * 
    //     * @return the stream with each element based on column.
    //     */
    //    public Stream<A> stream2() {
    //        return stream2(0, m);
    //    }
    //
    //    /**
    //     * 
    //     * @param fromColumnIndex
    //     * @param toColumnIndex
    //     * @return the stream with each element based on column.
    //     */
    //    public Stream<A> stream2(final int fromColumnIndex, final int toColumnIndex) {
    //        N.checkIndex(fromColumnIndex, toColumnIndex, m);
    //
    //        return Stream.of(new ImmutableIterator<A>() {
    //            private int cursor = fromColumnIndex;
    //
    //            @Override
    //            public boolean hasNext() {
    //                return cursor < toColumnIndex;
    //            }
    //
    //            @Override
    //            public A next() {
    //                if (cursor >= toColumnIndex) {
    //                    throw new NoSuchElementException();
    //                }
    //
    //                return column2(cursor++);
    //            }
    //
    //            @Override
    //            public long count() {
    //                return toColumnIndex - cursor;
    //            }
    //
    //            @Override
    //            public void skip(long n) {
    //                cursor = n < toColumnIndex - cursor ? cursor + (int) n : toColumnIndex;
    //            }
    //        });
    //    }
    //
    //    abstract A column2(final int j);

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
