/*
 * Copyright (c) 2015, Haiyang Li.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.landawn.abacus.util;

import java.util.Collection;
import java.util.Iterator;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 *
 * @param <L>
 * @param <M>
 * @param <R>
 */
public final class Triple<L, M, R> {
    public L left;
    public M middle;
    public R right;

    public Triple() {
    }

    public Triple(final L l, final M m, final R r) {
        this.left = l;
        this.middle = m;
        this.right = r;
    }

    public static <L, M, R> Triple<L, M, R> of(final L l, final M m, final R r) {
        return new Triple<L, M, R>(l, m, r);
    }

    public static <T> Triple<T, T, T> from(T[] a) {
        if (N.isNullOrEmpty(a)) {
            return new Triple<T, T, T>(null, null, null);
        } else if (a.length == 1) {
            return new Triple<T, T, T>(a[0], null, null);
        } else if (a.length == 2) {
            return new Triple<T, T, T>(a[0], a[1], null);
        } else {
            return new Triple<T, T, T>(a[0], a[1], a[2]);
        }
    }

    public static <T> Triple<T, T, T> from(Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return new Triple<T, T, T>(null, null, null);
        } else if (c.size() == 1) {
            return new Triple<T, T, T>(c.iterator().next(), null, null);
        } else if (c.size() == 2) {
            final Iterator<? extends T> iter = c.iterator();
            return new Triple<T, T, T>(iter.next(), iter.next(), null);
        } else {
            final Iterator<? extends T> iter = c.iterator();
            return new Triple<T, T, T>(iter.next(), iter.next(), iter.next());
        }
    }

    public L left() {
        return left;
    }

    public M middle() {
        return middle;
    }

    public R right() {
        return right;
    }

    public L getLeft() {
        return left;
    }

    public Triple<L, M, R> setLeft(final L left) {
        this.left = left;

        return this;
    }

    public M getMiddle() {
        return middle;
    }

    public Triple<L, M, R> setMiddle(final M middle) {
        this.middle = middle;

        return this;
    }

    public R getRight() {
        return right;
    }

    public Triple<L, M, R> setRight(final R right) {
        this.right = right;

        return this;
    }

    public Triple<L, M, R> set(final L left, final M middle, final R right) {
        this.left = left;
        this.middle = middle;
        this.right = right;

        return this;
    }

    //    /**
    //     * Swap the left and right value. they must be same type.
    //     */
    //    public void reverse() {
    //        Object tmp = left;
    //        this.left = (L) right;
    //        this.right = (R) tmp;
    //    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + N.hashCode(left);
        result = prime * result + N.hashCode(middle);
        result = prime * result + N.hashCode(right);
        return result;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof Triple) {
            final Triple<L, M, R> other = (Triple<L, M, R>) obj;

            return N.equals(left, other.left) && N.equals(middle, other.middle) && N.equals(right, other.right);
        }

        return false;
    }

    @Override
    public String toString() {
        return "{left=" + N.toString(left) + ", middle=" + N.toString(middle) + ", right=" + N.toString(right) + "}";
    }

    /**
     * Immutable Triple
     *
     * @param <L>
     * @param <M>
     * @param <R>
     */
    public static final class Triple0<L, M, R> {
        public final L left;
        public final M middle;
        public final R right;

        public Triple0(final L l, final M m, final R r) {
            this.left = l;
            this.middle = m;
            this.right = r;
        }

        public static <L, M, R> Triple0<L, M, R> of(final L l, final M m, final R r) {
            return new Triple0<L, M, R>(l, m, r);
        }

        public static <T> Triple0<T, T, T> from(T[] a) {
            if (N.isNullOrEmpty(a)) {
                return new Triple0<T, T, T>(null, null, null);
            } else if (a.length == 1) {
                return new Triple0<T, T, T>(a[0], null, null);
            } else if (a.length == 2) {
                return new Triple0<T, T, T>(a[0], a[1], null);
            } else {
                return new Triple0<T, T, T>(a[0], a[1], a[2]);
            }
        }

        public static <T> Triple0<T, T, T> from(Collection<? extends T> c) {
            if (N.isNullOrEmpty(c)) {
                return new Triple0<T, T, T>(null, null, null);
            } else if (c.size() == 1) {
                return new Triple0<T, T, T>(c.iterator().next(), null, null);
            } else if (c.size() == 2) {
                final Iterator<? extends T> iter = c.iterator();
                return new Triple0<T, T, T>(iter.next(), iter.next(), null);
            } else {
                final Iterator<? extends T> iter = c.iterator();
                return new Triple0<T, T, T>(iter.next(), iter.next(), iter.next());
            }
        }

        public L left() {
            return left;
        }

        public M middle() {
            return middle;
        }

        public R right() {
            return right;
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(left);
            result = prime * result + N.hashCode(middle);
            result = prime * result + N.hashCode(right);
            return result;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj instanceof Triple) {
                final Triple0<L, M, R> other = (Triple0<L, M, R>) obj;

                return N.equals(left, other.left) && N.equals(middle, other.middle) && N.equals(right, other.right);
            }

            return false;
        }

        @Override
        public String toString() {
            return "{left=" + N.toString(left) + ", middle=" + N.toString(middle) + ", right=" + N.toString(right) + "}";
        }
    }
}
