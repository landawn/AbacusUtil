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

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 *
 * @param <L>
 * @param <R>
 */
public final class Pair<L, R> {
    public L left;
    public R right;

    public Pair() {
    }

    public Pair(final L l, final R r) {
        this.left = l;
        this.right = r;
    }

    public static <L, R> Pair<L, R> of(final L l, final R r) {
        return new Pair<L, R>(l, r);
    }

    public L getLeft() {
        return left;
    }

    public Pair<L, R> setLeft(final L left) {
        this.left = left;

        return this;
    }

    public R getRight() {
        return right;
    }

    public Pair<L, R> setRight(final R right) {
        this.right = right;

        return this;
    }

    public Pair<L, R> set(final L left, final R right) {
        this.left = left;
        this.right = right;

        return this;
    }

    //    /**
    //     * Swap the left and right value. they must be same type.
    //     * 
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
        result = prime * result + N.hashCode(right);
        return result;
    }

    @Override
    public boolean equals(final Object obj) {
        if (this == obj) {
            return true;
        }

        if (obj instanceof Pair) {
            final Pair<L, R> other = (Pair<L, R>) obj;

            return N.equals(left, other.left) && N.equals(right, other.right);
        }

        return false;
    }

    @Override
    public String toString() {
        return "{left=" + N.toString(left) + ", right=" + N.toString(right) + "}";
    }

    /**
     * Immutable Pair
     *
     * @param <L>
     * @param <R>
     */
    public static final class Pair0<L, R> {
        public final L left;
        public final R right;

        public Pair0(final L l, final R r) {
            this.left = l;
            this.right = r;
        }

        public static <L, R> Pair0<L, R> of(final L l, final R r) {
            return new Pair0<L, R>(l, r);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + N.hashCode(left);
            result = prime * result + N.hashCode(right);
            return result;
        }

        @Override
        public boolean equals(final Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj instanceof Pair0) {
                final Pair0<L, R> other = (Pair0<L, R>) obj;

                return N.equals(left, other.left) && N.equals(right, other.right);
            }

            return false;
        }

        @Override
        public String toString() {
            return "{left=" + N.toString(left) + ", right=" + N.toString(right) + "}";
        }
    }
}
