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
import java.util.Map;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 *
 * @param <L>
 * @param <R>
 */
public final class Pair<L, R> implements Map.Entry<L, R> {
    public volatile L left;
    public volatile R right;

    public Pair() {
    }

    public Pair(final L l, final R r) {
        this.left = l;
        this.right = r;
    }

    public static <L, R> Pair<L, R> of(final L l, final R r) {
        return new Pair<>(l, r);
    }

    public static <T> Pair<T, T> from(T[] a) {
        if (N.isNullOrEmpty(a)) {
            return new Pair<>(null, null);
        } else if (a.length == 1) {
            return new Pair<>(a[0], null);
        } else {
            return new Pair<>(a[0], a[1]);
        }
    }

    public static <T> Pair<T, T> from(Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return new Pair<>(null, null);
        } else if (c.size() == 1) {
            return new Pair<T, T>(c.iterator().next(), null);
        } else {
            final Iterator<? extends T> iter = c.iterator();
            return new Pair<T, T>(iter.next(), iter.next());
        }
    }

    public L left() {
        return left;
    }

    public R right() {
        return right;
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
    public L getKey() {
        return left;
    }

    @Override
    public R getValue() {
        return right;
    }

    @Override
    public R setValue(R value) {
        R oldValue = this.right;
        this.right = value;

        return oldValue;
    }

    public Pair<R, L> swap() {
        return new Pair<>(this.right, this.left);
    }

    public Pair<L, R> copy() {
        return new Pair<>(this.left, this.right);
    }

    public Object[] toArray() {
        return new Object[] { left, right };
    }

    public <A> A[] toArray(A[] a) {
        if (a.length < 2) {
            a = N.copyOf(a, 2);
        }

        a[0] = (A) left;
        a[1] = (A) right;

        return a;
    }

    public <T> ExList<T> toList() {
        return (ExList<T>) ExList.of(left, right);
    }

    public <T> Seq<T> toSeq() {
        return Seq.of((ExList<T>) toList());
    }

    public void forEach(Consumer<?> comsumer) {
        final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

        objComsumer.accept(left);
        objComsumer.accept(right);
    }

    public void accept(final BiConsumer<? super L, ? super R> action) {
        action.accept(left, right);
    }

    public void accept(final Consumer<Pair<L, R>> action) {
        action.accept(this);
    }

    public <U> U apply(final BiFunction<? super L, ? super R, U> action) {
        return action.apply(left, right);
    }

    public <U> U apply(final Function<Pair<L, R>, U> action) {
        return action.apply(this);
    }

    public Stream<Pair<L, R>> stream() {
        return Stream.of(this);
    }

    public Pair0<L, R> __() {
        return Pair0.of(left, right);
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
    public static final class Pair0<L, R> implements Map.Entry<L, R> {
        public final L left;
        public final R right;

        // for Kryo.
        Pair0() {
            this(null, null);
        }

        public Pair0(final L l, final R r) {
            this.left = l;
            this.right = r;
        }

        public static <L, R> Pair0<L, R> of(final L l, final R r) {
            return new Pair0<>(l, r);
        }

        public static <T> Pair0<T, T> from(T[] a) {
            if (N.isNullOrEmpty(a)) {
                return new Pair0<>(null, null);
            } else if (a.length == 1) {
                return new Pair0<>(a[0], null);
            } else {
                return new Pair0<>(a[0], a[1]);
            }
        }

        public static <T> Pair0<T, T> from(Collection<? extends T> c) {
            if (N.isNullOrEmpty(c)) {
                return new Pair0<>(null, null);
            } else if (c.size() == 1) {
                return new Pair0<T, T>(c.iterator().next(), null);
            } else {
                final Iterator<? extends T> iter = c.iterator();
                return new Pair0<T, T>(iter.next(), iter.next());
            }
        }

        public L left() {
            return left;
        }

        public R right() {
            return right;
        }

        @Override
        public L getKey() {
            return left;
        }

        @Override
        public R getValue() {
            return right;
        }

        /**
         * @param R
         * @deprecated UnsupportedOperationException
         */
        @Override
        @Deprecated
        public R setValue(R value) {
            throw new UnsupportedOperationException();
        }

        public Pair0<R, L> swap() {
            return new Pair0<>(this.right, this.left);
        }

        public Object[] toArray() {
            return new Object[] { left, right };
        }

        public <A> A[] toArray(A[] a) {
            if (a.length < 2) {
                a = N.copyOf(a, 2);
            }

            a[0] = (A) left;
            a[1] = (A) right;

            return a;
        }

        public <T> ExList<T> toList() {
            return (ExList<T>) ExList.of(left, right);
        }

        public <T> Seq<T> toSeq() {
            return Seq.of((ExList<T>) toList());
        }

        public void forEach(Consumer<?> comsumer) {
            final Consumer<Object> objComsumer = (Consumer<Object>) comsumer;

            objComsumer.accept(left);
            objComsumer.accept(right);
        }

        public void accept(final BiConsumer<? super L, ? super R> action) {
            action.accept(left, right);
        }

        public void accept(final Consumer<Pair0<L, R>> action) {
            action.accept(this);
        }

        public <U> U apply(final BiFunction<? super L, ? super R, U> action) {
            return action.apply(left, right);
        }

        public <U> U apply(final Function<Pair0<L, R>, U> action) {
            return action.apply(this);
        }

        public Stream<Pair0<L, R>> stream() {
            return Stream.of(this);
        }

        public Pair<L, R> __() {
            return Pair.of(left, right);
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
            return "[" + N.toString(left) + ", " + N.toString(right) + "]";
        }
    }
}
