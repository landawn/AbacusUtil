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
import java.util.List;

import com.landawn.abacus.util.Tuple.Tuple3;
import com.landawn.abacus.util.stream.Stream;

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
    public volatile L left;
    public volatile M middle;
    public volatile R right;

    public Triple() {
    }

    Triple(final L l, final M m, final R r) {
        this.left = l;
        this.middle = m;
        this.right = r;
    }

    public static <L, M, R> Triple<L, M, R> of(final L l, final M m, final R r) {
        return new Triple<>(l, m, r);
    }

    public static <T> Triple<T, T, T> from(T[] a) {
        if (N.isNullOrEmpty(a)) {
            return new Triple<>(null, null, null);
        } else if (a.length == 1) {
            return new Triple<>(a[0], null, null);
        } else if (a.length == 2) {
            return new Triple<>(a[0], a[1], null);
        } else {
            return new Triple<>(a[0], a[1], a[2]);
        }
    }

    public static <T> Triple<T, T, T> from(Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return new Triple<>(null, null, null);
        }

        final List<T> list = c instanceof List ? (List<T>) c : null;

        if (c.size() == 1) {
            if (list != null) {
                return new Triple<T, T, T>(list.get(0), null, null);
            } else {
                return new Triple<T, T, T>(c.iterator().next(), null, null);
            }
        } else if (c.size() == 2) {
            if (list != null) {
                return new Triple<T, T, T>(list.get(0), list.get(1), null);
            } else {
                final Iterator<? extends T> iter = c.iterator();
                return new Triple<T, T, T>(iter.next(), iter.next(), null);
            }
        } else {
            if (list != null) {
                return new Triple<T, T, T>(list.get(0), list.get(1), list.get(2));
            } else {
                final Iterator<? extends T> iter = c.iterator();
                return new Triple<T, T, T>(iter.next(), iter.next(), iter.next());
            }
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

    public L getAndSetLeft(L newLeft) {
        final L res = left;
        left = newLeft;
        return res;
    }

    public L setAndGetLeft(L newLeft) {
        left = newLeft;
        return left;
    }

    public M getAndSetMiddle(M newMiddle) {
        final M res = middle;
        middle = newMiddle;
        return res;
    }

    public M setAndGetMiddle(M newMiddle) {
        middle = newMiddle;
        return middle;
    }

    public R getAndSetRight(R newRight) {
        final R res = newRight;
        right = newRight;
        return res;
    }

    public R setAndGetRight(R newRight) {
        right = newRight;
        return right;
    }

    /**
     * Set to the specified <code>newLeft</code> and returns <code>true</code>
     * if <code>predicate</code> returns true. Otherwise returns
     * <code>false</code> without setting the value to new value.
     * 
     * @param newLeft
     * @param predicate - the first parameter is current pair, the second
     *        parameter is the <code>newLeft</code>
     * @return
     */
    public <E extends Exception> boolean setLeftIf(final L newLeft, Try.BiPredicate<? super Triple<L, M, R>, ? super L, E> predicate) throws E {
        if (predicate.test(this, newLeft)) {
            this.left = newLeft;
            return true;
        }

        return false;
    }

    /**
     * Set to the specified <code>newMiddle</code> and returns <code>true</code>
     * if <code>predicate</code> returns true. Otherwise returns
     * <code>false</code> without setting the value to new value.
     * 
     * @param newMiddle
     * @param predicate - the first parameter is current pair, the second
     *        parameter is the <code>newMiddle</code>
     * @return
     */
    public <E extends Exception> boolean setMiddleIf(final M newMiddle, Try.BiPredicate<? super Triple<L, M, R>, ? super M, E> predicate) throws E {
        if (predicate.test(this, newMiddle)) {
            this.middle = newMiddle;
            return true;
        }

        return false;
    }

    /**
     * Set to the specified <code>newRight</code> and returns <code>true</code>
     * if <code>predicate</code> returns true. Otherwise returns
     * <code>false</code> without setting the value to new value.
     * 
     * @param newRight
     * @param predicate - the first parameter is current pair, the second
     *        parameter is the <code>newRight</code>
     * @return
     */
    public <E extends Exception> boolean setRightIf(final R newRight, Try.BiPredicate<? super Triple<L, M, R>, ? super R, E> predicate) throws E {
        if (predicate.test(this, newRight)) {
            this.right = newRight;
            return true;
        }

        return false;
    }

    /**
     * Set to the specified <code>newLeft</code> and <code>newRight</code> and
     * returns <code>true</code> if <code>predicate</code> returns true.
     * Otherwise returns <code>false</code> without setting the left/right to
     * new values.
     * 
     * @param newLeft
     * @param newMiddle
     * @param newRight
     * @param predicate - the first parameter is current pair, the second
     *        parameter is the <code>newLeft</code>, the third parameter is the
     *        <code>newMiddle</code>, the fourth parameter is the
     *        <code>newRight</code>
     * @return
     */
    public <E extends Exception> boolean setIf(final L newLeft, final M newMiddle, final R newRight,
            Try.QuadPredicate<? super Triple<L, M, R>, ? super L, ? super M, ? super R, E> predicate) throws E {
        if (predicate.test(this, newLeft, newMiddle, newRight)) {
            this.left = newLeft;
            this.middle = newMiddle;
            this.right = newRight;
            return true;
        }

        return false;
    }

    //    /**
    //     * Swap the left and right value. they must be same type.
    //     */
    //    public void reverse() {
    //        Object tmp = left;
    //        this.left = (L) right;
    //        this.right = (R) tmp;
    //    }

    /**
     * 
     * @return a new instance of Triple&lt;R, M, L&gt;.
     */
    public Triple<R, M, L> reversed() {
        return new Triple<>(this.right, this.middle, this.left);
    }

    public Triple<L, M, R> copy() {
        return new Triple<>(this.left, this.middle, this.right);
    }

    public Object[] toArray() {
        return new Object[] { left, middle, right };
    }

    public <A> A[] toArray(A[] a) {
        if (a.length < 3) {
            a = N.copyOf(a, 3);
        }

        a[0] = (A) left;
        a[1] = (A) middle;
        a[2] = (A) right;

        return a;
    }

    public <E extends Exception> void forEach(Try.Consumer<?, E> comsumer) throws E {
        final Try.Consumer<Object, E> objComsumer = (Try.Consumer<Object, E>) comsumer;

        objComsumer.accept(left);
        objComsumer.accept(middle);
        objComsumer.accept(right);
    }

    public <E extends Exception> void accept(final Try.TriConsumer<? super L, ? super M, ? super R, E> action) throws E {
        action.accept(left, middle, right);
    }

    public <E extends Exception> void accept(final Try.Consumer<Triple<L, M, R>, E> action) throws E {
        action.accept(this);
    }

    public <U, E extends Exception> U map(final Try.TriFunction<? super L, ? super M, ? super R, U, E> mapper) throws E {
        return mapper.apply(left, middle, right);
    }

    public <U, E extends Exception> U map(final Try.Function<Triple<L, M, R>, U, E> mapper) throws E {
        return mapper.apply(this);
    }

    public <E extends Exception> Optional<Triple<L, M, R>> filter(final Try.TriPredicate<? super L, ? super M, ? super R, E> predicate) throws E {
        return predicate.test(left, middle, right) ? Optional.of(this) : Optional.<Triple<L, M, R>> empty();
    }

    public <E extends Exception> Optional<Triple<L, M, R>> filter(final Try.Predicate<Triple<L, M, R>, E> predicate) throws E {
        return predicate.test(this) ? Optional.of(this) : Optional.<Triple<L, M, R>> empty();
    }

    public Stream<Triple<L, M, R>> stream() {
        return Stream.of(this);
    }

    public Tuple3<L, M, R> toTuple() {
        return Tuple.of(left, middle, right);
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
            final Triple<L, M, R> other = (Triple<L, M, R>) obj;

            return N.equals(left, other.left) && N.equals(middle, other.middle) && N.equals(right, other.right);
        }

        return false;
    }

    @Override
    public String toString() {
        return "[" + N.toString(left) + ", " + N.toString(middle) + ", " + N.toString(right) + "]";
    }
}
