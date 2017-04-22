/*
 * Copyright (c) 2017, Haiyang Li.
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

import java.util.AbstractCollection;
import java.util.AbstractList;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.RandomAccess;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.UnaryOperator;

/**
 * It's a wrapper for <code>Collection</code> to support more daily used/functional methods.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Seq<T> extends Slice<T> {

    public Seq() {
        super(new ArrayList<T>());
    }

    public Seq(int initialCapacity) {
        super(new ArrayList<T>(initialCapacity));
    }

    /**
     * The returned <code>Seq</code> and the specified <code>Collection</code> are backed by the same data.
     * Any changes to one will appear in the other.
     * 
     * @param c
     */
    public Seq(final Collection<T> c) {
        super(c);
    }

    /**
     * 
     * @param t1
     * @return an immutable <code>Seq</code>
     */
    public static <T> Seq<T> just(T t1) {
        return of(ImmutableList.of(t1));
    }

    /**
     * 
     * @param t1
     * @param t2
     * @return an immutable <code>Seq</code>
     */
    public static <T> Seq<T> just(T t1, T t2) {
        return of(ImmutableList.of(t1, t2));
    }

    /**
     * 
     * @param a
     * @return an immutable <code>Seq</code>
     */
    public static <T> Seq<T> just(T... a) {
        return of(ImmutableList.of(a));
    }

    public static <T> Seq<T> of(T... a) {
        return of(Array.asList(a));
    }

    /**
     * The returned <code>Seq</code> and the specified <code>Collection</code> are backed by the same data.
     * Any changes to one will appear in the other.
     * 
     * @param c
     * @return
     * @throws NullPointerException if the specified <code>Collection</code> is null.
     */
    public static <T> Seq<T> of(Collection<T> c) {
        return new Seq<>(c);
    }

    /**
     * 
     * @param map
     * @return
     * @throws NullPointerException if the specified <code>Map</code> is null.
     */
    public static <K, V> Seq<Map.Entry<K, V>> of(Map<K, V> map) {
        return of(map.entrySet());
    }

    @Override
    public boolean add(T e) {
        return coll.add(e);
    }

    @Override
    public boolean addAll(Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return coll.addAll(c);
    }

    public boolean addAll(T[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return addAll(Arrays.asList(a));
    }

    /**
     * 
     * @param e
     * @return <tt>true</tt> if this list contained the specified element
     */
    @Override
    public boolean remove(Object e) {
        return coll.remove(e);
    }

    /**
     * 
     * @param e
     * @param removeAllOccurrences
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean removeAllOccurrences(Object e) {
        return this.removeAll(Arrays.asList(e));
    }

    /**
     * 
     * @param c
     * @return
     * @see Collection#removeAll(Collection)
     */
    @Override
    public boolean removeAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return coll.removeAll(c);
    }

    public boolean removeAll(Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return coll.removeAll(Arrays.asList(a));
    }

    public boolean removeIf(Predicate<? super T> p) {
        final List<T> tmp = new ArrayList<>();

        for (T e : coll) {
            if (p.test(e)) {
                tmp.add(e);
            }
        }

        if (tmp.size() > 0) {
            return coll.removeAll(tmp);
        }

        return false;
    }

    /**
     * 
     * @param c
     * @return
     * @see Collection#retainAll(Collection)
     */
    @Override
    public boolean retainAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            boolean result = coll.size() > 0;
            coll.clear();
            return result;
        }

        return coll.retainAll(c);
    }

    public boolean retainAll(Object[] a) {
        if (N.isNullOrEmpty(a)) {
            boolean result = coll.size() > 0;
            coll.clear();
            return result;
        }

        return retainAll(Arrays.asList(a));
    }

    public int replaceAll(Object oldVal, T newVal) {
        int cnt = 0;

        if (coll instanceof List && coll instanceof RandomAccess) {
            final List<T> list = (List<T>) coll;

            for (int i = 0, size = size(); i < size; i++) {
                if (N.equals(list.get(i), oldVal)) {
                    list.set(i, newVal);
                    cnt++;
                }
            }
        } else {
            final List<T> tmp = new ArrayList<>(coll);
            this.coll.clear();

            for (T e : tmp) {
                if (N.equals(e, oldVal)) {
                    coll.add(newVal);
                    cnt++;
                } else {
                    coll.add(e);
                }
            }
        }

        return cnt;
    }

    public void replaceAll(UnaryOperator<T> operator) {
        if (coll instanceof List && coll instanceof RandomAccess) {
            final List<T> list = (List<T>) coll;

            for (int i = 0, size = size(); i < size; i++) {
                list.set(i, operator.apply(list.get(i)));
            }
        } else {
            final List<T> tmp = new ArrayList<>(coll);
            this.coll.clear();

            for (T e : tmp) {
                coll.add(operator.apply(e));
            }
        }
    }

    public boolean replaceIf(T newVal, Predicate<? super T> predicate) {
        boolean result = false;

        if (coll instanceof List && coll instanceof RandomAccess) {
            final List<T> list = (List<T>) coll;

            for (int i = 0, size = size(); i < size; i++) {
                if (predicate.test(list.get(i))) {
                    list.set(i, newVal);
                    result = true;
                }
            }
        } else {
            final List<T> tmp = new ArrayList<>(coll);
            this.coll.clear();

            for (T e : tmp) {
                if (predicate.test(e)) {
                    coll.add(newVal);
                    result = true;
                } else {
                    coll.add(e);
                }
            }
        }

        return result;
    }

    public void reverse() {
        if (size() > 1) {
            if (coll instanceof List) {
                N.reverse((List<T>) coll);
            } else {
                final Object[] tmp = coll.toArray();
                N.reverse(tmp);
                coll.clear();
                coll.addAll((List<T>) Arrays.asList(tmp));
            }
        }
    }

    public void rotate(int distance) {
        if (size() > 1) {
            if (coll instanceof List) {
                N.rotate((List<T>) coll, distance);
            } else {
                final Object[] tmp = coll.toArray();
                N.rotate(tmp, distance);
                coll.clear();
                coll.addAll((List<T>) Arrays.asList(tmp));
            }
        }
    }

    public void shuffle() {
        if (size() > 1) {
            if (coll instanceof List) {
                N.shuffle((List<T>) coll);
            } else {
                final Object[] tmp = coll.toArray();
                N.shuffle(tmp);
                coll.clear();
                coll.addAll((List<T>) Arrays.asList(tmp));
            }
        }
    }

    public void shuffle(final Random rnd) {
        if (size() > 1) {
            if (coll instanceof List) {
                N.shuffle((List<T>) coll, rnd);
            } else {
                final Object[] tmp = coll.toArray();
                N.shuffle(tmp, rnd);
                coll.clear();
                coll.addAll((List<T>) Arrays.asList(tmp));
            }
        }
    }

    @Override
    public void clear() {
        coll.clear();
    }

    //    public ExListBuilder<T> __() {
    //        return Builder.of(this);
    //    }
    //
    //    public ExListBuilder<T> __(Consumer<? super ExList<T>> func) {
    //        return Builder.of(this).__(func);
    //    }

    public static boolean disjoint(final Object[] a, final Object[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return true;
        }

        return ExList.of(a).disjoint(b);
    }

    /**
     * Returns {@code true} if the two specified arrays have no elements in common.
     * 
     * @param a
     * @param b
     * @return {@code true} if the two specified arrays have no elements in common.
     * @see Collections#disjoint(Collection, Collection)
     */
    public static boolean disjoint(final Collection<?> c1, final Collection<?> c2) {
        if (N.isNullOrEmpty(c1) || N.isNullOrEmpty(c2)) {
            return true;
        }

        if (c1 instanceof Set || (c2 instanceof Set == false && c1.size() > c2.size())) {
            for (Object e : c2) {
                if (c1.contains(e)) {
                    return false;
                }
            }
        } else {
            for (Object e : c1) {
                if (c2.contains(e)) {
                    return false;
                }
            }
        }

        return true;
    }

    public static <T> ExList<T> concat(final T[] a, final T[] b) {
        return ExList.of(N.concat(a, b));
    }

    public static <T> ExList<T> concat(final Collection<? extends T> a, final Collection<? extends T> b) {
        return N.concat(a, b);
    }

    public static <T> ExList<T> concat(final Collection<? extends T>... a) {
        return concat(Arrays.asList(a));
    }

    public static <T> ExList<T> concat(final List<? extends Collection<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return new ExList<>();
        }

        int count = 0;
        for (Collection<? extends T> e : c) {
            if (N.notNullOrEmpty(e)) {
                count += e.size();
            }
        }

        final ExList<T> result = new ExList<>(count);

        for (Collection<? extends T> e : c) {
            if (N.notNullOrEmpty(e)) {
                result.addAll(e);
            }
        }

        return result;
    }

    public static <T> Iterator<T> concat(final Iterator<? extends T> a, final Iterator<? extends T> b) {
        return concat(Arrays.asList(a, b));
    }

    public static <T> Iterator<T> concat(final Iterator<? extends T>... a) {
        return concat(Arrays.asList(a));
    }

    public static <T> Iterator<T> concat(final Collection<? extends Iterator<? extends T>> c) {
        return new ImmutableIterator<T>() {
            private final Iterator<? extends Iterator<? extends T>> iter = c.iterator();
            private Iterator<? extends T> cur;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && iter.hasNext()) {
                    cur = iter.next();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public T next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }
        };
    }

    public static <T> Iterator<T> iterate(T[]... a) {
        if (N.isNullOrEmpty(a)) {
            return ImmutableIterator.EMPTY;
        }

        final List<Iterator<T>> list = new ArrayList<>(a.length);

        for (T[] e : a) {
            if (N.notNullOrEmpty(e)) {
                list.add(ImmutableIterator.of(e));
            }
        }

        return concat(list);
    }

    public static <T> Iterator<T> iterate(Collection<? extends T>... a) {
        if (N.isNullOrEmpty(a)) {
            return ImmutableIterator.EMPTY;
        }

        return iterate(Arrays.asList(a));
    }

    public static <T> Iterator<T> iterate(Collection<? extends Collection<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return ImmutableIterator.EMPTY;
        }

        final List<Iterator<? extends T>> list = new ArrayList<>(c.size());

        for (Collection<? extends T> e : c) {
            if (N.notNullOrEmpty(e)) {
                list.add(e.iterator());
            }
        }

        return concat(list);
    }

    public static <T> ExList<T> merge(final T[] a, final T[] b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        final ExList<T> result = new ExList<>(a.length + b.length);
        final int lenA = a.length;
        final int lenB = b.length;
        int cursorA = 0;
        int cursorB = 0;

        while (cursorA < lenA || cursorB < lenB) {
            if (cursorA < lenA) {
                if (cursorB < lenB) {
                    if (nextSelector.apply(a[cursorA], b[cursorB]) == Nth.FIRST) {
                        result.add(a[cursorA++]);
                    } else {
                        result.add(b[cursorB++]);
                    }
                } else {
                    result.add(a[cursorA++]);
                }
            } else {
                result.add(b[cursorB++]);
            }
        }

        return result;
    }

    public static <T> ExList<T> merge(final Collection<? extends T> a, final Collection<? extends T> b,
            final BiFunction<? super T, ? super T, Nth> nextSelector) {
        final ExList<T> result = new ExList<>(a.size() + b.size());
        final Iterator<? extends T> iterA = a.iterator();
        final Iterator<? extends T> iterB = b.iterator();

        T nextA = null;
        T nextB = null;
        boolean hasNextA = false;
        boolean hasNextB = false;

        while (hasNextA || hasNextB || iterA.hasNext() || iterB.hasNext()) {
            if (hasNextA) {
                if (iterB.hasNext()) {
                    if (nextSelector.apply(nextA, (nextB = iterB.next())) == Nth.FIRST) {
                        hasNextA = false;
                        hasNextB = true;
                        result.add(nextA);
                    } else {
                        result.add(nextB);
                    }
                } else {
                    hasNextA = false;
                    result.add(nextA);
                }
            } else if (hasNextB) {
                if (iterA.hasNext()) {
                    if (nextSelector.apply((nextA = iterA.next()), nextB) == Nth.FIRST) {
                        result.add(nextA);
                    } else {
                        hasNextA = true;
                        hasNextB = false;
                        result.add(nextB);
                    }
                } else {
                    hasNextB = false;
                    result.add(nextB);
                }
            } else if (iterA.hasNext()) {
                if (iterB.hasNext()) {
                    if (nextSelector.apply((nextA = iterA.next()), (nextB = iterB.next())) == Nth.FIRST) {
                        hasNextB = true;
                        result.add(nextA);
                    } else {
                        hasNextA = true;
                        result.add(nextB);
                    }
                } else {
                    result.add(iterA.next());
                }
            } else {
                result.add(iterB.next());
            }
        }

        return result;
    }

    public static <T> Iterator<T> merge(final Iterator<? extends T> a, final Iterator<? extends T> b,
            final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return new ImmutableIterator<T>() {
            private final Iterator<? extends T> iterA = a;
            private final Iterator<? extends T> iterB = b;
            private T nextA = null;
            private T nextB = null;
            private boolean hasNextA = false;
            private boolean hasNextB = false;

            @Override
            public boolean hasNext() {
                return hasNextA || hasNextB || iterA.hasNext() || iterB.hasNext();
            }

            @Override
            public T next() {
                if (hasNextA) {
                    if (iterB.hasNext()) {
                        if (nextSelector.apply(nextA, (nextB = iterB.next())) == Nth.FIRST) {
                            hasNextA = false;
                            hasNextB = true;
                            return nextA;
                        } else {
                            return nextB;
                        }
                    } else {
                        hasNextA = false;
                        return nextA;
                    }
                } else if (hasNextB) {
                    if (iterA.hasNext()) {
                        if (nextSelector.apply((nextA = iterA.next()), nextB) == Nth.FIRST) {
                            return nextA;
                        } else {
                            hasNextA = true;
                            hasNextB = false;
                            return nextB;
                        }
                    } else {
                        hasNextB = false;
                        return nextB;
                    }
                } else if (iterA.hasNext()) {
                    if (iterB.hasNext()) {
                        if (nextSelector.apply((nextA = iterA.next()), (nextB = iterB.next())) == Nth.FIRST) {
                            hasNextB = true;
                            return nextA;
                        } else {
                            hasNextA = true;
                            return nextB;
                        }
                    } else {
                        return iterA.next();
                    }
                } else {
                    return iterB.next();
                }
            }
        };
    }

    public static <A, B, R> ExList<R> zip(final A[] a, final B[] b, final BiFunction<? super A, ? super B, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), zipFunction);
    }

    public static <A, B, R> ExList<R> zip(final Collection<A> a, final Collection<B> b, final BiFunction<? super A, ? super B, R> zipFunction) {
        final ExList<R> result = new ExList<>(N.min(a.size(), b.size()));

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();

        if (a.size() <= b.size()) {
            while (iterA.hasNext()) {
                result.add(zipFunction.apply(iterA.next(), iterB.next()));
            }
        } else {
            while (iterB.hasNext()) {
                result.add(zipFunction.apply(iterA.next(), iterB.next()));
            }
        }

        return result;
    }

    public static <A, B, C, R> ExList<R> zip(final A[] a, final B[] b, final C[] c, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), Arrays.asList(c), zipFunction);
    }

    public static <A, B, C, R> ExList<R> zip(final Collection<A> a, final Collection<B> b, final Collection<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final ExList<R> result = new ExList<>(N.min(a.size(), b.size(), c.size()));

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();
        final Iterator<C> iterC = c.iterator();

        while (iterA.hasNext() && iterB.hasNext() && iterC.hasNext()) {
            result.add(zipFunction.apply(iterA.next(), iterB.next(), iterC.next()));
        }

        return result;
    }

    public static <A, B, R> ExList<R> zip(final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R> ExList<R> zip(final Collection<A> a, final Collection<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        final ExList<R> result = new ExList<>(N.max(a.size(), b.size()));

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();

        if (a.size() >= b.size()) {
            while (iterA.hasNext()) {
                result.add(zipFunction.apply(iterA.next(), iterB.hasNext() ? iterB.next() : valueForNoneB));
            }
        } else {
            while (iterB.hasNext()) {
                result.add(zipFunction.apply(iterA.hasNext() ? iterA.next() : valueForNoneA, iterB.next()));
            }
        }

        return result;
    }

    public static <A, B, C, R> ExList<R> zip(final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), Arrays.asList(c), valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public static <A, B, C, R> ExList<R> zip(final Collection<A> a, final Collection<B> b, final Collection<C> c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final ExList<R> result = new ExList<>(N.max(a.size(), b.size(), c.size()));

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();
        final Iterator<C> iterC = c.iterator();

        while (iterA.hasNext() || iterB.hasNext() || iterC.hasNext()) {
            result.add(zipFunction.apply(iterA.hasNext() ? iterA.next() : valueForNoneA, iterB.hasNext() ? iterB.next() : valueForNoneB,
                    iterC.hasNext() ? iterC.next() : valueForNoneC));
        }

        return result;
    }

    /**
     * 
     * @param c
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R> Pair<ExList<L>, ExList<R>> unzip(final Collection<? extends T> c, final BiConsumer<? super T, Pair<L, R>> unzip) {
        final ExList<L> l = new ExList<L>(c.size());
        final ExList<R> r = new ExList<R>(c.size());

        final Pair<L, R> pair = new Pair<>();

        for (T e : c) {
            unzip.accept(e, pair);

            l.add(pair.left);
            r.add(pair.right);
        }

        return Pair.of(l, r);
    }

    /**
     * 
     * @param supplier
     * @param c
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R> Pair<Seq<L>, Seq<R>> unzip(final IntFunction<? extends Collection<?>> supplier, final Collection<? extends T> c,
            final BiConsumer<? super T, Pair<L, R>> unzip) {
        final Collection<L> l = (Collection<L>) supplier.apply(c.size());
        final Collection<R> r = (Collection<R>) supplier.apply(c.size());

        final Pair<L, R> pair = new Pair<>();

        for (T e : c) {
            unzip.accept(e, pair);

            l.add(pair.left);
            r.add(pair.right);
        }

        return Pair.of(l instanceof Seq ? (Seq<L>) l : Seq.of(l), r instanceof Seq ? (Seq<R>) r : Seq.of(r));
    }

    /**
     * 
     * @param c
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, M, R> Triple<ExList<L>, ExList<M>, ExList<R>> unzip3(final Collection<? extends T> c,
            final BiConsumer<? super T, Triple<L, M, R>> unzip) {
        final ExList<L> l = new ExList<L>(c.size());
        final ExList<M> m = new ExList<M>(c.size());
        final ExList<R> r = new ExList<R>(c.size());

        final Triple<L, M, R> triple = new Triple<>();

        for (T e : c) {
            unzip.accept(e, triple);

            l.add(triple.left);
            m.add(triple.middle);
            r.add(triple.right);
        }

        return Triple.of(l, m, r);
    }

    /**
     * 
     * @param supplier
     * @param c
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, M, R> Triple<Seq<L>, Seq<M>, Seq<R>> unzip3(final IntFunction<? extends Collection<?>> supplier, final Collection<? extends T> c,
            final BiConsumer<? super T, Triple<L, M, R>> unzip) {
        final Collection<L> l = (Collection<L>) supplier.apply(c.size());
        final Collection<M> m = (Collection<M>) supplier.apply(c.size());
        final Collection<R> r = (Collection<R>) supplier.apply(c.size());

        final Triple<L, M, R> triple = new Triple<>();

        for (T e : c) {
            unzip.accept(e, triple);

            l.add(triple.left);
            m.add(triple.middle);
            r.add(triple.right);
        }

        return Triple.of(l instanceof Seq ? (Seq<L>) l : Seq.of(l), m instanceof Seq ? (Seq<M>) m : Seq.of(m), r instanceof Seq ? (Seq<R>) r : Seq.of(r));
    }

    public static <T> Iterator<T> skipNull(final Iterator<? extends T> iter) {
        return new Iterator<T>() {
            private T next;

            @Override
            public boolean hasNext() {
                if (next == null && iter.hasNext()) {
                    next = iter.next();

                    if (next == null) {
                        while (iter.hasNext()) {
                            next = iter.next();

                            if (next != null) {
                                break;
                            }
                        }
                    }
                }

                return next != null;
            }

            @Override
            public T next() {
                if (next == null && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final T result = next;
                next = null;
                return result;
            }

            @Override
            public void remove() {
                iter.remove();
            }
        };
    }

    /**
     * Note: copy from Google Guava under Apache License v2.
     * <br />
     * 
     * Returns the set of all possible subsets of {@code set}. For example,
     * {@code powerSet(ImmutableSet.of(1, 2))} returns the set {@code {{},
     * {1}, {2}, {1, 2}}}.
     *
     * <p>Elements appear in these subsets in the same iteration order as they
     * appeared in the input set. The order in which these subsets appear in the
     * outer set is undefined. Note that the power set of the empty set is not the
     * empty set, but a one-element set containing the empty set.
     *
     * <p>The returned set and its constituent sets use {@code equals} to decide
     * whether two elements are identical, even if the input set uses a different
     * concept of equivalence.
     *
     * <p><i>Performance notes:</i> while the power set of a set with size {@code
     * n} is of size {@code 2^n}, its memory usage is only {@code O(n)}. When the
     * power set is constructed, the input set is merely copied. Only as the
     * power set is iterated are the individual subsets created, and these subsets
     * themselves occupy only a small constant amount of memory.
     *
     * @param set the set of elements to construct a power set from
     * @return the power set, as an immutable set of immutable sets
     * @throws IllegalArgumentException if {@code set} has more than 30 unique
     *     elements (causing the power set size to exceed the {@code int} range)
     * @throws NullPointerException if {@code set} is or contains {@code null}
     * @see <a href="http://en.wikipedia.org/wiki/Power_set">Power set article at
     *      Wikipedia</a>
     * @since 4.0
     */
    public static <E> Set<Set<E>> powerSet(Set<E> set) {
        return new PowerSet<>(set);
    }

    private static final class PowerSet<E> extends AbstractSet<Set<E>> {
        final ImmutableMap<E, Integer> inputSet;

        PowerSet(Set<E> input) {
            this.inputSet = indexMap(input);
            N.checkArgument(inputSet.size() <= 30, "Too many elements to create power set: %s > 30", inputSet.size());
        }

        @Override
        public int size() {
            return 1 << inputSet.size();
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public Iterator<Set<E>> iterator() {
            return new Iterator<Set<E>>() {
                private final int size = size();
                private int position;

                @Override
                public boolean hasNext() {
                    return position < size;
                }

                @Override
                public Set<E> next() {
                    if (!hasNext()) {
                        throw new NoSuchElementException();
                    }

                    return new SubSet<>(inputSet, position++);
                }

                @Override
                public void remove() {
                    throw new UnsupportedOperationException();
                }
            };
        }

        @Override
        public boolean contains(Object obj) {
            if (obj instanceof Set) {
                Set<?> set = (Set<?>) obj;
                return inputSet.keySet().containsAll(set);
            }
            return false;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof PowerSet) {
                PowerSet<?> that = (PowerSet<?>) obj;
                return inputSet.equals(that.inputSet);
            }
            return super.equals(obj);
        }

        @Override
        public int hashCode() {
            /*
             * The sum of the sums of the hash codes in each subset is just the sum of
             * each input element's hash code times the number of sets that element
             * appears in. Each element appears in exactly half of the 2^n sets, so:
             */
            return inputSet.keySet().hashCode() << (inputSet.size() - 1);
        }

        @Override
        public String toString() {
            return "powerSet(" + inputSet + ")";
        }

        /**
         * Returns a map from the ith element of list to i.
         */
        private static <E> ImmutableMap<E, Integer> indexMap(Collection<E> c) {
            final Map<E, Integer> map = new LinkedHashMap<>();

            int i = 0;

            for (E e : c) {
                map.put(e, i++);
            }

            return ImmutableMap.of(map);
        }
    }

    private static final class SubSet<E> extends AbstractSet<E> {
        private final ImmutableMap<E, Integer> inputSet;
        private final ImmutableList<E> elements;
        private final int mask;

        SubSet(ImmutableMap<E, Integer> inputSet, int mask) {
            this.inputSet = inputSet;
            this.elements = ImmutableList.of((E[]) inputSet.keySet().toArray());
            this.mask = mask;
        }

        @Override
        public Iterator<E> iterator() {
            return new Iterator<E>() {
                int remainingSetBits = mask;

                @Override
                public boolean hasNext() {
                    return remainingSetBits != 0;
                }

                @Override
                public E next() {
                    int index = Integer.numberOfTrailingZeros(remainingSetBits);
                    if (index == 32) {
                        throw new NoSuchElementException();
                    }
                    remainingSetBits &= ~(1 << index);
                    return elements.get(index);
                }

                @Override
                public void remove() {
                    throw new UnsupportedOperationException();
                }
            };
        }

        @Override
        public int size() {
            return Integer.bitCount(mask);
        }

        @Override
        public boolean contains(Object o) {
            Integer index = inputSet.get(o);
            return index != null && (mask & (1 << index)) != 0;
        }
    }

    /**
     * Note: copy from Google Guava under Apache License v2.
     * <br />
     * 
     * Returns a {@link Collection} of all the permutations of the specified
     * {@link Collection}.
     *
     * <p><i>Notes:</i> This is an implementation of the Plain Changes algorithm
     * for permutations generation, described in Knuth's "The Art of Computer
     * Programming", Volume 4, Chapter 7, Section 7.2.1.2.
     *
     * <p>If the input list contains equal elements, some of the generated
     * permutations will be equal.
     *
     * <p>An empty collection has only one permutation, which is an empty list.
     *
     * @param elements the original collection whose elements have to be permuted.
     * @return an immutable {@link Collection} containing all the different
     *     permutations of the original collection.
     * @throws NullPointerException if the specified collection is null or has any
     *     null elements.
     * @since 12.0
     */
    public static <E> Collection<ExList<E>> permutations(Collection<E> elements) {
        return new PermutationCollection<E>(elements);
    }

    private static final class PermutationCollection<E> extends AbstractCollection<ExList<E>> {
        final List<E> inputList;

        PermutationCollection(Collection<E> input) {
            this.inputList = new ArrayList<>(input);
        }

        @Override
        public int size() {
            return Math2.factorial(inputList.size());
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public Iterator<ExList<E>> iterator() {
            return PermutationIterator.of(inputList);
        }

        @Override
        public boolean contains(Object obj) {
            if (obj instanceof Collection) {
                return isPermutations(inputList, (Collection<?>) obj);
            }

            return false;
        }

        @Override
        public String toString() {
            return "permutations(" + inputList + ")";
        }
    }

    /**
     * Note: copy from Google Guava under Apache License v2.
     * <br />
     * 
     * Returns a {@link Collection} of all the permutations of the specified
     * {@link Iterable}.
     *
     * <p><i>Notes:</i> This is an implementation of the algorithm for
     * Lexicographical Permutations Generation, described in Knuth's "The Art of
     * Computer Programming", Volume 4, Chapter 7, Section 7.2.1.2. The
     * iteration order follows the lexicographical order. This means that
     * the first permutation will be in ascending order, and the last will be in
     * descending order.
     *
     * <p>Duplicate elements are considered equal. For example, the list [1, 1]
     * will have only one permutation, instead of two. This is why the elements
     * have to implement {@link Comparable}.
     *
     * <p>An empty iterable has only one permutation, which is an empty list.
     *
     * <p>This method is equivalent to
     * {@code Collections2.orderedPermutations(list, Ordering.natural())}.
     *
     * @param elements the original iterable whose elements have to be permuted.
     * @return an immutable {@link Collection} containing all the different
     *     permutations of the original iterable.
     * @throws NullPointerException if the specified iterable is null or has any
     *     null elements.
     * @since 12.0
     */
    public static <E extends Comparable<? super E>> Collection<ExList<E>> orderedPermutations(Collection<E> elements) {
        return orderedPermutations(elements, Comparators.OBJ_COMPARATOR);
    }

    /**
     * Note: copy from Google Guava under Apache License v2.
     * <br />
     * 
     * Returns a {@link Collection} of all the permutations of the specified
     * {@link Iterable} using the specified {@link Comparator} for establishing
     * the lexicographical ordering.
     *
     * <p>Examples: <pre>   {@code
     *
     *   for (List<String> perm : orderedPermutations(asList("b", "c", "a"))) {
     *     println(perm);
     *   }
     *   // -> ["a", "b", "c"]
     *   // -> ["a", "c", "b"]
     *   // -> ["b", "a", "c"]
     *   // -> ["b", "c", "a"]
     *   // -> ["c", "a", "b"]
     *   // -> ["c", "b", "a"]
     *
     *   for (List<Integer> perm : orderedPermutations(asList(1, 2, 2, 1))) {
     *     println(perm);
     *   }
     *   // -> [1, 1, 2, 2]
     *   // -> [1, 2, 1, 2]
     *   // -> [1, 2, 2, 1]
     *   // -> [2, 1, 1, 2]
     *   // -> [2, 1, 2, 1]
     *   // -> [2, 2, 1, 1]}</pre>
     *
     * <p><i>Notes:</i> This is an implementation of the algorithm for
     * Lexicographical Permutations Generation, described in Knuth's "The Art of
     * Computer Programming", Volume 4, Chapter 7, Section 7.2.1.2. The
     * iteration order follows the lexicographical order. This means that
     * the first permutation will be in ascending order, and the last will be in
     * descending order.
     *
     * <p>Elements that compare equal are considered equal and no new permutations
     * are created by swapping them.
     *
     * <p>An empty iterable has only one permutation, which is an empty list.
     *
     * @param elements the original iterable whose elements have to be permuted.
     * @param comparator a comparator for the iterable's elements.
     * @return an immutable {@link Collection} containing all the different
     *     permutations of the original iterable.
     * @throws NullPointerException If the specified iterable is null, has any
     *     null elements, or if the specified comparator is null.
     * @since 12.0
     */
    public static <E> Collection<ExList<E>> orderedPermutations(Collection<E> elements, Comparator<? super E> comparator) {
        return new OrderedPermutationCollection<E>(elements, comparator);
    }

    private static final class OrderedPermutationCollection<E> extends AbstractCollection<ExList<E>> {
        final List<E> inputList;
        final Comparator<? super E> comparator;
        final int size;

        OrderedPermutationCollection(Collection<E> input, Comparator<? super E> comparator) {
            this.inputList = new ArrayList<E>(input);
            N.sort(inputList, comparator);
            this.comparator = comparator;
            this.size = calculateSize(inputList, comparator);
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        public boolean isEmpty() {
            return false;
        }

        @Override
        public Iterator<ExList<E>> iterator() {
            return PermutationIterator.ordered(inputList, comparator);
        }

        @Override
        public boolean contains(Object obj) {
            if (obj instanceof Collection) {
                return isPermutations(inputList, (Collection<?>) obj);
            }
            return false;
        }

        @Override
        public String toString() {
            return "orderedPermutationCollection(" + inputList + ")";
        }

        /**
         * The number of permutations with repeated elements is calculated as
         * follows:
         * <ul>
         * <li>For an empty list, it is 1 (base case).</li>
         * <li>When r numbers are added to a list of n-r elements, the number of
         * permutations is increased by a factor of (n choose r).</li>
         * </ul>
         */
        private static <E> int calculateSize(List<E> sortedInputList, Comparator<? super E> comparator) {
            long permutations = 1;
            int n = 1;
            int r = 1;
            while (n < sortedInputList.size()) {
                int comparison = comparator.compare(sortedInputList.get(n - 1), sortedInputList.get(n));

                if (comparison < 0) {
                    // We move to the next non-repeated element.
                    permutations *= Math2.binomial(n, r);
                    r = 0;
                    if (!isPositiveInt(permutations)) {
                        return Integer.MAX_VALUE;
                    }
                }

                n++;
                r++;
            }

            permutations *= Math2.binomial(n, r);

            if (!isPositiveInt(permutations)) {
                return Integer.MAX_VALUE;
            }

            return (int) permutations;
        }

        private static boolean isPositiveInt(long n) {
            return n >= 0 && n <= Integer.MAX_VALUE;
        }
    }

    /**
     * Returns {@code true} if the second list is a permutation of the first.
     */
    private static boolean isPermutations(Collection<?> a, Collection<?> b) {
        if (a.size() != b.size()) {
            return false;
        }

        return N.difference(a, b).size() == 0;
    }

    /**
     * Note: copy from Google Guava under Apache License v2.
     * <br />
     * 
     * Returns every possible list that can be formed by choosing one element
     * from each of the given lists in order; the "n-ary
     * <a href="http://en.wikipedia.org/wiki/Cartesian_product">Cartesian
     * product</a>" of the lists. For example: <pre>   {@code
     *
     *   Lists.cartesianProduct(ImmutableList.of(
     *       ImmutableList.of(1, 2),
     *       ImmutableList.of("A", "B", "C")))}</pre>
     *
     * <p>returns a list containing six lists in the following order:
     *
     * <ul>
     * <li>{@code ImmutableList.of(1, "A")}
     * <li>{@code ImmutableList.of(1, "B")}
     * <li>{@code ImmutableList.of(1, "C")}
     * <li>{@code ImmutableList.of(2, "A")}
     * <li>{@code ImmutableList.of(2, "B")}
     * <li>{@code ImmutableList.of(2, "C")}
     * </ul>
     *
     * <p>The result is guaranteed to be in the "traditional", lexicographical
     * order for Cartesian products that you would get from nesting for loops:
     * <pre>   {@code
     *
     *   for (B b0 : lists.get(0)) {
     *     for (B b1 : lists.get(1)) {
     *       ...
     *       ImmutableList<B> tuple = ImmutableList.of(b0, b1, ...);
     *       // operate on tuple
     *     }
     *   }}</pre>
     *
     * <p>Note that if any input list is empty, the Cartesian product will also be
     * empty. If no lists at all are provided (an empty list), the resulting
     * Cartesian product has one element, an empty list (counter-intuitive, but
     * mathematically consistent).
     *
     * <p><i>Performance notes:</i> while the cartesian product of lists of size
     * {@code m, n, p} is a list of size {@code m x n x p}, its actual memory
     * consumption is much smaller. When the cartesian product is constructed, the
     * input lists are merely copied. Only as the resulting list is iterated are
     * the individual lists created, and these are not retained after iteration.
     *
     * @param cs the lists to choose elements from, in the order that
     *     the elements chosen from those lists should appear in the resulting
     *     lists
     * @param <E> any common base class shared by all axes (often just {@link
     *     Object})
     * @return the Cartesian product, as an immutable list containing immutable
     *     lists
     * @throws IllegalArgumentException if the size of the cartesian product would
     *     be greater than {@link Integer#MAX_VALUE}
     * @throws NullPointerException if {@code lists}, any one of the
     *     {@code lists}, or any element of a provided list is null
     * @since 19.0
     */
    @SafeVarargs
    public static <E> List<ExList<E>> cartesianProduct(final Collection<? extends E>... cs) {
        return cartesianProduct(Arrays.asList(cs));
    }

    /**
     * Note: copy from Google Guava under Apache License v2.
     * <br />
     * 
     * Returns every possible list that can be formed by choosing one element
     * from each of the given lists in order; the "n-ary
     * <a href="http://en.wikipedia.org/wiki/Cartesian_product">Cartesian
     * product</a>" of the lists. For example: <pre>   {@code
     *
     *   Lists.cartesianProduct(ImmutableList.of(
     *       ImmutableList.of(1, 2),
     *       ImmutableList.of("A", "B", "C")))}</pre>
     *
     * <p>returns a list containing six lists in the following order:
     *
     * <ul>
     * <li>{@code ImmutableList.of(1, "A")}
     * <li>{@code ImmutableList.of(1, "B")}
     * <li>{@code ImmutableList.of(1, "C")}
     * <li>{@code ImmutableList.of(2, "A")}
     * <li>{@code ImmutableList.of(2, "B")}
     * <li>{@code ImmutableList.of(2, "C")}
     * </ul>
     *
     * <p>The result is guaranteed to be in the "traditional", lexicographical
     * order for Cartesian products that you would get from nesting for loops:
     * <pre>   {@code
     *
     *   for (B b0 : lists.get(0)) {
     *     for (B b1 : lists.get(1)) {
     *       ...
     *       ImmutableList<B> tuple = ImmutableList.of(b0, b1, ...);
     *       // operate on tuple
     *     }
     *   }}</pre>
     *
     * <p>Note that if any input list is empty, the Cartesian product will also be
     * empty. If no lists at all are provided (an empty list), the resulting
     * Cartesian product has one element, an empty list (counter-intuitive, but
     * mathematically consistent).
     *
     * <p><i>Performance notes:</i> while the cartesian product of lists of size
     * {@code m, n, p} is a list of size {@code m x n x p}, its actual memory
     * consumption is much smaller. When the cartesian product is constructed, the
     * input lists are merely copied. Only as the resulting list is iterated are
     * the individual lists created, and these are not retained after iteration.
     *
     * @param cs the lists to choose elements from, in the order that
     *     the elements chosen from those lists should appear in the resulting
     *     lists
     * @param <E> any common base class shared by all axes (often just {@link
     *     Object})
     * @return the Cartesian product, as an immutable list containing immutable
     *     lists
     * @throws IllegalArgumentException if the size of the cartesian product would
     *     be greater than {@link Integer#MAX_VALUE}
     * @throws NullPointerException if {@code lists}, any one of the {@code lists},
     *     or any element of a provided list is null
     * @since 19.0
     */
    public static <E> List<ExList<E>> cartesianProduct(final Collection<? extends Collection<? extends E>> cs) {
        return new CartesianList<>(cs);
    }

    private static final class CartesianList<E> extends AbstractList<ExList<E>> implements RandomAccess {
        private final transient Object[][] axes;
        private final transient int[] axesSizeProduct;

        CartesianList(final Collection<? extends Collection<? extends E>> cs) {
            final Iterator<? extends Collection<? extends E>> iter = cs.iterator();
            this.axes = new Object[cs.size()][];

            for (int i = 0, len = this.axes.length; i < len; i++) {
                this.axes[i] = iter.next().toArray();
            }

            this.axesSizeProduct = new int[axes.length + 1];
            axesSizeProduct[axes.length] = 1;

            try {
                for (int i = axes.length - 1; i >= 0; i--) {
                    axesSizeProduct[i] = Math2.multiplyExact(axesSizeProduct[i + 1], axes[i].length);
                }
            } catch (ArithmeticException e) {
                throw new IllegalArgumentException("Cartesian product too large; must have size at most Integer.MAX_VALUE");
            }
        }

        @Override
        public ExList<E> get(final int index) {
            N.checkArgument(index < size(), "Invalid index %s. It must be less than the size %s", index, size());

            final ExList<E> result = new ExList<>(axes.length);

            for (int k = 0, len = axes.length; k < len; k++) {
                result.add((E) axes[k][getAxisIndexForProductIndex(index, k)]);
            }

            return result;
        }

        @Override
        public int size() {
            return axesSizeProduct[0];
        }

        @Override
        public boolean contains(Object obj) {
            if (!(obj instanceof Collection)) {
                return false;
            }

            final Collection<?> c = (Collection<?>) obj;

            if (c.size() != axes.length) {
                return false;
            }

            int idx = 0;
            for (Object e : c) {
                boolean found = false;

                for (Object p : axes[idx++]) {
                    if (N.equals(e, p)) {
                        found = true;
                        break;
                    }
                }

                if (found == false) {
                    return false;
                }
            }

            return true;
        }

        private int getAxisIndexForProductIndex(int index, int axis) {
            return (index / axesSizeProduct[axis + 1]) % axes[axis].length;
        }
    }
}
