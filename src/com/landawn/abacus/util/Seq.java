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
import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.Random;
import java.util.RandomAccess;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedBiFunction;
import com.landawn.abacus.util.function.IndexedConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.TriConsumer;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;

/**
 * It's an read-only wrapper for <code>Collection</code> to support more daily used/functional methods.
 * All the operations are null safety. And an empty <code>String</code>/<code>Array</code>/<code>Collection</code>/<code>Optional</code>/<code>NullabLe</code> will be returned if possible, instead of null.
 * 
 * <br />
 * <code>Seq</code> should not be passed as a parameter or returned as a result because it's a pure utility class for the operations/calculation based on Collection/Array
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Seq<T> extends ImmutableCollection<T> {

    @SuppressWarnings("rawtypes")
    private static final Seq EMPTY = Seq.of(Collections.EMPTY_LIST);

    /**
     * The returned <code>Seq</code> and the specified <code>Collection</code> are backed by the same data.
     * Any changes to one will appear in the other.
     * 
     * @param c
     */
    Seq(final Collection<T> c) {
        super(c);
    }

    public static <T> Seq<T> just(T t) {
        return of(t);
    }

    @SafeVarargs
    public static <T> Seq<T> of(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return EMPTY;
        }

        return of(Arrays.asList(a));
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
        return of(map == null ? null : map.entrySet());
    }

    //    /**
    //     * Returns the <code>Collection</code> the <code>Seq</code> is backed with recursively.
    //     * 
    //     * @return
    //     */
    //    public Collection<T> interior() {
    //        if (coll == null) {
    //            return coll;
    //        }
    //
    //        Collection<T> tmp = coll;
    //
    //        if (tmp instanceof Seq) {
    //            while (tmp instanceof Seq) {
    //                tmp = ((Seq<T>) tmp).coll;
    //            }
    //        }
    //
    //        if (tmp instanceof SubCollection) {
    //            while (tmp instanceof SubCollection) {
    //                tmp = ((SubCollection<T>) tmp).c;
    //            }
    //        }
    //
    //        if (tmp instanceof Seq) {
    //            return ((Seq<T>) tmp).interior();
    //        } else {
    //            return tmp;
    //        }
    //    }

    @Override
    public boolean contains(Object e) {
        if (N.isNullOrEmpty(coll)) {
            return false;
        }

        return coll.contains(e);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return true;
        } else if (N.isNullOrEmpty(coll)) {
            return false;
        }

        return coll.containsAll(c);
    }

    public boolean containsAll(Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        } else if (N.isNullOrEmpty(coll)) {
            return false;
        }

        return containsAll(Arrays.asList(a));
    }

    public boolean containsAny(Collection<?> c) {
        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(c)) {
            return false;
        }

        return !disjoint(c);
    }

    public boolean containsAny(Object[] a) {
        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(a)) {
            return false;
        }

        return !disjoint(a);
    }

    public boolean disjoint(final Collection<?> c) {
        return Seq.disjoint(this.coll, c);
    }

    public boolean disjoint(final Object[] a) {
        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(a)) {
            return true;
        }

        return disjoint(Arrays.asList(a));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public List<T> intersection(Collection<?> b) {
        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(b)) {
            return new ArrayList<>();
        }

        final Multiset<?> bOccurrences = Multiset.from(b);
        final List<T> result = new ArrayList<>(N.min(9, size(), b.size()));

        for (T e : coll) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }
        }

        return result;
    }

    public List<T> intersection(final Object[] a) {
        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        return intersection(Arrays.asList(a));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public List<T> difference(Collection<?> b) {
        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<>();
        } else if (N.isNullOrEmpty(b)) {
            return new ArrayList<>(coll);
        }

        final Multiset<?> bOccurrences = Multiset.from(b);
        final List<T> result = new ArrayList<>(N.min(size(), N.max(9, size() - b.size())));

        for (T e : coll) {
            if (bOccurrences.getAndRemove(e) < 1) {
                result.add(e);
            }
        }

        return result;
    }

    public List<T> difference(final Object[] a) {
        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<>();
        } else if (N.isNullOrEmpty(a)) {
            return new ArrayList<>(coll);
        }

        return difference(Arrays.asList(a));
    }

    /**
     * 
     * @param b
     * @return this.difference(b).addAll(b.difference(this))
     * @see IntList#symmetricDifference(IntList)
     */
    public List<T> symmetricDifference(Collection<T> b) {
        if (N.isNullOrEmpty(b)) {
            return N.isNullOrEmpty(coll) ? new ArrayList<T>() : new ArrayList<>(coll);
        } else if (N.isNullOrEmpty(coll)) {
            return new ArrayList<>(b);
        }

        final Multiset<?> bOccurrences = Multiset.from(b);
        final List<T> result = new ArrayList<>(N.max(9, Math.abs(size() - b.size())));

        for (T e : coll) {
            if (bOccurrences.getAndRemove(e) < 1) {
                result.add(e);
            }
        }

        for (T e : b) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }

            if (bOccurrences.isEmpty()) {
                break;
            }
        }

        return result;
    }

    public List<T> symmetricDifference(final T[] a) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(coll) ? new ArrayList<T>() : new ArrayList<>(coll);
        } else if (N.isNullOrEmpty(coll)) {
            return N.asList(a);
        }

        return symmetricDifference(Arrays.asList(a));
    }

    public int occurrencesOf(final Object objectToFind) {
        return N.isNullOrEmpty(coll) ? 0 : N.occurrencesOf(coll, objectToFind);
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> min() {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.min((Collection) coll));
    }

    public NullabLe<T> min(Comparator<? super T> cmp) {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.min(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> minBy(final Function<? super T, ? extends Comparable> keyExtractor) {
        return min(Fn.comparingBy(keyExtractor));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> max() {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.max((Collection) coll));
    }

    public NullabLe<T> max(Comparator<? super T> cmp) {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.max(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> maxBy(final Function<? super T, ? extends Comparable> keyExtractor) {
        return max(Fn.comparingBy(keyExtractor));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> median() {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.median((Collection) coll));
    }

    public NullabLe<T> median(Comparator<? super T> cmp) {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.median(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> kthLargest(final int k) {
        return size() < k ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.kthLargest((Collection) coll, k));
    }

    public NullabLe<T> kthLargest(final int k, Comparator<? super T> cmp) {
        return size() < k ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.kthLargest(coll, k, cmp));
    }

    public int sumInt() {
        if (N.isNullOrEmpty(coll)) {
            return 0;
        }

        int result = 0;

        for (T e : coll) {
            if (e != null) {
                result += ((Number) e).intValue();
            }
        }

        return result;
    }

    public int sumInt(final ToIntFunction<? super T> mapper) {
        if (N.isNullOrEmpty(coll)) {
            return 0;
        }

        int result = 0;

        for (T e : coll) {
            result += mapper.applyAsInt(e);
        }

        return result;
    }

    public long sumLong() {
        if (N.isNullOrEmpty(coll)) {
            return 0L;
        }

        long result = 0;

        for (T e : coll) {
            if (e != null) {
                result += ((Number) e).longValue();
            }
        }

        return result;
    }

    public long sumLong(final ToLongFunction<? super T> mapper) {
        if (N.isNullOrEmpty(coll)) {
            return 0L;
        }

        long result = 0L;

        for (T e : coll) {
            result += mapper.applyAsLong(e);
        }

        return result;
    }

    public double sumDouble() {
        if (N.isNullOrEmpty(coll)) {
            return 0D;
        }

        return sumDouble((ToDoubleFunction<? super T>) new ToDoubleFunction<Number>() {
            @Override
            public double applyAsDouble(Number value) {
                return value == null ? 0d : value.doubleValue();
            }
        });
    }

    public double sumDouble(final ToDoubleFunction<? super T> mapper) {
        return size() == 0 ? 0d : N.sumDouble(coll, mapper);
    }

    public OptionalDouble averageInt() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(((double) sumInt()) / size());
    }

    public OptionalDouble averageInt(final ToIntFunction<? super T> mapper) {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(((double) sumInt(mapper)) / size());
    }

    public OptionalDouble averageLong() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(((double) sumLong()) / size());
    }

    public OptionalDouble averageLong(final ToLongFunction<? super T> mapper) {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(((double) sumLong(mapper)) / size());
    }

    public OptionalDouble averageDouble() {
        return averageDouble((ToDoubleFunction<? super T>) new ToDoubleFunction<Number>() {
            @Override
            public double applyAsDouble(Number value) {
                return value == null ? 0d : value.doubleValue();
            }
        });
    }

    public OptionalDouble averageDouble(final ToDoubleFunction<? super T> mapper) {
        return size() == 0 ? OptionalDouble.empty() : N.averageDouble(coll, mapper);
    }

    public void forEach(final Consumer<? super T> action) {
        N.forEach(coll, action);
    }

    //    public void forEach(int fromIndex, final int toIndex, final Consumer<? super T> action) {
    //        N.forEach(coll, fromIndex, toIndex, action);
    //    }

    public void forEach(final IndexedConsumer<? super T> action) {
        N.forEach(coll, action);
    }

    //    public void forEach(int fromIndex, final int toIndex, final IndexedConsumer<? super T> action) {
    //        N.forEach(coll, fromIndex, toIndex, action);
    //    }

    public <R> R forEach(final R seed, BiFunction<R, ? super T, R> accumulator, final BiPredicate<? super R, ? super T> conditionToBreak) {
        return N.forEach(coll, seed, accumulator, conditionToBreak);
    }

    //    public <R> R forEach(int fromIndex, final int toIndex, final R seed, final BiFunction<R, ? super T, R> accumulator,
    //            final BiPredicate<? super R, ? super T> conditionToBreak) {
    //        return N.forEach(coll, fromIndex, toIndex, seed, accumulator, conditionToBreak);
    //    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public <R> R forEach(final R seed, final IndexedBiFunction<R, ? super T, R> accumulator, final BiPredicate<? super R, ? super T> conditionToBreak) {
        return N.forEach(coll, seed, accumulator, conditionToBreak);
    }

    public <U> void forEach(final Function<? super T, ? extends Collection<U>> flatMapper, final BiConsumer<? super T, ? super U> action) {
        N.forEach(coll, flatMapper, action);
    }

    public <T2, T3> void forEach(final Function<? super T, ? extends Collection<T2>> flatMapper,
            final Function<? super T2, ? extends Collection<T3>> flatMapper2, final TriConsumer<? super T, ? super T2, ? super T3> action) {
        N.forEach(coll, flatMapper, flatMapper2, action);
    }

    //    public void forEachNonNull(final Consumer<? super T> action) {
    //        N.forEachNonNull(coll, action);
    //    }

    public <U> void forEachNonNull(final Function<? super T, ? extends Collection<U>> flatMapper, final BiConsumer<? super T, ? super U> action) {
        N.forEachNonNull(coll, flatMapper, action);
    }

    public <T2, T3> void forEachNonNull(final Function<? super T, ? extends Collection<T2>> flatMapper,
            final Function<? super T2, ? extends Collection<T3>> flatMapper2, final TriConsumer<? super T, ? super T2, ? super T3> action) {
        N.forEachNonNull(coll, flatMapper, flatMapper2, action);
    }

    public void forEachPair(final BiConsumer<? super T, ? super T> action) {
        forEachPair(action, 1);
    }

    public void forEachPair(final BiConsumer<? super T, ? super T> action, final int increment) {
        final int windowSize = 2;
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        if (N.isNullOrEmpty(coll)) {
            return;
        }

        final Iterator<T> iter = coll.iterator();
        final T NONE = (T) N.NULL_MASK;
        T prev = NONE;

        while (iter.hasNext()) {
            if (increment > windowSize && prev != NONE) {
                int skipNum = increment - windowSize;

                while (skipNum-- > 0 && iter.hasNext()) {
                    iter.next();
                }

                if (iter.hasNext() == false) {
                    break;
                }

                prev = NONE;
            }

            if (increment == 1) {
                action.accept(prev == NONE ? iter.next() : prev, (prev = (iter.hasNext() ? iter.next() : null)));
            } else {
                action.accept(iter.next(), (prev = (iter.hasNext() ? iter.next() : null)));
            }
        }
    }

    public void forEachTriple(final TriConsumer<? super T, ? super T, ? super T> action) {
        forEachTriple(action, 1);
    }

    public void forEachTriple(final TriConsumer<? super T, ? super T, ? super T> action, final int increment) {
        final int windowSize = 3;
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        if (N.isNullOrEmpty(coll)) {
            return;
        }

        final Iterator<T> iter = coll.iterator();
        final T NONE = (T) N.NULL_MASK;
        T prev = NONE;
        T prev2 = NONE;

        while (iter.hasNext()) {
            if (increment > windowSize && prev != NONE) {
                int skipNum = increment - windowSize;

                while (skipNum-- > 0 && iter.hasNext()) {
                    iter.next();
                }

                if (iter.hasNext() == false) {
                    break;
                }

                prev = NONE;
            }

            if (increment == 1) {
                action.accept(prev2 == NONE ? iter.next() : prev2, (prev2 = (prev == NONE ? (iter.hasNext() ? iter.next() : null) : prev)),
                        (prev = (iter.hasNext() ? iter.next() : null)));
            } else if (increment == 2) {
                action.accept(prev == NONE ? iter.next() : prev, (prev2 = (iter.hasNext() ? iter.next() : null)),
                        (prev = (iter.hasNext() ? iter.next() : null)));
            } else {
                action.accept(iter.next(), (prev2 = (iter.hasNext() ? iter.next() : null)), (prev = (iter.hasNext() ? iter.next() : null)));
            }
        }
    }

    //    /**
    //     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
    //     * 
    //     * @param fromIndex
    //     * @param toIndex
    //     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
    //     * @param accumulator
    //     * @param conditionToBreak break if <code>true</code> is return.
    //     * @return
    //     */
    //    public <R> R forEach(int fromIndex, final int toIndex, final R seed, final IndexedBiFunction<R, ? super T, R> accumulator,
    //            final BiPredicate<? super R, ? super T> conditionToBreak) {
    //        return N.forEach(coll, fromIndex, toIndex, seed, accumulator, conditionToBreak);
    //    }

    public NullabLe<T> first() {
        if (size() == 0) {
            return NullabLe.empty();
        }

        if (coll instanceof List && coll instanceof RandomAccess) {
            return NullabLe.of(((List<T>) coll).get(0));
        } else {
            return NullabLe.of(coll.iterator().next());
        }
    }

    /**
     * Return at most first <code>n</code> elements.
     * 
     * @param n
     * @return
     */
    public List<T> first(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: " + n);

        if (N.isNullOrEmpty(coll) || n == 0) {
            return new ArrayList<>();
        } else if (coll.size() <= n) {
            return new ArrayList<>(coll);
        } else if (coll instanceof List) {
            return new ArrayList<>(((List<T>) coll).subList(0, n));
        } else {
            return new ArrayList<>(slice(0, n));
        }
    }

    public NullabLe<T> last() {
        if (size() == 0) {
            return NullabLe.empty();
        }

        if (coll instanceof List && coll instanceof RandomAccess) {
            return NullabLe.of(((List<T>) coll).get(size() - 1));
        } else {
            final Iterator<T> iter = iterator();
            T e = null;

            while (iter.hasNext()) {
                e = iter.next();
            }

            return NullabLe.of(e);
        }
    }

    /**
     * Return at most last <code>n</code> elements.
     * 
     * @param n
     * @return
     */
    public List<T> last(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: " + n);

        if (N.isNullOrEmpty(coll) || n == 0) {
            return new ArrayList<>();
        } else if (coll.size() <= n) {
            return new ArrayList<>(coll);
        } else if (coll instanceof List) {
            return new ArrayList<>(((List<T>) coll).subList(coll.size() - n, coll.size()));
        } else {
            return new ArrayList<>(slice(coll.size() - n, coll.size()));
        }
    }

    public NullabLe<T> findFirst(Predicate<? super T> predicate) {
        if (size() == 0) {
            return NullabLe.empty();
        }

        for (T e : coll) {
            if (predicate.test(e)) {
                return NullabLe.of(e);
            }
        }

        return NullabLe.empty();
    }

    public NullabLe<T> findLast(Predicate<? super T> predicate) {
        if (size() == 0) {
            return NullabLe.empty();
        }

        if (coll instanceof List) {
            final List<T> list = (List<T>) coll;

            if (coll instanceof RandomAccess) {
                for (int i = size() - 1; i >= 0; i--) {
                    if (predicate.test(list.get(i))) {
                        return NullabLe.of(list.get(i));
                    }
                }
            } else {
                final ListIterator<T> iter = list.listIterator(list.size());
                T pre = null;

                while (iter.hasPrevious()) {
                    if (predicate.test((pre = iter.previous()))) {
                        return NullabLe.of(pre);
                    }
                }
            }

            return NullabLe.empty();
        } else {
            T result = (T) N.NULL_MASK;

            for (T e : coll) {
                if (predicate.test(e)) {
                    result = e;
                }
            }

            return result == N.NULL_MASK ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(result);
        }
    }

    public OptionalInt findFirstIndex(Predicate<? super T> predicate) {
        if (size() == 0) {
            return OptionalInt.empty();
        }

        int idx = 0;

        for (T e : coll) {
            if (predicate.test(e)) {
                return OptionalInt.of(idx);
            }

            idx++;
        }

        return OptionalInt.empty();
    }

    public OptionalInt findLastIndex(Predicate<? super T> predicate) {
        if (size() == 0) {
            return OptionalInt.empty();
        }

        if (coll instanceof List) {
            final List<T> list = (List<T>) coll;

            if (coll instanceof RandomAccess) {
                for (int i = size() - 1; i >= 0; i--) {
                    if (predicate.test(list.get(i))) {
                        return OptionalInt.of(i);
                    }
                }
            } else {
                final ListIterator<T> iter = list.listIterator(list.size());

                for (int i = size() - 1; iter.hasPrevious(); i--) {
                    if (predicate.test(iter.previous())) {
                        return OptionalInt.of(i);
                    }
                }
            }

            return OptionalInt.empty();
        } else {
            int result = -1;
            int idx = 0;

            for (T e : coll) {
                if (predicate.test(e)) {
                    result = idx;
                }

                idx++;
            }

            return result == -1 ? OptionalInt.empty() : OptionalInt.of(result);
        }
    }

    public NullabLe<T> findFirstOrLast(final Predicate<? super T> predicateForFirst, final Predicate<? super T> predicateForLast) {
        if (N.isNullOrEmpty(coll)) {
            return NullabLe.<T> empty();
        }

        final Iterator<T> iter = iterator();
        T last = (T) N.NULL_MASK;
        T next = null;

        while (iter.hasNext()) {
            next = iter.next();

            if (predicateForFirst.test(next)) {
                return NullabLe.of(next);
            } else if (predicateForLast.test(next)) {
                last = next;
            }
        }

        return last == N.NULL_MASK ? NullabLe.<T> empty() : NullabLe.of(last);
    }

    public OptionalInt findFirstOrLastIndex(final Predicate<? super T> predicateForFirst, final Predicate<? super T> predicateForLast) {
        if (N.isNullOrEmpty(coll)) {
            return OptionalInt.empty();
        }

        final Iterator<T> iter = iterator();
        T next = null;
        int idx = 0, lastIndex = -1;

        while (iter.hasNext()) {
            next = iter.next();

            if (predicateForFirst.test(next)) {
                return OptionalInt.of(idx);
            } else if (predicateForLast.test(next)) {
                lastIndex = idx;
            }

            idx++;
        }

        return lastIndex == -1 ? OptionalInt.empty() : OptionalInt.of(lastIndex);
    }

    public Pair<NullabLe<T>, NullabLe<T>> findFirstAndLast(final Predicate<? super T> predicate) {
        return findFirstAndLast(predicate, predicate);
    }

    public Pair<NullabLe<T>, NullabLe<T>> findFirstAndLast(final Predicate<? super T> predicateForFirst, final Predicate<? super T> predicateForLast) {
        if (N.isNullOrEmpty(coll)) {
            return Pair.of(NullabLe.<T> empty(), NullabLe.<T> empty());
        }

        return Pair.of(findFirst(predicateForFirst), findLast(predicateForLast));
    }

    public Pair<OptionalInt, OptionalInt> findFirstAndLastIndex(final Predicate<? super T> predicate) {
        return findFirstAndLastIndex(predicate, predicate);
    }

    public Pair<OptionalInt, OptionalInt> findFirstAndLastIndex(final Predicate<? super T> predicateForFirst, final Predicate<? super T> predicateForLast) {
        if (N.isNullOrEmpty(coll)) {
            return Pair.of(OptionalInt.empty(), OptionalInt.empty());
        }

        return Pair.of(findFirstIndex(predicateForFirst), findLastIndex(predicateForLast));
    }

    public boolean allMatch(Predicate<? super T> filter) {
        if (N.isNullOrEmpty(coll)) {
            return true;
        }

        for (T e : coll) {
            if (filter.test(e) == false) {
                return false;
            }
        }

        return true;
    }

    public boolean anyMatch(Predicate<? super T> filter) {
        if (N.isNullOrEmpty(coll)) {
            return false;
        }

        for (T e : coll) {
            if (filter.test(e)) {
                return true;
            }
        }

        return false;
    }

    public boolean noneMatch(Predicate<? super T> filter) {
        if (N.isNullOrEmpty(coll)) {
            return true;
        }

        for (T e : coll) {
            if (filter.test(e)) {
                return false;
            }
        }

        return true;
    }

    public boolean hasDuplicates() {
        return N.hasDuplicates(coll, false);
    }

    public int count(Predicate<? super T> filter) {
        return N.count(coll, filter);
    }

    public List<T> filter(Predicate<? super T> filter) {
        return N.filter(coll, filter);
    }

    public List<T> filter(Predicate<? super T> filter, final int max) {
        return N.filter(coll, filter, max);
    }

    public <U> List<T> filter(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    //    public <R> List<R> filterThenMap(Predicate<? super T> filter, final Function<? super T, ? extends R> mapper) {
    //        if (N.isNullOrEmpty(coll)) {
    //            return new ArrayList<>();
    //        }
    //
    //        final List<R> res = new ArrayList<>();
    //
    //        for (T e : coll) {
    //            if (filter.test(e)) {
    //                res.add(mapper.apply(e));
    //            }
    //        }
    //
    //        return res;
    //    }
    //
    //    public <R> List<R> filterThenFlatMap(Predicate<? super T> filter, final Function<? super T, ? extends Collection<R>> mapper) {
    //        if (N.isNullOrEmpty(coll)) {
    //            return new ArrayList<>();
    //        }
    //
    //        final List<R> res = new ArrayList<>();
    //
    //        for (T e : coll) {
    //            if (filter.test(e)) {
    //                res.addAll(mapper.apply(e));
    //            }
    //        }
    //
    //        return res;
    //    }
    //
    //    public <R> List<R> filterThenFlatMap2(Predicate<? super T> filter, final Function<? super T, ? extends R[]> mapper) {
    //        if (N.isNullOrEmpty(coll)) {
    //            return new ArrayList<>();
    //        }
    //
    //        final List<R> res = new ArrayList<>();
    //        R[] a = null;
    //
    //        for (T e : coll) {
    //            if (filter.test(e)) {
    //                a = mapper.apply(e);
    //
    //                if (N.notNullOrEmpty(a)) {
    //                    if (a.length < 9) {
    //                        for (R r : a) {
    //                            res.add(r);
    //                        }
    //                    } else {
    //                        res.addAll(Arrays.asList(a));
    //                    }
    //                }
    //            }
    //        }
    //
    //        return res;
    //    }
    //
    //    public NullabLe<T> filterThenReduce(Predicate<? super T> filter, final BinaryOperator<T> accumulator) {
    //        if (N.isNullOrEmpty(coll)) {
    //            return NullabLe.<T> empty();
    //        }
    //
    //        T result = (T) N.NULL_MASK;
    //
    //        for (T e : coll) {
    //            if (filter.test(e)) {
    //                result = result == N.NULL_MASK ? e : accumulator.apply(result, e);
    //            }
    //        }
    //
    //        return result == N.NULL_MASK ? NullabLe.<T> empty() : NullabLe.of(result);
    //    }
    //
    //    public <U> NullabLe<U> filterThenReduce(Predicate<? super T> filter, final U identity, final BiFunction<U, ? super T, U> accumulator) {
    //        if (N.isNullOrEmpty(coll)) {
    //            return NullabLe.of(identity);
    //        }
    //
    //        U result = identity;
    //
    //        for (T e : coll) {
    //            if (filter.test(e)) {
    //                result = accumulator.apply(result, e);
    //            }
    //        }
    //
    //        return NullabLe.of(result);
    //    }
    //
    //    public <A, R> R filterThenCollect(Predicate<? super T> filter, final Supplier<R> supplier, final BiConsumer<R, ? super T> accumulator) {
    //        final R result = supplier.get();
    //
    //        if (N.notNullOrEmpty(coll)) {
    //            for (T e : coll) {
    //                if (filter.test(e)) {
    //                    accumulator.accept(result, e);
    //                }
    //            }
    //        }
    //
    //        return result;
    //    }
    //
    //    public <A, R> R filterThenCollect(Predicate<? super T> filter, final Collector<? super T, A, R> collector) {
    //        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
    //        final A result = collector.supplier().get();
    //
    //        if (N.notNullOrEmpty(coll)) {
    //            for (T e : coll) {
    //                if (filter.test(e)) {
    //                    accumulator.accept(result, e);
    //                }
    //            }
    //        }
    //
    //        return collector.finisher().apply(result);
    //    }

    public List<T> takeWhile(Predicate<? super T> filter) {
        final List<T> result = new ArrayList<>(N.min(9, size()));

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            if (filter.test(e)) {
                result.add(e);
            } else {
                break;
            }
        }

        return result;
    }

    public <U> List<T> takeWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return takeWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public List<T> takeWhileInclusive(Predicate<? super T> filter) {
        final List<T> result = new ArrayList<>(N.min(9, size()));

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.add(e);

            if (filter.test(e) == false) {
                break;
            }
        }

        return result;
    }

    public <U> List<T> takeWhileInclusive(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return takeWhileInclusive(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public List<T> dropWhile(Predicate<? super T> filter) {
        final List<T> result = new ArrayList<>(N.min(9, size()));

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        final Iterator<T> iter = iterator();
        T e = null;

        while (iter.hasNext()) {
            e = iter.next();

            if (filter.test(e) == false) {
                result.add(e);
                break;
            }
        }

        while (iter.hasNext()) {
            result.add(iter.next());
        }

        return result;
    }

    public <U> List<T> dropWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public List<T> skipUntil(final Predicate<? super T> filter) {
        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !filter.test(value);
            }
        });
    }

    public <U> List<T> skipUntil(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !predicate.test(value, seed);
            }
        });
    }

    public <R> List<R> map(final Function<? super T, ? extends R> func) {
        return N.map(coll, func);
    }

    public BooleanList mapToBoolean(final ToBooleanFunction<? super T> func) {
        return N.mapToBoolean(coll, func);
    }

    public CharList mapToChar(final ToCharFunction<? super T> func) {
        return N.mapToChar(coll, func);
    }

    public ByteList mapToByte(final ToByteFunction<? super T> func) {
        return N.mapToByte(coll, func);
    }

    public ShortList mapToShort(final ToShortFunction<? super T> func) {
        return N.mapToShort(coll, func);
    }

    public IntList mapToInt(final ToIntFunction<? super T> func) {
        return N.mapToInt(coll, func);
    }

    public LongList mapToLong(final ToLongFunction<? super T> func) {
        return N.mapToLong(coll, func);
    }

    public FloatList mapToFloat(final ToFloatFunction<? super T> func) {
        return N.mapToFloat(coll, func);
    }

    public DoubleList mapToDouble(final ToDoubleFunction<? super T> func) {
        return N.mapToDouble(coll, func);
    }

    public <R> List<R> flatMap(final Function<? super T, ? extends Collection<R>> func) {
        final List<R> result = new ArrayList<>(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <R> List<R> flatMap2(final Function<? super T, ? extends R[]> func) {
        final List<R> result = new ArrayList<>(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        R[] a = null;
        for (T e : coll) {
            a = func.apply(e);

            if (N.notNullOrEmpty(a)) {
                if (a.length < 9) {
                    for (R r : a) {
                        result.add(r);
                    }
                } else {
                    result.addAll(Arrays.asList(a));
                }
            }
        }

        return result;
    }

    public BooleanList flatMapToBoolean(final Function<? super T, ? extends Collection<Boolean>> func) {
        final BooleanList result = new BooleanList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (boolean b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public BooleanList flatMapToBoolean2(final Function<? super T, boolean[]> func) {
        final BooleanList result = new BooleanList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public CharList flatMapToChar(final Function<? super T, ? extends Collection<Character>> func) {
        final CharList result = new CharList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (char b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public CharList flatMapToChar2(final Function<? super T, char[]> func) {
        final CharList result = new CharList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public ByteList flatMapToByte(final Function<? super T, ? extends Collection<Byte>> func) {
        final ByteList result = new ByteList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (byte b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public ByteList flatMapToByte2(final Function<? super T, byte[]> func) {
        final ByteList result = new ByteList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public ShortList flatMapToShort(final Function<? super T, ? extends Collection<Short>> func) {
        final ShortList result = new ShortList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (short b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public ShortList flatMapToShort2(final Function<? super T, short[]> func) {
        final ShortList result = new ShortList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public IntList flatMapToInt(final Function<? super T, ? extends Collection<Integer>> func) {
        final IntList result = new IntList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (int b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public IntList flatMapToInt2(final Function<? super T, int[]> func) {
        final IntList result = new IntList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public LongList flatMapToLong(final Function<? super T, ? extends Collection<Long>> func) {
        final LongList result = new LongList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (long b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public LongList flatMapToLong2(final Function<? super T, long[]> func) {
        final LongList result = new LongList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public FloatList flatMapToFloat(final Function<? super T, ? extends Collection<Float>> func) {
        final FloatList result = new FloatList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (float b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public FloatList flatMapToFloat2(final Function<? super T, float[]> func) {
        final FloatList result = new FloatList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public DoubleList flatMapToDouble(final Function<? super T, ? extends Collection<Double>> func) {
        final DoubleList result = new DoubleList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            for (double b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public DoubleList flatMapToDouble2(final Function<? super T, double[]> func) {
        final DoubleList result = new DoubleList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <U, R> List<R> flatMap(final Function<? super T, ? extends Collection<U>> mapper, final BiFunction<? super T, ? super U, ? extends R> func) {
        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<R>();
        }

        final List<R> result = new ArrayList<R>(N.max(9, coll.size()));

        for (T e : coll) {
            final Collection<U> c = mapper.apply(e);
            if (N.notNullOrEmpty(c)) {
                for (U u : c) {
                    result.add(func.apply(e, u));
                }
            }
        }

        return result;
    }

    public <T2, T3, R> List<R> flatMap(final Function<? super T, ? extends Collection<T2>> mapper2,
            final Function<? super T2, ? extends Collection<T3>> mapper3, final TriFunction<? super T, ? super T2, ? super T3, R> func) {
        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<R>();
        }

        final List<R> result = new ArrayList<R>(N.max(9, coll.size()));

        for (T e : coll) {
            final Collection<T2> c2 = mapper2.apply(e);
            if (N.notNullOrEmpty(c2)) {
                for (T2 t2 : c2) {
                    final Collection<T3> c3 = mapper3.apply(t2);
                    if (N.notNullOrEmpty(c3)) {
                        for (T3 t3 : c3) {
                            result.add(func.apply(e, t2, t3));
                        }
                    }
                }
            }
        }

        return result;
    }

    /**
     * Merge series of adjacent elements which satisfy the given predicate using
     * the merger function and return a new stream.
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param collapsible
     * @param mergeFunction
     * @return
     */
    public List<T> collapse(final BiPredicate<? super T, ? super T> collapsible, final BiFunction<? super T, ? super T, T> mergeFunction) {
        final List<T> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        final Iterator<T> iter = iterator();
        boolean hasNext = false;
        T next = null;

        while (hasNext || iter.hasNext()) {
            T res = hasNext ? next : (next = iter.next());

            while ((hasNext = iter.hasNext())) {
                if (collapsible.test(next, (next = iter.next()))) {
                    res = mergeFunction.apply(res, next);
                } else {
                    break;
                }
            }

            result.add(res);
        }

        return result;
    }

    /**
     * Merge series of adjacent elements which satisfy the given predicate using
     * the merger function and return a new stream.
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     * 
     * @param collapsible
     * @param collector
     * @return
     */
    public <R, A> List<R> collapse(final BiPredicate<? super T, ? super T> collapsible, final Collector<? super T, A, R> collector) {
        final List<R> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        final Supplier<A> supplier = collector.supplier();
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final Function<A, R> finisher = collector.finisher();
        final Iterator<T> iter = iterator();
        boolean hasNext = false;
        T next = null;

        while (hasNext || iter.hasNext()) {
            final A c = supplier.get();
            accumulator.accept(c, hasNext ? next : (next = iter.next()));

            while ((hasNext = iter.hasNext())) {
                if (collapsible.test(next, (next = iter.next()))) {
                    accumulator.accept(c, next);
                } else {
                    break;
                }
            }

            result.add(finisher.apply(c));
        }

        return result;
    }

    /**
     * Returns a {@code Stream} produced by iterative application of a accumulation function
     * to an initial element {@code identity} and next element of the current stream.
     * Produces a {@code Stream} consisting of {@code identity}, {@code acc(identity, value1)},
     * {@code acc(acc(identity, value1), value2)}, etc.
     *
     * <p>This is an intermediate operation.
     *
     * <p>Example:
     * <pre>
     * accumulator: (a, b) -&gt; a + b
     * stream: [1, 2, 3, 4, 5]
     * result: [1, 3, 6, 10, 15]
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     *
     * @param accumulator  the accumulation function
     * @return the new stream which has the extract same size as this stream.
     */
    public List<T> scan(final BiFunction<? super T, ? super T, T> accumulator) {
        final List<T> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        final Iterator<T> iter = iterator();
        T next = null;

        if (iter.hasNext()) {
            result.add((next = iter.next()));
        }

        while (iter.hasNext()) {
            result.add((next = accumulator.apply(next, iter.next())));
        }

        return result;
    }

    /**
     * Returns a {@code Stream} produced by iterative application of a accumulation function
     * to an initial element {@code identity} and next element of the current stream.
     * Produces a {@code Stream} consisting of {@code identity}, {@code acc(identity, value1)},
     * {@code acc(acc(identity, value1), value2)}, etc.
     *
     * <p>This is an intermediate operation.
     *
     * <p>Example:
     * <pre>
     * seed:10
     * accumulator: (a, b) -&gt; a + b
     * stream: [1, 2, 3, 4, 5]
     * result: [11, 13, 16, 20, 25]
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     *
     * @param seed the initial value. it's only used once by <code>accumulator</code> to calculate the fist element in the returned stream. 
     * It will be ignored if this stream is empty and won't be the first element of the returned stream.
     * 
     * @param accumulator  the accumulation function
     * @return the new stream which has the extract same size as this stream.
     */
    public <R> List<R> scan(final R seed, final BiFunction<? super R, ? super T, R> accumulator) {
        final List<R> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        final Iterator<T> iter = iterator();
        R next = seed;

        while (iter.hasNext()) {
            result.add((next = accumulator.apply(next, iter.next())));
        }

        return result;
    }

    /**
     * This is equivalent to:
     * <pre>
     * <code>
     *    if (isEmpty()) {
     *        return NullabLe.empty();
     *    }
     *
     *    final Iterator<T> iter = iterator();
     *    T result = iter.next();
     *
     *    while (iter.hasNext()) {
     *        result = accumulator.apply(result, iter.next());
     *    }
     *
     *    return NullabLe.of(result);
     * </code>
     * </pre>
     * 
     * @param accumulator
     * @return
     */
    public NullabLe<T> reduce(BinaryOperator<T> accumulator) {
        if (isEmpty()) {
            return NullabLe.empty();
        }

        final Iterator<T> iter = iterator();
        T result = iter.next();

        while (iter.hasNext()) {
            result = accumulator.apply(result, iter.next());
        }

        return NullabLe.of(result);
    }

    /**
     * This is equivalent to:
     * <pre>
     * <code>
     *     if (isEmpty()) {
     *         return identity;
     *     }
     * 
     *     final Iterator<T> iter =  iterator();
     *     U result = identity;
     * 
     *     while (iter.hasNext()) {
     *         result = accumulator.apply(result, iter.next());
     *     }
     * 
     *     return result;
     * </code>
     * </pre>
     * 
     * @param identity
     * @param accumulator
     * @return
     */
    public <U> U reduce(U identity, BiFunction<U, ? super T, U> accumulator) {
        if (isEmpty()) {
            return identity;
        }

        final Iterator<T> iter = iterator();
        U result = identity;

        while (iter.hasNext()) {
            result = accumulator.apply(result, iter.next());
        }

        return result;
    }

    public <R> R collect(final Supplier<R> supplier, final BiConsumer<R, ? super T> accumulator) {
        final R result = supplier.get();

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                accumulator.accept(result, e);
            }
        }

        return result;
    }

    public <R, A> R collect(final Collector<? super T, A, R> collector) {
        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final A result = collector.supplier().get();

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                accumulator.accept(result, e);
            }
        }

        return collector.finisher().apply(result);
    }

    public <R, A, RR> RR collectAndThen(final Collector<T, A, R> downstream, final Function<R, RR> finisher) {
        return finisher.apply(collect(downstream));
    }

    @SafeVarargs
    public final List<T> append(T... a) {
        if (N.isNullOrEmpty(a)) {
            return toList();
        }

        return append(Arrays.asList(a));
    }

    public List<T> append(final Collection<? extends T> c) {
        return Seq.concat(this, c);
    }

    @SafeVarargs
    public final List<T> prepend(T... a) {
        if (N.isNullOrEmpty(a)) {
            return toList();
        }

        return prepend(Arrays.asList(a));
    }

    public List<T> prepend(final Collection<? extends T> c) {
        return Seq.concat(c, this);
    }

    public List<T> merge(final Collection<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return Seq.merge(this, b, nextSelector);
    }

    public <B, R> List<R> zipWith(final Collection<B> b, final BiFunction<? super T, ? super B, R> zipFunction) {
        return Seq.zip(this, b, zipFunction);
    }

    public <B, R> List<R> zipWith(final Collection<B> b, final T valueForNoneA, final B valueForNoneB, final BiFunction<? super T, ? super B, R> zipFunction) {
        return Seq.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public <B, C, R> List<R> zipWith(final Collection<B> b, final Collection<C> c, final TriFunction<? super T, ? super B, ? super C, R> zipFunction) {
        return Seq.zip(this, b, c, zipFunction);
    }

    public <B, C, R> List<R> zipWith(final Collection<B> b, final Collection<C> c, final T valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super T, ? super B, ? super C, R> zipFunction) {
        return Seq.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public List<T> intersperse(T value) {
        if (isEmpty()) {
            return new ArrayList<>();
        }

        final int size = size();
        final List<T> result = new ArrayList<>(size * 2 - 1);
        int idx = 0;

        for (T e : coll) {
            result.add(e);

            if (++idx < size) {
                result.add(value);
            }
        }

        return result;
    }

    public List<Indexed<T>> indexed() {
        final List<Indexed<T>> result = new ArrayList<>(size());

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        int idx = 0;

        for (T e : coll) {
            result.add(Indexed.of(e, idx++));
        }

        return result;
    }

    /**
     *
     * @return a new List with distinct elements
     */
    public List<T> distinct() {
        return N.distinct(coll);
    }

    /**
     * 
     * @param keyExtractor don't change value of the input parameter.
     * @return
     */
    public List<T> distinctBy(final Function<? super T, ?> keyExtractor) {
        return N.distinctBy(coll, keyExtractor);
    }

    @SuppressWarnings("rawtypes")
    public List<T> top(final int n) {
        return N.top((Collection) coll, n);
    }

    public List<T> top(final int n, final Comparator<? super T> cmp) {
        return N.top(coll, n, cmp);
    }

    /**
     * Returns consecutive sub lists of this list, each of the same size (the final list may be smaller),
     * or an empty List if the specified list is null or empty.
     *
     * @return
     */
    public List<List<T>> split(int size) {
        return N.split(coll, size);
    }

    public <U> List<List<T>> split(final Predicate<? super T> predicate) {
        N.requireNonNull(predicate);

        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<>();
        }

        final BiFunction<T, Object, Boolean> predicate2 = new BiFunction<T, Object, Boolean>() {
            @Override
            public Boolean apply(T t, Object u) {
                return predicate.test(t);
            }
        };

        return split(null, predicate2, null);
    }

    public <U> List<List<T>> split(final U identity, final BiFunction<? super T, ? super U, Boolean> predicate, final Consumer<? super U> identityUpdate) {
        N.requireNonNull(predicate);

        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<>();
        }

        final List<List<T>> res = new ArrayList<>();
        final Iterator<T> elements = iterator();
        T next = (T) N.NULL_MASK;
        boolean preCondition = false;

        while (next != N.NULL_MASK || elements.hasNext()) {
            final List<T> piece = new ArrayList<>();

            if (next == N.NULL_MASK) {
                next = elements.next();
            }

            while (next != N.NULL_MASK) {
                if (piece.size() == 0) {
                    piece.add(next);
                    preCondition = predicate.apply(next, identity);
                    next = elements.hasNext() ? elements.next() : (T) N.NULL_MASK;
                } else if (predicate.apply(next, identity) == preCondition) {
                    piece.add(next);
                    next = elements.hasNext() ? elements.next() : (T) N.NULL_MASK;
                } else {
                    if (identityUpdate != null) {
                        identityUpdate.accept(identity);
                    }

                    break;
                }
            }

            res.add(piece);
        }

        return res;
    }

    /**
     * 
     * @param n
     * @return
     */
    @SuppressWarnings("rawtypes")
    public Pair<List<T>, List<T>> splitAt(final int n) {
        N.checkArgument(n >= 0, "'n' can't be negative: ", n);

        List<T> left = null;
        List<T> right = null;

        if (N.isNullOrEmpty(coll)) {
            left = new ArrayList<>();
            right = new ArrayList<>();
        } else if (n == 0) {
            left = new ArrayList<>();
            right = new ArrayList<>(coll);
        } else if (n >= coll.size()) {
            left = new ArrayList<>();
            right = new ArrayList<>(coll);
        } else if (coll instanceof List) {
            left = new ArrayList<>(((List) coll).subList(0, n));
            right = new ArrayList<>(((List) coll).subList(n, size()));
        } else {
            left = new ArrayList<>(slice(0, n));
            right = new ArrayList<>(slice(n, size()));
        }

        return Pair.of(left, right);
    }

    public Pair<List<T>, List<T>> splitBy(final Predicate<? super T> predicate) {
        N.requireNonNull(predicate);

        final List<T> left = new ArrayList<>();
        final List<T> right = new ArrayList<>();

        if (N.notNullOrEmpty(coll)) {
            final Iterator<T> iter = iterator();
            T next = (T) N.NULL_MASK;

            while (iter.hasNext() && predicate.test((next = iter.next()))) {
                left.add(next);

                next = (T) N.NULL_MASK;
            }

            if (next != N.NULL_MASK) {
                right.add(next);
            }

            while (iter.hasNext()) {
                right.add(iter.next());
            }
        }

        return Pair.of(left, right);
    }

    public List<List<T>> sliding(final int windowSize) {
        return sliding(windowSize, 1);
    }

    public List<List<T>> sliding(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<>();
        }

        final Iterator<T> iter = coll.iterator();
        final List<List<T>> result = new ArrayList<>(coll.size() <= windowSize ? 1 : (1 + (coll.size() - windowSize)) / increment);

        while (iter.hasNext()) {
            if (increment > windowSize && result.size() > 0) {
                int skipNum = increment - windowSize;

                while (skipNum-- > 0 && iter.hasNext()) {
                    iter.next();
                }

                if (iter.hasNext() == false) {
                    break;
                }
            }

            final List<T> window = new ArrayList<>(windowSize);
            int cnt = 0;

            if (increment < windowSize && result.size() > 0) {
                final List<T> prev = result.get(result.size() - 1);
                cnt = windowSize - increment;

                if (cnt <= 8) {
                    for (int i = windowSize - cnt; i < windowSize; i++) {
                        window.add(prev.get(i));
                    }
                } else {
                    window.addAll(prev.subList(windowSize - cnt, windowSize));
                }
            }

            while (cnt++ < windowSize && iter.hasNext()) {
                window.add(iter.next());
            }

            result.add(window);
        }

        return result;
    }

    public String join() {
        return join(N.ELEMENT_SEPARATOR);
    }

    public String join(final char delimiter) {
        return N.join(coll, delimiter);
    }

    public String join(final String delimiter) {
        return N.join(coll, delimiter);
    }

    @Override
    public boolean isEmpty() {
        return coll == null || coll.isEmpty();
    }

    @Override
    public int size() {
        return coll == null ? 0 : coll.size();
    }

    @Override
    public Object[] toArray() {
        return coll == null ? N.EMPTY_OBJECT_ARRAY : coll.toArray();
    }

    @Override
    public <A> A[] toArray(A[] a) {
        return coll == null ? a : coll.toArray(a);
    }

    public List<T> toList() {
        return coll == null ? new ArrayList<T>() : new ArrayList<T>(coll);
    }

    public <R extends List<T>> R toList(final IntFunction<R> supplier) {
        final R result = supplier.apply(size());

        if (N.notNullOrEmpty(coll)) {
            result.addAll(coll);
        }

        return result;
    }

    public Set<T> toSet() {
        return coll == null ? new HashSet<T>() : new HashSet<T>(coll);
    }

    public <R extends Set<T>> R toSet(final IntFunction<R> supplier) {
        final R result = supplier.apply(size());

        if (N.notNullOrEmpty(coll)) {
            result.addAll(coll);
        }

        return result;
    }

    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>(N.initHashCapacity(size()));

        if (N.notNullOrEmpty(coll)) {
            result.addAll(coll);
        }

        return result;
    }

    public Multiset<T> toMultiset(final IntFunction<Multiset<T>> supplier) {
        final Multiset<T> result = supplier.apply(N.initHashCapacity(size()));

        if (N.notNullOrEmpty(coll)) {
            result.addAll(coll);
        }

        return result;
    }

    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction) {
        final Supplier<Map<K, U>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Iterator<T> iter = iterator();
        T element = null;

        while (iter.hasNext()) {
            element = iter.next();
            merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @SuppressWarnings("hiding")
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        final Supplier<Map<K, D>> mapFactory = Fn.Suppliers.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        final Iterator<T> iter = iterator();
        K key = null;
        A v = null;
        T element = null;

        while (iter.hasNext()) {
            element = iter.next();
            key = N.requireNonNull(classifier.apply(element), "element cannot be mapped to a null key");

            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, element);
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        replaceAll(intermediate, function);

        return result;
    }

    public <K> Map<K, List<T>> groupTo(Function<? super T, ? extends K> classifier) {
        final Supplier<Map<K, List<T>>> mapFactory = Fn.Suppliers.ofMap();

        return groupTo(classifier, mapFactory);
    }

    public <K, M extends Map<K, List<T>>> M groupTo(Function<? super T, ? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    public <K, U> Map<K, List<U>> groupTo(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper) {
        return toMap(keyExtractor, (Collector<T, ?, List<U>>) (Collector<?, ?, ?>) Collectors.mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> M groupTo(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        return toMap(keyExtractor, (Collector<T, ?, List<U>>) (Collector) Collectors.mapping(valueMapper, Collectors.toList()), mapFactory);
    }

    /**
     * 
     * @param keyExtractor
     * @return
     */
    public <K> ListMultimap<K, T> toMultimap(Function<? super T, ? extends K> keyExtractor) {
        final ListMultimap<K, T> result = N.newListMultimap();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.put(keyExtractor.apply(e), e);
        }

        return result;
    }

    public <K, V extends Collection<T>, M extends Multimap<K, T, V>> M toMultimap(Function<? super T, ? extends K> keyExtractor, Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.put(keyExtractor.apply(e), e);
        }

        return result;
    }

    /**
     * 
     * @param keyExtractor
     * @param valueMapper
     * @return
     */
    public <K, V> ListMultimap<K, V> toMultimap(Function<? super T, ? extends K> keyExtractor, Function<? super T, ? extends V> valueMapper) {
        final ListMultimap<K, V> result = N.newListMultimap();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.put(keyExtractor.apply(e), valueMapper.apply(e));
        }

        return result;
    }

    public <K, U, V extends Collection<U>, M extends Multimap<K, U, V>> M toMultimap(Function<? super T, ? extends K> keyExtractor,
            Function<? super T, ? extends U> valueMapper, Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.put(keyExtractor.apply(e), valueMapper.apply(e));
        }

        return result;
    }

    /**
     * 
     * The time complexity is <i>O(n + m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param leftKeyMapper
     * @param rightKeyMapper
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-full-join">sql join</a>
     */
    public <U> List<Pair<T, U>> innerJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final List<Pair<T, U>> result = new ArrayList<>(N.min(9, size(), b.size()));

        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(b)) {
            return result;
        }

        final ListMultimap<Object, U> rightKeyMap = ListMultimap.from(b, rightKeyMapper);

        for (T left : coll) {
            final List<U> rights = rightKeyMap.get(leftKeyMapper.apply(left));

            if (N.notNullOrEmpty(rights)) {
                for (U right : rights) {
                    result.add(Pair.of(left, right));
                }
            }
        }

        return result;
    }

    /**
     * 
     * The time complexity is <i>O(n * m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param predicate
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-ful
     */
    public <U> List<Pair<T, U>> innerJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final List<Pair<T, U>> result = new ArrayList<>(N.min(9, size(), b.size()));

        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(b)) {
            return result;
        }

        for (T left : coll) {
            for (U right : b) {
                if (predicate.test(left, right)) {
                    result.add(Pair.of(left, right));
                }
            }
        }

        return result;
    }

    /**
     * 
     * The time complexity is <i>O(n + m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param leftKeyMapper
     * @param rightKeyMapper
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-ful
     */
    public <U> List<Pair<T, U>> fullJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final List<Pair<T, U>> result = new ArrayList<>(N.max(9, size(), b.size()));

        if (N.isNullOrEmpty(coll)) {
            for (T left : coll) {
                result.add(Pair.of(left, (U) null));
            }
        } else if (N.isNullOrEmpty(b)) {
            for (U right : b) {
                result.add(Pair.of((T) null, right));
            }
        } else {
            final ListMultimap<Object, U> rightKeyMap = ListMultimap.from(b, rightKeyMapper);
            final Map<U, U> joinedRights = new IdentityHashMap<>();

            for (T left : coll) {
                final List<U> rights = rightKeyMap.get(leftKeyMapper.apply(left));

                if (N.notNullOrEmpty(rights)) {
                    for (U right : rights) {
                        result.add(Pair.of(left, right));
                        joinedRights.put(right, right);
                    }
                } else {
                    result.add(Pair.of(left, (U) null));
                }
            }

            for (U right : b) {
                if (joinedRights.containsKey(right) == false) {
                    result.add(Pair.of((T) null, right));
                }
            }
        }

        return result;
    }

    /**
     * The time complexity is <i>O(n * m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param predicate
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-ful
     */
    public <U> List<Pair<T, U>> fullJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final List<Pair<T, U>> result = new ArrayList<>(N.max(9, size(), b.size()));

        if (N.isNullOrEmpty(coll)) {
            for (T left : coll) {
                result.add(Pair.of(left, (U) null));
            }
        } else if (N.isNullOrEmpty(b)) {
            for (U right : b) {
                result.add(Pair.of((T) null, right));
            }
        } else {
            final Map<U, U> joinedRights = new IdentityHashMap<>();

            for (T left : coll) {
                boolean joined = false;

                for (U right : b) {
                    if (predicate.test(left, right)) {
                        result.add(Pair.of(left, right));
                        joinedRights.put(right, right);
                        joined = true;
                    }
                }

                if (joined == false) {
                    result.add(Pair.of(left, (U) null));
                }
            }

            for (U right : b) {
                if (joinedRights.containsKey(right) == false) {
                    result.add(Pair.of((T) null, right));
                }
            }
        }

        return result;
    }

    /**
     * 
     * The time complexity is <i>O(n + m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param leftKeyMapper
     * @param rightKeyMapper
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-ful
     */
    public <U> List<Pair<T, U>> leftJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final List<Pair<T, U>> result = new ArrayList<>(size());

        if (N.isNullOrEmpty(coll)) {
            return result;
        } else if (N.isNullOrEmpty(b)) {
            for (T left : coll) {
                result.add(Pair.of(left, (U) null));
            }
        } else {
            final ListMultimap<Object, U> rightKeyMap = ListMultimap.from(b, rightKeyMapper);

            for (T left : coll) {
                final List<U> rights = rightKeyMap.get(leftKeyMapper.apply(left));

                if (N.notNullOrEmpty(rights)) {
                    for (U right : rights) {
                        result.add(Pair.of(left, right));
                    }
                } else {
                    result.add(Pair.of(left, (U) null));
                }
            }
        }

        return result;
    }

    /**
     * The time complexity is <i>O(n * m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param predicate
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-ful
     */
    public <U> List<Pair<T, U>> leftJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final List<Pair<T, U>> result = new ArrayList<>(size());

        if (N.isNullOrEmpty(coll)) {
            return result;
        } else if (N.isNullOrEmpty(b)) {
            for (T left : coll) {
                result.add(Pair.of(left, (U) null));
            }
        } else {
            for (T left : coll) {
                boolean joined = false;

                for (U right : b) {
                    if (predicate.test(left, right)) {
                        result.add(Pair.of(left, right));
                        joined = true;
                    }
                }

                if (joined == false) {
                    result.add(Pair.of(left, (U) null));
                }
            }
        }

        return result;
    }

    /**
     * 
     * The time complexity is <i>O(n + m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param leftKeyMapper
     * @param rightKeyMapper
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-ful
     */
    public <U> List<Pair<T, U>> rightJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final List<Pair<T, U>> result = new ArrayList<>(b.size());

        if (N.isNullOrEmpty(b)) {
            return result;
        } else if (N.isNullOrEmpty(coll)) {
            for (U right : b) {
                result.add(Pair.of((T) null, right));
            }
        } else {
            final ListMultimap<Object, T> leftKeyMap = ListMultimap.from(coll, leftKeyMapper);

            for (U right : b) {
                final List<T> lefts = leftKeyMap.get(rightKeyMapper.apply(right));

                if (N.notNullOrEmpty(lefts)) {
                    for (T left : lefts) {
                        result.add(Pair.of(left, right));
                    }
                } else {
                    result.add(Pair.of((T) null, right));
                }
            }
        }

        return result;
    }

    /**
     * The time complexity is <i>O(n * m)</i> : <i>n</i> is the size of this <code>Seq</code> and <i>m</i> is the size of specified collection <code>b</code>.
     * 
     * @param b
     * @param predicate
     * @return
     * @see <a href="http://stackoverflow.com/questions/5706437/whats-the-difference-between-inner-join-left-join-right-join-and-ful
     */
    public <U> List<Pair<T, U>> rightJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final List<Pair<T, U>> result = new ArrayList<>(b.size());

        if (N.isNullOrEmpty(b)) {
            return result;
        } else if (N.isNullOrEmpty(coll)) {
            for (U right : b) {
                result.add(Pair.of((T) null, right));
            }
        } else {
            for (U right : b) {
                boolean joined = false;

                for (T left : coll) {
                    if (predicate.test(left, right)) {
                        result.add(Pair.of(left, right));
                        joined = true;
                    }
                }

                if (joined == false) {
                    result.add(Pair.of((T) null, right));
                }
            }
        }

        return result;
    }

    /**
     * Returns a read-only <code>Seq</code>.
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public Seq<T> slice(final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, size());

        if (N.isNullOrEmpty(coll)) {
            return this;
        }

        if (coll instanceof List) {
            return new Seq<T>(((List<T>) coll).subList(fromIndex, toIndex));
        }

        return new Seq<T>(new SubCollection<>(coll, fromIndex, toIndex));
    }

    @Override
    public Iterator<T> iterator() {
        return coll == null ? ImmutableIterator.EMPTY : new ImmutableIterator<T>() {
            private final Iterator<T> iter = coll.iterator();

            @Override
            public boolean hasNext() {
                return iter.hasNext();
            }

            @Override
            public T next() {
                return iter.next();
            }
        };
    }

    //    public Stream<T> stream() {
    //        return N.isNullOrEmpty(coll) ? Stream.<T> empty() : Stream.of(coll);
    //    }
    //
    //    public ListBuilder<T> __() {
    //        return Builder.of(this);
    //    }
    //
    //    public ListBuilder<T> __(Consumer<? super List<T>> func) {
    //        return Builder.of(this).__(func);
    //    }

    @Override
    public int hashCode() {
        return coll == null ? 0 : coll.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (obj instanceof Seq) {
            final Seq<T> other = (Seq<T>) obj;

            return N.equals(this.coll, other.coll);
        }

        return false;
    }

    @Override
    public String toString() {
        return coll == null ? N.NULL_STRING : coll.toString();
    }

    public void println() {
        N.println(toString());
    }

    //    /**
    //     * It's the short-cut for <code>Seq.of(seq.xxx())</code>.
    //     * <code>Stream</code> is recommended for lazy evaluation and skip/limit/… operations
    //     * 
    //     * @param transfer
    //     * @return
    //     */
    //    public <U> Seq<U> __(Function<? super Seq<T>, ? extends Collection<U>> transfer) {
    //        return Seq.of(transfer.apply(this));
    //    }

    static <K, V> void replaceAll(Map<K, V> map, BiFunction<? super K, ? super V, ? extends V> function) {
        Objects.requireNonNull(function);
        for (Map.Entry<K, V> entry : map.entrySet()) {
            K k;
            V v;
            try {
                k = entry.getKey();
                v = entry.getValue();
            } catch (IllegalStateException ise) {
                // this usually means the entry is no longer in the map.
                throw new ConcurrentModificationException(ise);
            }

            // ise thrown from function is not a cme.
            v = function.apply(k, v);

            try {
                entry.setValue(v);
            } catch (IllegalStateException ise) {
                // this usually means the entry is no longer in the map.
                throw new ConcurrentModificationException(ise);
            }
        }
    }

    static <K, V> V merge(Map<K, V> map, K key, V value, BiFunction<? super V, ? super V, ? extends V> remappingFunction) {
        Objects.requireNonNull(remappingFunction);
        Objects.requireNonNull(value);

        V oldValue = map.get(key);
        V newValue = (oldValue == null) ? value : remappingFunction.apply(oldValue, value);
        if (newValue == null) {
            map.remove(key);
        } else {
            map.put(key, newValue);
        }

        return newValue;
    }

    private static final class SubCollection<E> implements Collection<E> {
        private final Collection<E> c;
        private final int fromIndex;
        private final int toIndex;

        SubCollection(Collection<E> c, int fromIndex, int toIndex) {
            this.c = c;
            this.fromIndex = fromIndex;
            this.toIndex = toIndex;
        }

        @Override
        public boolean add(E e) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean remove(Object o) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean contains(Object o) {
            final Iterator<E> iter = this.iterator();

            while (iter.hasNext()) {
                if (N.equals(iter.next(), o)) {
                    return true;
                }
            }

            return false;
        }

        @Override
        public boolean containsAll(Collection<?> c) {
            for (Object e : c) {
                if (contains(e) == false) {
                    return false;
                }
            }

            return true;
        }

        @Override
        public boolean addAll(Collection<? extends E> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean removeAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean retainAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void clear() {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean isEmpty() {
            return size() == 0;
        }

        @Override
        public int size() {
            return toIndex - fromIndex;
        }

        @Override
        public Iterator<E> iterator() {
            final Iterator<E> iter = c == null ? ImmutableIterator.EMPTY : c.iterator();

            if (fromIndex > 0) {
                int offset = 0;

                while (offset++ < fromIndex) {
                    iter.next();
                }
            }

            return new Iterator<E>() {
                private int cursor = fromIndex;

                @Override
                public boolean hasNext() {
                    return cursor < toIndex;
                }

                @Override
                public E next() {
                    if (cursor >= toIndex) {
                        throw new NoSuchElementException();
                    }

                    cursor++;
                    return iter.next();
                }

                @Override
                public void remove() {
                    throw new UnsupportedOperationException();
                }
            };
        }

        @Override
        public Object[] toArray() {
            final Iterator<E> iter = this.iterator();
            final Object[] a = new Object[size()];

            for (int i = 0, len = a.length; i < len; i++) {
                a[i] = iter.next();
            }

            return a;
        }

        @Override
        public <A> A[] toArray(A[] a) {
            if (a.length < size()) {
                a = N.copyOf(a, size());
            }

            final Iterator<E> iter = this.iterator();

            for (int i = 0, len = a.length; i < len; i++) {
                a[i] = (A) iter.next();
            }

            return a;
        }
    }

    public static <T extends Comparable<? super T>> void reverseSort(List<T> c) {
        N.sort(c, Fn.reversedOrder());
    }

    public static <T extends Comparable<? super T>> void reverseSort(Object[] a) {
        N.sort(a, Fn.reversedOrder());
    }

    public static <T> List<T> repeat(final T value, final int n) {
        return new ArrayList<>(Arrays.asList(Array.repeat(value, n)));
    }

    /**
     * <pre>
     * <code>
     * Seq.repeat(N.asList(1, 2, 3), 2) => [1, 2, 3, 1, 2, 3]
     * </code>
     * </pre>
     * @param c
     * @param n
     * @return
     */
    public static <T> List<T> repeat(final Collection<T> c, final int n) {
        if (n < 1) {
            throw new IllegalArgumentException("The specified count must be greater than 0");
        }

        if (N.isNullOrEmpty(c)) {
            return new ArrayList<T>();
        }

        final List<T> result = new ArrayList<>(c.size() * n);

        for (int i = 0; i < n; i++) {
            result.addAll(c);
        }

        return result;
    }

    /**
     * Repeats the elements in the specified Collection one by one.
     * 
     * <pre>
     * <code>
     * Seq.nRepeat(N.asList(1, 2, 3), 2) => [1, 1, 2, 2, 3, 3]
     * </code>
     * </pre>
     * 
     * @param c
     * @param n
     * @return
     */
    public static <T> List<T> nRepeat(final Collection<T> c, final int n) {
        if (n < 1) {
            throw new IllegalArgumentException("The specified count must be greater than 0");
        }

        if (N.isNullOrEmpty(c)) {
            return new ArrayList<T>();
        }

        final List<T> result = new ArrayList<>(c.size() * n);

        for (T e : c) {
            for (int i = 0; i < n; i++) {
                result.add(e);
            }
        }

        return result;
    }

    /**
     * 
     * <pre>
     * <code>
     * Seq.repeatToSize(N.asList(1, 2, 3), 5) => [1, 2, 3, 1, 2]
     * </code>
     * </pre>
     * 
     * @param c
     * @param size
     * @return
     */
    public static <T> List<T> repeatToSize(final Collection<T> c, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The specified size must be greater than 0");
        } else if (N.isNullOrEmpty(c) && size > 0) {
            throw new IllegalArgumentException("The specified collection can't be null or empty when size > 0");
        }

        if (N.isNullOrEmpty(c)) {
            return new ArrayList<T>();
        }

        final List<T> result = new ArrayList<>(size);

        while (result.size() < size) {
            if (c.size() <= size - result.size()) {
                result.addAll(c);
            } else {
                final Iterator<T> iter = c.iterator();

                for (int i = 0, len = size - result.size(); i < len; i++) {
                    result.add(iter.next());
                }
            }
        }

        return result;
    }

    /**
     * Repeats the elements in the specified Collection one by one till reach the specified size.
     * 
     * <pre>
     * <code>
     * Seq.nRepeatToSize(N.asList(1, 2, 3), 5) => [1, 1, 2, 2, 3]
     * </code>
     * </pre>
     * 
     * @param c
     * @param size
     * @return
     */
    public static <T> List<T> nRepeatToSize(final Collection<T> c, final int size) {
        if (size < 1) {
            throw new IllegalArgumentException("The specified size must be greater than 0");
        } else if (N.isNullOrEmpty(c) && size > 0) {
            throw new IllegalArgumentException("The specified collection can't be null or empty when size > 0");
        }

        if (N.isNullOrEmpty(c)) {
            return new ArrayList<T>();
        }

        final int n = size / c.size();
        int mod = size % c.size();

        final List<T> result = new ArrayList<>(size);

        for (T e : c) {
            for (int i = 0, len = mod-- > 0 ? n + 1 : n; i < len; i++) {
                result.add(e);
            }

            if (result.size() == size) {
                break;
            }
        }

        return result;
    }

    public static <T> void reverse(final Collection<T> c) {
        if (N.isNullOrEmpty(c) && c.size() < 2) {
            return;
        }

        if (c instanceof List) {
            N.reverse((List<T>) c);
        } else {
            final Object[] tmp = c.toArray();
            N.reverse(tmp);
            c.clear();
            c.addAll((List<T>) Arrays.asList(tmp));
        }
    }

    public static <T> void rotate(final Collection<T> c, final int distance) {
        if (N.isNullOrEmpty(c) && c.size() < 2) {
            return;
        }

        if (c instanceof List) {
            N.rotate((List<T>) c, distance);
        } else {
            final Object[] tmp = c.toArray();
            N.rotate(tmp, distance);
            c.clear();
            c.addAll((List<T>) Arrays.asList(tmp));
        }
    }

    public static <T> void shuffle(final Collection<T> c) {
        if (N.isNullOrEmpty(c) && c.size() < 2) {
            return;
        }

        if (c instanceof List) {
            N.shuffle((List<T>) c);
        } else {
            final Object[] tmp = c.toArray();
            N.shuffle(tmp);
            c.clear();
            c.addAll((List<T>) Arrays.asList(tmp));
        }
    }

    public static <T> void shuffle(final Collection<T> c, final Random rnd) {
        if (N.isNullOrEmpty(c) && c.size() < 2) {
            return;
        }

        if (c instanceof List) {
            N.shuffle((List<T>) c, rnd);
        } else {
            final Object[] tmp = c.toArray();
            N.shuffle(tmp, rnd);
            c.clear();
            c.addAll((List<T>) Arrays.asList(tmp));
        }
    }

    //    public ListBuilder<T> __() {
    //        return Builder.of(this);
    //    }
    //
    //    public ListBuilder<T> __(Consumer<? super List<T>> func) {
    //        return Builder.of(this).__(func);
    //    }

    public static boolean disjoint(final Object[] a, final Object[] b) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return true;
        }

        return a.length >= b.length ? disjoint(Arrays.asList(a), N.asSet(b)) : disjoint(N.asSet(a), Arrays.asList(b));
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

    public static <T> List<T> concat(final T[] a, final T[] b) {
        if (N.isNullOrEmpty(a)) {
            if (N.isNullOrEmpty(b)) {
                return new ArrayList<>();
            } else {
                final List<T> res = new ArrayList<>(b.length);
                res.addAll(N.asList(b));
                return res;
            }
        } else {
            final List<T> res = new ArrayList<>(a.length + (b == null ? 0 : b.length));
            res.addAll(N.asList(a));

            if (N.notNullOrEmpty(b)) {
                res.addAll(N.asList(b));
            }
            return res;
        }
    }

    public static <T> List<T> concat(final T[]... a) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        int count = 0;

        for (T[] e : a) {
            if (N.notNullOrEmpty(e)) {
                count += e.length;
            }
        }

        final List<T> result = new ArrayList<>(count);

        for (T[] e : a) {
            if (N.notNullOrEmpty(e)) {
                if (a.length < 9) {
                    for (T t : e) {
                        result.add(t);
                    }
                } else {
                    result.addAll(Arrays.asList(e));
                }
            }
        }

        return result;
    }

    public static <T> List<T> concat(final Collection<? extends T> a, final Collection<? extends T> b) {
        return N.concat(a, b);
    }

    @SafeVarargs
    public static <T> List<T> concat(final Collection<? extends T>... a) {
        if (N.isNullOrEmpty(a)) {
            return new ArrayList<>();
        }

        return concat(Arrays.asList(a));
    }

    public static <T> List<T> concat(final Collection<? extends Collection<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        int count = 0;

        for (Collection<? extends T> e : c) {
            if (N.notNullOrEmpty(e)) {
                count += e.size();
            }
        }

        final List<T> result = new ArrayList<>(count);

        for (Collection<? extends T> e : c) {
            if (N.notNullOrEmpty(e)) {
                result.addAll(e);
            }
        }

        return result;
    }

    public static <T> ImmutableIterator<T> concat(final Iterator<? extends T> a, final Iterator<? extends T> b) {
        return Iterators.concat(a, b);
    }

    @SafeVarargs
    public static <T> ImmutableIterator<T> concat(final Iterator<? extends T>... a) {
        return Iterators.concat(a);
    }

    @SafeVarargs
    public static <T> ImmutableIterator<T> iterate(final T[]... a) {
        return Iterators.concat(a);
    }

    @SafeVarargs
    public static <T> ImmutableIterator<T> iterate(final Collection<? extends T>... a) {
        return Iterators.concat(a);
    }

    public static <T> ImmutableIterator<T> iterate(final Collection<? extends Collection<? extends T>> c) {
        if (N.isNullOrEmpty(c)) {
            return ImmutableIterator.empty();
        }

        final List<Iterator<? extends T>> list = new ArrayList<>(c.size());

        for (Collection<? extends T> e : c) {
            if (N.notNullOrEmpty(e)) {
                list.add(e.iterator());
            }
        }

        return Iterators.concat(list);
    }

    public static <T> List<T> merge(final T[] a, final T[] b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? new ArrayList<T>() : N.asList(b);
        } else if (N.isNullOrEmpty(b)) {
            return N.asList(a);
        }

        final List<T> result = new ArrayList<>(a.length + b.length);
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

    public static <T> List<T> merge(final Collection<? extends T> a, final Collection<? extends T> b,
            final BiFunction<? super T, ? super T, Nth> nextSelector) {
        if (N.isNullOrEmpty(a)) {
            return N.isNullOrEmpty(b) ? new ArrayList<T>() : new ArrayList<T>(b);
        } else if (N.isNullOrEmpty(b)) {
            return new ArrayList<T>(a);
        }

        final List<T> result = new ArrayList<>(a.size() + b.size());
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

    public static <T> ImmutableIterator<T> merge(final Iterator<? extends T> a, final Iterator<? extends T> b,
            final BiFunction<? super T, ? super T, Nth> nextSelector) {

        return new ImmutableIterator<T>() {
            private final Iterator<? extends T> iterA = a == null ? ImmutableIterator.EMPTY : a;
            private final Iterator<? extends T> iterB = b == null ? ImmutableIterator.EMPTY : b;
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

    public static <A, B, R> List<R> zip(final A[] a, final B[] b, final BiFunction<? super A, ? super B, R> zipFunction) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return new ArrayList<>();
        }

        final int minLen = N.min(a.length, b.length);
        final List<R> result = new ArrayList<>(minLen);

        for (int i = 0; i < minLen; i++) {
            result.add(zipFunction.apply(a[i], b[i]));
        }

        return result;
    }

    public static <A, B, R> List<R> zip(final Collection<A> a, final Collection<B> b, final BiFunction<? super A, ? super B, R> zipFunction) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b)) {
            return new ArrayList<>();
        }

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();
        final int minLen = N.min(a.size(), b.size());
        final List<R> result = new ArrayList<>(minLen);

        for (int i = 0; i < minLen; i++) {
            result.add(zipFunction.apply(iterA.next(), iterB.next()));
        }

        return result;
    }

    public static <A, B, R> ImmutableIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final BiFunction<? super A, ? super B, R> zipFunction) {
        return new ImmutableIterator<R>() {
            private final Iterator<A> iterA = a == null ? ImmutableIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ImmutableIterator.<B> empty() : b;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() && iterB.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.next(), iterB.next());
            }
        };
    }

    public static <A, B, C, R> List<R> zip(final A[] a, final B[] b, final C[] c, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b) || N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        final int minLen = N.min(a.length, b.length, c.length);
        final List<R> result = new ArrayList<>(minLen);

        for (int i = 0; i < minLen; i++) {
            result.add(zipFunction.apply(a[i], b[i], c[i]));
        }

        return result;
    }

    public static <A, B, C, R> List<R> zip(final Collection<A> a, final Collection<B> b, final Collection<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        if (N.isNullOrEmpty(a) || N.isNullOrEmpty(b) || N.isNullOrEmpty(c)) {
            return new ArrayList<>();
        }

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();
        final Iterator<C> iterC = c.iterator();
        final int minLen = N.min(a.size(), b.size(), c.size());
        final List<R> result = new ArrayList<>(minLen);

        for (int i = 0; i < minLen; i++) {
            result.add(zipFunction.apply(iterA.next(), iterB.next(), iterC.next()));
        }

        return result;
    }

    public static <A, B, C, R> ImmutableIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final Iterator<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return new ImmutableIterator<R>() {
            private final Iterator<A> iterA = a == null ? ImmutableIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ImmutableIterator.<B> empty() : b;
            private final Iterator<C> iterC = c == null ? ImmutableIterator.<C> empty() : c;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() && iterB.hasNext() && iterC.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.next(), iterB.next(), iterC.next());
            }
        };
    }

    public static <A, B, R> List<R> zip(final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        final int lenA = a == null ? 0 : a.length;
        final int lenB = b == null ? 0 : b.length;
        final int maxLen = N.max(lenA, lenB);
        final List<R> result = new ArrayList<>(maxLen);

        for (int i = 0; i < maxLen; i++) {
            result.add(zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB));
        }

        return result;
    }

    public static <A, B, R> List<R> zip(final Collection<A> a, final Collection<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        final Iterator<A> iterA = a == null ? ImmutableIterator.<A> empty() : a.iterator();
        final Iterator<B> iterB = b == null ? ImmutableIterator.<B> empty() : b.iterator();
        final int lenA = a == null ? 0 : a.size();
        final int lenB = b == null ? 0 : b.size();
        final int maxLen = N.max(lenA, lenB);
        final List<R> result = new ArrayList<>(maxLen);

        for (int i = 0; i < maxLen; i++) {
            result.add(zipFunction.apply(i < lenA ? iterA.next() : valueForNoneA, i < lenB ? iterB.next() : valueForNoneB));
        }

        return result;
    }

    public static <A, B, R> ImmutableIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        return new ImmutableIterator<R>() {
            private final Iterator<A> iterA = a == null ? ImmutableIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ImmutableIterator.<B> empty() : b;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() || iterB.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.hasNext() ? iterA.next() : valueForNoneA, iterB.hasNext() ? iterB.next() : valueForNoneB);
            }
        };
    }

    public static <A, B, C, R> List<R> zip(final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final int lenA = a == null ? 0 : a.length;
        final int lenB = b == null ? 0 : b.length;
        final int lenC = c == null ? 0 : c.length;
        final int maxLen = N.max(lenA, lenB, lenC);
        final List<R> result = new ArrayList<>(maxLen);

        for (int i = 0; i < maxLen; i++) {
            result.add(zipFunction.apply(i < lenA ? a[i] : valueForNoneA, i < lenB ? b[i] : valueForNoneB, i < lenC ? c[i] : valueForNoneC));
        }

        return result;
    }

    public static <A, B, C, R> List<R> zip(final Collection<A> a, final Collection<B> b, final Collection<C> c, final A valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final Iterator<A> iterA = a == null ? ImmutableIterator.<A> empty() : a.iterator();
        final Iterator<B> iterB = b == null ? ImmutableIterator.<B> empty() : b.iterator();
        final Iterator<C> iterC = c == null ? ImmutableIterator.<C> empty() : c.iterator();
        final int lenA = a == null ? 0 : a.size();
        final int lenB = b == null ? 0 : b.size();
        final int lenC = c == null ? 0 : c.size();
        final int maxLen = N.max(lenA, lenB, lenC);
        final List<R> result = new ArrayList<>(maxLen);

        for (int i = 0; i < maxLen; i++) {
            result.add(zipFunction.apply(i < lenA ? iterA.next() : valueForNoneA, i < lenB ? iterB.next() : valueForNoneB,
                    i < lenC ? iterC.next() : valueForNoneC));
        }

        return result;
    }

    public static <A, B, C, R> ImmutableIterator<R> zip(final Iterator<A> a, final Iterator<B> b, final Iterator<C> c, final A valueForNoneA,
            final B valueForNoneB, final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return new ImmutableIterator<R>() {
            private final Iterator<A> iterA = a == null ? ImmutableIterator.<A> empty() : a;
            private final Iterator<B> iterB = b == null ? ImmutableIterator.<B> empty() : b;
            private final Iterator<C> iterC = c == null ? ImmutableIterator.<C> empty() : c;

            @Override
            public boolean hasNext() {
                return iterA.hasNext() || iterB.hasNext() || iterC.hasNext();
            }

            @Override
            public R next() {
                return zipFunction.apply(iterA.hasNext() ? iterA.next() : valueForNoneA, iterB.hasNext() ? iterB.next() : valueForNoneB,
                        iterC.hasNext() ? iterC.next() : valueForNoneC);
            }
        };
    }

    /**
     * 
     * @param c
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R> Pair<List<L>, List<R>> unzip(final Collection<? extends T> c, final BiConsumer<? super T, Pair<L, R>> unzip) {
        final int len = c == null ? 0 : c.size();

        final List<L> l = new ArrayList<L>(len);
        final List<R> r = new ArrayList<R>(len);
        final Pair<L, R> p = new Pair<>();

        if (N.notNullOrEmpty(c)) {
            for (T e : c) {
                unzip.accept(e, p);

                l.add(p.left);
                r.add(p.right);
            }
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
    public static <T, L, R, LC extends Collection<L>, RC extends Collection<R>> Pair<LC, RC> unzip(final IntFunction<? extends Collection<?>> supplier,
            final Collection<? extends T> c, final BiConsumer<? super T, Pair<L, R>> unzip) {
        final int len = c == null ? 0 : c.size();

        final LC l = (LC) supplier.apply(len);
        final RC r = (RC) supplier.apply(len);
        final Pair<L, R> p = new Pair<>();

        if (N.notNullOrEmpty(c)) {
            for (T e : c) {
                unzip.accept(e, p);

                l.add(p.left);
                r.add(p.right);
            }
        }

        return Pair.of(l, r);
    }

    /**
     * 
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R> Pair<List<L>, List<R>> unzip(final Iterator<? extends T> iter, final BiConsumer<? super T, Pair<L, R>> unzip) {
        final List<L> l = new ArrayList<L>();
        final List<R> r = new ArrayList<R>();
        final Pair<L, R> p = new Pair<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), p);

                l.add(p.left);
                r.add(p.right);
            }
        }

        return Pair.of(l, r);
    }

    /**
     * 
     * @param supplier
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, R, LC extends Collection<L>, RC extends Collection<R>> Pair<LC, RC> unzip(final Supplier<? extends Collection<?>> supplier,
            final Iterator<? extends T> iter, final BiConsumer<? super T, Pair<L, R>> unzip) {
        final LC l = (LC) supplier.get();
        final RC r = (RC) supplier.get();

        final Pair<L, R> p = new Pair<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), p);

                l.add(p.left);
                r.add(p.right);
            }
        }

        return Pair.of(l, r);
    }

    /**
     * 
     * @param c
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, M, R> Triple<List<L>, List<M>, List<R>> unzip3(final Collection<? extends T> c, final BiConsumer<? super T, Triple<L, M, R>> unzip) {
        final int len = c == null ? 0 : c.size();

        final List<L> l = new ArrayList<L>(len);
        final List<M> m = new ArrayList<M>(len);
        final List<R> r = new ArrayList<R>(len);
        final Triple<L, M, R> t = new Triple<>();

        if (N.notNullOrEmpty(c)) {
            for (T e : c) {
                unzip.accept(e, t);

                l.add(t.left);
                m.add(t.middle);
                r.add(t.right);
            }
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
    public static <T, L, M, R, LC extends Collection<L>, MC extends Collection<M>, RC extends Collection<R>> Triple<LC, MC, RC> unzip3(
            final IntFunction<? extends Collection<?>> supplier, final Collection<? extends T> c, final BiConsumer<? super T, Triple<L, M, R>> unzip) {
        final int len = c == null ? 0 : c.size();

        final LC l = (LC) supplier.apply(len);
        final MC m = (MC) supplier.apply(len);
        final RC r = (RC) supplier.apply(len);
        final Triple<L, M, R> t = new Triple<>();

        if (N.notNullOrEmpty(c)) {
            for (T e : c) {
                unzip.accept(e, t);

                l.add(t.left);
                m.add(t.middle);
                r.add(t.right);
            }
        }

        return Triple.of(l, m, r);
    }

    /**
     * 
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, M, R> Triple<List<L>, List<M>, List<R>> unzip3(final Iterator<? extends T> iter, final BiConsumer<? super T, Triple<L, M, R>> unzip) {
        final List<L> l = new ArrayList<L>();
        final List<M> m = new ArrayList<M>();
        final List<R> r = new ArrayList<R>();
        final Triple<L, M, R> t = new Triple<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), t);

                l.add(t.left);
                m.add(t.middle);
                r.add(t.right);
            }
        }

        return Triple.of(l, m, r);
    }

    /**
     * 
     * @param supplier
     * @param iter
     * @param unzip the second parameter is an output parameter.
     * @return
     */
    public static <T, L, M, R, LC extends Collection<L>, MC extends Collection<M>, RC extends Collection<R>> Triple<LC, MC, RC> unzip3(
            final Supplier<? extends Collection<?>> supplier, final Iterator<? extends T> iter, final BiConsumer<? super T, Triple<L, M, R>> unzip) {
        final LC l = (LC) supplier.get();
        final MC m = (MC) supplier.get();
        final RC r = (RC) supplier.get();
        final Triple<L, M, R> t = new Triple<>();

        if (iter != null) {
            while (iter.hasNext()) {
                unzip.accept(iter.next(), t);

                l.add(t.left);
                m.add(t.middle);
                r.add(t.right);
            }
        }

        return Triple.of(l, m, r);
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
    public static <E> Collection<List<E>> permutations(Collection<E> elements) {
        return new PermutationCollection<E>(elements);
    }

    private static final class PermutationCollection<E> extends AbstractCollection<List<E>> {
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
        public Iterator<List<E>> iterator() {
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
    public static <E extends Comparable<? super E>> Collection<List<E>> orderedPermutations(Collection<E> elements) {
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
    public static <E> Collection<List<E>> orderedPermutations(Collection<E> elements, Comparator<? super E> comparator) {
        return new OrderedPermutationCollection<E>(elements, comparator);
    }

    private static final class OrderedPermutationCollection<E> extends AbstractCollection<List<E>> {
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
        public Iterator<List<E>> iterator() {
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
    public static <E> List<List<E>> cartesianProduct(final Collection<? extends E>... cs) {
        if (N.isNullOrEmpty(cs)) {
            return new ArrayList<>();
        }

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
    public static <E> List<List<E>> cartesianProduct(final Collection<? extends Collection<? extends E>> cs) {
        return new CartesianList<>(cs);
    }

    private static final class CartesianList<E> extends AbstractList<List<E>> implements RandomAccess {
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
        public List<E> get(final int index) {
            N.checkArgument(index < size(), "Invalid index %s. It must be less than the size %s", index, size());

            final List<E> result = new ArrayList<>(axes.length);

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
