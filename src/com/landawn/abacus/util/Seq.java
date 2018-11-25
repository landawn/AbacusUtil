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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.RandomAccess;
import java.util.Set;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.exception.NonUniqueResultException;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;

/**
 * It's an read-only wrapper for <code>Collection</code> to support more daily used/functional methods.
 * All the operations are null safety. And an empty <code>String</code>/<code>Array</code>/<code>Collection</code>/<code>Optional</code>/<code>Nullable</code> will be returned if possible, instead of null.
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
    private static final Seq EMPTY = new Seq<>(Collections.EMPTY_LIST);

    /**
     * The returned <code>Seq</code> and the specified <code>Collection</code> are backed by the same data.
     * Any changes to one will appear in the other.
     * 
     * @param c
     */
    Seq(final Collection<T> c) {
        super(c);
    }

    public static <T> Seq<T> empty() {
        return EMPTY;
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
     */
    public static <T> Seq<T> of(Collection<T> c) {
        if (N.isNullOrEmpty(c)) {
            return EMPTY;
        }

        return new Seq<>(c);
    }

    /**
     * 
     * @param map
     * @return
     */
    public static <K, V> Seq<Map.Entry<K, V>> of(Map<K, V> map) {
        if (N.isNullOrEmpty(map)) {
            return EMPTY;
        }

        return of(map.entrySet());
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
        return N.disjoint(this.coll, c);
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
        return N.intersection(coll, b);
    }

    public List<T> intersection(final Object[] a) {
        return N.intersection(coll, Array.asList(a));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public List<T> difference(Collection<?> b) {
        return N.difference(coll, b);
    }

    public List<T> difference(final Object[] a) {
        return N.difference(coll, Array.asList(a));
    }

    /**
     * 
     * @param b
     * @return this.difference(b).addAll(b.difference(this))
     * @see IntList#symmetricDifference(IntList)
     */
    public List<T> symmetricDifference(Collection<? extends T> b) {
        return N.symmetricDifference(coll, b);
    }

    public List<T> symmetricDifference(final T[] a) {
        return N.symmetricDifference(coll, Array.asList(a));
    }

    public int occurrencesOf(final Object objectToFind) {
        return N.isNullOrEmpty(coll) ? 0 : N.occurrencesOf(coll, objectToFind);
    }

    @SuppressWarnings("rawtypes")
    public Nullable<T> min() {
        return size() == 0 ? (Nullable<T>) Nullable.empty() : Nullable.of((T) N.min((Collection) coll));
    }

    public Nullable<T> min(Comparator<? super T> cmp) {
        return size() == 0 ? (Nullable<T>) Nullable.empty() : Nullable.of(N.min(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public Nullable<T> minBy(final Function<? super T, ? extends Comparable> keyExtractor) {
        return min(Fn.comparingBy(keyExtractor));
    }

    @SuppressWarnings("rawtypes")
    public Nullable<T> max() {
        return size() == 0 ? (Nullable<T>) Nullable.empty() : Nullable.of((T) N.max((Collection) coll));
    }

    public Nullable<T> max(Comparator<? super T> cmp) {
        return size() == 0 ? (Nullable<T>) Nullable.empty() : Nullable.of(N.max(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public Nullable<T> maxBy(final Function<? super T, ? extends Comparable> keyExtractor) {
        return max(Fn.comparingBy(keyExtractor));
    }

    @SuppressWarnings("rawtypes")
    public Nullable<T> median() {
        return size() == 0 ? (Nullable<T>) Nullable.empty() : Nullable.of((T) N.median((Collection) coll));
    }

    public Nullable<T> median(Comparator<? super T> cmp) {
        return size() == 0 ? (Nullable<T>) Nullable.empty() : Nullable.of(N.median(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public Nullable<T> kthLargest(final int k) {
        N.checkArgPositive(k, "k");

        return size() < k ? (Nullable<T>) Nullable.empty() : Nullable.of((T) N.kthLargest((Collection) coll, k));
    }

    public Nullable<T> kthLargest(final int k, Comparator<? super T> cmp) {
        N.checkArgPositive(k, "k");

        return size() < k ? (Nullable<T>) Nullable.empty() : Nullable.of(N.kthLargest(coll, k, cmp));
    }

    /**
     * 
     * @return
     * @deprecated replaced by {@code sumInt(com.landawn.abacus.util.Try.ToIntFunction)}.
     */
    @Deprecated
    public int sumInt() {
        if (N.isNullOrEmpty(coll)) {
            return 0;
        }

        return sumInt((ToIntFunction<T>) Fn.numToInt());
    }

    public <E extends Exception> int sumInt(final Try.ToIntFunction<? super T, E> mapper) throws E {
        if (N.isNullOrEmpty(coll)) {
            return 0;
        }

        return N.sumInt(coll, mapper);
    }

    /**
     * 
     * @return
     * @deprecated replaced by {@code sumLong(com.landawn.abacus.util.Try.ToLongFunction)}.
     */
    @Deprecated
    public long sumLong() {
        if (N.isNullOrEmpty(coll)) {
            return 0L;
        }

        return sumLong((ToLongFunction<T>) Fn.numToLong());
    }

    public <E extends Exception> long sumLong(final Try.ToLongFunction<? super T, E> mapper) throws E {
        if (N.isNullOrEmpty(coll)) {
            return 0L;
        }

        return N.sumLong(coll, mapper);
    }

    /**
     * 
     * @return
     * @deprecated replaced by {@code sumDouble(com.landawn.abacus.util.Try.ToDoubleFunction)}.
     */
    @Deprecated
    public double sumDouble() {
        if (N.isNullOrEmpty(coll)) {
            return 0D;
        }

        return sumDouble((ToDoubleFunction<T>) Fn.numToDouble());
    }

    public <E extends Exception> double sumDouble(final Try.ToDoubleFunction<? super T, E> mapper) throws E {
        if (N.isNullOrEmpty(coll)) {
            return 0L;
        }

        return N.sumDouble(coll, mapper);
    }

    /**
     * 
     * @return
     * @deprecated replaced by {@code averageInt(com.landawn.abacus.util.Try.ToIntFunction)}
     */
    @Deprecated
    public OptionalDouble averageInt() {
        if (N.isNullOrEmpty(coll)) {
            return OptionalDouble.empty();
        }

        return averageInt((ToIntFunction<T>) Fn.numToInt());
    }

    public <E extends Exception> OptionalDouble averageInt(final Try.ToIntFunction<? super T, E> mapper) throws E {
        return N.averageInt(coll, mapper);
    }

    /**
     * 
     * @return
     * @deprecated replaced by {@code averageLong(com.landawn.abacus.util.Try.ToLongFunction)}
     */
    @Deprecated
    public OptionalDouble averageLong() {
        if (N.isNullOrEmpty(coll)) {
            return OptionalDouble.empty();
        }

        return averageLong((ToLongFunction<T>) Fn.numToLong());
    }

    public <E extends Exception> OptionalDouble averageLong(final Try.ToLongFunction<? super T, E> mapper) throws E {
        return N.averageLong(coll, mapper);
    }

    /**
     * 
     * @return
     * @deprecated replaced by {@code averageDouble(com.landawn.abacus.util.Try.ToDoubleFunction)}
     */
    @Deprecated
    public OptionalDouble averageDouble() {
        if (N.isNullOrEmpty(coll)) {
            return OptionalDouble.empty();
        }

        return averageDouble((ToDoubleFunction<T>) Fn.numToDouble());
    }

    public <E extends Exception> OptionalDouble averageDouble(final Try.ToDoubleFunction<? super T, E> mapper) throws E {
        return N.averageDouble(coll, mapper);
    }

    public <E extends Exception> void foreach(final Try.Consumer<? super T, E> action) throws E {
        N.forEach(coll, action);
    }

    //    public <E extends Exception> void forEach(int fromIndex, final int toIndex, final Consumer<? super T> action) throws E {
    //        N.forEach(coll, fromIndex, toIndex, action);
    //    }

    public <E extends Exception> void forEach(final Try.IndexedConsumer<? super T, E> action) throws E {
        N.forEach(coll, action);
    }

    public <U, E extends Exception, E2 extends Exception> void forEach(final Try.Function<? super T, ? extends Collection<U>, E> flatMapper,
            final Try.BiConsumer<? super T, ? super U, E2> action) throws E, E2 {
        N.forEach(coll, flatMapper, action);
    }

    public <T2, T3, E extends Exception, E2 extends Exception, E3 extends Exception> void forEach(
            final Try.Function<? super T, ? extends Collection<T2>, E> flatMapper, final Try.Function<? super T2, ? extends Collection<T3>, E2> flatMapper2,
            final Try.TriConsumer<? super T, ? super T2, ? super T3, E3> action) throws E, E2, E3 {
        N.forEach(coll, flatMapper, flatMapper2, action);
    }

    public <E extends Exception> void forEachNonNull(final Try.Consumer<? super T, E> action) throws E {
        N.forEachNonNull(coll, action);
    }

    public <U, E extends Exception, E2 extends Exception> void forEachNonNull(final Try.Function<? super T, ? extends Collection<U>, E> flatMapper,
            final Try.BiConsumer<? super T, ? super U, E2> action) throws E, E2 {
        N.forEachNonNull(coll, flatMapper, action);
    }

    public <T2, T3, E extends Exception, E2 extends Exception, E3 extends Exception> void forEachNonNull(
            final Try.Function<? super T, ? extends Collection<T2>, E> flatMapper, final Try.Function<? super T2, ? extends Collection<T3>, E2> flatMapper2,
            final Try.TriConsumer<? super T, ? super T2, ? super T3, E3> action) throws E, E2, E3 {
        N.forEachNonNull(coll, flatMapper, flatMapper2, action);
    }

    public <E extends Exception> void forEachPair(final Try.BiConsumer<? super T, ? super T, E> action) throws E {
        forEachPair(action, 1);
    }

    public <E extends Exception> void forEachPair(final Try.BiConsumer<? super T, ? super T, E> action, final int increment) throws E {
        N.checkArgNotNull(action);
        final int windowSize = 2;
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        if (N.isNullOrEmpty(coll)) {
            return;
        }

        final Iterator<T> iter = coll.iterator();
        Iterators.forEachPair(iter, action, increment);
    }

    public <E extends Exception> void forEachTriple(final Try.TriConsumer<? super T, ? super T, ? super T, E> action) throws E {
        forEachTriple(action, 1);
    }

    public <E extends Exception> void forEachTriple(final Try.TriConsumer<? super T, ? super T, ? super T, E> action, final int increment) throws E {
        N.checkArgNotNull(action);
        final int windowSize = 3;
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        if (N.isNullOrEmpty(coll)) {
            return;
        }

        final Iterator<T> iter = coll.iterator();
        Iterators.forEachTriple(iter, action, increment);
    }

    public Nullable<T> first() {
        return N.first(coll);
    }

    public Optional<T> firstNonNull() {
        return N.firstNonNull(coll);
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

    public Nullable<T> last() {
        return N.last(coll);
    }

    public Optional<T> lastNonNull() {
        return N.lastNonNull(coll);
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

    public <E extends Exception> Nullable<T> findFirst(Try.Predicate<? super T, E> predicate) throws E {
        if (size() == 0) {
            return Nullable.empty();
        }

        for (T e : coll) {
            if (predicate.test(e)) {
                return Nullable.of(e);
            }
        }

        return Nullable.empty();
    }

    public <E extends Exception> Nullable<T> findLast(Try.Predicate<? super T, E> predicate) throws E {
        if (size() == 0) {
            return Nullable.empty();
        }

        if (coll instanceof List) {
            final List<T> list = (List<T>) coll;

            if (coll instanceof RandomAccess) {
                for (int i = size() - 1; i >= 0; i--) {
                    if (predicate.test(list.get(i))) {
                        return Nullable.of(list.get(i));
                    }
                }
            } else {
                final ListIterator<T> iter = list.listIterator(list.size());
                T pre = null;

                while (iter.hasPrevious()) {
                    if (predicate.test((pre = iter.previous()))) {
                        return Nullable.of(pre);
                    }
                }
            }

            return Nullable.empty();
        } else if (coll instanceof Deque) {
            final Iterator<T> iter = ((Deque<T>) coll).descendingIterator();
            T next = null;

            while (iter.hasNext()) {
                if (predicate.test((next = iter.next()))) {
                    return Nullable.of(next);
                }
            }

            return Nullable.empty();
        } else {
            final T[] a = (T[]) coll.toArray();

            for (int i = a.length - 1; i >= 0; i--) {
                if (predicate.test(a[i])) {
                    return Nullable.of(a[i]);
                }
            }

            return Nullable.empty();
        }
    }

    public <E extends Exception> OptionalInt findFirstIndex(Try.Predicate<? super T, E> predicate) throws E {
        return Iterables.findFirstIndex(coll, predicate);
    }

    public <E extends Exception> OptionalInt findLastIndex(Try.Predicate<? super T, E> predicate) throws E {
        return Iterables.findLastIndex(coll, predicate);
    }

    public <E extends Exception, E2 extends Exception> Nullable<T> findFirstOrLast(final Try.Predicate<? super T, E> predicateForFirst,
            final Try.Predicate<? super T, E2> predicateForLast) throws E, E2 {
        if (N.isNullOrEmpty(coll)) {
            return Nullable.<T> empty();
        }

        final Nullable<T> res = findFirst(predicateForFirst);

        return res.isPresent() ? res : findLast(predicateForLast);
    }

    public <E extends Exception, E2 extends Exception> OptionalInt findFirstOrLastIndex(final Try.Predicate<? super T, E> predicateForFirst,
            final Try.Predicate<? super T, E2> predicateForLast) throws E, E2 {
        if (N.isNullOrEmpty(coll)) {
            return OptionalInt.empty();
        }

        final OptionalInt res = findFirstIndex(predicateForFirst);

        return res.isPresent() ? res : findLastIndex(predicateForLast);
    }

    public <E extends Exception> Pair<Nullable<T>, Nullable<T>> findFirstAndLast(final Try.Predicate<? super T, E> predicate) throws E {
        return findFirstAndLast(predicate, predicate);
    }

    public <E extends Exception, E2 extends Exception> Pair<Nullable<T>, Nullable<T>> findFirstAndLast(final Try.Predicate<? super T, E> predicateForFirst,
            final Try.Predicate<? super T, E2> predicateForLast) throws E, E2 {
        if (N.isNullOrEmpty(coll)) {
            return Pair.of(Nullable.<T> empty(), Nullable.<T> empty());
        }

        return Pair.of(findFirst(predicateForFirst), findLast(predicateForLast));
    }

    public <E extends Exception> Pair<OptionalInt, OptionalInt> findFirstAndLastIndex(final Try.Predicate<? super T, E> predicate) throws E {
        return findFirstAndLastIndex(predicate, predicate);
    }

    public <E extends Exception, E2 extends Exception> Pair<OptionalInt, OptionalInt> findFirstAndLastIndex(final Try.Predicate<? super T, E> predicateForFirst,
            final Try.Predicate<? super T, E2> predicateForLast) throws E, E2 {
        if (N.isNullOrEmpty(coll)) {
            return Pair.of(OptionalInt.empty(), OptionalInt.empty());
        }

        return Pair.of(findFirstIndex(predicateForFirst), findLastIndex(predicateForLast));
    }

    public <E extends Exception> boolean allMatch(Try.Predicate<? super T, E> filter) throws E {
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

    public <E extends Exception> boolean anyMatch(Try.Predicate<? super T, E> filter) throws E {
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

    public <E extends Exception> boolean noneMatch(Try.Predicate<? super T, E> filter) throws E {
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

    public <E extends Exception> int count(Try.Predicate<? super T, E> filter) throws E {
        return N.count(coll, filter);
    }

    public <E extends Exception> List<T> filter(Try.Predicate<? super T, E> filter) throws E {
        return N.filter(coll, filter);
    }

    public <E extends Exception> List<T> filter(Try.Predicate<? super T, E> filter, final int max) throws E {
        return N.filter(coll, filter, max);
    }

    public <C extends Collection<T>, E extends Exception> C filter(Try.Predicate<? super T, E> filter, IntFunction<C> supplier) throws E {
        return N.filter(coll, filter, supplier);
    }

    public <C extends Collection<T>, E extends Exception> C filter(Try.Predicate<? super T, E> filter, final int max, IntFunction<C> supplier) throws E {
        return N.filter(coll, filter, max, supplier);
    }

    public <U, E extends Exception> List<T> filter(final U seed, final Try.BiPredicate<? super T, ? super U, E> filter) throws E {
        N.checkArgNotNull(filter);

        return filter(Fn.pp(seed, filter));
    }

    public <E extends Exception> List<T> takeWhile(Try.Predicate<? super T, E> filter) throws E {
        N.checkArgNotNull(filter);

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

    public <U, E extends Exception> List<T> takeWhile(final U seed, final Try.BiPredicate<? super T, ? super U, E> filter) throws E {
        N.checkArgNotNull(filter);

        return takeWhile(Fn.pp(seed, filter));
    }

    public <E extends Exception> List<T> takeWhileInclusive(Try.Predicate<? super T, E> filter) throws E {
        N.checkArgNotNull(filter);

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

    public <U, E extends Exception> List<T> takeWhileInclusive(final U seed, final Try.BiPredicate<? super T, ? super U, E> filter) throws E {
        N.checkArgNotNull(filter);

        return takeWhileInclusive(Fn.pp(seed, filter));
    }

    public <E extends Exception> List<T> dropWhile(Try.Predicate<? super T, E> filter) throws E {
        N.checkArgNotNull(filter);

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

    public <U, E extends Exception> List<T> dropWhile(final U seed, final Try.BiPredicate<? super T, ? super U, E> filter) throws E {
        N.checkArgNotNull(filter);

        return dropWhile(Fn.pp(seed, filter));
    }

    public <E extends Exception> List<T> skipUntil(final Try.Predicate<? super T, E> filter) throws E {
        N.checkArgNotNull(filter);

        return dropWhile(new Try.Predicate<T, E>() {
            @Override
            public boolean test(T value) throws E {
                return !filter.test(value);
            }
        });
    }

    public <U, E extends Exception> List<T> skipUntil(final U seed, final Try.BiPredicate<? super T, ? super U, E> filter) throws E {
        N.checkArgNotNull(filter);

        return dropWhile(new Try.Predicate<T, E>() {
            @Override
            public boolean test(T value) throws E {
                return !filter.test(value, seed);
            }
        });
    }

    public <R, E extends Exception> List<R> map(final Try.Function<? super T, ? extends R, E> func) throws E {
        return N.map(coll, func);
    }

    public <E extends Exception> BooleanList mapToBoolean(final Try.ToBooleanFunction<? super T, E> func) throws E {
        return N.mapToBoolean(coll, func);
    }

    public <E extends Exception> CharList mapToChar(final Try.ToCharFunction<? super T, E> func) throws E {
        return N.mapToChar(coll, func);
    }

    public <E extends Exception> ByteList mapToByte(final Try.ToByteFunction<? super T, E> func) throws E {
        return N.mapToByte(coll, func);
    }

    public <E extends Exception> ShortList mapToShort(final Try.ToShortFunction<? super T, E> func) throws E {
        return N.mapToShort(coll, func);
    }

    public <E extends Exception> IntList mapToInt(final Try.ToIntFunction<? super T, E> func) throws E {
        return N.mapToInt(coll, func);
    }

    public <E extends Exception> LongList mapToLong(final Try.ToLongFunction<? super T, E> func) throws E {
        return N.mapToLong(coll, func);
    }

    public <E extends Exception> FloatList mapToFloat(final Try.ToFloatFunction<? super T, E> func) throws E {
        return N.mapToFloat(coll, func);
    }

    public <E extends Exception> DoubleList mapToDouble(final Try.ToDoubleFunction<? super T, E> func) throws E {
        return N.mapToDouble(coll, func);
    }

    public <R, E extends Exception> List<R> flatMap(final Try.Function<? super T, ? extends Collection<? extends R>, E> func) throws E {
        N.checkArgNotNull(func);

        final List<R> result = new ArrayList<>(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <R, E extends Exception> List<R> flattMap(final Try.Function<? super T, ? extends R[], E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> BooleanList flatMapToBoolean(final Try.Function<? super T, ? extends Collection<Boolean>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> BooleanList flattMapToBoolean(final Try.Function<? super T, boolean[], E> func) throws E {
        N.checkArgNotNull(func);

        final BooleanList result = new BooleanList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <E extends Exception> CharList flatMapToChar(final Try.Function<? super T, ? extends Collection<Character>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> CharList flattMapToChar(final Try.Function<? super T, char[], E> func) throws E {
        N.checkArgNotNull(func);

        final CharList result = new CharList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <E extends Exception> ByteList flatMapToByte(final Try.Function<? super T, ? extends Collection<Byte>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> ByteList flattMapToByte(final Try.Function<? super T, byte[], E> func) throws E {
        N.checkArgNotNull(func);

        final ByteList result = new ByteList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <E extends Exception> ShortList flatMapToShort(final Try.Function<? super T, ? extends Collection<Short>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> ShortList flattMapToShort(final Try.Function<? super T, short[], E> func) throws E {
        N.checkArgNotNull(func);

        final ShortList result = new ShortList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <E extends Exception> IntList flatMapToInt(final Try.Function<? super T, ? extends Collection<Integer>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> IntList flattMapToInt(final Try.Function<? super T, int[], E> func) throws E {
        N.checkArgNotNull(func);

        final IntList result = new IntList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <E extends Exception> LongList flatMapToLong(final Try.Function<? super T, ? extends Collection<Long>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> LongList flattMapToLong(final Try.Function<? super T, long[], E> func) throws E {
        N.checkArgNotNull(func);

        final LongList result = new LongList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <E extends Exception> FloatList flatMapToFloat(final Try.Function<? super T, ? extends Collection<Float>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> FloatList flattMapToFloat(final Try.Function<? super T, float[], E> func) throws E {
        N.checkArgNotNull(func);

        final FloatList result = new FloatList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <E extends Exception> DoubleList flatMapToDouble(final Try.Function<? super T, ? extends Collection<Double>, E> func) throws E {
        N.checkArgNotNull(func);

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

    public <E extends Exception> DoubleList flattMapToDouble(final Try.Function<? super T, double[], E> func) throws E {
        N.checkArgNotNull(func);

        final DoubleList result = new DoubleList(size() > N.MAX_ARRAY_SIZE / 2 ? N.MAX_ARRAY_SIZE : size() * 2);

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <U, R, E extends Exception, E2 extends Exception> List<R> flatMap(final Try.Function<? super T, ? extends Collection<U>, E> mapper,
            final Try.BiFunction<? super T, ? super U, ? extends R, E2> func) throws E, E2 {
        N.checkArgNotNull(mapper);
        N.checkArgNotNull(func);

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

    public <T2, T3, R, E extends Exception, E2 extends Exception, E3 extends Exception> List<R> flatMap(
            final Try.Function<? super T, ? extends Collection<T2>, E> mapper2, final Try.Function<? super T2, ? extends Collection<T3>, E2> mapper3,
            final Try.TriFunction<? super T, ? super T2, ? super T3, R, E3> func) throws E, E2, E3 {
        N.checkArgNotNull(mapper2);
        N.checkArgNotNull(mapper3);
        N.checkArgNotNull(func);

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
     * For better performance, comparing to {@code Stream}.
     * 
     * @param filter
     * @param mapper
     * @return
     * @throws E
     * @throws E2
     */
    @Beta
    public <R, E extends Exception, E2 extends Exception> List<R> filterThenMap(final Try.Predicate<? super T, E> filter,
            final Try.Function<? super T, ? extends R, E2> mapper) throws E, E2 {
        N.checkArgNotNull(filter);
        N.checkArgNotNull(mapper);

        final List<R> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            if (filter.test(e)) {
                result.add(mapper.apply(e));
            }
        }

        return result;
    }

    /**
     * For better performance, comparing to {@code Stream}.
     * 
     * @param filter
     * @param mapper
     * @return
     * @throws E
     * @throws E2
     */
    @Beta
    public <R, E extends Exception, E2 extends Exception> List<R> filterThenFlatMap(final Try.Predicate<? super T, E> filter,
            final Try.Function<? super T, ? extends Collection<? extends R>, E2> mapper) throws E, E2 {
        N.checkArgNotNull(filter);
        N.checkArgNotNull(mapper);

        final List<R> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        Collection<? extends R> c = null;

        for (T e : coll) {
            if (filter.test(e)) {
                c = mapper.apply(e);

                if (N.notNullOrEmpty(c)) {
                    result.addAll(c);
                }
            }
        }

        return result;
    }

    /**
     * For better performance, comparing to {@code Stream}.
     * 
     * @param mapper
     * @param filter
     * @return
     * @throws E
     * @throws E2
     */
    @Beta
    public <R, E extends Exception, E2 extends Exception> List<R> mapThenFilter(final Try.Function<? super T, ? extends R, E> mapper,
            final Try.Predicate<? super R, E2> filter) throws E, E2 {
        N.checkArgNotNull(mapper);
        N.checkArgNotNull(filter);

        final List<R> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        R r = null;
        for (T e : coll) {
            r = mapper.apply(e);

            if (filter.test(r)) {
                result.add(r);
            }
        }

        return result;
    }

    /**
     * For better performance, comparing to {@code Stream}.
     * 
     * @param mapper
     * @param filter
     * @return
     * @throws E
     * @throws E2
     */
    @Beta
    public <R, E extends Exception, E2 extends Exception> List<R> flatMapThenFilter(final Try.Function<? super T, ? extends Collection<? extends R>, E> mapper,
            final Try.Predicate<? super R, E2> filter) throws E, E2 {
        N.checkArgNotNull(mapper);
        N.checkArgNotNull(filter);

        final List<R> result = new ArrayList<>();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        Collection<? extends R> c = null;

        for (T e : coll) {
            c = mapper.apply(e);

            if (N.notNullOrEmpty(c)) {
                for (R r : c) {
                    if (filter.test(r)) {
                        result.add(r);
                    }
                }
            }
        }

        return result;
    }

    /**
     * For better performance, comparing to {@code Stream}.
     * 
     * @param filter
     * @param action
     * @return
     * @throws E
     * @throws E2
     */
    @Beta
    public <R, E extends Exception, E2 extends Exception> void filterThenForEach(final Try.Predicate<? super T, E> filter,
            final Try.Consumer<? super T, E2> action) throws E, E2 {
        N.checkArgNotNull(filter);
        N.checkArgNotNull(action);

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                if (filter.test(e)) {
                    action.accept(e);
                }
            }
        }
    }

    /**
     * For better performance, comparing to {@code Stream}.
     * 
     * @param mapper
     * @param action
     * @return
     * @throws E
     * @throws E2
     */
    @Beta
    public <R, E extends Exception, E2 extends Exception> void mapThenForEach(final Try.Function<? super T, ? extends R, E> mapper,
            final Try.Consumer<? super R, E2> action) throws E, E2 {
        N.checkArgNotNull(mapper);
        N.checkArgNotNull(action);

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                action.accept(mapper.apply(e));
            }
        }
    }

    /**
     * For better performance, comparing to {@code Stream}.
     * 
     * @param mapper
     * @param action
     * @return
     * @throws E
     * @throws E2
     */
    @Beta
    public <R, E extends Exception, E2 extends Exception> void flatMapThenForEach(final Try.Function<? super T, ? extends Collection<? extends R>, E> mapper,
            final Try.Consumer<? super R, E2> action) throws E, E2 {
        N.checkArgNotNull(mapper);
        N.checkArgNotNull(action);

        Collection<? extends R> c = null;

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                c = mapper.apply(e);

                if (N.notNullOrEmpty(c)) {
                    for (R r : c) {
                        action.accept(r);
                    }
                }
            }
        }
    }

    /**
     * Merge series of adjacent elements which satisfy the given predicate using the merger function.
     * 
     * <p>Example:
     * <pre>
     * <code>
     * Seq.of(new Integer[0]).collapse((a, b) -> a < b, (a, b) -> a + b) => []
     * Seq.of(1).collapse((a, b) -> a < b, (a, b) -> a + b) => [1]
     * Seq.of(1, 2).collapse((a, b) -> a < b, (a, b) -> a + b) => [3]
     * Seq.of(1, 2, 3).collapse((a, b) -> a < b, (a, b) -> a + b) => [6]
     * Seq.of(1, 2, 3, 3, 2, 1).collapse((a, b) -> a < b, (a, b) -> a + b) => [6, 3, 2, 1]
     * </code>
     * </pre>
     * 
     * @param collapsible
     * @param mergeFunction
     * @return
     */
    public <E extends Exception, E2 extends Exception> List<T> collapse(final Try.BiPredicate<? super T, ? super T, E> collapsible,
            final Try.BiFunction<? super T, ? super T, T, E2> mergeFunction) throws E, E2 {
        N.checkArgNotNull(collapsible);
        N.checkArgNotNull(mergeFunction);

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
     * Merge series of adjacent elements which satisfy the given predicate using the merger function.
     * 
     * <p>Example:
     * <pre>
     * <code>
     * Seq.of(new Integer[0]).collapse((a, b) -> a < b, Collectors.summingInt(Fn.unboxI())) => []
     * Seq.of(1).collapse((a, b) -> a < b, Collectors.summingInt(Fn.unboxI())) => [1]
     * Seq.of(1, 2).collapse((a, b) -> a < b, Collectors.summingInt(Fn.unboxI())) => [3]
     * Seq.of(1, 2, 3).collapse((a, b) -> a < b, Collectors.summingInt(Fn.unboxI())) => [6]
     * Seq.of(1, 2, 3, 3, 2, 1).collapse((a, b) -> a < b, Collectors.summingInt(Fn.unboxI())) => [6, 3, 2, 1]
     * </code>
     * </pre>
     * 
     * @param collapsible
     * @param collector
     * @return
     */
    public <R, A, E extends Exception> List<R> collapse(final Try.BiPredicate<? super T, ? super T, E> collapsible, final Collector<? super T, A, R> collector)
            throws E {
        N.checkArgNotNull(collapsible);
        N.checkArgNotNull(collector);

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
     * <p>Example:
     * <pre>
     * <code>
     * Seq.of(new Integer[0]).scan((a, b) -> a + b) => []
     * Seq.of(1).scan((a, b) -> a + b) => [1]
     * Seq.of(1, 2).scan((a, b) -> a + b) => [1, 3]
     * Seq.of(1, 2, 3).scan((a, b) -> a + b) => [1, 3, 6]
     * Seq.of(1, 2, 3, 3, 2, 1).scan((a, b) -> a + b) => [1, 3, 6, 9, 11, 12]
     * </code>
     * </pre>
     *
     * @param accumulator the accumulation function
     * @return
     */
    public <E extends Exception> List<T> scan(final Try.BiFunction<? super T, ? super T, T, E> accumulator) throws E {
        N.checkArgNotNull(accumulator);

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
     * <p>Example:
     * <pre>
     * <code>
     * Seq.of(new Integer[0]).scan(10, (a, b) -> a + b) => []
     * Seq.of(1).scan(10, (a, b) -> a + b) => [11]
     * Seq.of(1, 2).scan(10, (a, b) -> a + b) => [11, 13]
     * Seq.of(1, 2, 3).scan(10, (a, b) -> a + b) => [11, 13, 16]
     * Seq.of(1, 2, 3, 3, 2, 1).scan(10, (a, b) -> a + b) => [11, 13, 16, 19, 21, 22]
     * </code>
     * </pre>
     *
     * @param seed the initial value. it's only used once by <code>accumulator</code> to calculate the fist element in the returned stream. 
     * It will be ignored if this stream is empty and won't be the first element of the returned stream.
     * 
     * @param accumulator the accumulation function
     * @return
     */
    public <R, E extends Exception> List<R> scan(final R seed, final Try.BiFunction<? super R, ? super T, R, E> accumulator) throws E {
        N.checkArgNotNull(accumulator);

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
     *        return Nullable.empty();
     *    }
     *
     *    final Iterator<T> iter = iterator();
     *    T result = iter.next();
     *
     *    while (iter.hasNext()) {
     *        result = accumulator.apply(result, iter.next());
     *    }
     *
     *    return Nullable.of(result);
     * </code>
     * </pre>
     * 
     * @param accumulator
     * @return
     */
    public <E extends Exception> Nullable<T> reduce(Try.BinaryOperator<T, E> accumulator) throws E {
        N.checkArgNotNull(accumulator);

        if (isEmpty()) {
            return Nullable.empty();
        }

        final Iterator<T> iter = iterator();
        T result = iter.next();

        while (iter.hasNext()) {
            result = accumulator.apply(result, iter.next());
        }

        return Nullable.of(result);
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
    public <U, E extends Exception> U reduce(U identity, Try.BiFunction<U, ? super T, U, E> accumulator) throws E {
        N.checkArgNotNull(accumulator);

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

    public <R, E extends Exception> R collect(final Supplier<R> supplier, final Try.BiConsumer<R, ? super T, E> accumulator) throws E {
        N.checkArgNotNull(supplier);
        N.checkArgNotNull(accumulator);

        final R result = supplier.get();

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                accumulator.accept(result, e);
            }
        }

        return result;
    }

    public <A, R, E extends Exception, E2 extends Exception> R collect(final Supplier<A> supplier, final Try.BiConsumer<A, ? super T, E> accumulator,
            final Try.Function<A, R, E2> finisher) throws E, E2 {
        N.checkArgNotNull(supplier);
        N.checkArgNotNull(accumulator);
        N.checkArgNotNull(finisher);

        final A result = supplier.get();

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                accumulator.accept(result, e);
            }
        }

        return finisher.apply(result);
    }

    public <A, R> R collect(final Collector<? super T, A, R> collector) {
        N.checkArgNotNull(collector);

        final BiConsumer<A, ? super T> accumulator = collector.accumulator();
        final A result = collector.supplier().get();

        if (N.notNullOrEmpty(coll)) {
            for (T e : coll) {
                accumulator.accept(result, e);
            }
        }

        return collector.finisher().apply(result);
    }

    public <A, R, RR, E extends Exception> RR collectThenApply(final Collector<T, A, R> downstream, final Try.Function<? super R, ? extends RR, E> mapper)
            throws E {
        return mapper.apply(collect(downstream));
    }

    public <A, R, E extends Exception> void collectThenAccept(final Collector<T, A, R> downstream, final Try.Consumer<? super R, E> consumer) throws E {
        consumer.accept(collect(downstream));
    }

    @SafeVarargs
    public final List<T> append(T... a) {
        if (N.isNullOrEmpty(a)) {
            return toList();
        }

        return append(Arrays.asList(a));
    }

    public List<T> append(final Collection<? extends T> c) {
        return N.concat(this, c);
    }

    @SafeVarargs
    public final List<T> prepend(T... a) {
        if (N.isNullOrEmpty(a)) {
            return toList();
        }

        return prepend(Arrays.asList(a));
    }

    public List<T> prepend(final Collection<? extends T> c) {
        return N.concat(c, this);
    }

    public <E extends Exception> List<T> merge(final Collection<? extends T> b, final Try.BiFunction<? super T, ? super T, Nth, E> nextSelector) throws E {
        return N.merge(this, b, nextSelector);
    }

    public <B, R, E extends Exception> List<R> zipWith(final Collection<B> b, final Try.BiFunction<? super T, ? super B, R, E> zipFunction) throws E {
        return N.zip(this, b, zipFunction);
    }

    public <B, R, E extends Exception> List<R> zipWith(final Collection<B> b, final T valueForNoneA, final B valueForNoneB,
            final Try.BiFunction<? super T, ? super B, R, E> zipFunction) throws E {
        return N.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public <B, C, R, E extends Exception> List<R> zipWith(final Collection<B> b, final Collection<C> c,
            final Try.TriFunction<? super T, ? super B, ? super C, R, E> zipFunction) throws E {
        return N.zip(this, b, c, zipFunction);
    }

    public <B, C, R, E extends Exception> List<R> zipWith(final Collection<B> b, final Collection<C> c, final T valueForNoneA, final B valueForNoneB,
            final C valueForNoneC, final Try.TriFunction<? super T, ? super B, ? super C, R, E> zipFunction) throws E {
        return N.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
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
    public <E extends Exception> List<T> distinctBy(final Try.Function<? super T, ?, E> keyExtractor) throws E {
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

    public <U, E extends Exception> List<List<T>> split(final Try.Predicate<? super T, E> predicate) throws E {
        N.checkArgNotNull(predicate);

        if (N.isNullOrEmpty(coll)) {
            return new ArrayList<>();
        }

        final Try.BiPredicate<T, Object, E> predicate2 = new Try.BiPredicate<T, Object, E>() {
            @Override
            public boolean test(T t, Object u) throws E {
                return predicate.test(t);
            }
        };

        return split(null, predicate2, Fn.doNothing());
    }

    public <U, E extends Exception, E2 extends Exception> List<List<T>> split(final U identity, final Try.BiPredicate<? super T, ? super U, E> predicate,
            final Try.Consumer<? super U, E2> identityUpdate) throws E, E2 {
        N.checkArgNotNull(predicate);

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
                    preCondition = predicate.test(next, identity);
                    next = elements.hasNext() ? elements.next() : (T) N.NULL_MASK;
                } else if (predicate.test(next, identity) == preCondition) {
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

    public <E extends Exception> Pair<List<T>, List<T>> splitBy(final Try.Predicate<? super T, E> predicate) throws E {
        N.checkArgNotNull(predicate);

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
        return StringUtil.join(coll, delimiter);
    }

    public String join(final String delimiter) {
        return StringUtil.join(coll, delimiter);
    }

    public <E extends Exception> String join(final Try.Function<? super T, String, E> toStringFunc, final String delimiter) throws E {
        if (N.isNullOrEmpty(coll)) {
            return N.EMPTY_STRING;
        }

        try (final Joiner joiner = Joiner.with(delimiter).reuseStringBuilder(true)) {
            for (T e : coll) {
                joiner.append(e);
            }

            return joiner.toString();
        }
    }

    /**
     * 
     * @return
     * @throws NonUniqueResultException if there are more than one element in this {@code Seq}.
     */
    public Nullable<T> onlyOne() throws NonUniqueResultException {
        if (isEmpty()) {
            return Nullable.empty();
        } else if (size() == 1) {
            return first();
        } else {
            throw new NonUniqueResultException(N.toString(coll));
        }
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

    public Set<T> toSet() {
        return coll == null ? new HashSet<T>() : new HashSet<T>(coll);
    }

    public <C extends Collection<T>> C toCollection(final IntFunction<? extends C> supplier) {
        final C result = supplier.apply(size());

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
        final Multiset<T> result = supplier.apply(size());

        if (N.notNullOrEmpty(coll)) {
            result.addAll(coll);
        }

        return result;
    }

    public <K, V, E extends Exception, E2 extends Exception> Map<K, V> toMap(Try.Function<? super T, ? extends K, E> keyExtractor,
            Try.Function<? super T, ? extends V, E2> valueMapper) throws E, E2 {
        final IntFunction<Map<K, V>> mapFactory = Fn.Factory.ofMap();

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    public <K, V, M extends Map<K, V>, E extends Exception, E2 extends Exception> M toMap(Try.Function<? super T, ? extends K, E> keyExtractor,
            Try.Function<? super T, ? extends V, E2> valueMapper, IntFunction<M> mapFactory) throws E, E2 {
        final BinaryOperator<V> mergeFunction = Fn.throwingMerger();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public <K, V, E extends Exception, E2 extends Exception, E3 extends Exception> Map<K, V> toMap(Try.Function<? super T, ? extends K, E> keyExtractor,
            Try.Function<? super T, ? extends V, E2> valueMapper, Try.BinaryOperator<V, E3> mergeFunction) throws E, E2, E3 {
        final IntFunction<Map<K, V>> mapFactory = Fn.Factory.ofMap();

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public <K, V, M extends Map<K, V>, E extends Exception, E2 extends Exception, E3 extends Exception> M toMap(
            Try.Function<? super T, ? extends K, E> keyExtractor, Try.Function<? super T, ? extends V, E2> valueMapper, Try.BinaryOperator<V, E3> mergeFunction,
            IntFunction<M> mapFactory) throws E, E2, E3 {
        final M result = mapFactory.apply(size());
        final Iterator<T> iter = iterator();
        T element = null;

        while (iter.hasNext()) {
            element = iter.next();
            N.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    public <K, A, D, E extends Exception> Map<K, D> toMap(Try.Function<? super T, ? extends K, E> classifier, Collector<? super T, A, D> downstream) throws E {
        final IntFunction<Map<K, D>> mapFactory = Fn.Factory.ofMap();

        return toMap(classifier, downstream, mapFactory);
    }

    public <K, A, D, M extends Map<K, D>, E extends Exception> M toMap(final Try.Function<? super T, ? extends K, E> classifier,
            final Collector<? super T, A, D> downstream, final IntFunction<M> mapFactory) throws E {
        final M result = mapFactory.apply(size());
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        final Iterator<T> iter = iterator();
        K key = null;
        A v = null;
        T element = null;

        while (iter.hasNext()) {
            element = iter.next();
            key = N.checkArgNotNull(classifier.apply(element), "element cannot be mapped to a null key");

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

        N.replaceAll(intermediate, function);

        return result;
    }

    public <K, U, A, D, E extends Exception, E2 extends Exception> Map<K, D> toMap(Try.Function<? super T, ? extends K, E> classifier,
            Try.Function<? super T, ? extends U, E2> valueMapper, Collector<? super U, A, D> downstream) throws E, E2 {
        final IntFunction<Map<K, D>> mapFactory = Fn.Factory.ofMap();

        return toMap(classifier, valueMapper, downstream, mapFactory);
    }

    public <K, U, A, D, M extends Map<K, D>, E extends Exception, E2 extends Exception> M toMap(final Try.Function<? super T, ? extends K, E> classifier,
            Try.Function<? super T, ? extends U, E2> valueMapper, final Collector<? super U, A, D> downstream, final IntFunction<M> mapFactory) throws E, E2 {
        final M result = mapFactory.apply(size());
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super U> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        final Iterator<T> iter = iterator();
        K key = null;
        A v = null;
        T element = null;

        while (iter.hasNext()) {
            element = iter.next();
            key = N.checkArgNotNull(classifier.apply(element), "element cannot be mapped to a null key");

            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, valueMapper.apply(element));
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        N.replaceAll(intermediate, function);

        return result;
    }

    public <K, E extends Exception> Map<K, List<T>> groupTo(Try.Function<? super T, ? extends K, E> classifier) throws E {
        final IntFunction<Map<K, List<T>>> mapFactory = Fn.Factory.ofMap();

        return groupTo(classifier, mapFactory);
    }

    public <K, M extends Map<K, List<T>>, E extends Exception> M groupTo(Try.Function<? super T, ? extends K, E> classifier, IntFunction<M> mapFactory)
            throws E {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    public <K, U, E extends Exception, E2 extends Exception> Map<K, List<U>> groupTo(Try.Function<? super T, ? extends K, E> keyExtractor,
            Try.Function<? super T, ? extends U, E2> valueMapper) throws E, E2 {
        final IntFunction<Map<K, List<U>>> mapFactory = Fn.Factory.ofMap();

        return groupTo(keyExtractor, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, List<U>>, E extends Exception, E2 extends Exception> M groupTo(Try.Function<? super T, ? extends K, E> keyExtractor,
            Try.Function<? super T, ? extends U, E2> valueMapper, IntFunction<M> mapFactory) throws E, E2 {
        final M result = mapFactory.apply(size());
        final Iterator<T> iter = iterator();
        T element = null;
        K key = null;
        List<U> values = null;

        while (iter.hasNext()) {
            element = iter.next();
            key = keyExtractor.apply(element);
            values = result.get(key);

            if (values == null) {
                values = new ArrayList<U>();
                result.put(key, values);
            }

            values.add(valueMapper.apply(element));
        }

        return result;
    }

    /**
     * 
     * @param keyExtractor
     * @return
     */
    public <K, E extends Exception> ListMultimap<K, T> toMultimap(Try.Function<? super T, ? extends K, E> keyExtractor) throws E {
        final ListMultimap<K, T> result = N.newListMultimap();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.put(keyExtractor.apply(e), e);
        }

        return result;
    }

    public <K, V extends Collection<T>, M extends Multimap<K, T, V>, E extends Exception> M toMultimap(Try.Function<? super T, ? extends K, E> keyExtractor,
            IntFunction<M> mapFactory) throws E {
        final M result = mapFactory.apply(size());

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
    public <K, U, E extends Exception, E2 extends Exception> ListMultimap<K, U> toMultimap(Try.Function<? super T, ? extends K, E> keyExtractor,
            Try.Function<? super T, ? extends U, E2> valueMapper) throws E, E2 {
        final ListMultimap<K, U> result = N.newListMultimap();

        if (N.isNullOrEmpty(coll)) {
            return result;
        }

        for (T e : coll) {
            result.put(keyExtractor.apply(e), valueMapper.apply(e));
        }

        return result;
    }

    public <K, U, V extends Collection<U>, M extends Multimap<K, U, V>, E extends Exception, E2 extends Exception> M toMultimap(
            Try.Function<? super T, ? extends K, E> keyExtractor, Try.Function<? super T, ? extends U, E2> valueMapper, IntFunction<M> mapFactory)
            throws E, E2 {
        final M result = mapFactory.apply(size());

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
    public <U, E extends Exception, E2 extends Exception> List<Pair<T, U>> innerJoin(final Collection<U> b, final Try.Function<? super T, ?, E> leftKeyMapper,
            final Try.Function<? super U, ?, E2> rightKeyMapper) throws E, E2 {
        final List<Pair<T, U>> result = new ArrayList<>(N.min(9, size(), N.len(b)));

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
    public <U, E extends Exception> List<Pair<T, U>> innerJoin(final Collection<U> b, final Try.BiPredicate<? super T, ? super U, E> predicate) throws E {
        final List<Pair<T, U>> result = new ArrayList<>(N.min(9, size(), N.len(b)));

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
    public <U, E extends Exception, E2 extends Exception> List<Pair<T, U>> fullJoin(final Collection<U> b, final Try.Function<? super T, ?, E> leftKeyMapper,
            final Try.Function<? super U, ?, E2> rightKeyMapper) throws E, E2 {
        final List<Pair<T, U>> result = new ArrayList<>(N.max(9, size(), N.len(b)));

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
    public <U, E extends Exception> List<Pair<T, U>> fullJoin(final Collection<U> b, final Try.BiPredicate<? super T, ? super U, E> predicate) throws E {
        final List<Pair<T, U>> result = new ArrayList<>(N.max(9, size(), N.len(b)));

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
    public <U, E extends Exception, E2 extends Exception> List<Pair<T, U>> leftJoin(final Collection<U> b, final Try.Function<? super T, ?, E> leftKeyMapper,
            final Try.Function<? super U, ?, E2> rightKeyMapper) throws E, E2 {
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
    public <U, E extends Exception> List<Pair<T, U>> leftJoin(final Collection<U> b, final Try.BiPredicate<? super T, ? super U, E> predicate) throws E {
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
    public <U, E extends Exception, E2 extends Exception> List<Pair<T, U>> rightJoin(final Collection<U> b, final Try.Function<? super T, ?, E> leftKeyMapper,
            final Try.Function<? super U, ?, E2> rightKeyMapper) throws E, E2 {
        final List<Pair<T, U>> result = new ArrayList<>(N.len(b));

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
    public <U, E extends Exception> List<Pair<T, U>> rightJoin(final Collection<U> b, final Try.BiPredicate<? super T, ? super U, E> predicate) throws E {
        final List<Pair<T, U>> result = new ArrayList<>(N.len(b));

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
    public ObjIterator<T> iterator() {
        return ObjIterator.of(coll);
    }

    //    public Stream<T> stream() {
    //        return N.isNullOrEmpty(coll) ? Stream.<T> empty() : Stream.of(coll);
    //    }

    public <R, E extends Exception> R apply(Try.Function<? super Seq<T>, R, E> func) throws E {
        return func.apply(this);
    }

    public <R, E extends Exception> Optional<R> applyIfNotEmpty(Try.Function<? super Seq<T>, R, E> func) throws E {
        return isEmpty() ? Optional.<R> empty() : Optional.ofNullable(func.apply(this));
    }

    public <E extends Exception> void accept(Try.Consumer<? super Seq<T>, E> action) throws E {
        action.accept(this);
    }

    public <E extends Exception> void acceptIfNotEmpty(Try.Consumer<? super Seq<T>, E> action) throws E {
        if (size() > 0) {
            action.accept(this);
        }
    }

    public void println() {
        N.println(toString());
    }

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
            final Iterator<E> iter = c == null ? ObjIterator.<E> empty() : c.iterator();

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
}
