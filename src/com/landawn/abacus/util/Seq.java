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
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
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
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.UnaryOperator;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;
import com.landawn.abacus.util.stream.Stream;

/**
 * It'a wrapper for <code>Collection</code> to support more daily used/functional methods.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class Seq<T> implements Collection<T> {
    private final Collection<T> coll;

    public Seq() {
        this.coll = new ArrayList<>();
    }

    public Seq(int initialCapacity) {
        this.coll = new ArrayList<>(initialCapacity);
    }

    /**
     * The returned <code>Seq</code> and the specified <code>Collection</code> are backed by the same data.
     * Any changes to one will appear in the other.
     * 
     * @param c
     */
    public Seq(final Collection<T> c) {
        N.requireNonNull(c);

        this.coll = c;
    }

    /**
     * The returned <code>Seq</code> and the specified <code>Collection</code> are backed by the same data.
     * Any changes to one will appear in the other.
     * 
     * @param c
     * @return
     */
    public static <T> Seq<T> of(Collection<T> c) {
        return new Seq<>(c);
    }

    public static <K, V> Seq<Map.Entry<K, V>> of(Map<K, V> map) {
        return of(map.entrySet());
    }

    /**
     * Returns the <code>Collection</code> the <code>Seq</code> is backed with recursively.
     * 
     * @return
     */
    public Collection<T> interior() {
        return coll instanceof Seq ? ((Seq<T>) coll).interior() : coll;
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
        return coll.retainAll(c);
    }

    public boolean retainAll(Object[] a) {
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

    @Override
    public boolean contains(Object e) {
        return coll.contains(e);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        return coll.containsAll(c);
    }

    public boolean containsAll(Object[] a) {
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
        return Collections.disjoint(this.coll, c);
    }

    public boolean disjoint(final Object[] a) {
        return disjoint(Arrays.asList(a));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public Seq<T> intersection(Collection<?> b) {
        final Multiset<?> bOccurrences = Multiset.from(b);
        final Seq<T> result = new Seq<>(N.min(9, size(), b.size()));

        for (T e : coll) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }
        }

        return result;
    }

    public Seq<T> intersection(final Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return new Seq<>();
        }

        return intersection(Arrays.asList(a));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public Seq<T> difference(Collection<?> b) {
        final Multiset<?> bOccurrences = Multiset.from(b);
        final Seq<T> result = new Seq<>(N.min(size(), N.max(9, size() - b.size())));

        for (T e : coll) {
            if (bOccurrences.getAndRemove(e) < 1) {
                result.add(e);
            }
        }

        return result;
    }

    public Seq<T> difference(final Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return new Seq<>(new ArrayList<>(coll));
        }

        return difference(Arrays.asList(a));
    }

    /**
     * 
     * @param b
     * @return this.difference(b).addAll(b.difference(this))
     * @see IntList#symmetricDifference(IntList)
     */
    public Seq<T> symmetricDifference(Collection<T> b) {
        final Multiset<?> bOccurrences = Multiset.from(b);
        final Seq<T> result = new Seq<>(N.max(9, Math.abs(size() - b.size())));

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

    public Seq<T> symmetricDifference(final T[] a) {
        if (N.isNullOrEmpty(a)) {
            return new Seq<>(new ArrayList<>(coll));
        }

        return symmetricDifference(Arrays.asList(a));
    }

    public int occurrencesOf(final Object objectToFind) {
        return N.occurrencesOf(coll, objectToFind);
    }

    @SuppressWarnings("rawtypes")
    public OptionalNullable<T> min() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of((T) N.min((Collection) coll));
    }

    public OptionalNullable<T> min(Comparator<? super T> cmp) {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.min(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public OptionalNullable<T> median() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of((T) N.median((Collection) coll));
    }

    public OptionalNullable<T> median(Comparator<? super T> cmp) {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.median(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public OptionalNullable<T> max() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of((T) N.max((Collection) coll));
    }

    public OptionalNullable<T> max(Comparator<? super T> cmp) {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.max(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public OptionalNullable<T> kthLargest(final int k) {
        return size() < k ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of((T) N.kthLargest((Collection) coll, k));
    }

    public OptionalNullable<T> kthLargest(final int k, Comparator<? super T> cmp) {
        return size() < k ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.kthLargest(coll, k, cmp));
    }

    public Long sumInt() {
        long result = 0L;

        for (T e : coll) {
            if (e != null) {
                result += ((Number) e).intValue();
            }
        }

        return result;
    }

    public Long sumInt(final ToIntFunction<? super T> mapper) {
        long result = 0L;

        for (T e : coll) {
            result += mapper.applyAsInt(e);
        }

        return result;
    }

    public Long sumLong() {
        long result = 0L;

        for (T e : coll) {
            if (e != null) {
                result += ((Number) e).longValue();
            }
        }

        return result;
    }

    public Long sumLong(final ToLongFunction<? super T> mapper) {
        long result = 0L;

        for (T e : coll) {
            result += mapper.applyAsLong(e);
        }

        return result;
    }

    public Double sumDouble() {
        return sumDouble((ToDoubleFunction<? super T>) new ToDoubleFunction<Number>() {
            @Override
            public double applyAsDouble(Number value) {
                return value == null ? 0d : value.doubleValue();
            }
        });
    }

    public Double sumDouble(final ToDoubleFunction<? super T> mapper) {
        return size() == 0 ? 0d : N.sumDouble(coll, mapper);
    }

    public OptionalDouble averageInt() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(sumInt().doubleValue() / size());
    }

    public OptionalDouble averageInt(final ToIntFunction<? super T> mapper) {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(sumInt(mapper).doubleValue() / size());
    }

    public OptionalDouble averageLong() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(sumLong().doubleValue() / size());
    }

    public OptionalDouble averageLong(final ToLongFunction<? super T> mapper) {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(sumLong(mapper).doubleValue() / size());
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
        forEach(0, size(), action);
    }

    public void forEach(int fromIndex, final int toIndex, final Consumer<? super T> action) {
        N.forEach(coll, fromIndex, toIndex, action);
    }

    public void forEach(final IndexedConsumer<? super T, Collection<T>> action) {
        forEach(0, size(), action);
    }

    public void forEach(int fromIndex, final int toIndex, final IndexedConsumer<? super T, Collection<T>> action) {
        N.forEach(coll, fromIndex, toIndex, action);
    }

    public <R> R forEach(final R seed, BiFunction<R, ? super T, R> accumulator, final BiPredicate<? super T, ? super R> conditionToBreak) {
        return forEach(0, size(), seed, accumulator, conditionToBreak);
    }

    public <R> R forEach(int fromIndex, final int toIndex, final R seed, final BiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super T, ? super R> conditionToBreak) {
        return N.forEach(coll, fromIndex, toIndex, seed, accumulator, conditionToBreak);
    }

    public <R> R forEach(final R seed, final IndexedBiFunction<R, ? super T, Collection<T>, R> accumulator,
            final BiPredicate<? super T, ? super R> conditionToBreak) {
        return forEach(0, size(), seed, accumulator, conditionToBreak);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param seed
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public <R> R forEach(int fromIndex, final int toIndex, final R seed, final IndexedBiFunction<R, ? super T, Collection<T>, R> accumulator,
            final BiPredicate<? super T, ? super R> conditionToBreak) {
        return N.forEach(coll, fromIndex, toIndex, seed, accumulator, conditionToBreak);
    }

    public OptionalNullable<T> first() {
        if (size() == 0) {
            return OptionalNullable.empty();
        }

        if (coll instanceof List && coll instanceof RandomAccess) {
            return OptionalNullable.of(((List<T>) coll).get(0));
        } else {
            return OptionalNullable.of(coll.iterator().next());
        }
    }

    public OptionalNullable<T> last() {
        if (size() == 0) {
            return OptionalNullable.empty();
        }

        if (coll instanceof List && coll instanceof RandomAccess) {
            return OptionalNullable.of(((List<T>) coll).get(size() - 1));
        } else {
            final Iterator<T> iter = coll.iterator();
            T e = null;

            while (iter.hasNext()) {
                e = iter.next();
            }

            return OptionalNullable.of(e);
        }
    }

    public OptionalNullable<T> findFirst(Predicate<? super T> predicate) {
        if (size() == 0) {
            return OptionalNullable.empty();
        }

        for (T e : coll) {
            if (predicate.test(e)) {
                return OptionalNullable.of(e);
            }
        }

        return OptionalNullable.empty();
    }

    public OptionalNullable<T> findLast(Predicate<? super T> predicate) {
        if (size() == 0) {
            return OptionalNullable.empty();
        }

        if (coll instanceof List && coll instanceof RandomAccess) {
            final List<T> list = (List<T>) coll;

            for (int i = size() - 1; i >= 0; i--) {
                if (predicate.test(list.get(i))) {
                    return OptionalNullable.of(list.get(i));
                }
            }

            return OptionalNullable.empty();
        } else {
            T result = (T) N.NULL_MASK;

            for (T e : coll) {
                if (predicate.test(e)) {
                    result = e;
                }
            }

            return result == N.NULL_MASK ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(result);
        }
    }

    public OptionalInt findFirstIndex(Predicate<? super T> predicate) {
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

        if (coll instanceof List && coll instanceof RandomAccess) {
            final List<T> list = (List<T>) coll;

            for (int i = size() - 1; i >= 0; i--) {
                if (predicate.test(list.get(i))) {
                    return OptionalInt.of(i);
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

    public boolean allMatch(Predicate<? super T> filter) {
        for (T e : coll) {
            if (filter.test(e) == false) {
                return false;
            }
        }

        return true;
    }

    public boolean anyMatch(Predicate<? super T> filter) {
        for (T e : coll) {
            if (filter.test(e)) {
                return true;
            }
        }

        return false;
    }

    public boolean noneMatch(Predicate<? super T> filter) {
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

    public ExList<T> filter(Predicate<? super T> filter) {
        return N.filter(coll, filter);
    }

    public ExList<T> filter(Predicate<? super T> filter, final int max) {
        return N.filter(coll, filter, max);
    }

    public <U> ExList<T> filter(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public ExList<T> takeWhile(Predicate<? super T> filter) {
        final ExList<T> result = new ExList<>(N.min(9, size()));

        for (T e : coll) {
            if (filter.test(e)) {
                result.add(e);
            } else {
                break;
            }
        }

        return result;
    }

    public <U> ExList<T> takeWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return takeWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public ExList<T> dropWhile(Predicate<? super T> filter) {
        final ExList<T> result = new ExList<>(N.min(9, size()));
        final Iterator<T> iter = coll.iterator();
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

    public <U> ExList<T> dropWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public <R> ExList<R> map(final Function<? super T, ? extends R> func) {
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

    public <R> ExList<R> flatMap(final Function<? super T, ? extends Collection<R>> func) {
        final ExList<R> result = new ExList<>(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <R> ExList<R> flatMap2(final Function<? super T, ? extends R[]> func) {
        final ExList<R> result = new ExList<>(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public BooleanList flatMapToBoolean(final Function<? super T, ? extends Collection<Boolean>> func) {
        final BooleanList result = new BooleanList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (boolean b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public BooleanList flatMapToBoolean2(final Function<? super T, boolean[]> func) {
        final BooleanList result = new BooleanList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public CharList flatMapToChar(final Function<? super T, ? extends Collection<Character>> func) {
        final CharList result = new CharList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (char b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public CharList flatMapToChar2(final Function<? super T, char[]> func) {
        final CharList result = new CharList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public ByteList flatMapToByte(final Function<? super T, ? extends Collection<Byte>> func) {
        final ByteList result = new ByteList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (byte b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public ByteList flatMapToByte2(final Function<? super T, byte[]> func) {
        final ByteList result = new ByteList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public ShortList flatMapToShort(final Function<? super T, ? extends Collection<Short>> func) {
        final ShortList result = new ShortList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (short b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public ShortList flatMapToShort2(final Function<? super T, short[]> func) {
        final ShortList result = new ShortList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public IntList flatMapToInt(final Function<? super T, ? extends Collection<Integer>> func) {
        final IntList result = new IntList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (int b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public IntList flatMapToInt2(final Function<? super T, int[]> func) {
        final IntList result = new IntList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public LongList flatMapToLong(final Function<? super T, ? extends Collection<Long>> func) {
        final LongList result = new LongList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (long b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public LongList flatMapToLong2(final Function<? super T, long[]> func) {
        final LongList result = new LongList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public FloatList flatMapToFloat(final Function<? super T, ? extends Collection<Float>> func) {
        final FloatList result = new FloatList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (float b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public FloatList flatMapToFloat2(final Function<? super T, float[]> func) {
        final FloatList result = new FloatList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public DoubleList flatMapToDouble(final Function<? super T, ? extends Collection<Double>> func) {
        final DoubleList result = new DoubleList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            for (double b : func.apply(e)) {
                result.add(b);
            }
        }

        return result;
    }

    public DoubleList flatMapToDouble2(final Function<? super T, double[]> func) {
        final DoubleList result = new DoubleList(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
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
    public ExList<T> collapse(final BiPredicate<? super T, ? super T> collapsible, final BiFunction<? super T, ? super T, T> mergeFunction) {
        final ExList<T> result = new ExList<>();
        final Iterator<T> iter = coll.iterator();
        T next = null;
        boolean hasNext = false;

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
     * @param seed
     * @param collapsible
     * @param mergeFunction
     * @return
     */
    public <R> ExList<R> collapse(final R seed, final BiPredicate<? super T, ? super T> collapsible, final BiFunction<? super R, ? super T, R> mergeFunction) {
        final ExList<R> result = new ExList<>();
        final Iterator<T> iter = coll.iterator();
        T next = null;
        boolean hasNext = false;

        while (hasNext || iter.hasNext()) {
            R res = mergeFunction.apply(seed, hasNext ? next : (next = iter.next()));

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
     * @param supplier usually it's a creator of collection.
     * @param collapsible
     * @param mergeFunction
     * @return
     */
    public <C extends Collection<?>> ExList<C> collapse(final Supplier<C> supplier, final BiPredicate<? super T, ? super T> collapsible,
            final BiConsumer<? super C, ? super T> mergeFunction) {
        final ExList<C> result = new ExList<>();
        final Iterator<T> iter = coll.iterator();
        T next = null;
        boolean hasNext = false;

        while (hasNext || iter.hasNext()) {
            final C c = supplier.get();
            mergeFunction.accept(c, hasNext ? next : (next = iter.next()));

            while ((hasNext = iter.hasNext())) {
                if (collapsible.test(next, (next = iter.next()))) {
                    mergeFunction.accept(c, next);
                } else {
                    break;
                }
            }

            result.add(c);
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
     * identity: 0
     * accumulator: (a, b) -&gt; a + b
     * stream: [1, 2, 3, 4, 5]
     * result: [0, 1, 3, 6, 10, 15]
     * </pre>
     * 
     * <br />
     * This method only run sequentially, even in parallel stream.
     *
     * @param accumulator  the accumulation function
     * @return the new stream which has the extract same size as this stream.
     */
    public ExList<T> scan(final BiFunction<? super T, ? super T, T> accumulator) {
        final ExList<T> result = new ExList<>();
        final Iterator<T> iter = coll.iterator();
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
    public <R> ExList<R> scan(final R seed, final BiFunction<? super R, ? super T, R> accumulator) {
        final ExList<R> result = new ExList<>();
        final Iterator<T> iter = coll.iterator();
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
     *        return OptionalNullable.empty();
     *    }
     *
     *    final Iterator<T> iter = iterator();
     *    T result = iter.next();
     *
     *    while (iter.hasNext()) {
     *        result = accumulator.apply(result, iter.next());
     *    }
     *
     *    return OptionalNullable.of(result);
     * </code>
     * </pre>
     * 
     * @param accumulator
     * @return
     */
    public OptionalNullable<T> reduce(BinaryOperator<T> accumulator) {
        if (isEmpty()) {
            return OptionalNullable.empty();
        }

        final Iterator<T> iter = iterator();
        T result = iter.next();

        while (iter.hasNext()) {
            result = accumulator.apply(result, iter.next());
        }

        return OptionalNullable.of(result);
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

    public ExList<T> merge(final Collection<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return Seq.merge(this, b, nextSelector);
    }

    public <B, R> ExList<R> zipWith(final Collection<B> b, final BiFunction<? super T, ? super B, R> zipFunction) {
        return Seq.zip(this, b, zipFunction);
    }

    public <B, R> ExList<R> zipWith(final Collection<B> b, final T valueForNoneA, final B valueForNoneB,
            final BiFunction<? super T, ? super B, R> zipFunction) {
        return Seq.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public <B, C, R> ExList<R> zipWith(final Collection<B> b, final Collection<C> c, final TriFunction<? super T, ? super B, ? super C, R> zipFunction) {
        return Seq.zip(this, b, c, zipFunction);
    }

    public <B, C, R> ExList<R> zipWith(final Collection<B> b, final Collection<C> c, final T valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super T, ? super B, ? super C, R> zipFunction) {
        return Seq.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public ExList<Indexed<T>> indexed() {
        final ExList<Indexed<T>> result = new ExList<>(size());
        int idx = 0;

        for (T e : coll) {
            result.add(Indexed.of(idx++, e));
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

    /**
     *
     * @return a new List with distinct elements
     */
    public Seq<T> distinct() {
        return of(N.distinct(coll));
    }

    /**
     * 
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    public Seq<T> distinct(final Function<? super T, ?> keyMapper) {
        return of(N.distinct(coll, keyMapper));
    }

    @SuppressWarnings("rawtypes")
    public Seq<T> top(final int n) {
        return of((List<T>) N.top((Collection) coll, n));
    }

    public Seq<T> top(final int n, final Comparator<? super T> cmp) {
        return of(N.top(coll, n, cmp));
    }

    /**
     * Returns consecutive sub lists of this list, each of the same size (the final list may be smaller),
     * or an empty List if the specified list is null or empty.
     *
     * @return
     */
    public ExList<Seq<T>> split(int size) {
        final ExList<List<T>> list = N.split(coll, size);
        @SuppressWarnings("rawtypes")
        final ExList<Seq<T>> result = (ExList) list;

        for (int i = 0, len = list.size(); i < len; i++) {
            result.set(i, of(list.get(i)));
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
    public void clear() {
        coll.clear();
    }

    @Override
    public boolean isEmpty() {
        return coll.isEmpty();
    }

    @Override
    public int size() {
        return coll.size();
    }

    @Override
    public Object[] toArray() {
        return coll.toArray();
    }

    @Override
    public <A> A[] toArray(A[] a) {
        return coll.toArray(a);
    }

    public List<T> toList() {
        final List<T> result = new ArrayList<>(size());

        result.addAll(coll);

        return result;
    }

    public List<T> toList(final IntFunction<List<T>> supplier) {
        final List<T> result = supplier.apply(size());

        result.addAll(coll);

        return result;
    }

    public Set<T> toSet() {
        final Set<T> result = new HashSet<>(N.initHashCapacity(size()));

        result.addAll(coll);

        return result;
    }

    public Set<T> toSet(final IntFunction<Set<T>> supplier) {
        final Set<T> result = supplier.apply(N.initHashCapacity(size()));

        result.addAll(coll);

        return result;
    }

    public Multiset<T> toMultiset() {
        final Multiset<T> result = new Multiset<>(N.initHashCapacity(size()));

        result.addAll(coll);

        return result;
    }

    public Multiset<T> toMultiset(final IntFunction<Multiset<T>> supplier) {
        final Multiset<T> result = supplier.apply(N.initHashCapacity(size()));

        result.addAll(coll);

        return result;
    }

    public ExList<T> toExList() {
        return ExList.of((T[]) toArray());
    }

    public <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, List<T>>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, mapFactory);
    }

    public <K, M extends Map<K, List<T>>> M toMap(Function<? super T, ? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, D>> mapFactory = (Supplier) Supplier.MAP;

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

    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = BinaryOperator.THROWING_MERGER;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Iterator<T> iter = iterator();
        T element = null;

        while (iter.hasNext()) {
            element = iter.next();
            merge(result, keyMapper.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    public <K, U> Map<K, List<U>> toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return toMap(keyMapper, (Collector<T, ?, List<U>>) (Collector<?, ?, ?>) Collectors.mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> M toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        return toMap(keyMapper, (Collector<T, ?, List<U>>) (Collector) Collectors.mapping(valueMapper, Collectors.toList()), mapFactory);
    }

    //    /**
    //     * 
    //     * @param classifier
    //     * @return
    //     * @see Collectors#groupingBy(Function)
    //     */
    //    public <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier) {
    //        return stream0().toMap(classifier);
    //    }
    //
    //    /**
    //     * 
    //     * @param classifier
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#groupingBy(Function, Supplier)
    //     */
    //    public <K, M extends Map<K, List<T>>> M toMap(final Function<? super T, ? extends K> classifier, final Supplier<M> mapFactory) {
    //        return stream0().toMap(classifier, mapFactory);
    //    }
    //
    //    /**
    //     * 
    //     * @param classifier
    //     * @param downstream
    //     * @return
    //     * @see Collectors#groupingBy(Function, Collector)
    //     */
    //    @SuppressWarnings("hiding")
    //    public <K, A, D> Map<K, D> toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream) {
    //        return stream0().toMap(classifier, downstream);
    //    }
    //
    //    /**
    //     * 
    //     * @param classifier
    //     * @param downstream
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#groupingBy(Function, Collector, Supplier)
    //     */
    //    @SuppressWarnings("hiding")
    //    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
    //            final Supplier<M> mapFactory) {
    //        return stream0().toMap(classifier, downstream, mapFactory);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @return
    //     * @see Collectors#toMap(Function, Function)
    //     */
    //    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
    //        return stream0().toMap(keyMapper, valueMapper);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#toMap(Function, Function, Supplier)
    //     */
    //    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
    //            Supplier<M> mapFactory) {
    //        return stream0().toMap(keyMapper, valueMapper, mapFactory);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mergeFunction
    //     * @return
    //     * @see Collectors#toMap(Function, Function, BinaryOperator)
    //     */
    //    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
    //        return stream0().toMap(keyMapper, valueMapper, mergeFunction);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mergeFunction
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
    //     */
    //    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
    //            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
    //        return stream0().toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    //    }
    //
    //    public <K, U> Map<K, List<U>> toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
    //        return stream0().toMap2(keyMapper, valueMapper);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#toMultimap(Function, Function, Supplier)
    //     */
    //    public <K, U, M extends Map<K, List<U>>> M toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
    //            Supplier<M> mapFactory) {
    //        return stream0().toMap2(keyMapper, valueMapper, mapFactory);
    //    }

    public Seq<T> copyToList() {
        return new Seq<>(new ArrayList<>(coll));
    }

    @SuppressWarnings("rawtypes")
    public Seq<T> copyToList(final int fromIndex, final int toIndex) {
        if (coll instanceof List) {
            return new Seq<>(N.copyOfRange((List) coll, fromIndex, toIndex));
        } else {
            return subSeq(fromIndex, toIndex).copyToList();
        }
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    @SuppressWarnings("rawtypes")
    public Seq<T> copyToList(final int from, final int to, final int step) {
        if (step == 1) {
            return copyToList(from, to);
        }

        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, coll.size());

        if (coll instanceof List) {
            return new Seq<>(N.copyOfRange((List) coll, from, to, step));
        } else {
            final T[] a = (T[]) (coll instanceof ExList ? ((ExList) coll).array() : coll.toArray());
            final T[] b = N.copyOfRange(a, from, to, step);

            return new Seq<>(Object[].class.equals(b.getClass()) ? Array.asList(b) : N.asList(b));
        }
    }

    public Seq<T> copyToSet() {
        return new Seq<>(new HashSet<>(coll));
    }

    public Seq<T> copyToSet(final int fromIndex, final int toIndex) {
        return subSeq(fromIndex, toIndex).copyToSet();
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    public Seq<T> copyToSet(final int from, final int to, final int step) {
        if (step == 1) {
            return copyToSet(from, to);
        }

        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, coll.size());

        @SuppressWarnings("rawtypes")
        final T[] a = (T[]) (coll instanceof ExList ? ((ExList) coll).array() : coll.toArray());
        return new Seq<>(new HashSet<>(Arrays.asList(N.copyOfRange(a, from, to, step))));
    }

    public Seq<T> copyTo(final IntFunction<? extends Collection<T>> supplier) {
        final Collection<T> c = supplier.apply(coll.size());
        c.addAll(coll);

        return new Seq<>(c);
    }

    public Seq<T> copyTo(final IntFunction<? extends Collection<T>> supplier, final int fromIndex, final int toIndex) {
        return subSeq(fromIndex, toIndex).copyTo(supplier);
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    public Seq<T> copyTo(final IntFunction<? extends Collection<T>> supplier, final int from, final int to, final int step) {
        if (step == 1) {
            return copyTo(supplier, from, to);
        }

        N.checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, coll.size());

        @SuppressWarnings("rawtypes")
        final T[] a = (T[]) (coll instanceof ExList ? ((ExList) coll).array() : coll.toArray());
        final T[] b = N.copyOfRange(a, from, to, step);

        final Collection<T> c = supplier.apply(b.length);
        c.addAll(Arrays.asList(b));

        return new Seq<>(c);
    }

    /**
     * Returns a read-only <code>Seq</code>.
     * 
     * @param fromIndex
     * @param toIndex
     * @return
     */
    public Seq<T> subSeq(final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, size());

        if (coll instanceof List) {
            return of(((List<T>) coll).subList(fromIndex, toIndex));
        }

        return of(new SubCollection<>(coll, fromIndex, toIndex));
    }

    @Override
    public Iterator<T> iterator() {
        return coll.iterator();
    }

    public Stream<T> stream0() {
        return Stream.of(coll);
    }

    //    public ExListBuilder<T> __() {
    //        return Builder.of(this);
    //    }
    //
    //    public ExListBuilder<T> __(Consumer<? super ExList<T>> func) {
    //        return Builder.of(this).__(func);
    //    }

    @Override
    public int hashCode() {
        return coll.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (obj instanceof Seq) {
            final Seq<T> other = (Seq<T>) obj;

            return this.coll.equals(other.coll);
        }

        return false;
    }

    @Override
    public String toString() {
        return coll.toString();
    }

    public void println() {
        N.println(toString());
    }

    public static <T> ExList<T> merge(final T[] a, final T[] b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return merge(Arrays.asList(a), Arrays.asList(b), nextSelector);
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

        while (iterA.hasNext() || iterB.hasNext()) {
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
        return orderedPermutations(elements, N.nullMinOrder());
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
            final Iterator<E> iter = c.iterator();

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
