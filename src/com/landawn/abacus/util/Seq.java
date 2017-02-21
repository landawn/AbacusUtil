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
import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Objects;
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
 * It'a wrapper for <code>Collection</code> to more daily used and functional methods.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class Seq<T> implements Collection<T> {
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

        return OptionalNullable.of(coll.iterator().next());
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

    public ObjectList<T> filter(Predicate<? super T> filter) {
        return N.filter(coll, filter);
    }

    public ObjectList<T> filter(Predicate<? super T> filter, final int max) {
        return N.filter(coll, filter, max);
    }

    public <U> ObjectList<T> filter(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return filter(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public ObjectList<T> takeWhile(Predicate<? super T> filter) {
        final ObjectList<T> result = new ObjectList<>(N.min(9, size()));

        for (T e : coll) {
            if (filter.test(e)) {
                result.add(e);
            } else {
                break;
            }
        }

        return result;
    }

    public <U> ObjectList<T> takeWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return takeWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public ObjectList<T> dropWhile(Predicate<? super T> filter) {
        final ObjectList<T> result = new ObjectList<>(N.min(9, size()));
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

    public <U> ObjectList<T> dropWhile(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public <R> ObjectList<R> map(final Function<? super T, ? extends R> func) {
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

    public <R> ObjectList<R> flatMap(final Function<? super T, ? extends Collection<R>> func) {
        final ObjectList<R> result = new ObjectList<>(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
        }

        return result;
    }

    public <R> ObjectList<R> flatMap2(final Function<? super T, ? extends R[]> func) {
        final ObjectList<R> result = new ObjectList<>(coll.size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : coll.size() * 2);

        for (T e : coll) {
            result.addAll(func.apply(e));
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

    public ObjectList<T> merge(final Collection<? extends T> b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return Seq.merge(this, b, nextSelector);
    }

    public <B, R> ObjectList<R> zipWith(final Collection<B> b, final BiFunction<? super T, ? super B, R> zipFunction) {
        return Seq.zip(this, b, zipFunction);
    }

    public <B, R> ObjectList<R> zipWith(final Collection<B> b, final T valueForNoneA, final B valueForNoneB,
            final BiFunction<? super T, ? super B, R> zipFunction) {
        return Seq.zip(this, b, valueForNoneA, valueForNoneB, zipFunction);
    }

    public <B, C, R> ObjectList<R> zipWith(final Collection<B> b, final Collection<C> c, final TriFunction<? super T, ? super B, ? super C, R> zipFunction) {
        return Seq.zip(this, b, c, zipFunction);
    }

    public <B, C, R> ObjectList<R> zipWith(final Collection<B> b, final Collection<C> c, final T valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super T, ? super B, ? super C, R> zipFunction) {
        return Seq.zip(this, b, c, valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public ObjectList<Indexed<T>> indexed() {
        final ObjectList<Indexed<T>> result = new ObjectList<>(size());
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
    public ObjectList<Seq<T>> split(int size) {
        final ObjectList<List<T>> list = N.split(coll, size);
        @SuppressWarnings("rawtypes")
        final ObjectList<Seq<T>> result = (ObjectList) list;

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

    public ObjectList<T> toObjectList() {
        return ObjectList.of((T[]) toArray());
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
        N.checkIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, coll.size());

        if (coll instanceof List) {
            return new Seq<>(N.copyOfRange((List) coll, from, to, step));
        } else {
            final T[] a = (T[]) (coll instanceof ObjectList ? ((ObjectList) coll).array() : coll.toArray());
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
        N.checkIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, coll.size());

        @SuppressWarnings("rawtypes")
        final T[] a = (T[]) (coll instanceof ObjectList ? ((ObjectList) coll).array() : coll.toArray());
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
        N.checkIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from, coll.size());

        @SuppressWarnings("rawtypes")
        final T[] a = (T[]) (coll instanceof ObjectList ? ((ObjectList) coll).array() : coll.toArray());
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
        N.checkIndex(fromIndex, toIndex, size());

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

    //    public ObjectListBuilder<T> __() {
    //        return Builder.of(this);
    //    }
    //
    //    public ObjectListBuilder<T> __(Consumer<? super ObjectList<T>> func) {
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

    public static <T> ObjectList<T> merge(final T[] a, final T[] b, final BiFunction<? super T, ? super T, Nth> nextSelector) {
        return merge(Arrays.asList(a), Arrays.asList(b), nextSelector);
    }

    public static <T> ObjectList<T> merge(final Collection<? extends T> a, final Collection<? extends T> b,
            final BiFunction<? super T, ? super T, Nth> nextSelector) {
        final ObjectList<T> result = new ObjectList<>(a.size() + b.size());
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

    public static <A, B, R> ObjectList<R> zip(final A[] a, final B[] b, final BiFunction<? super A, ? super B, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), zipFunction);
    }

    public static <A, B, R> ObjectList<R> zip(final Collection<A> a, final Collection<B> b, final BiFunction<? super A, ? super B, R> zipFunction) {
        final ObjectList<R> result = new ObjectList<>(N.min(a.size(), b.size()));

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

    public static <A, B, C, R> ObjectList<R> zip(final A[] a, final B[] b, final C[] c, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), Arrays.asList(c), zipFunction);
    }

    public static <A, B, C, R> ObjectList<R> zip(final Collection<A> a, final Collection<B> b, final Collection<C> c,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final ObjectList<R> result = new ObjectList<>(N.min(a.size(), b.size(), c.size()));

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();
        final Iterator<C> iterC = c.iterator();

        while (iterA.hasNext() && iterB.hasNext() && iterC.hasNext()) {
            result.add(zipFunction.apply(iterA.next(), iterB.next(), iterC.next()));
        }

        return result;
    }

    public static <A, B, R> ObjectList<R> zip(final A[] a, final B[] b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), valueForNoneA, valueForNoneB, zipFunction);
    }

    public static <A, B, R> ObjectList<R> zip(final Collection<A> a, final Collection<B> b, final A valueForNoneA, final B valueForNoneB,
            final BiFunction<? super A, ? super B, R> zipFunction) {
        final ObjectList<R> result = new ObjectList<>(N.max(a.size(), b.size()));

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

    public static <A, B, C, R> ObjectList<R> zip(final A[] a, final B[] b, final C[] c, final A valueForNoneA, final B valueForNoneB, final C valueForNoneC,
            final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        return zip(Arrays.asList(a), Arrays.asList(b), Arrays.asList(c), valueForNoneA, valueForNoneB, valueForNoneC, zipFunction);
    }

    public static <A, B, C, R> ObjectList<R> zip(final Collection<A> a, final Collection<B> b, final Collection<C> c, final A valueForNoneA,
            final B valueForNoneB, final C valueForNoneC, final TriFunction<? super A, ? super B, ? super C, R> zipFunction) {
        final ObjectList<R> result = new ObjectList<>(N.max(a.size(), b.size(), c.size()));

        final Iterator<A> iterA = a.iterator();
        final Iterator<B> iterB = b.iterator();
        final Iterator<C> iterC = c.iterator();

        while (iterA.hasNext() || iterB.hasNext() || iterC.hasNext()) {
            result.add(zipFunction.apply(iterA.hasNext() ? iterA.next() : valueForNoneA, iterB.hasNext() ? iterB.next() : valueForNoneB,
                    iterC.hasNext() ? iterC.next() : valueForNoneC));
        }

        return result;
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

    static class SubCollection<E> implements Collection<E> {
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
