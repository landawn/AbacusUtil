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
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.HashSet;
import java.util.IdentityHashMap;
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
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;
import com.landawn.abacus.util.stream.Stream;

/**
 * It'a read-only wrapper for <code>Collection</code> to support more daily used/functional methods.
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class Slice<T> implements Collection<T> {
    final Collection<T> coll;

    /**
     * The returned <code>Seq</code> and the specified <code>Collection</code> are backed by the same data.
     * Any changes to one will appear in the other.
     * 
     * @param c
     */
    Slice(final Collection<T> c) {
        N.requireNonNull(c);

        this.coll = c;
    }

    /**
     * Returns the <code>Collection</code> the <code>Seq</code> is backed with recursively.
     * 
     * @return
     */
    public Collection<T> interior() {
        return coll instanceof Slice ? ((Slice<T>) coll).interior() : coll;
    }

    @Deprecated
    @Override
    public boolean add(T e) {
        throw new UnsupportedOperationException();
    }

    @Deprecated
    @Override
    public boolean remove(Object o) {
        throw new UnsupportedOperationException();
    }

    @Deprecated
    @Override
    public boolean addAll(Collection<? extends T> c) {
        throw new UnsupportedOperationException();
    }

    @Deprecated
    @Override
    public boolean removeAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Deprecated
    @Override
    public boolean retainAll(Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Deprecated
    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean contains(Object e) {
        return coll.contains(e);
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return true;
        }

        return coll.containsAll(c);
    }

    public boolean containsAll(Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
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
        if (N.isNullOrEmpty(a)) {
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
    public Seq<T> intersection(Collection<?> b) {
        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(b)) {
            return new Seq<>();
        }

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
        if (N.isNullOrEmpty(coll) || N.isNullOrEmpty(a)) {
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
        if (N.isNullOrEmpty(b)) {
            return new Seq<>(new ArrayList<>(coll));
        }

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
        if (N.isNullOrEmpty(b)) {
            return new Seq<>(new ArrayList<>(coll));
        } else if (N.isNullOrEmpty(coll)) {
            return new Seq<>(new ArrayList<>(b));
        }

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
        } else if (N.isNullOrEmpty(coll)) {
            return new Seq<>(N.asList(a));
        }

        return symmetricDifference(Arrays.asList(a));
    }

    public int occurrencesOf(final Object objectToFind) {
        return N.occurrencesOf(coll, objectToFind);
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> min() {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.min((Collection) coll));
    }

    public NullabLe<T> min(Comparator<? super T> cmp) {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.min(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> median() {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.median((Collection) coll));
    }

    public NullabLe<T> median(Comparator<? super T> cmp) {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.median(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> max() {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.max((Collection) coll));
    }

    public NullabLe<T> max(Comparator<? super T> cmp) {
        return size() == 0 ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.max(coll, cmp));
    }

    @SuppressWarnings("rawtypes")
    public NullabLe<T> kthLargest(final int k) {
        return size() < k ? (NullabLe<T>) NullabLe.empty() : NullabLe.of((T) N.kthLargest((Collection) coll, k));
    }

    public NullabLe<T> kthLargest(final int k, Comparator<? super T> cmp) {
        return size() < k ? (NullabLe<T>) NullabLe.empty() : NullabLe.of(N.kthLargest(coll, k, cmp));
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
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public <R> R forEach(int fromIndex, final int toIndex, final R seed, final IndexedBiFunction<R, ? super T, Collection<T>, R> accumulator,
            final BiPredicate<? super T, ? super R> conditionToBreak) {
        return N.forEach(coll, fromIndex, toIndex, seed, accumulator, conditionToBreak);
    }

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

    public NullabLe<T> last() {
        if (size() == 0) {
            return NullabLe.empty();
        }

        if (coll instanceof List && coll instanceof RandomAccess) {
            return NullabLe.of(((List<T>) coll).get(size() - 1));
        } else {
            final Iterator<T> iter = coll.iterator();
            T e = null;

            while (iter.hasNext()) {
                e = iter.next();
            }

            return NullabLe.of(e);
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

        if (coll instanceof List && coll instanceof RandomAccess) {
            final List<T> list = (List<T>) coll;

            for (int i = size() - 1; i >= 0; i--) {
                if (predicate.test(list.get(i))) {
                    return NullabLe.of(list.get(i));
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

    public <U> NullabLe<T> findFirstOrLast(final Predicate<? super T> predicateForFirst, final Predicate<? super T> predicateForLast) {
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

    public ExList<T> skipUntil(final Predicate<? super T> filter) {
        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !filter.test(value);
            }
        });
    }

    public <U> ExList<T> skipUntil(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return dropWhile(new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return !predicate.test(value, seed);
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

    public ExList<T> append(final Collection<? extends T> c) {
        return Seq.concat(this, c);
    }

    public ExList<T> prepend(final Collection<? extends T> c) {
        return Seq.concat(c, this);
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

    public ExList<T> intersperse(T value) {
        if (isEmpty()) {
            return new ExList<>();
        }

        final int size = size();
        final ExList<T> result = new ExList<>(size * 2 - 1);
        int idx = 0;

        for (T e : coll) {
            result.add(e);

            if (++idx < size) {
                result.add(value);
            }
        }

        return result;
    }

    public ExList<Indexed<T>> indexed() {
        final ExList<Indexed<T>> result = new ExList<>(size());
        int idx = 0;

        for (T e : coll) {
            result.add(Indexed.of(idx++, e));
        }

        return result;
    }

    /**
     *
     * @return a new List with distinct elements
     */
    public Seq<T> distinct() {
        return new Seq<T>(N.distinct(coll));
    }

    /**
     * 
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    public Seq<T> distinct(final Function<? super T, ?> keyMapper) {
        return new Seq<>(N.distinct(coll, keyMapper));
    }

    @SuppressWarnings("rawtypes")
    public Seq<T> top(final int n) {
        return new Seq<>(N.top((Collection) coll, n));
    }

    public Seq<T> top(final int n, final Comparator<? super T> cmp) {
        return new Seq<>(N.top(coll, n, cmp));
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
            result.set(i, new Seq<>(list.get(i)));
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

    /**
     * 
     * @param keyMapper
     * @return
     */
    public <K> Multimap<K, T, List<T>> toMultimap(Function<? super T, ? extends K> keyMapper) {
        final Multimap<K, T, List<T>> m = N.newListMultimap();

        for (T e : coll) {
            m.put(keyMapper.apply(e), e);
        }

        return m;
    }

    /**
     * 
     * @param keyMapper
     * @param valueMapper
     * @return
     */
    public <K, V> Multimap<K, V, List<V>> toMultimap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends V> valueMapper) {
        final Multimap<K, V, List<V>> m = N.newListMultimap();

        for (T e : coll) {
            m.put(keyMapper.apply(e), valueMapper.apply(e));
        }

        return m;
    }

    public Seq<T> copyToList() {
        return new Seq<>(new ArrayList<>(coll));
    }

    @SuppressWarnings("rawtypes")
    public Seq<T> copyToList(final int fromIndex, final int toIndex) {
        if (coll instanceof List) {
            return new Seq<>(N.copyOfRange((List) coll, fromIndex, toIndex));
        } else {
            return slice(fromIndex, toIndex).copyToList();
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
        return slice(fromIndex, toIndex).copyToSet();
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
        return slice(fromIndex, toIndex).copyTo(supplier);
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
    public Slice<T> slice(final int fromIndex, final int toIndex) {
        N.checkFromToIndex(fromIndex, toIndex, size());

        if (coll instanceof List) {
            return new Slice<T>(((List<T>) coll).subList(fromIndex, toIndex));
        }

        return new Slice<T>(new SubCollection<>(coll, fromIndex, toIndex));
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
    public <U> ExList<Pair<T, U>> innerJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final ExList<Pair<T, U>> result = new ExList<>(N.min(9, size(), b.size()));
        final Multimap<Object, U, List<U>> rightKeyMap = Multimap.from(b, rightKeyMapper);

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
    public <U> ExList<Pair<T, U>> innerJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final ExList<Pair<T, U>> result = new ExList<>(N.min(9, size(), b.size()));

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
    public <U> ExList<Pair<T, U>> fullJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final ExList<Pair<T, U>> result = new ExList<>(N.max(9, size(), b.size()));
        final Multimap<Object, U, List<U>> rightKeyMap = Multimap.from(b, rightKeyMapper);
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
    public <U> ExList<Pair<T, U>> fullJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final ExList<Pair<T, U>> result = new ExList<>(N.max(9, size(), b.size()));
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
    public <U> ExList<Pair<T, U>> leftJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final ExList<Pair<T, U>> result = new ExList<>(size());
        final Multimap<Object, U, List<U>> rightKeyMap = Multimap.from(b, rightKeyMapper);

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
    public <U> ExList<Pair<T, U>> leftJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final ExList<Pair<T, U>> result = new ExList<>(size());

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
    public <U> ExList<Pair<T, U>> rightJoin(final Collection<U> b, final Function<? super T, ?> leftKeyMapper, final Function<? super U, ?> rightKeyMapper) {
        final ExList<Pair<T, U>> result = new ExList<>(b.size());
        final Multimap<Object, T, List<T>> leftKeyMap = Multimap.from(coll, leftKeyMapper);

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
    public <U> ExList<Pair<T, U>> rightJoin(final Collection<U> b, final BiPredicate<? super T, ? super U> predicate) {
        final ExList<Pair<T, U>> result = new ExList<>(b.size());

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

        return result;
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

        if (obj instanceof Slice) {
            final Slice<T> other = (Slice<T>) obj;

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
