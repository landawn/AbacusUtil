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

import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.landawn.abacus.annotation.Internal;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.ObjLongConsumer;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.TriFunction;
import com.landawn.abacus.util.function.TriPredicate;
import com.landawn.abacus.util.stream.EntryStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * A collection that supports order-independent equality, like {@link Set}, but
 * may have duplicate elements.
 *
 * <p>Elements of a LongMultiset that are equal to one another are referred to as
 * <i>occurrences</i> of the same single element. The total number of
 * occurrences of an element in a LongMultiset is called the <i>count</i> of that
 * element (the terms "frequency" and "multiplicity" are equivalent, but not
 * used in this API). Since the count of an element is represented as an {@code
 * long}, a LongMultiset may never contain more than {@link MutableLong#MAX_VALUE}
 * occurrences of any one element.
 *
 * @param <E>
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public final class LongMultiset<E> implements Iterable<E> {
    private static final Comparator<Map.Entry<?, MutableLong>> cmpByCount = new Comparator<Map.Entry<?, MutableLong>>() {
        @Override
        public int compare(Entry<?, MutableLong> a, Entry<?, MutableLong> b) {
            return N.compare(a.getValue().value(), b.getValue().value());
        }
    };

    private final Map<E, MutableLong> valueMap;

    public LongMultiset() {
        this(HashMap.class);
    }

    public LongMultiset(int initialCapacity) {
        this(new HashMap<E, MutableLong>(initialCapacity));
    }

    @SuppressWarnings("rawtypes")
    public LongMultiset(final Class<? extends Map> valueMapType) {
        this(N.newInstance(valueMapType));
    }

    public LongMultiset(final Collection<? extends E> c) {
        this();

        addAll(c);
    }

    /**
     *
     * @param valueMap The valueMap and this Multiset share the same data; any changes to one will appear in the other.
     */
    @Internal
    LongMultiset(final Map<E, MutableLong> valueMap) {
        this.valueMap = valueMap;
    }

    @SafeVarargs
    public static <T> LongMultiset<T> of(final T... a) {
        if (N.isNullOrEmpty(a)) {
            return new LongMultiset<>();
        }

        final LongMultiset<T> multiset = new LongMultiset<>(new HashMap<T, MutableLong>(N.initHashCapacity(a.length)));

        for (T e : a) {
            multiset.add(e);
        }

        return multiset;
    }

    public static <T> LongMultiset<T> from(final Collection<? extends T> coll) {
        return new LongMultiset<>(coll);
    }

    public static <T> LongMultiset<T> from(final Map<? extends T, Long> m) {
        if (N.isNullOrEmpty(m)) {
            return new LongMultiset<T>();
        }

        final LongMultiset<T> multiset = new LongMultiset<>(N.initHashCapacity(m.size()));

        multiset.setAll(m);

        return multiset;
    }

    public static <T> LongMultiset<T> from2(final Map<? extends T, Integer> m) {
        if (N.isNullOrEmpty(m)) {
            return new LongMultiset<T>();
        }

        final LongMultiset<T> multiset = new LongMultiset<>(N.initHashCapacity(m.size()));

        for (Map.Entry<? extends T, Integer> entry : m.entrySet()) {
            checkOccurrences(entry.getValue().intValue());
        }

        for (Map.Entry<? extends T, Integer> entry : m.entrySet()) {
            multiset.set(entry.getKey(), entry.getValue().intValue());
        }

        return multiset;
    }

    public static <T> LongMultiset<T> from(final Multiset<? extends T> multiset) {
        if (N.isNullOrEmpty(multiset)) {
            return new LongMultiset<T>();
        }

        final LongMultiset<T> result = new LongMultiset<>(N.initHashCapacity(multiset.size()));

        for (Map.Entry<? extends T, MutableInt> entry : multiset.valueMap.entrySet()) {
            result.set(entry.getKey(), entry.getValue().intValue());
        }

        return result;
    }

    /**
     *
     * @param e
     * @return the occurrences of the specified object. zero is returned if it's not in this set.
     */
    public long get(final Object e) {
        final MutableLong count = valueMap.get(e);

        return count == null ? 0 : count.value();
    }

    /**
     * 
     * @param e
     * @param defaultValue
     * @return the occurrences of the specified object. the specified defaultValue is returned if it's not in this set.
     */
    public long getOrDefault(final Object e, long defaultValue) {
        final MutableLong count = valueMap.get(e);

        return count == null ? defaultValue : count.value();
    }

    /**
     * The element will be removed if the specified count is 0.
     * 
     * @param e
     * @param occurrences
     * @return
     */
    public long getAndSet(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        final MutableLong count = valueMap.get(e);
        long result = count == null ? 0 : count.value();

        if (occurrences == 0) {
            if (count != null) {
                valueMap.remove(e);
            }
        } else {
            if (count == null) {
                valueMap.put(e, MutableLong.of(occurrences));
            } else {
                count.setValue(occurrences);
            }
        }

        return result;
    }

    /**
     * The element will be removed if the specified count is 0.
     * 
     * @param e
     * @param occurrences
     * @return
     */
    public long setAndGet(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        final MutableLong count = valueMap.get(e);

        if (occurrences == 0) {
            if (count != null) {
                valueMap.remove(e);
            }
        } else {
            if (count == null) {
                valueMap.put(e, MutableLong.of(occurrences));
            } else {
                count.setValue(occurrences);
            }
        }

        return occurrences;
    }

    /**
     * The element will be removed if the specified count is 0.
     *
     * @param e
     * @param occurrences
     * @return this LongMultiset.
     * @throws IllegalArgumentException if the occurrences of element is less than 0
     */
    public LongMultiset<E> set(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        if (occurrences == 0) {
            valueMap.remove(e);
        } else {
            final MutableLong count = valueMap.get(e);

            if (count == null) {
                valueMap.put(e, MutableLong.of(occurrences));
            } else {
                count.setValue(occurrences);
            }
        }

        return this;
    }

    public LongMultiset<E> setAll(final Collection<? extends E> c, final long occurrences) {
        checkOccurrences(occurrences);

        if (N.notNullOrEmpty(c)) {
            for (E e : c) {
                set(e, occurrences);
            }
        }

        return this;
    }

    /**
     * 
     * @param m
     * @return this LongMultiset.
     * @throws IllegalArgumentException if the occurrences of element is less than 0.
     */
    public LongMultiset<E> setAll(final Map<? extends E, Long> m) throws IllegalArgumentException {
        if (N.notNullOrEmpty(m)) {
            for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
                checkOccurrences(entry.getValue().longValue());
            }

            for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
                set(entry.getKey(), entry.getValue().longValue());
            }
        }

        return this;
    }

    /**
     * 
     * @param m
     * @return this LongMultiset.
     * @throws IllegalArgumentException if the occurrences of element is less than 0.
     */
    public LongMultiset<E> setAll(final LongMultiset<? extends E> multiset) throws IllegalArgumentException {
        if (N.notNullOrEmpty(multiset)) {
            for (Map.Entry<? extends E, MutableLong> entry : multiset.valueMap.entrySet()) {
                set(entry.getKey(), entry.getValue().value());
            }
        }

        return this;
    }

    public long occurrencesOf(final Object e) {
        return get(e);
    }

    public Optional<Pair<E, Long>> minOccurrences() {
        if (size() == 0) {
            return Optional.empty();
        }

        final Iterator<Map.Entry<E, MutableLong>> it = valueMap.entrySet().iterator();
        Map.Entry<E, MutableLong> entry = it.next();
        E minCountElement = entry.getKey();
        long minCount = entry.getValue().value();

        while (it.hasNext()) {
            entry = it.next();

            if (entry.getValue().value() < minCount) {
                minCountElement = entry.getKey();
                minCount = entry.getValue().value();
            }
        }

        return Optional.of(Pair.of(minCountElement, minCount));
    }

    public Optional<Pair<E, Long>> maxOccurrences() {
        if (size() == 0) {
            return Optional.empty();
        }

        final Iterator<Map.Entry<E, MutableLong>> it = valueMap.entrySet().iterator();
        Map.Entry<E, MutableLong> entry = it.next();
        E maxCountElement = entry.getKey();
        long maxCount = entry.getValue().value();

        while (it.hasNext()) {
            entry = it.next();

            if (entry.getValue().value() > maxCount) {
                maxCountElement = entry.getKey();
                maxCount = entry.getValue().value();
            }
        }

        return Optional.of(Pair.of(maxCountElement, maxCount));
    }

    /**
     * 
     * @return
     * @throws ArithmeticException if total occurrences overflows the maximum value of long.
     */
    public long sumOfOccurrences() {
        long sum = 0;

        for (MutableLong count : valueMap.values()) {
            sum = Math2.addExact(sum, count.value());
        }

        return sum;
    }

    public OptionalDouble averageOfOccurrences() {
        if (size() == 0) {
            return OptionalDouble.empty();
        }

        final double sum = sumOfOccurrences();

        return OptionalDouble.of(sum / size());
    }

    /**
     *
     * @param e
     * @return always true
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public boolean add(final E e) throws IllegalArgumentException {
        return add(e, 1);
    }

    /**
     *
     * @param e
     * @param occurrences
     * @return true if the specified occurrences is bigger than 0.
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public boolean add(final E e, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        MutableLong count = valueMap.get(e);

        if (count != null && occurrences > (Long.MAX_VALUE - count.value())) {
            throw new IllegalArgumentException("The total count is out of the bound of long");
        }

        if (count == null) {
            if (occurrences > 0) {
                count = MutableLong.of(occurrences);
                valueMap.put(e, count);
            }
        } else {
            count.add(occurrences);
        }

        return occurrences > 0;
    }

    /**
     * 
     * @param e
     * @return true if the specified element is absent.
     * @throws IllegalArgumentException
     */
    public boolean addIfAbsent(final E e) throws IllegalArgumentException {
        return addIfAbsent(e, 1);
    }

    /**
     * 
     * @param e
     * @param occurrences
     * @return true if the specified element is absent and occurrences is bigger than 0.
     * @throws IllegalArgumentException 
     */
    public boolean addIfAbsent(final E e, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        MutableLong count = valueMap.get(e);

        if (count == null && occurrences > 0) {
            count = MutableLong.of(occurrences);
            valueMap.put(e, count);

            return true;
        }

        return false;
    }

    public long addAndGet(final E e) {
        return addAndGet(e, 1);
    }

    public long addAndGet(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        MutableLong count = valueMap.get(e);

        if (count != null && occurrences > (Long.MAX_VALUE - count.value())) {
            throw new IllegalArgumentException("The total count is out of the bound of long");
        }

        if (count == null) {
            if (occurrences > 0) {
                count = MutableLong.of(occurrences);
                valueMap.put(e, count);
            }
        } else {
            count.add(occurrences);
        }

        return count == null ? 0 : count.value();
    }

    public long getAndAdd(final E e) {
        return getAndAdd(e, 1);
    }

    public long getAndAdd(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        MutableLong count = valueMap.get(e);

        if (count != null && occurrences > (Long.MAX_VALUE - count.value())) {
            throw new IllegalArgumentException("The total count is out of the bound of long");
        }

        final long result = count == null ? 0 : count.value();

        if (count == null) {
            if (occurrences > 0) {
                count = MutableLong.of(occurrences);
                valueMap.put(e, count);
            }
        } else {
            count.add(occurrences);
        }

        return result;
    }

    /**
     * 
     * @param c
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public boolean addAll(final Collection<? extends E> c) throws IllegalArgumentException {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return addAll(c, 1);
    }

    /**
     * 
     * @param c
     * @param occurrences
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public boolean addAll(final Collection<? extends E> c, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        if (N.isNullOrEmpty(c) || occurrences == 0) {
            return false;
        }

        for (E e : c) {
            add(e, occurrences);
        }

        return occurrences > 0;
    }

    /**
     * 
     * @param m
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public boolean addAll(final Map<? extends E, Long> m) throws IllegalArgumentException {
        if (N.isNullOrEmpty(m)) {
            return false;
        }

        for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
            checkOccurrences(entry.getValue().longValue());
        }

        boolean result = false;

        for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
            if (result == false) {
                result = add(entry.getKey(), entry.getValue().longValue());
            } else {
                add(entry.getKey(), entry.getValue().longValue());
            }
        }

        return result;
    }

    /**
     * 
     * @param m
     * @throws IllegalArgumentException if the occurrences of element is less than 0.
     */
    public boolean addAll(final LongMultiset<? extends E> multiset) throws IllegalArgumentException {
        if (N.isNullOrEmpty(multiset)) {
            return false;
        }

        for (Map.Entry<? extends E, MutableLong> entry : multiset.valueMap.entrySet()) {
            add(entry.getKey(), entry.getValue().value());
        }

        return true;
    }

    public boolean contains(final Object o) {
        return valueMap.containsKey(o);
    }

    public boolean containsAll(final Collection<?> c) {
        return valueMap.keySet().containsAll(c);
    }

    /**
     * Remove one occurrence from the specified elements. 
     * The element will be removed from this <code>Multiset</code> if the occurrences equals to or less than 0 after the operation.
     *
     * @param e
     * @param occurrences
     * @return
     */
    public boolean remove(final Object e) {
        return remove(e, 1);
    }

    /**
     * Remove the specified occurrences from the specified element. 
     * The element will be removed from this <code>Multiset</code> if the occurrences equals to or less than 0 after the operation.
     *
     * @param e
     * @param occurrences
     * @return
     */
    public boolean remove(final Object e, final long occurrences) {
        checkOccurrences(occurrences);

        final MutableLong count = valueMap.get(e);

        if (count == null) {
            return false;
        } else {
            count.subtract(occurrences);

            if (count.value() <= 0) {
                valueMap.remove(e);
            }

            return occurrences > 0;
        }
    }

    public long removeAndGet(final Object e) {
        return removeAndGet(e, 1);
    }

    public long removeAndGet(final Object e, final long occurrences) {
        checkOccurrences(occurrences);

        final MutableLong count = valueMap.get(e);

        if (count == null) {
            return 0;
        } else {
            count.subtract(occurrences);

            if (count.value() <= 0) {
                valueMap.remove(e);
            }

            return count.value() > 0 ? count.value() : 0;
        }
    }

    public long getAndRemove(final Object e) {
        return getAndRemove(e, 1);
    }

    public long getAndRemove(final Object e, final long occurrences) {
        checkOccurrences(occurrences);

        final MutableLong count = valueMap.get(e);
        final long result = count == null ? 0 : count.value();

        if (count != null) {
            count.subtract(occurrences);

            if (count.value() <= 0) {
                valueMap.remove(e);
            }
        }

        return result;
    }

    /**
     * 
     * @param e
     * @return the occurrences of the specified element before it's removed.
     */
    public long removeAllOccurrences(final Object e) {
        final MutableLong count = valueMap.remove(e);

        return count == null ? 0 : count.value();
    }

    public boolean removeAllOccurrencesIf(Predicate<? super E> predicate) {
        Set<E> removingKeys = null;

        for (E key : this.valueMap.keySet()) {
            if (predicate.test(key)) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(key);
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        removeAll(removingKeys);

        return true;
    }

    public boolean removeAllOccurrencesIf(BiPredicate<? super E, ? super Long> predicate) {
        Set<E> removingKeys = null;

        for (Map.Entry<E, MutableLong> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue().value())) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(entry.getKey());
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        removeAll(removingKeys);

        return true;
    }

    public boolean removeIf(final long occurrences, Predicate<? super E> predicate) {
        checkOccurrences(occurrences);

        Set<E> removingKeys = null;

        for (E key : this.valueMap.keySet()) {
            if (predicate.test(key)) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(key);
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        removeAll(removingKeys, occurrences);

        return true;
    }

    public boolean removeIf(final long occurrences, BiPredicate<? super E, ? super Long> predicate) {
        checkOccurrences(occurrences);

        Set<E> removingKeys = null;

        for (Map.Entry<E, MutableLong> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue().value())) {
                if (removingKeys == null) {
                    removingKeys = new HashSet<>();
                }

                removingKeys.add(entry.getKey());
            }
        }

        if (N.isNullOrEmpty(removingKeys)) {
            return false;
        }

        removeAll(removingKeys, occurrences);

        return true;
    }

    /**
     * Removes all of this Multiset's elements that are also contained in the
     * specified collection (optional operation).  After this call returns,
     * this Multiset will contain no elements in common with the specified
     * collection. This method ignores how often any element might appear in
     * {@code c}, and only cares whether or not an element appears at all.
     * 
     * @param c
     * @return <tt>true</tt> if this set changed as a result of the call
     * @see Collection#removeAll(Collection)
     */
    public boolean removeAll(final Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        boolean result = false;

        for (Object e : c) {
            if (result == false) {
                result = valueMap.remove(e) != null;
            } else {
                valueMap.remove(e);
            }
        }

        return result;
    }

    /**
     * Remove the specified occurrences from the specified elements. 
     * The elements will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param c
     * @param occurrences
     *            the occurrences to remove if the element is in the specified collection <code>c</code>.
     * @return <tt>true</tt> if this set changed as a result of the call
     */
    public boolean removeAll(final Collection<?> c, final long occurrences) {
        checkOccurrences(occurrences);

        if (N.isNullOrEmpty(c) || occurrences == 0) {
            return false;
        }

        boolean result = false;

        for (Object e : c) {
            if (result == false) {
                result = remove(e, occurrences);
            } else {
                remove(e, occurrences);
            }
        }

        return result;
    }

    /**
     * 
     * @param m
     * @return
     */
    public boolean removeAll(final Map<?, Long> m) {
        if (N.isNullOrEmpty(m)) {
            return false;
        }

        for (Map.Entry<?, Long> entry : m.entrySet()) {
            checkOccurrences(entry.getValue().longValue());
        }

        boolean result = false;

        for (Map.Entry<?, Long> entry : m.entrySet()) {
            if (result == false) {
                result = remove(entry.getKey(), entry.getValue().longValue());
            } else {
                remove(entry.getKey(), entry.getValue().longValue());
            }
        }

        return result;
    }

    /**
     * 
     * @param m
     */
    public boolean removeAll(final LongMultiset<?> multiset) throws IllegalArgumentException {
        if (N.isNullOrEmpty(multiset)) {
            return false;
        }

        for (Map.Entry<?, MutableLong> entry : multiset.valueMap.entrySet()) {
            remove(entry.getKey(), entry.getValue().value());
        }

        return true;
    }

    /**
     * The associated elements will be removed if zero or negative occurrences are returned by the specified <code>function</code>.
     * 
     * @param function
     */
    public void replaceAll(BiFunction<? super E, ? super Long, Long> function) {
        List<E> keyToRemove = null;
        Long newVal = null;

        for (Map.Entry<E, MutableLong> entry : this.valueMap.entrySet()) {
            newVal = function.apply(entry.getKey(), entry.getValue().value());

            if (newVal == null || newVal.longValue() <= 0) {
                if (keyToRemove == null) {
                    keyToRemove = new ArrayList<>();
                }

                keyToRemove.add(entry.getKey());
            } else {
                entry.getValue().setValue(newVal);
            }
        }

        if (N.notNullOrEmpty(keyToRemove)) {
            for (E key : keyToRemove) {
                valueMap.remove(key);
            }
        }
    }

    public boolean replaceIf(Predicate<? super E> predicate, final long newOccurrences) {
        checkNewOccurrences(newOccurrences);

        boolean modified = false;

        for (Map.Entry<E, MutableLong> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey())) {
                entry.getValue().setValue(newOccurrences);

                modified = true;
            }
        }

        return modified;
    }

    public boolean replaceIf(BiPredicate<? super E, ? super Long> predicate, final long newOccurrences) {
        checkNewOccurrences(newOccurrences);

        boolean modified = false;

        for (Map.Entry<E, MutableLong> entry : this.valueMap.entrySet()) {
            if (predicate.test(entry.getKey(), entry.getValue().value())) {
                entry.getValue().setValue(newOccurrences);

                modified = true;
            }
        }

        return modified;
    }

    /**
     * Retains only the elements in this collection that are contained in the
     * specified collection (optional operation).  In other words, removes from
     * this collection all of its elements that are not contained in the
     * specified collection.
     *
     * @param c
     * @return <tt>true</tt> if this set changed as a result of the call
     * @see Collection#retainAll(Collection)
     */
    public boolean retainAll(final Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            boolean result = size() > 0;
            clear();
            return result;
        }

        Set<E> others = null;

        for (E e : valueMap.keySet()) {
            if (!c.contains(e)) {
                if (others == null) {
                    others = new HashSet<>(valueMap.size());
                }

                others.add(e);
            }
        }

        return N.isNullOrEmpty(others) ? false : removeAll(others, Long.MAX_VALUE);
    }

    public Set<E> elements() {
        return valueMap.keySet();
    }

    public int size() {
        return valueMap.size();
    }

    public boolean isEmpty() {
        return valueMap.isEmpty();
    }

    public void clear() {
        valueMap.clear();
    }

    @Override
    public Iterator<E> iterator() {
        return valueMap.keySet().iterator();
    }

    //    public Set<Map.Entry<E, MutableLong>> entrySet() {
    //        return valueMap.entrySet();
    //    }

    public Object[] toArray() {
        return valueMap.keySet().toArray();
    }

    public <T> T[] toArray(final T[] a) {
        return valueMap.keySet().toArray(a);
    }

    public Map<E, Long> toMap() {
        final Map<E, Long> result = valueMap instanceof IdentityHashMap ? new IdentityHashMap<E, Long>(N.initHashCapacity(size()))
                : new HashMap<E, Long>(N.initHashCapacity(size()));

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            result.put(entry.getKey(), entry.getValue().value());
        }

        return result;
    }

    public <M extends Map<E, Long>> M toMap(final IntFunction<M> supplier) {
        final M result = supplier.apply(size());

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            result.put(entry.getKey(), entry.getValue().value());
        }

        return result;
    }

    @SuppressWarnings("rawtypes")
    public Map<E, Long> toMapSortedByOccurrences() {
        return toMapSortedBy((Comparator) cmpByCount);
    }

    public Map<E, Long> toMapSortedByOccurrences(final Comparator<? super Long> cmp) {
        return toMapSortedBy(new Comparator<Map.Entry<E, MutableLong>>() {
            @Override
            public int compare(Entry<E, MutableLong> o1, Entry<E, MutableLong> o2) {
                return cmp.compare(o1.getValue().value(), o2.getValue().value());
            }
        });
    }

    public Map<E, Long> toMapSortedByKey(final Comparator<? super E> cmp) {
        return toMapSortedBy(new Comparator<Map.Entry<E, MutableLong>>() {
            @Override
            public int compare(Entry<E, MutableLong> o1, Entry<E, MutableLong> o2) {
                return cmp.compare(o1.getKey(), o2.getKey());
            }
        });
    }

    Map<E, Long> toMapSortedBy(final Comparator<Map.Entry<E, MutableLong>> cmp) {
        if (N.isNullOrEmpty(valueMap)) {
            return new LinkedHashMap<>();
        }

        final Map.Entry<E, MutableLong>[] entries = valueMap.entrySet().toArray(new Map.Entry[size()]);
        Arrays.sort(entries, cmp);

        final Map<E, Long> sortedValues = new LinkedHashMap<>(N.initHashCapacity(size()));

        for (Map.Entry<E, MutableLong> entry : entries) {
            sortedValues.put(entry.getKey(), entry.getValue().value());
        }

        return sortedValues;
    }

    /**
     * 
     * @return a list with all elements, each of them is repeated with the occurrences in this <code>LongMultiset</code>   
     */
    public List<E> flatten() {
        final long totalOccurrences = sumOfOccurrences();

        if (totalOccurrences > Integer.MAX_VALUE) {
            throw new RuntimeException("The total occurrences(" + totalOccurrences + ") is bigger than the max value of int.");
        }

        final Object[] a = new Object[(int) totalOccurrences];

        int fromIndex = 0;
        int toIndex = 0;

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            toIndex = fromIndex + (int) entry.getValue().value();

            Arrays.fill(a, fromIndex, toIndex, entry.getKey());
            fromIndex = toIndex;
        }

        return N.asList((E[]) a);
    }

    public LongMultiset<E> filter(Predicate<? super E> filter) {
        final LongMultiset<E> result = new LongMultiset<>(valueMap instanceof IdentityHashMap ? IdentityHashMap.class : LinkedHashMap.class);

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey())) {
                result.add(entry.getKey(), entry.getValue().longValue());
            }
        }

        return result;
    }

    public LongMultiset<E> filter(BiPredicate<? super E, Long> filter) {
        final LongMultiset<E> result = new LongMultiset<>(valueMap instanceof IdentityHashMap ? IdentityHashMap.class : LinkedHashMap.class);

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            if (filter.test(entry.getKey(), entry.getValue().longValue())) {
                result.add(entry.getKey(), entry.getValue().longValue());
            }
        }

        return result;
    }

    public void forEach(final ObjLongConsumer<? super E> action) {
        N.requireNonNull(action);

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            action.accept(entry.getKey(), entry.getValue().value());
        }
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param seed The seed element is both the initial value of the reduction and the default result if there are no elements.
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public <R> R forEach(final R seed, TriFunction<R, ? super E, Long, R> accumulator, final TriPredicate<? super R, ? super E, Long> conditionToBreak) {
        N.requireNonNull(accumulator);
        N.requireNonNull(conditionToBreak);

        R result = seed;

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            result = accumulator.apply(result, entry.getKey(), entry.getValue().value());

            if (conditionToBreak.test(result, entry.getKey(), entry.getValue().value())) {
                break;
            }
        }

        return result;
    }

    /**
     * The implementation is equivalent to performing the following steps for this LongMultiset:
     * 
     * <pre>
     * final long oldValue = get(e);
     * 
     * if (oldValue > 0) {
     *     return oldValue;
     * }
     * 
     * final long newValue = mappingFunction.apply(e);
     * 
     * if (newValue > 0) {
     *     set(e, newValue);
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param e
     * @param mappingFunction
     * @return
     */
    public long computeIfAbsent(E e, Function<? super E, Long> mappingFunction) {
        N.requireNonNull(mappingFunction);

        final long oldValue = get(e);

        if (oldValue > 0) {
            return oldValue;
        }

        final long newValue = mappingFunction.apply(e);

        if (newValue > 0) {
            set(e, newValue);
        }

        return newValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this LongMultiset:
     * 
     * <pre> 
     * final long oldValue = get(e);
     * 
     * if (oldValue == 0) {
     *     return oldValue;
     * }
     * 
     * final long newValue = remappingFunction.apply(e, oldValue);
     * 
     * if (newValue > 0) {
     *     set(e, newValue);
     * } else {
     *     remove(e);
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param e
     * @param remappingFunction
     * @return
     */
    public long computeIfPresent(E e, BiFunction<? super E, Long, Long> remappingFunction) {
        N.requireNonNull(remappingFunction);

        final long oldValue = get(e);

        if (oldValue == 0) {
            return oldValue;
        }

        final long newValue = remappingFunction.apply(e, oldValue);

        if (newValue > 0) {
            set(e, newValue);
        } else {
            remove(e);
        }

        return newValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this LongMultiset:
     * 
     * <pre>
     * final long oldValue = get(key);
     * final long newValue = remappingFunction.apply(key, oldValue);
     * 
     * if (newValue > 0) {
     *     set(key, newValue);
     * } else {
     *     if (oldValue > 0) {
     *         remove(key);
     *     }
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param key
     * @param remappingFunction
     * @return
     */
    public long compute(E key, BiFunction<? super E, Long, Long> remappingFunction) {
        N.requireNonNull(remappingFunction);

        final long oldValue = get(key);
        final long newValue = remappingFunction.apply(key, oldValue);

        if (newValue > 0) {
            set(key, newValue);
        } else {
            if (oldValue > 0) {
                remove(key);
            }
        }

        return newValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this LongMultiset:
     * 
     * <pre>
     * long oldValue = get(key);
     * long newValue = (oldValue == 0) ? value : remappingFunction.apply(oldValue, value);
     * 
     * if (newValue > 0) {
     *     set(key, newValue);
     * } else {
     *     if (oldValue > 0) {
     *         remove(key);
     *     }
     * }
     * 
     * return newValue;
     * </pre>
     * 
     * @param key
     * @param value
     * @param remappingFunction
     * @return
     */
    public long merge(E key, long value, BiFunction<Long, Long, Long> remappingFunction) {
        N.requireNonNull(remappingFunction);
        N.requireNonNull(value);

        long oldValue = get(key);
        long newValue = (oldValue == 0) ? value : remappingFunction.apply(oldValue, value);

        if (newValue > 0) {
            set(key, newValue);
        } else {
            if (oldValue > 0) {
                remove(key);
            }
        }

        return newValue;
    }

    public Stream<Entry<E, Long>> stream() {
        return Stream.of(valueMap.entrySet()).map(new Function<Map.Entry<E, MutableLong>, Map.Entry<E, Long>>() {
            @Override
            public Entry<E, Long> apply(Entry<E, MutableLong> t) {
                return new AbstractMap.SimpleImmutableEntry<>(t.getKey(), t.getValue().value());
            }
        });
    }

    public EntryStream<E, Long> entryStream() {
        return EntryStream.of(stream());
    }

    @Override
    public int hashCode() {
        return valueMap.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        return obj == this || (obj instanceof LongMultiset && valueMap.equals(((LongMultiset<E>) obj).valueMap));
    }

    @Override
    public String toString() {
        return valueMap.toString();
    }

    private static void checkOccurrences(final long occurrences) {
        if (occurrences < 0) {
            throw new IllegalArgumentException("The specified 'occurrences' can not be less than 0");
        }
    }

    private static void checkNewOccurrences(final long newOccurrences) {
        if (newOccurrences < 1) {
            throw new IllegalArgumentException("The specified 'newOccurrences' can not be less than 1");
        }
    }
}
