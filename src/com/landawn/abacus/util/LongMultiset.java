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

import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.landawn.abacus.annotation.Internal;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.stream.Stream;

/**
 * A collection that supports order-independent equality, like {@link Set}, but
 * may have duplicate elements.
 *
 * <p>Elements of a Multiset that are equal to one another are referred to as
 * <i>occurrences</i> of the same single element. The total number of
 * occurrences of an element in a Multiset is called the <i>count</i> of that
 * element (the terms "frequency" and "multiplicity" are equivalent, but not
 * used in this API). Since the count of an element is represented as an {@code
 * long}, a Multiset may never contain more than {@link Long#MAX_VALUE}
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
            return N.compare(a.getValue().longValue(), b.getValue().longValue());
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

    /**
     *
     * @param valueMap The valueMap and this Multiset share the same data; any changes to one will appear in the other.
     */
    @Internal
    LongMultiset(final Map<E, MutableLong> valueMap) {
        this.valueMap = valueMap;
    }

    public LongMultiset(final Collection<? extends E> c) {
        this();

        addAll(c);
    }

    public static <T> LongMultiset<T> of(final T... a) {
        final LongMultiset<T> multiset = new LongMultiset<T>(new HashMap<T, MutableLong>(N.initHashCapacity(a.length)));

        for (T e : a) {
            multiset.add(e);
        }

        return multiset;
    }

    //    @SuppressWarnings("rawtypes")
    //    static <T> Multiset<T> of(final Class<? extends Map> valueMapType, final T... a) {
    //        final Multiset<T> multiset = new Multiset<T>(valueMapType);
    //
    //        for (T e : a) {
    //            multiset.add(e);
    //        }
    //
    //        return multiset;
    //    }

    public static <T> LongMultiset<T> of(final Collection<? extends T> coll) {
        return new LongMultiset<T>(coll);
    }

    //    @SuppressWarnings("rawtypes")
    //    static <T> Multiset<T> of(final Class<? extends Map> valueMapType, final Collection<T> coll) {
    //        final Multiset<T> multiset = new Multiset<T>(valueMapType);
    //
    //        multiset.addAll(coll);
    //
    //        return multiset;
    //    }

    public static <T> LongMultiset<T> from(final Map<? extends T, Long> m) {
        final LongMultiset<T> multiset = new LongMultiset<T>(N.initHashCapacity(m.size()));

        multiset.setAll(m);

        return multiset;
    }

    //    @SuppressWarnings("rawtypes")
    //    public static <T> Multiset<T> from(final Class<? extends Map> valueMapType, final Map<? extends T, Long> m) {
    //        final Multiset<T> multiset = new Multiset<T>(valueMapType);
    //
    //        multiset.setAll(m);
    //
    //        return multiset;
    //    }

    /**
     *
     * @param e
     * @return the occurrences of the specified object. zero is returned if it's not in this set.
     */
    public long get(final Object e) {
        MutableLong count = valueMap.get(e);

        if (count == null) {
            return 0;
        } else {
            return count.longValue();
        }
    }

    public long getOrDefault(final Object e, long defaultValue) {
        MutableLong count = valueMap.get(e);

        if (count == null) {
            return defaultValue;
        } else {
            return count.longValue();
        }
    }

    /**
     * The element will be removed if the specified count is 0.
     *
     * @param e
     * @param occurrences
     * @return the previous count associated with element, or 0 if the element not exists. 
     * @throws IllegalArgumentException if the occurrences of element is less than 0
     */
    public long set(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        if (occurrences == 0) {
            MutableLong value = valueMap.remove(e);

            return value == null ? 0 : value.longValue();
        } else {
            MutableLong value = valueMap.get(e);

            if (value == null) {
                valueMap.put(e, MutableLong.of(occurrences));

                return 0;
            } else {
                long result = value.longValue();
                value.setValue(occurrences);
                return result;
            }
        }
    }

    /**
     * 
     * @param m
     * @throws IllegalArgumentException if the occurrences of element is less than 0.
     */
    public void setAll(final Map<? extends E, Long> m) throws IllegalArgumentException {
        for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
            checkOccurrences(entry.getValue().intValue());
        }

        for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
            set(entry.getKey(), entry.getValue().longValue());
        }
    }

    /**
     * 
     * @param m
     * @throws IllegalArgumentException if the occurrences of element is less than 0.
     */
    public void setAll(final LongMultiset<? extends E> multiset) throws IllegalArgumentException {
        for (Map.Entry<? extends E, MutableLong> entry : multiset.entrySet()) {
            set(entry.getKey(), entry.getValue().longValue());
        }
    }

    public Optional<Map.Entry<E, Long>> minOccurrences() {
        if (size() == 0) {
            return Optional.empty();
        }

        final Iterator<Map.Entry<E, MutableLong>> it = valueMap.entrySet().iterator();
        Map.Entry<E, MutableLong> entry = it.next();
        E minCountElement = entry.getKey();
        long minCount = entry.getValue().longValue();

        while (it.hasNext()) {
            entry = it.next();

            if (entry.getValue().longValue() < minCount) {
                minCountElement = entry.getKey();
                minCount = entry.getValue().longValue();
            }
        }

        return Optional.of((Map.Entry<E, Long>) MapEntry.of(minCountElement, minCount));
    }

    public Optional<Map.Entry<E, Long>> maxOccurrences() {
        if (size() == 0) {
            return Optional.empty();
        }

        final Iterator<Map.Entry<E, MutableLong>> it = valueMap.entrySet().iterator();
        Map.Entry<E, MutableLong> entry = it.next();
        E maxCountElement = entry.getKey();
        long maxCount = entry.getValue().longValue();

        while (it.hasNext()) {
            entry = it.next();

            if (entry.getValue().longValue() > maxCount) {
                maxCountElement = entry.getKey();
                maxCount = entry.getValue().longValue();
            }
        }

        return Optional.of((Map.Entry<E, Long>) MapEntry.of(maxCountElement, maxCount));
    }

    public Long sumOfOccurrences() {
        long sum = 0;

        for (MutableLong count : valueMap.values()) {
            sum += count.longValue();
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

    public Map<E, Long> toMap() {
        final Map<E, Long> result = new LinkedHashMap<>(N.initHashCapacity(size()));

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            result.put(entry.getKey(), entry.getValue().longValue());
        }

        return result;
    }

    @SuppressWarnings("rawtypes")
    public Map<E, Long> toMapSortedByOccurrences() {
        return toMapSortedBy((Comparator) cmpByCount);
    }

    public Map<E, Long> toMapSortedBy(final Comparator<Map.Entry<E, MutableLong>> cmp) {
        if (N.isNullOrEmpty(valueMap)) {
            return new LinkedHashMap<>();
        }

        final Map.Entry<E, MutableLong>[] entries = entrySet().toArray(new Map.Entry[size()]);
        Arrays.sort(entries, cmp);

        final Map<E, Long> sortedValues = new LinkedHashMap<>(N.initHashCapacity(size()));

        for (Map.Entry<E, MutableLong> entry : entries) {
            sortedValues.put(entry.getKey(), entry.getValue().longValue());
        }

        return sortedValues;
    }

    /**
     *
     * @param e
     * @return the count of the element after the operation.
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public long add(final E e) throws IllegalArgumentException {
        return add(e, 1);
    }

    /**
     *
     * @param e
     * @param occurrences
     * @return the count of the element after the operation.
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public long add(final E e, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        MutableLong count = valueMap.get(e);

        if (count != null && occurrences > (Long.MAX_VALUE - count.longValue())) {
            throw new IllegalArgumentException("The total count is out of the bound of long");
        }

        if (count == null) {
            count = MutableLong.of(occurrences);

            if (count.longValue() > 0) {
                valueMap.put(e, count);
            }
        } else {
            count.add(occurrences);

            if (count.longValue() <= 0) {
                valueMap.remove(e);
            }
        }

        return count.longValue();
    }

    public long addIfAbsent(final E e) throws IllegalArgumentException {
        return addIfAbsent(e, 1);
    }

    public long addIfAbsent(final E e, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        final long oldValue = get(e);

        if (oldValue == 0) {
            return add(e, occurrences);
        }

        return oldValue;
    }

    public long addAndGet(final E e) {
        add(e);

        return get(e);
    }

    public long getAndAdd(final E e) {
        final long result = get(e);

        add(e);

        return result;
    }

    public long addAndGet(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        add(e, occurrences);

        return get(e);
    }

    public long getAndAdd(final E e, final long occurrences) {
        checkOccurrences(occurrences);

        final long result = get(e);

        add(e, occurrences);

        return result;
    }

    /**
     * 
     * @param c
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public void addAll(final Collection<? extends E> c) throws IllegalArgumentException {
        addAll(c, 1);
    }

    /**
     * 
     * @param c
     * @param occurrences
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public void addAll(final Collection<? extends E> c, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        for (E e : c) {
            add(e, occurrences);
        }
    }

    /**
     * 
     * @param m
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public void addAll(final Map<? extends E, Long> m) throws IllegalArgumentException {
        for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
            checkOccurrences(entry.getValue().longValue());
        }

        for (Map.Entry<? extends E, Long> entry : m.entrySet()) {
            add(entry.getKey(), entry.getValue().longValue());
        }
    }

    /**
     * 
     * @param m
     * @throws IllegalArgumentException if the occurrences of element is less than 0.
     */
    public void addAll(final LongMultiset<? extends E> multiset) throws IllegalArgumentException {
        for (Map.Entry<? extends E, MutableLong> entry : multiset.entrySet()) {
            add(entry.getKey(), entry.getValue().longValue());
        }
    }

    public boolean contains(final Object o) {
        return valueMap.containsKey(o);
    }

    public boolean containsAll(final Collection<?> c) {
        return valueMap.keySet().containsAll(c);
    }

    /**
     * The element will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param e
     * @param occurrences
     * @return the count of the element after the operation. It could be a negative number if the present occurrences is less than the specified <code>occurrences</code> to remove.
     */
    public long remove(final Object e) throws IllegalArgumentException {
        return remove(e, 1);
    }

    /**
     * The element will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param e
     * @param occurrences
     * @return the count of the element after the operation. It could be a negative number if the present occurrences is less than the specified <code>occurrences</code> to remove.
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public long remove(final Object e, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        MutableLong count = valueMap.get(e);

        if (count != null && occurrences < (count.longValue() - Long.MAX_VALUE)) {
            throw new IllegalArgumentException("The total count is out of the bound of long");
        }

        if (count == null) {
            count = MutableLong.of(-occurrences);

            if (count.longValue() > 0) {
                valueMap.put((E) e, count);
            }
        } else {
            count.subtract(occurrences);

            if (count.longValue() <= 0) {
                valueMap.remove(e);
            }
        }

        return count.longValue();
    }

    public long removeAndGet(final Object e) {
        remove(e);

        return get(e);
    }

    public long getAndRemove(final Object e) {
        final long result = get(e);

        remove(e);

        return result;
    }

    public long removeAndGet(final Object e, final long occurrences) {
        checkOccurrences(occurrences);

        remove(e, occurrences);

        return get(e);
    }

    public long getAndRemove(final Object e, final long occurrences) {
        checkOccurrences(occurrences);

        final long result = get(e);

        remove(e, occurrences);

        return result;
    }

    public void removeAllOccurrences(final Object e) {
        valueMap.remove(e);
    }

    /**
     * The elements will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param c
     * @return <tt>true</tt> if this set changed as a result of the call
     */
    public boolean removeAll(final Collection<?> c) {
        return removeAll(c, 1);
    }

    /**
     * The elements will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param c
     * @param occurrences
     *            the occurrences to remove if the element is in the specified collection <code>c</code>.
     * @return <tt>true</tt> if this set changed as a result of the call
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public boolean removeAll(final Collection<?> c, final long occurrences) throws IllegalArgumentException {
        checkOccurrences(occurrences);

        boolean result = false;

        for (Object e : c) {
            if (result == false) {
                result = valueMap.containsKey(e);
            }

            remove(e, occurrences);
        }

        return result;
    }

    /**
     * 
     * @param m
     * @return
     * @throws IllegalArgumentException if the occurrences of element after this operation is bigger than Long.MAX_VALUE.
     */
    public boolean removeAll(final Map<?, Long> m) throws IllegalArgumentException {
        for (Map.Entry<?, Long> entry : m.entrySet()) {
            checkOccurrences(entry.getValue().longValue());
        }

        boolean result = false;

        for (Map.Entry<?, Long> entry : m.entrySet()) {
            if (result == false) {
                result = valueMap.containsKey(entry.getKey());
            }

            remove(entry.getKey(), entry.getValue().longValue());
        }

        return result;
    }

    /**
     * 
     * @param m
     * @throws IllegalArgumentException if the occurrences of element is less than 0.
     */
    public boolean removeAll(final LongMultiset<?> multiset) throws IllegalArgumentException {
        boolean result = false;

        for (Map.Entry<?, MutableLong> entry : multiset.entrySet()) {
            if (result == false) {
                result = valueMap.containsKey(entry.getKey());
            }

            remove(entry.getKey(), entry.getValue().longValue());
        }

        return result;
    }

    /**
     * Retains only the elements in this collection that are contained in the
     * specified collection (optional operation).  In other words, removes from
     * this collection all of its elements that are not contained in the
     * specified collection.
     *
     * @param c
     * @return <tt>true</tt> if this set changed as a result of the call
     */
    public boolean retainAll(final Collection<?> c) {
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

    public Set<E> keySet() {
        return valueMap.keySet();
    }

    public Set<Map.Entry<E, MutableLong>> entrySet() {
        return valueMap.entrySet();
    }

    public Object[] toArray() {
        return valueMap.keySet().toArray();
    }

    public <T> T[] toArray(final T[] a) {
        return valueMap.keySet().toArray(a);
    }

    /**
     * 
     * @return a list with all elements, each of them is repeated with the occurrences in this <code>Multiset</code>     
     */
    public List<E> flat() {
        final Object[] a = new Object[sumOfOccurrences().intValue()];

        int fromIndex = 0;
        int toIndex = 0;

        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            toIndex = fromIndex + entry.getValue().intValue();

            Arrays.fill(a, fromIndex, toIndex, entry.getKey());
            fromIndex = toIndex;
        }

        return N.asList((E[]) a);
    }

    public void forEach(BiConsumer<? super E, MutableLong> action) {
        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            action.accept(entry.getKey(), entry.getValue());
        }
    }

    /**
     * 
     * @param action break if the action returns false.
     * @return false if it breaks, otherwise true.
     */
    public boolean forEach2(BiFunction<? super E, MutableLong, Boolean> action) {
        for (Map.Entry<E, MutableLong> entry : valueMap.entrySet()) {
            if (action.apply(entry.getKey(), entry.getValue()).booleanValue() == false) {
                return false;
            }
        }

        return true;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multiset:
     * 
     * <pre>
     * final long oldValue = get(e);
     * 
     * if (oldValue == 0) {
     *     final long newValue = mappingFunction.apply(e);
     * 
     *     if (newValue != 0) {
     *         set(e, newValue);
     *         return newValue;
     *     }
     * }
     * 
     * return oldValue;
     * </pre>
     * 
     * @param e
     * @param mappingFunction
     * @return
     */
    public long computeIfAbsent(E e, Function<? super E, Long> mappingFunction) {
        N.requireNonNull(mappingFunction);
        final long oldValue = get(e);

        if (oldValue == 0) {
            final long newValue = mappingFunction.apply(e);

            if (newValue != 0) {
                set(e, newValue);
                return newValue;
            }
        }

        return oldValue;
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multiset:
     * 
     * <pre> 
     * final long oldValue = get(e);
     * 
     * if (oldValue == 0) {
     *     return 0;
     * }
     * 
     * final long newValue = remappingFunction.apply(e, oldValue);
     * 
     * if (newValue == 0) {
     *     remove(e);
     *     return 0;
     * } else {
     *     set(e, newValue);
     *     return newValue;
     * }
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
            return 0;
        }

        final long newValue = remappingFunction.apply(e, oldValue);

        if (newValue == 0) {
            remove(e);
            return 0;
        } else {
            set(e, newValue);
            return newValue;
        }
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multiset:
     * 
     * <pre>
     * final long oldValue = get(key);
     * final long newValue = remappingFunction.apply(key, oldValue);
     * 
     * if (newValue == 0) {
     *     if (oldValue != 0) {
     *         remove(key);
     *     }
     * 
     *     return 0;
     * } else {
     *     set(key, newValue);
     *     return newValue;
     * }
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

        if (newValue == 0) {
            if (oldValue != 0) {
                remove(key);
            }

            return 0;
        } else {
            set(key, newValue);
            return newValue;
        }
    }

    /**
     * The implementation is equivalent to performing the following steps for this Multiset:
     * 
     * <pre>
     * long oldValue = get(key);
     * long newValue = (oldValue == 0) ? value : remappingFunction.apply(oldValue, value);
     * 
     * if (newValue == 0) {
     * if (oldValue != 0) {
     *         remove(key);
     *     }
     * } else {
     *     set(key, newValue);
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

        if (newValue == 0) {
            if (oldValue != 0) {
                remove(key);
            }
        } else {
            set(key, newValue);
        }

        return newValue;
    }

    public Stream<Map.Entry<E, MutableLong>> stream() {
        return Stream.of(valueMap.entrySet());
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

    private void checkOccurrences(final long occurrences) {
        if (occurrences < 0) {
            throw new IllegalArgumentException("The specified 'occurrences' can not be less than 0");
        }
    }
}
