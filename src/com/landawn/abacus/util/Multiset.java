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
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.annotation.Internal;

/**
 * A collection that supports order-independent equality, like {@link Set}, but
 * may have duplicate elements.
 *
 * <p>Elements of a Multiset that are equal to one another are referred to as
 * <i>occurrences</i> of the same single element. The total number of
 * occurrences of an element in a Multiset is called the <i>count</i> of that
 * element (the terms "frequency" and "multiplicity" are equivalent, but not
 * used in this API). Since the count of an element is represented as an {@code
 * int}, a Multiset may never contain more than {@link Integer#MAX_VALUE}
 * occurrences of any one element.
 *
 * @param <E>
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public final class Multiset<E> implements Iterable<E> {
    private final Map<E, Integer> valueMap;

    public Multiset() {
        this(HashMap.class);
    }

    @SuppressWarnings("rawtypes")
    public Multiset(final Class<? extends Map> valueMapType) {
        this(N.newInstance(valueMapType));
    }

    /**
     *
     * @param valueMap The valueMap and this Multiset share the same data; any changes to one will appear in the other.
     */
    @Internal
    Multiset(final Map<E, Integer> valueMap) {
        this.valueMap = valueMap;
    }

    public Multiset(final Collection<? extends E> c) {
        this();

        addAll(c);
    }

    /**
     *
     * @param e
     * @return the occurrences of the specified object. zero is returned if it's not in this set.
     */
    public int get(final Object e) {
        Integer count = valueMap.get(e);

        if (count == null) {
            return 0;
        } else {
            return count;
        }
    }

    /**
     * The element will be removed if the specified count is 0.
     *
     * @param e
     * @param count
     * @return the previous count associated with element, or 0 if the element not exists. 
     */
    public int set(final E e, final int count) {
        if (count < 0) {
            throw new IllegalArgumentException("The specified 'occurrences' can not be less than 0");
        }

        Integer prev = null;

        if (count == 0) {
            prev = valueMap.remove(e);
        } else {
            prev = valueMap.put(e, count);
        }

        return prev == null ? 0 : prev.intValue();
    }

    public void setAll(final Map<? extends E, Integer> m) {
        for (Map.Entry<? extends E, Integer> entry : m.entrySet()) {
            set(entry.getKey(), entry.getValue());
        }
    }

    public int maxCount() {
        if (valueMap.size() == 0) {
            return 0;
        }

        int maxCount = 0;

        for (Integer count : valueMap.values()) {
            if (count > maxCount) {
                maxCount = count;
            }
        }

        return maxCount;
    }

    public int minCount() {
        if (valueMap.size() == 0) {
            return 0;
        }

        int minCount = Integer.MAX_VALUE;

        for (Integer count : valueMap.values()) {
            if (count < minCount) {
                minCount = count;
            }
        }

        return minCount;
    }

    public long sumCount() {
        long sum = 0;

        for (Integer count : valueMap.values()) {
            sum += count;
        }

        return sum;
    }

    public double avgCount() {
        if (size() == 0) {
            return 0d;
        }

        double sum = sumCount();

        return sum / size();
    }

    @Beta
    int countOf(final Object e) {
        return get(e);
    }

    public E elementOfMaxCount() {
        E maxCountElement = null;
        int maxCount = 0;
        int count = 0;

        for (E k : valueMap.keySet()) {
            count = valueMap.get(k);

            if (maxCountElement == null || count > maxCount) {
                maxCount = count;
                maxCountElement = k;
            }
        }

        return maxCountElement;
    }

    public E elementOfMinCount() {
        E minCountElement = null;
        int minCount = Integer.MAX_VALUE;
        int count = 0;

        for (E k : valueMap.keySet()) {
            count = valueMap.get(k);

            if (minCountElement == null || count < minCount) {
                minCount = count;
                minCountElement = k;
            }
        }

        return minCountElement;
    }

    public Map<E, Integer> toMap() {
        return N.newLinkedHashMap(valueMap);
    }

    public Map<E, Integer> toMapSortedByCount() {
        if (N.isNullOrEmpty(valueMap)) {
            return N.newLinkedHashMap();
        }

        final Map.Entry<E, Integer>[] entries = entrySet().toArray(new Map.Entry[size()]);

        final Comparator<Map.Entry<E, Integer>> cmp = new Comparator<Map.Entry<E, Integer>>() {
            @Override
            public int compare(final Map.Entry<E, Integer> o1, final Map.Entry<E, Integer> o2) {
                return o1.getValue() - o2.getValue();
            }
        };

        Arrays.sort(entries, cmp);

        final Map<E, Integer> sortedValues = N.newLinkedHashMap(N.initHashCapacity(size()));

        for (Map.Entry<E, Integer> entry : entries) {
            sortedValues.put(entry.getKey(), entry.getValue());
        }

        return sortedValues;
    }

    public Map<E, Integer> toMapSortedByElement() {
        if (N.isNullOrEmpty(valueMap)) {
            return N.newLinkedHashMap();
        }

        if (valueMap instanceof SortedMap) {
            return N.newLinkedHashMap(valueMap);
        } else {
            final Map.Entry<? extends Comparable<E>, Integer>[] entries = entrySet().toArray(new Map.Entry[size()]);

            final Comparator<Map.Entry<? extends Comparable<E>, Integer>> cmp = new Comparator<Map.Entry<? extends Comparable<E>, Integer>>() {
                @Override
                public int compare(final Map.Entry<? extends Comparable<E>, Integer> o1, final Map.Entry<? extends Comparable<E>, Integer> o2) {
                    return (o1.getKey() == null) ? ((o2.getKey() == null) ? 0 : (-1)) : ((o2.getKey() == null) ? 1 : o1.getKey().compareTo((E) o2.getKey()));
                }
            };

            Arrays.sort(entries, cmp);

            final Map<E, Integer> sortedValues = N.newLinkedHashMap(N.initHashCapacity(size()));
            Map.Entry<E, Integer>[] newEntries = (Entry<E, Integer>[]) entries;

            for (Map.Entry<E, Integer> entry : newEntries) {
                sortedValues.put(entry.getKey(), entry.getValue());
            }

            return sortedValues;
        }
    }

    public Map<E, Integer> toMapSortedByElement(final Comparator<? super E> cmp) {
        if (N.isNullOrEmpty(valueMap)) {
            return N.newLinkedHashMap();
        }

        if (valueMap instanceof SortedMap && cmp == null) {
            return N.newLinkedHashMap(valueMap);
        } else {
            final Comparator<? super E> comparator = cmp == null ? N.comparableCmp : cmp;
            final Map.Entry<E, Integer>[] entries = entrySet().toArray(new Map.Entry[size()]);
            final Comparator<Map.Entry<E, Integer>> entryCmp = new Comparator<Map.Entry<E, Integer>>() {
                @Override
                public int compare(final Map.Entry<E, Integer> o1, final Map.Entry<E, Integer> o2) {
                    return comparator.compare(o1.getKey(), o2.getKey());
                }
            };

            Arrays.sort(entries, entryCmp);

            final Map<E, Integer> sortedValues = N.newLinkedHashMap(N.initHashCapacity(size()));

            for (Map.Entry<E, Integer> entry : entries) {
                sortedValues.put(entry.getKey(), entry.getValue());
            }

            return sortedValues;
        }
    }

    /**
     *
     * @param e
     * @return the count of the element after the operation.
     */
    public int add(final E e) {
        return add(e, 1);
    }

    /**
     *
     * @param e
     * @param occurrences
     * @return the count of the element after the operation.
     */
    public int add(final E e, final int occurrences) {
        /*
        if (occurrences < 1) {
            throw new IllegalArgumentException("The specified 'occurrences' must be greater than 0");
        }
        */

        int count = get(e);

        if (occurrences > (Integer.MAX_VALUE - count)) {
            throw new IllegalArgumentException("The total count is out of the bound of integer");
        }

        count += occurrences;

        if (count > 0) {
            valueMap.put(e, count);
        } else {
            valueMap.remove(e);
        }

        return count;
    }

    public void addAll(final Collection<? extends E> c) {
        addAll(c, 1);
    }

    public void addAll(final Collection<? extends E> c, final int occurrences) {
        for (E e : c) {
            add(e, occurrences);
        }
    }

    public void addAll(final Map<? extends E, Integer> m) {
        for (Map.Entry<? extends E, Integer> entry : m.entrySet()) {
            add(entry.getKey(), entry.getValue());
        }
    }

    public boolean contains(final Object o) {
        return valueMap.containsKey(o);
    }

    public boolean containsAll(final Collection<? extends E> c) {
        return valueMap.keySet().containsAll(c);
    }

    /**
     * The element will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param e
     * @param occurrences
     * @return the count of the element after the operation. It could be a negative number if the present occurrences is less than the specified <code>occurrences</code> to remove.
     */
    public int remove(final E e) {
        return remove(e, 1);
    }

    /**
     * The element will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param e
     * @param occurrences
     * @return the count of the element after the operation. It could be a negative number if the present occurrences is less than the specified <code>occurrences</code> to remove.
     */
    public int remove(final E e, final int occurrences) {
        /*
        if (occurrences < 1) {
            throw new IllegalArgumentException("The specified 'occurrences' must be greater than 0");
        }
        */

        int count = get(e);

        if (occurrences < (count - Integer.MAX_VALUE)) {
            throw new IllegalArgumentException("The total count is out of the bound of integer");
        }

        count -= occurrences;

        if (count > 0) {
            valueMap.put(e, count);
        } else {
            valueMap.remove(e);
        }

        return count;
    }

    /**
     * The elements will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param c
     * @return <tt>true</tt> if this set changed as a result of the call
     */
    public boolean removeAll(final Collection<? extends E> c) {
        return removeAll(c, 1);
    }

    /**
     * The elements will be removed from this set if the occurrences equals to or less than 0 after the operation.
     *
     * @param c
     * @param occurrences
     *            the occurrences to remove if the element is in the specified collection <code>c</code>.
     * @return <tt>true</tt> if this set changed as a result of the call
     */
    public boolean removeAll(final Collection<? extends E> c, final int occurrences) {
        boolean result = false;

        for (E e : c) {
            if (result == false) {
                result = valueMap.containsKey(e);
            }

            remove(e, occurrences);
        }

        return result;
    }

    public boolean removeAll(final Map<? extends E, Integer> m) {
        boolean result = false;

        for (Map.Entry<? extends E, Integer> entry : m.entrySet()) {
            if (result == false) {
                result = valueMap.containsKey(entry.getKey());
            }

            remove(entry.getKey(), entry.getValue());
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
    public boolean retainAll(final Collection<? extends E> c) {
        Set<E> others = null;

        for (E e : valueMap.keySet()) {
            if (!c.contains(e)) {
                if (others == null) {
                    others = N.newHashSet(valueMap.size());
                }

                others.add(e);
            }
        }

        return N.isNullOrEmpty(others) ? false : removeAll(others, Integer.MAX_VALUE);
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

    public Set<Map.Entry<E, Integer>> entrySet() {
        return valueMap.entrySet();
    }

    public Object[] toArray() {
        return valueMap.keySet().toArray();
    }

    public <T> T[] toArray(final T[] a) {
        return valueMap.keySet().toArray(a);
    }

    @Override
    public int hashCode() {
        return valueMap.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        return obj == this || (obj instanceof Multiset && valueMap.equals(((Multiset<E>) obj).valueMap));
    }

    @Override
    public String toString() {
        return valueMap.toString();
    }
}
