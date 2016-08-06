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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.function.BooleanBinaryOperator;
import com.landawn.abacus.util.function.BooleanConsumer;
import com.landawn.abacus.util.function.BooleanFunction;
import com.landawn.abacus.util.function.BooleanPredicate;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class BooleanList extends AbastractArrayList<BooleanConsumer, BooleanPredicate, Boolean, boolean[], BooleanList> {
    private boolean[] elementData = N.EMPTY_BOOLEAN_ARRAY;
    private int size = 0;

    public BooleanList() {
        super();
    }

    public BooleanList(int initialCapacity) {
        this();

        elementData = new boolean[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public BooleanList(boolean[] a) {
        this(a, a.length);
    }

    public BooleanList(boolean[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static BooleanList empty() {
        return new BooleanList(N.EMPTY_BOOLEAN_ARRAY);
    }

    public static BooleanList of(boolean... a) {
        return new BooleanList(a);
    }

    public static BooleanList of(boolean[] a, int size) {
        return new BooleanList(a, size);
    }

    public static BooleanList from(String... a) {
        return from(a, 0, a.length);
    }

    public static BooleanList from(String[] a, int startIndex, int endIndex) {
        if (startIndex < 0 || endIndex < 0 || endIndex < startIndex) {
            throw new IllegalArgumentException("Invalid startIndex or endIndex: " + startIndex + ", " + endIndex);
        }

        final boolean[] elementData = new boolean[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = Boolean.valueOf(a[i]);
        }

        return of(elementData);
    }

    public static BooleanList from(List<String> c) {
        return from(c, false);
    }

    public static BooleanList from(List<String> c, boolean defaultValueForNull) {
        final boolean[] a = new boolean[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : Boolean.valueOf(e);
        }

        return of(a);
    }

    public static BooleanList from(Collection<Boolean> c) {
        return from(c, false);
    }

    public static BooleanList from(Collection<Boolean> c, boolean defaultValueForNull) {
        final boolean[] a = new boolean[c.size()];
        int idx = 0;

        for (Boolean e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
    @Override
    public boolean[] array() {
        return elementData;
    }

    /**
     * Return the first element of the array list.
     * @return
     */
    @Beta
    public OptionalBoolean findFirst() {
        return size() == 0 ? OptionalBoolean.empty() : OptionalBoolean.of(elementData[0]);
    }

    /**
     * Return the last element of the array list.
     * @return
     */
    @Beta
    public OptionalBoolean findLast() {
        return size() == 0 ? OptionalBoolean.empty() : OptionalBoolean.of(elementData[size - 1]);
    }

    public boolean get(int index) {
        rangeCheck(index);

        return elementData[index];
    }

    private void rangeCheck(int index) {
        if (index >= size) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
        }
    }

    /**
     * 
     * @param index
     * @param e
     * @return the old value in the specified position.
     */
    public boolean set(int index, boolean e) {
        rangeCheck(index);

        boolean oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(boolean e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, boolean e) {
        rangeCheckForAdd(index);

        ensureCapacityInternal(size + 1);

        int numMoved = size - index;

        if (numMoved > 0) {
            N.copy(elementData, index, elementData, index + 1, numMoved);
        }

        elementData[index] = e;

        size++;
    }

    @Override
    public void addAll(BooleanList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, BooleanList c) {
        rangeCheckForAdd(index);

        int numNew = c.size();

        ensureCapacityInternal(size + numNew); // Increments modCount

        int numMoved = size - index;

        if (numMoved > 0) {
            N.copy(elementData, index, elementData, index + numNew, numMoved);
        }

        N.copy(c.array(), 0, elementData, index, numNew);

        size += numNew;
    }

    private void rangeCheckForAdd(int index) {
        if (index > size || index < 0) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
        }
    }

    /**
     * 
     * @param e
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean remove(boolean e) {
        for (int i = 0; i < size; i++) {
            if (elementData[i] == e) {

                fastRemove(i);

                return true;
            }
        }

        return false;
    }

    /**
     * 
     * @param e
     * @param removeAllOccurrences
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean remove(boolean e, boolean removeAllOccurrences) {
        if (removeAllOccurrences) {
            int w = 0;

            for (int i = 0; i < size; i++) {
                if (elementData[i] != e) {
                    elementData[w++] = elementData[i];
                }
            }

            int numRemoved = size - w;

            if (numRemoved > 0) {
                N.fill(elementData, w, size, false);

                size = w;
            }

            return numRemoved > 0;

        } else {
            return remove(e);
        }
    }

    private void fastRemove(int index) {
        int numMoved = size - index - 1;

        if (numMoved > 0) {
            N.copy(elementData, index + 1, elementData, index, numMoved);
        }

        elementData[--size] = false; // clear to let GC do its work
    }

    @Override
    public boolean removeAll(BooleanList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean retainAll(BooleanList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(BooleanList c, boolean complement) {
        final boolean[] elementData = this.elementData;

        int w = 0;

        for (int i = 0; i < size; i++) {
            if (c.contains(elementData[i]) == complement) {
                elementData[w++] = elementData[i];
            }
        }

        int numRemoved = size - w;

        if (numRemoved > 0) {
            N.fill(elementData, w, size, false);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element
     */
    public boolean delete(int index) {
        rangeCheck(index);

        boolean oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    public boolean contains(boolean e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(BooleanList c) {
        final boolean[] srcElementData = c.array();

        for (int i = 0, srcSize = c.size(); i < srcSize; i++) {

            if (!contains(srcElementData[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public BooleanList subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new BooleanList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(boolean e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, boolean e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(boolean e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, boolean e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, BooleanConsumer action) {
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                action.accept(elementData[i]);
            }
        }
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, BooleanPredicate filter) {
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                if (filter.test(elementData[i]) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    @Override
    public boolean anyMatch(final int fromIndex, final int toIndex, BooleanPredicate filter) {
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                if (filter.test(elementData[i])) {
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    public boolean noneMatch(final int fromIndex, final int toIndex, BooleanPredicate filter) {
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                if (filter.test(elementData[i])) {
                    return false;
                }
            }
        }

        return true;
    }

    @Override
    public int count(final int fromIndex, final int toIndex, BooleanPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public BooleanList filter(final int fromIndex, final int toIndex, BooleanPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    public <R> List<R> map(final BooleanFunction<? extends R> func) {
        return map(0, size(), func);
    }

    public <R> List<R> map(final int fromIndex, final int toIndex, final BooleanFunction<? extends R> func) {
        return map(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final BooleanFunction<? extends R> func) {
        return map(collClass, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final BooleanFunction<? extends R> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.add(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap(final BooleanFunction<? extends Collection<? extends R>> func) {
        return flatMap(0, size(), func);
    }

    public <R> List<R> flatMap(final int fromIndex, final int toIndex, final BooleanFunction<? extends Collection<? extends R>> func) {
        return flatMap(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass, final BooleanFunction<? extends Collection<? extends R>> func) {
        return flatMap(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final BooleanFunction<? extends Collection<? extends R>> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap2(final BooleanFunction<R[]> func) {
        return flatMap2(0, size(), func);
    }

    public <R> List<R> flatMap2(final int fromIndex, final int toIndex, final BooleanFunction<R[]> func) {
        return flatMap2(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final BooleanFunction<R[]> func) {
        return flatMap2(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final BooleanFunction<R[]> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(Arrays.asList(func.apply(elementData[i])));
        }

        return res;
    }

    public <K> Map<K, List<Boolean>> groupBy(final BooleanFunction<? extends K> func) {
        return groupBy(0, size(), func);
    }

    public <K> Map<K, List<Boolean>> groupBy(final int fromIndex, final int toIndex, final BooleanFunction<? extends K> func) {
        return groupBy(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Boolean>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final BooleanFunction<? extends K> func) {
        return groupBy(HashMap.class, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Boolean>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final BooleanFunction<? extends K> func) {
        return groupBy(HashMap.class, List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Boolean>, R extends Map<? super K, V>> R groupBy(final Class<R> outputClass, final Class<? extends Collection> collClass,
            final BooleanFunction<? extends K> func) {

        return groupBy(outputClass, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Boolean>, R extends Map<? super K, V>> R groupBy(final Class<R> outputClass, final Class<? extends Collection> collClass,
            final int fromIndex, final int toIndex, final BooleanFunction<? extends K> func) {
        checkIndex(fromIndex, toIndex);

        final R outputResult = N.newInstance(outputClass);

        K key = null;
        V values = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = func.apply(elementData[i]);
            values = outputResult.get(key);

            if (values == null) {
                values = (V) N.newInstance(collClass);
                outputResult.put(key, values);
            }

            values.add(elementData[i]);
        }

        return outputResult;
    }

    public OptionalBoolean reduce(final BooleanBinaryOperator accumulator) {
        return size() == 0 ? OptionalBoolean.empty() : OptionalBoolean.of(reduce(false, accumulator));
    }

    public OptionalBoolean reduce(final int fromIndex, final int toIndex, final BooleanBinaryOperator accumulator) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalBoolean.empty() : OptionalBoolean.of(reduce(fromIndex, toIndex, false, accumulator));
    }

    public boolean reduce(final boolean identity, final BooleanBinaryOperator accumulator) {
        return reduce(0, size(), identity, accumulator);
    }

    public boolean reduce(final int fromIndex, final int toIndex, final boolean identity, final BooleanBinaryOperator accumulator) {
        checkIndex(fromIndex, toIndex);

        boolean result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.applyAsBoolean(result, elementData[i]);
        }

        return result;
    }

    @Override
    public BooleanList distinct(final int fromIndex, final int toIndex) {
        if (size > 1) {
            final Boolean[] a = new Boolean[2];

            for (int i = 0; i < size; i++) {
                if (a[0] == null) {
                    a[0] = elementData[i];
                } else if (a[0].booleanValue() != elementData[i]) {
                    a[1] = elementData[i];
                    break;
                }
            }

            return a[1] == null ? of(Array.of(a[0].booleanValue())) : of(Array.of(a[0].booleanValue(), a[1].booleanValue()));
        } else {
            return of(N.copyOfRange(elementData, 0, size));
        }
    }

    @Override
    public List<BooleanList> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<boolean[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<BooleanList> result = new ArrayList<>(list.size());

        for (boolean[] a : list) {
            result.add(of(a));
        }

        return result;
    }

    @Override
    public BooleanList top(int top) {
        throw new UnsupportedOperationException();
    }

    @Override
    public BooleanList top(int fromIndex, int toIndex, int top) {
        throw new UnsupportedOperationException();
    }

    @Override
    public BooleanList top(int top, Comparator<Boolean> cmp) {
        throw new UnsupportedOperationException();
    }

    @Override
    public BooleanList top(int fromIndex, int toIndex, int top, Comparator<Boolean> cmp) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void sort() {
        if (size <= 1) {
            return;
        }

        final int[] count = new int[2];

        for (int i = 0; i < size; i++) {
            count[elementData[i] == false ? 0 : 1]++;
        }

        if (count[0] > 0) {
            N.fill(elementData, 0, count[0], false);
        }

        if (count[1] > 0) {
            N.fill(elementData, count[0], count[0] + count[1], true);
        }
    }

    @Override
    public BooleanList copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new BooleanList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    @Override
    public BooleanList trimToSize() {
        if (elementData.length != size) {
            elementData = N.copyOfRange(elementData, 0, size);
        }

        return this;
    }

    @Override
    public void clear() {
        if (size > 0) {
            N.fill(elementData, 0, size, false);
        }

        size = 0;
    }

    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    @Override
    public int size() {
        return size;
    }

    @Override
    public void toList(List<Boolean> list, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public void toSet(Set<Boolean> set, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public void toMultiset(Multiset<Boolean> multiset, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }
    }

    public <K, U> Map<K, U> toMap(final BooleanFunction<? extends K> keyMapper, final BooleanFunction<? extends U> valueMapper) {
        return toMap(HashMap.class, keyMapper, valueMapper);
    }

    public <K, U, R extends Map<K, U>> R toMap(final Class<R> outputClass, final BooleanFunction<? extends K> keyMapper,
            final BooleanFunction<? extends U> valueMapper) {
        return toMap(outputClass, 0, size(), keyMapper, valueMapper);
    }

    public <K, U> Map<K, U> toMap(final int fromIndex, final int toIndex, final BooleanFunction<? extends K> keyMapper,
            final BooleanFunction<? extends U> valueMapper) {
        return toMap(HashMap.class, fromIndex, toIndex, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, R extends Map<K, U>> R toMap(final Class<? extends Map> outputClass, final int fromIndex, final int toIndex,
            final BooleanFunction<? extends K> keyMapper, final BooleanFunction<? extends U> valueMapper) {
        checkIndex(fromIndex, toIndex);

        final Map<K, U> map = N.newInstance(outputClass);

        for (int i = fromIndex; i < toIndex; i++) {
            map.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
        }

        return (R) map;
    }

    public <K, U> Multimap<K, U, List<U>> toMultimap(final BooleanFunction<? extends K> keyMapper, final BooleanFunction<? extends U> valueMapper) {
        return toMultimap(HashMap.class, List.class, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final Class<? extends Map> outputClass, final Class<? extends Collection> collClass,
            final BooleanFunction<? extends K> keyMapper, final BooleanFunction<? extends U> valueMapper) {
        return toMultimap(outputClass, collClass, 0, size(), keyMapper, valueMapper);
    }

    public <K, U> Multimap<K, U, List<U>> toMultimap(final int fromIndex, final int toIndex, final BooleanFunction<? extends K> keyMapper,
            final BooleanFunction<? extends U> valueMapper) {
        return toMultimap(HashMap.class, List.class, fromIndex, toIndex, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final Class<? extends Map> outputClass, final Class<? extends Collection> collClass,
            final int fromIndex, final int toIndex, final BooleanFunction<? extends K> keyMapper, final BooleanFunction<? extends U> valueMapper) {
        checkIndex(fromIndex, toIndex);

        final Multimap<K, U, V> multimap = new Multimap(outputClass, collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            multimap.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
        }

        return multimap;
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size());
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof BooleanList && N.equals(elementData, 0, size(), ((BooleanList) obj).elementData));

    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_BOOLEAN_ARRAY) {
            minCapacity = Math.max(DEFAULT_CAPACITY, minCapacity);
        }

        ensureExplicitCapacity(minCapacity);
    }

    private void ensureExplicitCapacity(int minCapacity) {
        if (minCapacity - elementData.length > 0) {
            grow(minCapacity);
        }
    }

    private void grow(int minCapacity) {
        int oldCapacity = elementData.length;
        int newCapacity = oldCapacity + (oldCapacity >> 1);

        if (newCapacity - minCapacity < 0) {
            newCapacity = minCapacity;
        }

        if (newCapacity - MAX_ARRAY_SIZE > 0) {
            newCapacity = hugeCapacity(minCapacity);
        }

        elementData = Arrays.copyOf(elementData, newCapacity);
    }
}
