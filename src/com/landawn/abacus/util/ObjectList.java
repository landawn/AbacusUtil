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
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class ObjectList<T> extends AbastractArrayList<Consumer<T>, Predicate<T>, T, T[], ObjectList<T>> {
    private T[] elementData = null;
    private int size = 0;

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public ObjectList(T[] a) {
        super();

        elementData = a;
        size = a.length;
    }

    public ObjectList(T[] a, int size) {
        super();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static <T> ObjectList<T> of(T[] a) {
        return new ObjectList<T>(a);
    }

    public static <T> ObjectList<T> of(T[] a, int size) {
        return new ObjectList<T>(a, size);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
    @Override
    public T[] array() {
        return elementData;
    }

    /**
     * Return the first element of the array list, or {@code OptionalNullable.empty()} if there is no element.
     * @return
     */
    @Beta
    public OptionalNullable<T> findFirst() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(elementData[0]);
    }

    /**
     * Return the last element of the array list, or {@code OptionalNullable.empty()} if there is no element.
     * 
     * @return
     */
    @Beta
    public OptionalNullable<T> findFirstNonNull() {
        for (int i = 0; i < size; i++) {
            if (elementData[i] != null) {
                return OptionalNullable.of(elementData[i]);
            }
        }

        return OptionalNullable.empty();
    }

    /**
     * Return the last non-null element of the array list, or {@code OptionalNullable.empty()} if there is no non-null element.
     * @return
     */
    @Beta
    public OptionalNullable<T> findLast() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(elementData[size - 1]);
    }

    /**
     * Return the last non-null element of the array list.
     * @return
     */
    @Beta
    public OptionalNullable<T> findLastNonNull() {
        for (int i = size - 1; i >= 0; i--) {
            if (elementData[i] != null) {
                return OptionalNullable.of(elementData[i]);
            }
        }

        return OptionalNullable.empty();
    }

    public T get(int index) {
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
    public T set(int index, T e) {
        rangeCheck(index);

        T oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(T e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, T e) {
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
    public void addAll(ObjectList<T> c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, ObjectList<T> c) {
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
    public boolean remove(T e) {
        for (int i = 0; i < size; i++) {
            if (N.equals(elementData[i], e)) {

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
    public boolean remove(T e, boolean removeAllOccurrences) {
        if (removeAllOccurrences) {
            int w = 0;

            for (int i = 0; i < size; i++) {
                if (!N.equals(elementData[i], e)) {
                    elementData[w++] = elementData[i];
                }
            }

            int numRemoved = size - w;

            if (numRemoved > 0) {
                N.fill(elementData, w, size, null);

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

        elementData[--size] = null; // clear to let GC do its work
    }

    @Override
    public boolean removeAll(ObjectList<T> c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean retainAll(ObjectList<T> c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(ObjectList<T> c, boolean complement) {
        final T[] elementData = this.elementData;

        int w = 0;

        for (int i = 0; i < size; i++) {
            if (c.contains(elementData[i]) == complement) {
                elementData[w++] = elementData[i];
            }
        }

        int numRemoved = size - w;

        if (numRemoved > 0) {
            N.fill(elementData, w, size, null);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element
     */
    public T delete(int index) {
        rangeCheck(index);

        T oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    public boolean contains(T e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(ObjectList<T> c) {
        final T[] srcElementData = c.array();

        for (int i = 0, srcSize = c.size(); i < srcSize; i++) {

            if (!contains(srcElementData[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public ObjectList<T> subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new ObjectList<T>(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(T e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, T e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(T e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, T e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public T min() {
        return (T) N.min((Comparable[]) elementData, 0, size);
    }

    public T min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return (T) N.min((Comparable[]) elementData, fromIndex, toIndex);
    }

    public T min(Comparator<T> cmp) {
        return N.min(elementData, 0, size, cmp);
    }

    public T min(final int fromIndex, final int toIndex, Comparator<T> cmp) {
        checkIndex(fromIndex, toIndex);

        return N.min(elementData, fromIndex, toIndex, cmp);
    }

    public T max() {
        return (T) N.max((Comparable[]) elementData, 0, size);
    }

    public T max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return (T) N.max((Comparable[]) elementData, fromIndex, toIndex);
    }

    public T max(Comparator<T> cmp) {
        return N.max(elementData, 0, size, cmp);
    }

    public T max(final int fromIndex, final int toIndex, Comparator<T> cmp) {
        checkIndex(fromIndex, toIndex);

        return N.max(elementData, fromIndex, toIndex, cmp);
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, Consumer<T> action) {
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                action.accept(elementData[i]);
            }
        }
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, Predicate<T> filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, Predicate<T> filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, Predicate<T> filter) {
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
    public int count(final int fromIndex, final int toIndex, Predicate<T> filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public ObjectList<T> filter(final int fromIndex, final int toIndex, Predicate<T> filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    public <R> List<R> map(final Function<? super T, ? extends R> func) {
        return map(0, size(), func);
    }

    public <R> List<R> map(final int fromIndex, final int toIndex, final Function<? super T, ? extends R> func) {
        return map(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final Function<? super T, ? extends R> func) {
        return map(collClass, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final Function<? super T, ? extends R> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.add(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap(final Function<? super T, ? extends Collection<? extends R>> func) {
        return flatMap(0, size(), func);
    }

    public <R> List<R> flatMap(final int fromIndex, final int toIndex, final Function<? super T, ? extends Collection<? extends R>> func) {
        return flatMap(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass,
            final Function<? super T, ? extends Collection<? extends R>> func) {
        return flatMap(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final Function<? super T, ? extends Collection<? extends R>> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap2(final Function<? super T, R[]> func) {
        return flatMap2(0, size(), func);
    }

    public <R> List<R> flatMap2(final int fromIndex, final int toIndex, final Function<? super T, R[]> func) {
        return flatMap2(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final Function<? super T, R[]> func) {
        return flatMap2(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final Function<? super T, R[]> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(Arrays.asList(func.apply(elementData[i])));
        }

        return res;
    }

    public <K> Map<K, List<T>> groupBy(final Function<? super T, ? extends K> func) {
        return groupBy(0, size(), func);
    }

    public <K> Map<K, List<T>> groupBy(final int fromIndex, final int toIndex, final Function<? super T, ? extends K> func) {
        return groupBy(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<T>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final Function<? super T, ? extends K> func) {
        return groupBy(HashMap.class, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<T>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final Function<? super T, ? extends K> func) {
        return groupBy(HashMap.class, List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<T>, R extends Map<? super K, V>> R groupBy(final Class<R> outputClass, final Class<? extends Collection> collClass,
            final Function<? super T, ? extends K> func) {

        return groupBy(outputClass, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<T>, R extends Map<? super K, V>> R groupBy(final Class<R> outputClass, final Class<? extends Collection> collClass,
            final int fromIndex, final int toIndex, final Function<? super T, ? extends K> func) {
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

    public T reduce(final BiFunction<T, T, T> accumulator) {
        return reduce(0, size(), accumulator);
    }

    public T reduce(final int fromIndex, final int toIndex, final BiFunction<T, T, T> accumulator) {
        return reduce(fromIndex, toIndex, null, accumulator);
    }

    public T reduce(final T identity, final BiFunction<T, T, T> accumulator) {
        return reduce(0, size(), identity, accumulator);
    }

    public T reduce(final int fromIndex, final int toIndex, final T identity, final BiFunction<T, T, T> accumulator) {
        checkIndex(fromIndex, toIndex);

        T result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.apply(result, elementData[i]);
        }

        return result;
    }

    @Override
    public ObjectList<T> distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (size > 1) {
            return of(N.removeDuplicates(elementData, fromIndex, toIndex, false));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
    }

    @Override
    public List<ObjectList<T>> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<T[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<ObjectList<T>> result = new ArrayList<>(list.size());

        for (T[] a : list) {
            result.add(ObjectList.of(a));
        }

        return result;
    }

    @Override
    public ObjectList<T> top(final int top) {
        return top(0, size(), top);
    }

    @Override
    public ObjectList<T> top(final int fromIndex, final int toIndex, final int top) {
        checkIndex(fromIndex, toIndex);

        return of((T[]) N.top((Comparable[]) elementData, fromIndex, toIndex, top));
    }

    @Override
    public ObjectList<T> top(final int top, Comparator<T> cmp) {
        return top(0, size(), top, cmp);
    }

    @Override
    public ObjectList<T> top(final int fromIndex, final int toIndex, final int top, Comparator<T> cmp) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, top, cmp));
    }

    @Override
    public void sort() {
        if (size > 1) {
            N.sort(elementData, 0, size);
        }
    }

    @Override
    public ObjectList<T> copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new ObjectList<T>(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    @Override
    public ObjectList<T> trimToSize() {
        if (elementData.length != size) {
            elementData = N.copyOfRange(elementData, 0, size);
        }

        return this;
    }

    @Override
    public void clear() {
        if (size > 0) {
            N.fill(elementData, 0, size, null);
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
    public void toList(List<T> list, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public void toSet(Set<T> set, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public void toMultiset(Multiset<T> multiset, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }
    }

    public Stream<T> stream() {
        return stream(0, size());
    }

    public Stream<T> stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return Stream.of(elementData, fromIndex, toIndex);
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size());
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof ObjectList && N.equals(elementData, 0, size(), ((ObjectList<T>) obj).elementData));

    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (N.isNullOrEmpty(elementData)) {
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
