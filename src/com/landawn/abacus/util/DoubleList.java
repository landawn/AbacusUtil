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
import java.util.List;
import java.util.Set;

import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoublePredicate;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class DoubleList extends AbastractPrimitiveList<DoubleConsumer, DoublePredicate, Double, double[], DoubleList> {
    private double[] elementData = N.EMPTY_DOUBLE_ARRAY;
    private int size = 0;

    public DoubleList() {
        super();
    }

    public DoubleList(int initialCapacity) {
        this();

        elementData = new double[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public DoubleList(double[] a) {
        this();

        elementData = a;
        size = a.length;
    }

    public DoubleList(double[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static DoubleList of(double[] a) {
        return new DoubleList(a);
    }

    public static DoubleList of(double[] a, int size) {
        return new DoubleList(a, size);
    }

    public static DoubleList of(float[] a) {
        return of(a, 0, a.length);
    }

    public static DoubleList of(float[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final double[] elementData = new double[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = a[i];
        }

        return of(elementData);
    }

    public static DoubleList of(Collection<? extends Number> c) {
        final double[] a = new double[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                continue;
            }

            a[idx++] = e.doubleValue();
        }

        return of(a);
    }

    public static DoubleList of(Collection<? extends Number> c, double defaultValueForNull) {
        final double[] a = new double[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                a[idx++] = e.doubleValue();
            }
        }

        return of(a);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
    @Override
    public double[] array() {
        return elementData;
    }

    public double get(int index) {
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
    public double set(int index, double e) {
        rangeCheck(index);

        double oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(double e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, double e) {
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
    public void addAll(DoubleList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, DoubleList c) {
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
    public boolean remove(double e) {
        for (int index = 0; index < size; index++) {
            if (Double.compare(elementData[index], e) == 0) {

                fastRemove(index);

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
    public boolean remove(double e, boolean removeAllOccurrences) {
        if (removeAllOccurrences) {
            int w = 0;

            for (int i = 0; i < size; i++) {
                if (Double.compare(elementData[i], e) != 0) {
                    elementData[w++] = elementData[i];
                }
            }

            int numRemoved = size - w;

            if (numRemoved > 0) {
                N.fill(elementData, w, size, 0);

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

        elementData[--size] = 0; // clear to let GC do its work
    }

    @Override
    public boolean removeAll(DoubleList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean retainAll(DoubleList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(DoubleList c, boolean complement) {
        final double[] elementData = this.elementData;

        int w = 0;

        for (int i = 0; i < size; i++) {
            if (c.contains(elementData[i]) == complement) {
                elementData[w++] = elementData[i];
            }
        }

        int numRemoved = size - w;

        if (numRemoved > 0) {
            N.fill(elementData, w, size, 0);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element.
     */
    public double delete(int index) {
        rangeCheck(index);

        double oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    public boolean contains(double e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(DoubleList c) {
        final double[] srcElementData = c.array();

        for (int i = 0, srcSize = c.size(); i < srcSize; i++) {

            if (!contains(srcElementData[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public DoubleList subList(int fromIndex, int toIndex) {
        subListRangeCheck(fromIndex, toIndex, size);

        return new DoubleList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(double e) {
        for (int i = 0; i < size; i++) {
            if (Double.compare(elementData[i], e) == 0) {

                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(double e) {
        for (int i = size; i > 0;) {
            if (Double.compare(elementData[--i], e) == 0) {

                return i;
            }
        }

        return -1;
    }

    public double sum() {
        return N.sum(elementData, 0, size).doubleValue();
    }

    public double min() {
        return N.min(elementData, 0, size);
    }

    public double max() {
        return N.max(elementData, 0, size);
    }

    public double avg() {
        return N.avg(elementData, 0, size).doubleValue();
    }

    @Override
    public void forEach(DoubleConsumer action) {
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                action.accept(elementData[i]);
            }
        }
    }

    @Override
    public boolean allMatch(DoublePredicate filter) {
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                if (filter.test(elementData[i]) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    @Override
    public boolean anyMatch(DoublePredicate filter) {
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                if (filter.test(elementData[i])) {
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    public boolean noneMatch(DoublePredicate filter) {
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                if (filter.test(elementData[i])) {
                    return false;
                }
            }
        }

        return true;
    }

    @Override
    public int count(DoublePredicate filter) {
        return N.count(elementData, 0, size, filter);
    }

    @Override
    public DoubleList filter(DoublePredicate filter) {
        return of(N.filter(elementData, 0, size, filter));
    }

    @Override
    public DoubleList distinct() {
        if (size > 1) {
            return of(N.removeDuplicates(elementData, 0, size, false));
        } else {
            return of(N.copyOfRange(elementData, 0, size));
        }
    }

    @Override
    public void sort() {
        if (size > 1) {
            N.sort(elementData, 0, size);
        }
    }

    @Override
    public DoubleList copy() {
        return new DoubleList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public DoubleList trimToSize() {
        if (elementData.length > size) {
            elementData = N.copyOfRange(elementData, 0, size);
        }

        return this;
    }

    @Override
    public void clear() {
        if (size > 0) {
            N.fill(elementData, 0, size, 0);
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
    public List<Double> toList() {
        if (size == 0) {
            return N.newArrayList();
        }

        final List<Double> list = N.newArrayList(size);

        toList(list);

        return list;
    }

    @Override
    public void toList(List<Double> list) {
        for (int i = 0; i < size; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public Set<Double> toSet() {
        if (size == 0) {
            return N.newLinkedHashSet();
        }

        final Set<Double> set = N.newLinkedHashSet();

        toSet(set);

        return set;
    }

    @Override
    public void toSet(Set<Double> set) {
        for (int i = 0; i < size; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public Multiset<Double> toMultiset() {
        if (size == 0) {
            return N.newLinkedMultiset();
        }

        final Multiset<Double> multiset = N.newLinkedMultiset();

        toMultiset(multiset);

        return multiset;
    }

    @Override
    public void toMultiset(Multiset<Double> multiset) {
        for (int i = 0; i < size; i++) {
            multiset.add(elementData[i]);
        }
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size());
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof DoubleList && N.equals(elementData, 0, size(), ((DoubleList) obj).elementData));

    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_DOUBLE_ARRAY) {
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
