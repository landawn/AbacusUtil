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

import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatPredicate;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class FloatList extends AbastractPrimitiveList<FloatConsumer, FloatPredicate, Float, float[], FloatList> {
    private float[] elementData = N.EMPTY_FLOAT_ARRAY;
    private int size = 0;

    public FloatList() {
        super();
    }

    public FloatList(int initialCapacity) {
        this();

        elementData = new float[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public FloatList(float[] a) {
        this();

        elementData = a;
        size = a.length;
    }

    public FloatList(float[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static FloatList of(float[] a) {
        return new FloatList(a);
    }

    public static FloatList of(float[] a, int size) {
        return new FloatList(a, size);
    }

    public static FloatList of(Collection<? extends Number> c) {
        final float[] a = new float[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                continue;
            }

            a[idx++] = e.floatValue();
        }

        return of(a);
    }

    public static FloatList of(Collection<? extends Number> c, float defaultValueForNull) {
        final float[] a = new float[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                a[idx++] = e.floatValue();
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
    public float[] array() {
        return elementData;
    }

    public float get(int index) {
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
    public float set(int index, float e) {
        rangeCheck(index);

        float oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(float e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, float e) {
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
    public void addAll(FloatList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, FloatList c) {
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
    public boolean remove(float e) {
        for (int index = 0; index < size; index++) {
            if (Float.compare(elementData[index], e) == 0) {

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
    public boolean remove(float e, boolean removeAllOccurrences) {
        if (removeAllOccurrences) {
            int w = 0;

            for (int i = 0; i < size; i++) {
                if (Float.compare(elementData[i], e) != 0) {
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
    public boolean removeAll(FloatList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean retainAll(FloatList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(FloatList c, boolean complement) {
        final float[] elementData = this.elementData;

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
    public float delete(int index) {
        rangeCheck(index);

        float oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    public boolean contains(float e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(FloatList c) {
        final float[] srcElementData = c.array();

        for (int i = 0, srcSize = c.size(); i < srcSize; i++) {

            if (!contains(srcElementData[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public FloatList subList(int fromIndex, int toIndex) {
        subListRangeCheck(fromIndex, toIndex, size);

        return new FloatList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(float e) {
        for (int i = 0; i < size; i++) {
            if (Float.compare(elementData[i], e) == 0) {

                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(float e) {
        for (int i = size; i > 0;) {
            if (Float.compare(elementData[--i], e) == 0) {

                return i;
            }
        }

        return -1;
    }

    public double sum() {
        return N.sum(elementData, 0, size).doubleValue();
    }

    public float min() {
        return N.min(elementData, 0, size);
    }

    public float max() {
        return N.max(elementData, 0, size);
    }

    public double avg() {
        return N.avg(elementData, 0, size).doubleValue();
    }

    @Override
    public void forEach(FloatConsumer action) {
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                action.accept(elementData[i]);
            }
        }
    }

    @Override
    public boolean allMatch(FloatPredicate filter) {
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
    public boolean anyMatch(FloatPredicate filter) {
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
    public boolean noneMatch(FloatPredicate filter) {
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
    public int count(FloatPredicate filter) {
        return N.count(elementData, 0, size, filter);
    }

    @Override
    public FloatList filter(FloatPredicate filter) {
        return of(N.filter(elementData, 0, size, filter));
    }

    @Override
    public FloatList distinct() {
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
    public FloatList copy() {
        return new FloatList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public FloatList trimToSize() {
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
    public List<Float> toList() {
        if (size == 0) {
            return N.newArrayList();
        }

        final List<Float> list = N.newArrayList(size);

        toList(list);

        return list;
    }

    @Override
    public void toList(List<Float> list) {
        for (int i = 0; i < size; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public Set<Float> toSet() {
        if (size == 0) {
            return N.newLinkedHashSet();
        }

        final Set<Float> set = N.newLinkedHashSet();

        toSet(set);

        return set;
    }

    @Override
    public void toSet(Set<Float> set) {
        for (int i = 0; i < size; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public Multiset<Float> toMultiset() {
        if (size == 0) {
            return N.newLinkedMultiset();
        }

        final Multiset<Float> multiset = N.newLinkedMultiset();

        toMultiset(multiset);

        return multiset;
    }

    @Override
    public void toMultiset(Multiset<Float> multiset) {
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
        return obj == this || (obj instanceof FloatList && N.equals(elementData, 0, size(), ((FloatList) obj).elementData));

    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_FLOAT_ARRAY) {
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
