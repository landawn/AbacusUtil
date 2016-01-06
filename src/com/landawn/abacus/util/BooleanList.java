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
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import com.landawn.abacus.util.function.BooleanConsumer;
import com.landawn.abacus.util.function.BooleanPredicate;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class BooleanList extends AbastractPrimitiveList<BooleanConsumer, BooleanPredicate, Boolean, boolean[], BooleanList> {
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
        this();

        elementData = a;
        size = a.length;
    }

    public BooleanList(boolean[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static BooleanList of(boolean[] a) {
        return new BooleanList(a);
    }

    public static BooleanList of(boolean[] a, int size) {
        return new BooleanList(a, size);
    }

    public static BooleanList of(Collection<Boolean> c) {
        final boolean[] a = new boolean[c.size()];
        int idx = 0;

        for (Boolean e : c) {
            if (e == null) {
                continue;
            }

            a[idx++] = e;
        }

        return of(a);
    }

    public static BooleanList of(Collection<Boolean> c, boolean defaultValueForNull) {
        final boolean[] a = new boolean[c.size()];
        int idx = 0;

        for (Boolean e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                a[idx++] = e;
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
    public boolean[] array() {
        return elementData;
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
        for (int index = 0; index < size; index++) {
            if (elementData[index] == e) {

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
     * @return the deleted element.
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
    public BooleanList subList(int fromIndex, int toIndex) {
        subListRangeCheck(fromIndex, toIndex, size);

        return new BooleanList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(boolean e) {
        for (int index = 0; index < size; index++) {
            if (elementData[index] == e) {

                return index;
            }
        }

        return -1;
    }

    public int lastIndexOf(boolean e) {
        for (int index = size; index > 0;) {
            if (elementData[--index] == e) {

                return index;
            }
        }

        return -1;
    }

    @Override
    public void forEach(BooleanConsumer action) {
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                action.accept(elementData[i]);
            }
        }
    }

    @Override
    public boolean allMatch(BooleanPredicate filter) {
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
    public boolean anyMatch(BooleanPredicate filter) {
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
    public boolean noneMatch(BooleanPredicate filter) {
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
    public int count(BooleanPredicate filter) {
        return N.count(elementData, 0, size, filter);
    }

    @Override
    public BooleanList filter(BooleanPredicate filter) {
        return of(N.filter(elementData, 0, size, filter));
    }

    @Override
    public BooleanList distinct() {
        if (size > 1) {
            final Set<Boolean> set = N.newLinkedHashSet();

            for (int i = 0; i < size; i++) {
                set.add(elementData[i]);

                if (set.size() >= 2) {
                    break;
                }
            }

            final Iterator<Boolean> it = set.iterator();

            return set.size() == 1 ? of(N.arrayOf(it.next())) : of(N.arrayOf(it.next(), it.next()));
        } else {
            return of(N.copyOfRange(elementData, 0, size));
        }
    }

    @Override
    public void sort() {
        if (size > 1) {
            final Set<Boolean> set = N.newLinkedHashSet();

            for (int i = 0; i < size; i++) {
                set.add(elementData[i]);

                if (set.size() >= 2) {
                    break;
                }
            }

            if (set.size() == 1) {
                elementData[0] = set.iterator().next();
            } else {
                elementData[0] = false;
                elementData[1] = true;
            }

            size = set.size();
        }
    }

    @Override
    public BooleanList copy() {
        return new BooleanList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public BooleanList trimToSize() {
        if (elementData.length > size) {
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
    public List<Boolean> toList() {
        if (size == 0) {
            return N.newArrayList();
        }

        final List<Boolean> list = N.newArrayList(size);

        toList(list);

        return list;
    }

    @Override
    public void toList(List<Boolean> list) {
        for (int i = 0; i < size; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public Set<Boolean> toSet() {
        if (size == 0) {
            return N.newLinkedHashSet();
        }

        final Set<Boolean> set = N.newLinkedHashSet();

        toSet(set);

        return set;
    }

    @Override
    public void toSet(Set<Boolean> set) {
        for (int i = 0; i < size; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public Multiset<Boolean> toMultiset() {
        if (size == 0) {
            return N.newLinkedMultiset();
        }

        final Multiset<Boolean> multiset = N.newLinkedMultiset();

        toMultiset(multiset);

        return multiset;
    }

    @Override
    public void toMultiset(Multiset<Boolean> multiset) {
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
