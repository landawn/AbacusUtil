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

import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.stream.IntStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ByteList extends AbastractPrimitiveList<ByteConsumer, BytePredicate, Byte, byte[], ByteList> {
    private byte[] elementData = N.EMPTY_BYTE_ARRAY;
    private int size = 0;

    public ByteList() {
        super();
    }

    public ByteList(int initialCapacity) {
        this();

        elementData = new byte[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public ByteList(byte[] a) {
        this();

        elementData = a;
        size = a.length;
    }

    public ByteList(byte[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static ByteList of(byte[] a) {
        return new ByteList(a);
    }

    public static ByteList of(byte[] a, int size) {
        return new ByteList(a, size);
    }

    public static ByteList of(int[] a) {
        return of(a, 0, a.length);
    }

    public static ByteList of(int[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final byte[] elementData = new byte[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = (byte) a[i];
        }

        return of(elementData);
    }

    public static ByteList of(Collection<? extends Number> c) {
        final byte[] a = new byte[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                continue;
            }

            a[idx++] = e.byteValue();
        }

        return of(a);
    }

    public static ByteList of(Collection<? extends Number> c, byte defaultValueForNull) {
        final byte[] a = new byte[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                a[idx++] = e.byteValue();
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
    public byte[] array() {
        return elementData;
    }

    public byte get(int index) {
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
    public byte set(int index, byte e) {
        rangeCheck(index);

        byte oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(byte e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, byte e) {
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
    public void addAll(ByteList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, ByteList c) {
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
    public boolean remove(byte e) {
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
    public boolean remove(byte e, boolean removeAllOccurrences) {
        if (removeAllOccurrences) {
            int w = 0;

            for (int i = 0; i < size; i++) {
                if (elementData[i] != e) {
                    elementData[w++] = elementData[i];
                }
            }

            int numRemoved = size - w;

            if (numRemoved > 0) {
                N.fill(elementData, w, size, (byte) 0);

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
    public boolean removeAll(ByteList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean retainAll(ByteList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(ByteList c, boolean complement) {
        final byte[] elementData = this.elementData;

        int w = 0;

        for (int i = 0; i < size; i++) {
            if (c.contains(elementData[i]) == complement) {
                elementData[w++] = elementData[i];
            }
        }

        int numRemoved = size - w;

        if (numRemoved > 0) {
            N.fill(elementData, w, size, (byte) 0);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element
     */
    public byte delete(int index) {
        rangeCheck(index);

        byte oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    public boolean contains(byte e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(ByteList c) {
        final byte[] srcElementData = c.array();

        for (int i = 0, srcSize = c.size(); i < srcSize; i++) {

            if (!contains(srcElementData[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public ByteList subList(int fromIndex, int toIndex) {
        subListRangeCheck(fromIndex, toIndex, size);

        return new ByteList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(byte e) {
        for (int index = 0; index < size; index++) {
            if (elementData[index] == e) {

                return index;
            }
        }

        return -1;
    }

    public int lastIndexOf(byte e) {
        for (int index = size; index > 0;) {
            if (elementData[--index] == e) {

                return index;
            }
        }

        return -1;
    }

    public long sum() {
        return N.sum(elementData, 0, size).longValue();
    }

    public byte min() {
        return N.min(elementData, 0, size);
    }

    public byte max() {
        return N.max(elementData, 0, size);
    }

    public double avg() {
        return N.avg(elementData, 0, size).doubleValue();
    }

    @Override
    public void forEach(ByteConsumer action) {
        if (size > 0) {
            for (int i = 0; i < size; i++) {
                action.accept(elementData[i]);
            }
        }
    }

    @Override
    public boolean allMatch(BytePredicate filter) {
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
    public boolean anyMatch(BytePredicate filter) {
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
    public boolean noneMatch(BytePredicate filter) {
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
    public int count(BytePredicate filter) {
        return N.count(elementData, 0, size, filter);
    }

    @Override
    public ByteList filter(BytePredicate filter) {
        return of(N.filter(elementData, 0, size, filter));
    }

    @Override
    public ByteList distinct() {
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
    public ByteList copy() {
        return new ByteList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public ByteList trimToSize() {
        if (elementData.length > size) {
            elementData = N.copyOfRange(elementData, 0, size);
        }

        return this;
    }

    @Override
    public void clear() {
        if (size > 0) {
            N.fill(elementData, 0, size, (byte) 0);
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
    public List<Byte> toList() {
        if (size == 0) {
            return N.newArrayList();
        }

        final List<Byte> list = N.newArrayList(size);

        toList(list);

        return list;
    }

    @Override
    public void toList(List<Byte> list) {
        for (int i = 0; i < size; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public Set<Byte> toSet() {
        if (size == 0) {
            return N.newLinkedHashSet();
        }

        final Set<Byte> set = N.newLinkedHashSet();

        toSet(set);

        return set;
    }

    @Override
    public void toSet(Set<Byte> set) {
        for (int i = 0; i < size; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public Multiset<Byte> toMultiset() {
        if (size == 0) {
            return N.newLinkedMultiset();
        }

        final Multiset<Byte> multiset = N.newLinkedMultiset();

        toMultiset(multiset);

        return multiset;
    }

    @Override
    public void toMultiset(Multiset<Byte> multiset) {
        for (int i = 0; i < size; i++) {
            multiset.add(elementData[i]);
        }
    }

    public IntStream stream() {
        return Stream.of(elementData, 0, size());
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size());
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof ByteList && N.equals(elementData, 0, size(), ((ByteList) obj).elementData));

    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_BYTE_ARRAY) {
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
