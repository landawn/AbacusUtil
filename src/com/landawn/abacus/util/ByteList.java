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
import java.util.List;
import java.util.Set;

import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.IndexedByteConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.stream.ByteStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ByteList extends AbstractNumberList<ByteConsumer, BytePredicate, Byte, byte[], ByteList> {
    private byte[] elementData = N.EMPTY_BYTE_ARRAY;
    private int size = 0;

    public ByteList() {
        super();
    }

    public ByteList(int initialCapacity) {
        elementData = initialCapacity == 0 ? N.EMPTY_BYTE_ARRAY : new byte[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public ByteList(byte[] a) {
        this(a, a.length);
    }

    public ByteList(byte[] a, int size) {
        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static ByteList empty() {
        return new ByteList(N.EMPTY_BYTE_ARRAY);
    }

    public static ByteList of(byte... a) {
        return a == null ? empty() : new ByteList(a);
    }

    public static ByteList of(byte[] a, int size) {
        return a == null && size == 0 ? empty() : new ByteList(a, size);
    }

    public static ByteList from(int... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static ByteList from(int[] a, final int startIndex, final int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final byte[] elementData = new byte[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (a[i] < Byte.MIN_VALUE || a[i] > Byte.MAX_VALUE) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (byte) a[i];
        }

        return of(elementData);
    }

    static ByteList from(List<String> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, (byte) 0);
    }

    static ByteList from(List<String> c, byte defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final byte[] a = new byte[c.size()];
        int idx = 0;

        for (String e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                double val = N.asDouble(e);

                if (N.compare(val, Byte.MIN_VALUE) < 0 || N.compare(val, Byte.MAX_VALUE) > 0) {
                    throw new ArithmeticException("overflow");
                }

                a[idx++] = (byte) val;
            }
        }

        return of(a);
    }

    public static ByteList from(Collection<Byte> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, (byte) 0);
    }

    public static ByteList from(Collection<Byte> c, byte defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final byte[] a = new byte[c.size()];
        int idx = 0;

        for (Byte e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }

    public static ByteList range(byte startInclusive, final byte endExclusive) {
        return of(Array.range(startInclusive, endExclusive));
    }

    public static ByteList range(byte startInclusive, final byte endExclusive, final byte by) {
        return of(Array.range(startInclusive, endExclusive, by));
    }

    public static ByteList rangeClosed(byte startInclusive, final byte endInclusive) {
        return of(Array.rangeClosed(startInclusive, endInclusive));
    }

    public static ByteList rangeClosed(byte startInclusive, final byte endInclusive, final byte by) {
        return of(Array.range(startInclusive, endInclusive, by));
    }

    public static ByteList repeat(byte element, final int len) {
        return of(Array.repeat(element, len));
    }

    public static ByteList random(final int len) {
        final byte[] a = new byte[len];

        // Keep consistent with ByteStream/ShortList/ShortStream/CharList/CharStream.
        // RAND.nextBytes(a);
        for (int i = 0; i < len; i++) {
            a[i] = (byte) RAND.nextInt();
        }

        return of(a);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
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

    public void addAll(ByteList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

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

    @Override
    public void addAll(byte[] a) {
        addAll(size(), a);
    }

    @Override
    public void addAll(int index, byte[] a) {
        rangeCheckForAdd(index);

        if (N.isNullOrEmpty(a)) {
            return;
        }

        int numNew = a.length;

        ensureCapacityInternal(size + numNew); // Increments modCount

        int numMoved = size - index;

        if (numMoved > 0) {
            N.copy(elementData, index, elementData, index + numNew, numMoved);
        }

        N.copy(a, 0, elementData, index, numNew);

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
    public boolean removeAllOccurrences(byte e) {
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
    }

    private void fastRemove(int index) {
        int numMoved = size - index - 1;

        if (numMoved > 0) {
            N.copy(elementData, index + 1, elementData, index, numMoved);
        }

        elementData[--size] = 0; // clear to let GC do its work
    }

    public boolean removeAll(ByteList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    public boolean retainAll(ByteList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(ByteList c, boolean complement) {
        final byte[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<Byte> set = c.toSet();

            for (int i = 0; i < size; i++) {
                if (set.contains(elementData[i]) == complement) {
                    elementData[w++] = elementData[i];
                }
            }
        } else {
            for (int i = 0; i < size; i++) {
                if (c.contains(elementData[i]) == complement) {
                    elementData[w++] = elementData[i];
                }
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

    @Override
    public void deleteAll(int... indices) {
        N.deleteAll(elementData, indices);
    }

    public int replaceAll(byte oldVal, byte newVal) {
        if (size() == 0) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = size(); i < len; i++) {
            if (elementData[i] == oldVal) {
                elementData[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public void fill(final byte val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final byte val) {
        checkIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(byte e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(ByteList c) {
        final byte[] srcElementData = c.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Byte> set = c.toSet();

            for (int i = 0, srcSize = c.size(); i < srcSize; i++) {
                if (set.contains(srcElementData[i]) == false) {
                    return false;
                }
            }
        } else {
            for (int i = 0, srcSize = c.size(); i < srcSize; i++) {
                if (contains(srcElementData[i]) == false) {
                    return false;
                }
            }
        }

        return true;
    }

    @Override
    public boolean containsAll(byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return containsAll(of(a));
    }

    public boolean disjoint(final ByteList c) {
        final ByteList container = size() >= c.size() ? this : c;
        final byte[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (iterElements.length > 3 && container.size() > 9) {
            final Set<Byte> set = container.toSet();

            for (int i = 0, srcSize = size() >= c.size() ? c.size() : this.size(); i < srcSize; i++) {
                if (set.contains(iterElements[i])) {
                    return false;
                }
            }
        } else {
            for (int i = 0, srcSize = size() >= c.size() ? c.size() : this.size(); i < srcSize; i++) {
                if (container.contains(iterElements[i])) {
                    return false;
                }
            }
        }

        return true;
    }

    @Override
    public boolean disjoint(final byte[] b) {
        if (N.isNullOrEmpty(b)) {
            return true;
        }

        return disjoint(of(b));
    }

    public int occurrencesOf(final byte objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    @Override
    public ByteList subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new ByteList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#except(IntList)
     */
    public ByteList except(ByteList b) {
        final Multiset<Byte> bOccurrences = b.toMultiset();

        final ByteList c = new ByteList(N.min(size(), N.max(9, size() - b.size())));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) < 1) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#intersect(IntList)
     */
    public ByteList intersect(ByteList b) {
        final Multiset<Byte> bOccurrences = b.toMultiset();

        final ByteList c = new ByteList(N.min(9, size(), b.size()));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) > 0) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    /**
     * 
     * @param b
     * @return this.except(b).addAll(b.except(this))
     * @see IntList#xor(IntList)
     */
    public ByteList xor(ByteList b) {
        //        final ByteList result = this.except(b);
        //
        //        result.addAll(b.except(this));
        //
        //        return result; 

        final Multiset<Byte> bOccurrences = b.toMultiset();

        final ByteList c = new ByteList(N.max(9, Math.abs(size() - b.size())));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) < 1) {
                c.add(elementData[i]);
            }
        }

        for (int i = 0, len = b.size(); i < len; i++) {
            if (bOccurrences.getAndRemove(b.elementData[i]) > 0) {
                c.add(b.elementData[i]);
            }

            if (bOccurrences.isEmpty()) {
                break;
            }
        }

        return c;
    }

    public int indexOf(byte e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, byte e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(byte e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, byte e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public OptionalByte min() {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(N.min(elementData, 0, size));
    }

    public OptionalByte min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(N.min(elementData, fromIndex, toIndex));
    }

    public OptionalByte median() {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(N.median(elementData, 0, size));
    }

    public OptionalByte median(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(N.median(elementData, fromIndex, toIndex));
    }

    public OptionalByte max() {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(N.max(elementData, 0, size));
    }

    public OptionalByte max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(N.max(elementData, fromIndex, toIndex));
    }

    public OptionalByte kthLargest(final int k) {
        return kthLargest(0, size(), k);
    }

    public OptionalByte kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? OptionalByte.empty() : OptionalByte.of(N.kthLargest(elementData, fromIndex, toIndex, k));
    }

    public Long sum() {
        return sum(0, size());
    }

    public Long sum(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return N.sum(elementData, fromIndex, toIndex);
    }

    @Override
    public OptionalDouble average(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(N.average(elementData, fromIndex, toIndex));
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, ByteConsumer action) {
        if (fromIndex <= toIndex) {
            checkIndex(fromIndex, toIndex);
        } else {
            checkIndex(toIndex, fromIndex);
        }

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(elementData[i]);
                }
            } else {
                for (int i = fromIndex - 1; i >= toIndex; i--) {
                    action.accept(elementData[i]);
                }
            }
        }
    }

    public void forEach(IndexedByteConsumer action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, IndexedByteConsumer action) {
        if (fromIndex <= toIndex) {
            checkIndex(fromIndex, toIndex);
        } else {
            checkIndex(toIndex, fromIndex);
        }

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(i, elementData[i], elementData);
                }
            } else {
                for (int i = fromIndex - 1; i >= toIndex; i--) {
                    action.accept(i, elementData[i], elementData);
                }
            }
        }
    }

    public OptionalByte findFirst(BytePredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalByte.of(elementData[i]);
            }
        }

        return OptionalByte.empty();
    }

    public OptionalByte findLast(BytePredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalByte.of(elementData[i]);
            }
        }

        return OptionalByte.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, BytePredicate filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, BytePredicate filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, BytePredicate filter) {
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
    public boolean hasDuplicates() {
        return N.hasDuplicates(elementData, 0, size, false);
    }

    @Override
    public int count(final int fromIndex, final int toIndex, BytePredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public ByteList filter(final int fromIndex, final int toIndex, BytePredicate filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    @Override
    public ByteList filter(final int fromIndex, final int toIndex, BytePredicate filter, final int max) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter, max));
    }

    @Override
    public ByteList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.distinct(elementData, fromIndex, toIndex));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
    }

    @Override
    public void sort() {
        if (size > 1) {
            N.sort(elementData, 0, size);
        }
    }

    public void parallelSort() {
        if (size > 1) {
            N.parallelSort(elementData, 0, size);
        }
    }

    /**
     * This List should be sorted first.
     * 
     * @param key
     * @return
     */
    public int binarySearch(final byte key) {
        return N.binarySearch(elementData, key);
    }

    /**
     * This List should be sorted first.
     *
     * @param fromIndex
     * @param toIndex
     * @param key
     * @return
     */
    public int binarySearch(final int fromIndex, final int toIndex, final byte key) {
        checkIndex(fromIndex, toIndex);

        return N.binarySearch(elementData, fromIndex, toIndex, key);
    }

    @Override
    public void reverse() {
        if (size > 1) {
            N.reverse(elementData, 0, size);
        }
    }

    @Override
    public void reverse(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            N.reverse(elementData, fromIndex, toIndex);
        }
    }

    @Override
    public void rotate(int distance) {
        if (size > 1) {
            N.rotate(elementData, distance);
        }
    }

    @Override
    public void shuffle() {
        N.shuffle(elementData);
    }

    @Override
    public void swap(int i, int j) {
        rangeCheck(i);
        rangeCheck(j);

        set(i, set(j, elementData[i]));
    }

    @Override
    public ByteList copy() {
        return new ByteList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public List<ByteList> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<byte[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<ByteList> result = new ArrayList<>(list.size());

        for (byte[] a : list) {
            result.add(of(a));
        }

        return result;
    }

    @Override
    public List<ByteList> split(int fromIndex, int toIndex, BytePredicate predicate) {
        checkIndex(fromIndex, toIndex);

        final List<ByteList> result = new ArrayList<>();
        ByteList piece = null;

        for (int i = fromIndex; i < toIndex;) {
            if (piece == null) {
                piece = ByteList.of(N.EMPTY_BYTE_ARRAY);
            }

            if (predicate.test(elementData[i])) {
                piece.add(elementData[i]);
                i++;
            } else {
                result.add(piece);
                piece = null;
            }
        }

        if (piece != null) {
            result.add(piece);
        }

        return result;
    }

    @Override
    public String join(int fromIndex, int toIndex, char delimiter) {
        checkIndex(fromIndex, toIndex);

        return N.join(elementData, fromIndex, toIndex, delimiter);
    }

    @Override
    public String join(int fromIndex, int toIndex, String delimiter) {
        checkIndex(fromIndex, toIndex);

        return N.join(elementData, fromIndex, toIndex, delimiter);
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

    public ObjectList<Byte> boxed() {
        return boxed(0, size);
    }

    public ObjectList<Byte> boxed(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Byte[] b = new Byte[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ObjectList.of(b);
    }

    @Override
    public List<Byte> toList(final int fromIndex, final int toIndex, final IntFunction<List<Byte>> supplier) {
        checkIndex(fromIndex, toIndex);

        final List<Byte> list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public Set<Byte> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<Byte>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Set<Byte> set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<Byte> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<Byte>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Multiset<Byte> multiset = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    public ByteStream stream() {
        return stream(0, size());
    }

    public ByteStream stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return ByteStream.of(elementData, fromIndex, toIndex);
    }

    //    public ByteListBuilder __() {
    //        return Builder.of(this);
    //    }
    //
    //    public ByteListBuilder __(Consumer<? super ByteList> func) {
    //        return Builder.of(this).__(func);
    //    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (obj instanceof ByteList) {
            final ByteList other = (ByteList) obj;

            return this.size == other.size && N.equals(this.elementData, 0, other.elementData, 0, this.size);
        }

        return false;
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
