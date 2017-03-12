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
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Random;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.function.ByteUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedByteConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.ByteStream;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ByteList extends AbstractList<ByteConsumer, BytePredicate, Byte, byte[], ByteList> {
    private static final long serialVersionUID = 6361439693114081075L;

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
        N.checkFromIndexSize(0, size, a.length);

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

        N.checkFromToIndex(startIndex, endIndex, a == null ? 0 : a.length);

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

    public boolean addAll(ByteList c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;

        return true;
    }

    public boolean addAll(int index, ByteList c) {
        rangeCheckForAdd(index);

        if (N.isNullOrEmpty(c)) {
            return false;
        }

        int numNew = c.size();

        ensureCapacityInternal(size + numNew); // Increments modCount

        int numMoved = size - index;

        if (numMoved > 0) {
            N.copy(elementData, index, elementData, index + numNew, numMoved);
        }

        N.copy(c.array(), 0, elementData, index, numNew);

        size += numNew;

        return true;
    }

    @Override
    public boolean addAll(byte[] a) {
        return addAll(size(), a);
    }

    @Override
    public boolean addAll(int index, byte[] a) {
        rangeCheckForAdd(index);

        if (N.isNullOrEmpty(a)) {
            return false;
        }

        int numNew = a.length;

        ensureCapacityInternal(size + numNew); // Increments modCount

        int numMoved = size - index;

        if (numMoved > 0) {
            N.copy(elementData, index, elementData, index + numNew, numMoved);
        }

        N.copy(a, 0, elementData, index, numNew);

        size += numNew;

        return true;
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
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    @Override
    public boolean removeIf(BytePredicate p) {
        final ByteList tmp = new ByteList(size());

        for (int i = 0; i < size; i++) {
            if (p.test(elementData[i]) == false) {
                tmp.add(elementData[i]);
            }
        }

        if (tmp.size() == this.size()) {
            return false;
        }

        N.copy(tmp.elementData, 0, this.elementData, 0, tmp.size());

        return true;
    }

    public boolean retainAll(ByteList c) {
        if (N.isNullOrEmpty(c)) {
            boolean result = size() > 0;
            clear();
            return result;
        }

        return batchRemove(c, true) > 0;
    }

    public boolean retainAll(byte[] a) {
        if (N.isNullOrEmpty(a)) {
            boolean result = size() > 0;
            clear();
            return result;
        }

        return retainAll(ByteList.of(a));
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

    public void replaceAll(ByteUnaryOperator operator) {
        for (int i = 0, len = size(); i < len; i++) {
            elementData[i] = operator.applyAsByte(elementData[i]);
        }
    }

    public boolean replaceIf(byte newValue, BytePredicate predicate) {
        boolean result = false;

        for (int i = 0, len = size(); i < len; i++) {
            if (predicate.test(elementData[i])) {
                elementData[i] = newValue;

                result = true;
            }
        }

        return result;
    }

    public void fill(final byte val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final byte val) {
        checkFromToIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(byte e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(ByteList c) {
        if (N.isNullOrEmpty(c)) {
            return true;
        }

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
        if (N.isNullOrEmpty(c)) {
            return true;
        }

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

    /**
     * 
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public ByteList intersection(final ByteList b) {
        if (N.isNullOrEmpty(b)) {
            return empty();
        }

        final Multiset<Byte> bOccurrences = b.toMultiset();

        final ByteList c = new ByteList(N.min(9, size(), b.size()));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) > 0) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    public ByteList intersection(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        return intersection(of(a));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#difference(IntList)
     */
    public ByteList difference(ByteList b) {
        if (N.isNullOrEmpty(b)) {
            return of(N.copyOfRange(elementData, 0, size()));
        }

        final Multiset<Byte> bOccurrences = b.toMultiset();

        final ByteList c = new ByteList(N.min(size(), N.max(9, size() - b.size())));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) < 1) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    public ByteList difference(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return of(N.copyOfRange(elementData, 0, size()));
        }

        return difference(of(a));
    }

    /**
     * 
     * @param b
     * @return this.difference(b).addAll(b.difference(this))
     * @see IntList#symmetricDifference(IntList)
     */
    public ByteList symmetricDifference(ByteList b) {
        if (N.isNullOrEmpty(b)) {
            return this.copy();
        } else if (this.isEmpty()) {
            return b.copy();
        }

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

    public ByteList symmetricDifference(final byte[] a) {
        if (N.isNullOrEmpty(a)) {
            return of(N.copyOfRange(elementData, 0, size()));
        } else if (this.isEmpty()) {
            return of(N.copyOfRange(a, 0, a.length));
        }

        return symmetricDifference(of(a));
    }

    public int occurrencesOf(final byte objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    public int indexOf(byte e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, byte e) {
        checkFromToIndex(fromIndex, size);

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
        checkFromToIndex(0, fromIndex);

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
        checkFromToIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(N.min(elementData, fromIndex, toIndex));
    }

    public OptionalByte median() {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(N.median(elementData, 0, size));
    }

    public OptionalByte median(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(N.median(elementData, fromIndex, toIndex));
    }

    public OptionalByte max() {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(N.max(elementData, 0, size));
    }

    public OptionalByte max(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(N.max(elementData, fromIndex, toIndex));
    }

    public OptionalByte kthLargest(final int k) {
        return kthLargest(0, size(), k);
    }

    public OptionalByte kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkFromToIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? OptionalByte.empty() : OptionalByte.of(N.kthLargest(elementData, fromIndex, toIndex, k));
    }

    public Long sum() {
        return sum(0, size());
    }

    public Long sum(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return N.sum(elementData, fromIndex, toIndex);
    }

    public OptionalDouble average() {
        return average(0, size());
    }

    public OptionalDouble average(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(N.average(elementData, fromIndex, toIndex));
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, ByteConsumer action) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex, size);

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(elementData[i]);
                }
            } else {
                for (int i = N.min(size - 1, fromIndex); i > toIndex; i--) {
                    action.accept(elementData[i]);
                }
            }
        }
    }

    public void forEach(IndexedByteConsumer action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, IndexedByteConsumer action) {
        N.checkFromToIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex, size);

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(i, elementData[i], elementData);
                }
            } else {
                for (int i = N.min(size - 1, fromIndex); i > toIndex; i--) {
                    action.accept(i, elementData[i], elementData);
                }
            }
        }
    }

    public OptionalByte first() {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(elementData[0]);
    }

    public OptionalByte last() {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(elementData[size() - 1]);
    }

    public OptionalByte findFirst(BytePredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalByte.of(elementData[i]);
            }
        }

        return OptionalByte.empty();
    }

    //    public Optional<IndexedByte> findFirst2(BytePredicate predicate) {
    //        for (int i = 0; i < size; i++) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedByte.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalByte findLast(BytePredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalByte.of(elementData[i]);
            }
        }

        return OptionalByte.empty();
    }

    //    public Optional<IndexedByte> findLast2(BytePredicate predicate) {
    //        for (int i = size - 1; i >= 0; i--) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedByte.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalInt findFirstIndex(BytePredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    public OptionalInt findLastIndex(BytePredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, BytePredicate filter) {
        checkFromToIndex(fromIndex, toIndex);

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
        checkFromToIndex(fromIndex, toIndex);

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
        checkFromToIndex(fromIndex, toIndex);

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
        checkFromToIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public ByteList filter(final int fromIndex, final int toIndex, BytePredicate filter) {
        checkFromToIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public ByteList filter(final int fromIndex, final int toIndex, BytePredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter, max);
    }

    public <T> ExList<T> mapToObj(final ByteFunction<? extends T> mapper) {
        return mapToObj(0, size, mapper);
    }

    public <T> ExList<T> mapToObj(final int fromIndex, final int toIndex, final ByteFunction<? extends T> mapper) {
        checkFromToIndex(fromIndex, toIndex);

        final ExList<T> result = new ExList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(mapper.apply(elementData[i]));
        }

        return result;
    }

    /**
     * This is equivalent to:
     * <pre>
     * <code>
     *    if (isEmpty()) {
     *        return OptionalByte.empty();
     *    }
     *
     *    byte result = elementData[0];
     *
     *    for (int i = 1; i < size; i++) {
     *        result = accumulator.applyAsByte(result, elementData[i]);
     *    }
     *
     *    return OptionalByte.of(result);
     * </code>
     * </pre>
     * 
     * @param accumulator
     * @return
     */
    public OptionalByte reduce(final ByteBinaryOperator accumulator) {
        if (isEmpty()) {
            return OptionalByte.empty();
        }

        byte result = elementData[0];

        for (int i = 1; i < size; i++) {
            result = accumulator.applyAsByte(result, elementData[i]);
        }

        return OptionalByte.of(result);
    }

    /**
     * This is equivalent to:
     * <pre>
     * <code>
     *     if (isEmpty()) {
     *         return identity;
     *     }
     * 
     *     byte result = identity;
     * 
     *     for (int i = 0; i < size; i++) {
     *         result = accumulator.applyAsByte(result, elementData[i]);
     *    }
     * 
     *     return result;
     * </code>
     * </pre>
     * 
     * @param identity
     * @param accumulator
     * @return
     */
    public byte reduce(final byte identity, final ByteBinaryOperator accumulator) {
        if (isEmpty()) {
            return identity;
        }

        byte result = identity;

        for (int i = 0; i < size; i++) {
            result = accumulator.applyAsByte(result, elementData[i]);
        }

        return result;
    }

    @Override
    public ByteList distinct(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

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
        checkFromToIndex(fromIndex, toIndex);

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
        checkFromToIndex(fromIndex, toIndex);

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
        if (size() > 1) {
            N.shuffle(elementData);
        }
    }

    @Override
    public void shuffle(final Random rnd) {
        if (size() > 1) {
            N.shuffle(elementData, rnd);
        }
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
    public ByteList copy(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return new ByteList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    @Override
    public ByteList copy(final int from, final int to, final int step) {
        checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from);

        return new ByteList(N.copyOfRange(elementData, from, to, step));
    }

    @Override
    public ExList<ByteList> split(final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex);

        final ExList<byte[]> list = N.split(elementData, fromIndex, toIndex, size);
        @SuppressWarnings("rawtypes")
        final ExList<ByteList> result = (ExList) list;

        for (int i = 0, len = list.size(); i < len; i++) {
            result.set(i, of(list.get(i)));
        }

        return result;
    }

    //    @Override
    //    public List<ByteList> split(int fromIndex, int toIndex, BytePredicate predicate) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final List<ByteList> result = new ArrayList<>();
    //        ByteList piece = null;
    //
    //        for (int i = fromIndex; i < toIndex;) {
    //            if (piece == null) {
    //                piece = ByteList.of(N.EMPTY_BYTE_ARRAY);
    //            }
    //
    //            if (predicate.test(elementData[i])) {
    //                piece.add(elementData[i]);
    //                i++;
    //            } else {
    //                result.add(piece);
    //                piece = null;
    //            }
    //        }
    //
    //        if (piece != null) {
    //            result.add(piece);
    //        }
    //
    //        return result;
    //    }

    @Override
    public String join(int fromIndex, int toIndex, char delimiter) {
        checkFromToIndex(fromIndex, toIndex);

        return N.join(elementData, fromIndex, toIndex, delimiter);
    }

    @Override
    public String join(int fromIndex, int toIndex, String delimiter) {
        checkFromToIndex(fromIndex, toIndex);

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

    public ExList<Byte> boxed() {
        return boxed(0, size);
    }

    public ExList<Byte> boxed(int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        final Byte[] b = new Byte[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ExList.of(b);
    }

    public IntList toIntList() {
        return IntList.from(elementData, 0, size);
    }

    @Override
    public List<Byte> toList(final int fromIndex, final int toIndex, final IntFunction<List<Byte>> supplier) {
        checkFromToIndex(fromIndex, toIndex);

        final List<Byte> list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public Set<Byte> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<Byte>> supplier) {
        checkFromToIndex(fromIndex, toIndex);

        final Set<Byte> set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<Byte> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<Byte>> supplier) {
        checkFromToIndex(fromIndex, toIndex);

        final Multiset<Byte> multiset = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    public <K> Map<K, List<Byte>> toMap(ByteFunction<? extends K> classifier) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, List<Byte>>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, mapFactory);
    }

    public <K, M extends Map<K, List<Byte>>> M toMap(ByteFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Byte, ?, List<Byte>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D> Map<K, D> toMap(ByteFunction<? extends K> classifier, Collector<Byte, A, D> downstream) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, D>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D, M extends Map<K, D>> M toMap(final ByteFunction<? extends K> classifier, final Collector<Byte, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Byte> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;

        for (int i = 0; i < size; i++) {
            key = N.requireNonNull(classifier.apply(elementData[i]), "element cannot be mapped to a null key");

            if ((v = intermediate.get(key)) == null) {
                if ((v = downstreamSupplier.get()) != null) {
                    intermediate.put(key, v);
                }
            }

            downstreamAccumulator.accept(v, elementData[i]);
        }

        final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
            @Override
            public A apply(K k, A v) {
                return (A) downstream.finisher().apply(v);
            }
        };

        Seq.replaceAll(intermediate, function);

        return result;
    }

    public <K, U> Map<K, U> toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = BinaryOperator.THROWING_MERGER;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U> Map<K, U> toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = 0; i < size; i++) {
            Seq.merge(result, keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]), mergeFunction);
        }

        return result;
    }

    public <K, U> Map<K, List<U>> toMap2(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper) {
        return toMap(keyMapper, (Collector<Byte, ?, List<U>>) (Collector<?, ?, ?>) mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> M toMap2(ByteFunction<? extends K> keyMapper, ByteFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        return toMap(keyMapper, (Collector<Byte, ?, List<U>>) (Collector) mapping(valueMapper, Collectors.toList()), mapFactory);
    }

    private <U, A, R> Collector<Byte, ?, R> mapping(final ByteFunction<? extends U> mapper, final Collector<? super U, A, R> downstream) {
        return Collectors.mapping(new Function<Byte, U>() {
            @Override
            public U apply(Byte t) {
                return mapper.apply(t);
            }
        }, downstream);
    }

    //    public Seq<Byte> toSeq() {
    //        return toSeq(0, size());
    //    }
    //
    //    public Seq<Byte> toSeq(final int fromIndex, final int toIndex) {
    //        return Seq.of(toList(fromIndex, toIndex));
    //    }
    //
    //    public Seq<Byte> toSeq(final IntFunction<Collection<Byte>> supplier) {
    //        return toSeq(0, size(), supplier);
    //    }
    //
    //    public Seq<Byte> toSeq(final int fromIndex, final int toIndex, final IntFunction<Collection<Byte>> supplier) {
    //        final Collection<Byte> c = supplier.apply(toIndex - fromIndex);
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            c.add(elementData[i]);
    //        }
    //
    //        return Seq.of(c);
    //    }

    public ByteIterator byteIterator() {
        if (isEmpty()) {
            return ByteIterator.EMPTY;
        }

        return new ByteIterator() {
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < size;
            }

            @Override
            public byte next() {
                if (cursor >= size) {
                    throw new NoSuchElementException();
                }

                return elementData[cursor++];
            }

            @Override
            public void remove() {
                throw new UnsupportedOperationException();
            }
        };
    }

    public ByteStream stream0() {
        return ByteStream.of(elementData, 0, size());
    }

    public ByteStream stream0(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

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
