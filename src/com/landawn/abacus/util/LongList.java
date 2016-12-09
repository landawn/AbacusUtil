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
import java.util.List;
import java.util.Set;

import com.landawn.abacus.util.function.IndexedLongConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.stream.LongStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class LongList extends AbstractNumberList<LongConsumer, LongPredicate, Long, long[], LongList> {
    private long[] elementData = N.EMPTY_LONG_ARRAY;
    private int size = 0;

    public LongList() {
        super();
    }

    public LongList(int initialCapacity) {
        this();

        elementData = new long[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public LongList(long[] a) {
        this();

        elementData = a;
        size = a.length;
    }

    public LongList(long[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static LongList empty() {
        return new LongList(N.EMPTY_LONG_ARRAY);
    }

    public static LongList of(long... a) {
        return a == null ? empty() : new LongList(a);
    }

    public static LongList of(long[] a, int size) {
        return a == null && size == 0 ? empty() : new LongList(a, size);
    }

    public static LongList from(int... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static LongList from(int[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final long[] elementData = new long[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static LongList from(float... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static LongList from(float[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final long[] elementData = new long[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (N.compare(a[i], Long.MIN_VALUE) < 0 || N.compare(a[i], Long.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (long) a[i];
        }

        return of(elementData);
    }

    public static LongList from(double... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static LongList from(double[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final long[] elementData = new long[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (N.compare(a[i], Long.MIN_VALUE) < 0 || N.compare(a[i], Long.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (long) a[i];
        }

        return of(elementData);
    }

    static LongList from(List<String> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, 0);
    }

    static LongList from(List<String> c, long defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final long[] a = new long[c.size()];
        int idx = 0;

        for (String e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                double val = N.asDouble(e);

                if (N.compare(val, Long.MIN_VALUE) < 0 || N.compare(val, Long.MAX_VALUE) > 0) {
                    throw new ArithmeticException("overflow");
                }

                a[idx++] = (long) val;
            }
        }

        return of(a);
    }

    public static LongList from(Collection<Long> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, 0);
    }

    public static LongList from(Collection<Long> c, long defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final long[] a = new long[c.size()];
        int idx = 0;

        for (Long e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }

    public static LongList range(long startInclusive, final long endExclusive) {
        return of(Array.range(startInclusive, endExclusive));
    }

    public static LongList range(long startInclusive, final long endExclusive, final long by) {
        return of(Array.range(startInclusive, endExclusive, by));
    }

    public static LongList rangeClosed(long startInclusive, final long endInclusive) {
        return of(Array.rangeClosed(startInclusive, endInclusive));
    }

    public static LongList rangeClosed(long startInclusive, final long endInclusive, final long by) {
        return of(Array.range(startInclusive, endInclusive, by));
    }

    public static LongList repeat(long element, final int len) {
        return of(Array.repeat(element, len));
    }

    public static LongList random(final int len) {
        final long[] a = new long[len];

        for (int i = 0; i < len; i++) {
            a[i] = RAND.nextLong();
        }

        return of(a);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
    @Override
    public long[] array() {
        return elementData;
    }

    public long get(int index) {
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
    public long set(int index, long e) {
        rangeCheck(index);

        long oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(long e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, long e) {
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
    public void addAll(LongList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, LongList c) {
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
    public void addAll(long[] a) {
        addAll(size(), a);
    }

    @Override
    public void addAll(int index, long[] a) {
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
    public boolean remove(long e) {
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
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean removeAllOccurrences(long e) {
        int w = 0;

        for (int i = 0; i < size; i++) {
            if (elementData[i] != e) {
                elementData[w++] = elementData[i];
            }
        }

        int numRemoved = size - w;

        if (numRemoved > 0) {
            N.fill(elementData, w, size, 0);

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

    public boolean removeAll(LongList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(long[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    public boolean retainAll(LongList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(LongList c, boolean complement) {
        final long[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<Long> set = c.toSet();

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
            N.fill(elementData, w, size, 0);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element
     */
    public long delete(int index) {
        rangeCheck(index);

        long oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    @Override
    public void deleteAll(int... indices) {
        N.deleteAll(elementData, indices);
    }

    public int replaceAll(long oldVal, long newVal) {
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

    public void fill(final long val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final long val) {
        checkIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(long e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(LongList c) {
        final long[] srcElementData = c.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Long> set = c.toSet();

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
    public boolean containsAll(long[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return containsAll(of(a));
    }

    public boolean disjoint(final LongList c) {
        final LongList container = size() >= c.size() ? this : c;
        final long[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (iterElements.length > 3 && container.size() > 9) {
            final Set<Long> set = container.toSet();

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
    public boolean disjoint(final long[] b) {
        if (N.isNullOrEmpty(b)) {
            return true;
        }

        return disjoint(of(b));
    }

    public int occurrencesOf(final long objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    @Override
    public LongList subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new LongList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#except(IntList)
     */
    public LongList except(LongList b) {
        final Multiset<Long> bOccurrences = b.toMultiset();

        final LongList c = new LongList(N.min(size(), N.max(9, size() - b.size())));

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
    public LongList intersect(LongList b) {
        final Multiset<Long> bOccurrences = b.toMultiset();

        final LongList c = new LongList(N.min(9, size(), b.size()));

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
    public LongList xor(LongList b) {
        //        final LongList result = this.except(b);
        //
        //        result.addAll(b.except(this));
        //
        //        return result;

        final Multiset<Long> bOccurrences = b.toMultiset();

        final LongList c = new LongList(N.max(9, Math.abs(size() - b.size())));

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

    public int indexOf(long e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, long e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(long e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, long e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public OptionalLong min() {
        return size() == 0 ? OptionalLong.empty() : OptionalLong.of(N.min(elementData, 0, size));
    }

    public OptionalLong min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalLong.empty() : OptionalLong.of(N.min(elementData, fromIndex, toIndex));
    }

    public OptionalLong median() {
        return size() == 0 ? OptionalLong.empty() : OptionalLong.of(N.median(elementData, 0, size));
    }

    public OptionalLong median(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalLong.empty() : OptionalLong.of(N.median(elementData, fromIndex, toIndex));
    }

    public OptionalLong max() {
        return size() == 0 ? OptionalLong.empty() : OptionalLong.of(N.max(elementData, 0, size));
    }

    public OptionalLong max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalLong.empty() : OptionalLong.of(N.max(elementData, fromIndex, toIndex));
    }

    public OptionalLong kthLargest(final int k) {
        return kthLargest(0, size(), k);
    }

    public OptionalLong kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? OptionalLong.empty() : OptionalLong.of(N.kthLargest(elementData, fromIndex, toIndex, k));
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
    public void forEach(final int fromIndex, final int toIndex, LongConsumer action) {
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

    public void forEach(IndexedLongConsumer action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, IndexedLongConsumer action) {
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

    public OptionalLong findFirst(LongPredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalLong.of(elementData[i]);
            }
        }

        return OptionalLong.empty();
    }

    public OptionalLong findLast(LongPredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalLong.of(elementData[i]);
            }
        }

        return OptionalLong.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, LongPredicate filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, LongPredicate filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, LongPredicate filter) {
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
    public int count(final int fromIndex, final int toIndex, LongPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public LongList filter(final int fromIndex, final int toIndex, LongPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    @Override
    public LongList filter(final int fromIndex, final int toIndex, LongPredicate filter, final int max) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter, max));
    }

    @Override
    public LongList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.distinct(elementData, fromIndex, toIndex));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
    }

    public LongList top(final int n) {
        return top(0, size(), n);
    }

    public LongList top(final int fromIndex, final int toIndex, final int n) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, n));
    }

    public LongList top(final int n, Comparator<? super Long> cmp) {
        return top(0, size(), n, cmp);
    }

    public LongList top(final int fromIndex, final int toIndex, final int n, Comparator<? super Long> cmp) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, n, cmp));
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
    public int binarySearch(final long key) {
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
    public int binarySearch(final int fromIndex, final int toIndex, final long key) {
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
    public LongList copy() {
        return new LongList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public List<LongList> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<long[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<LongList> result = new ArrayList<>(list.size());

        for (long[] a : list) {
            result.add(LongList.of(a));
        }

        return result;
    }

    @Override
    public List<LongList> split(int fromIndex, int toIndex, LongPredicate predicate) {
        checkIndex(fromIndex, toIndex);

        final List<LongList> result = new ArrayList<>();
        LongList piece = null;

        for (int i = fromIndex; i < toIndex;) {
            if (piece == null) {
                piece = LongList.of(N.EMPTY_LONG_ARRAY);
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
    public LongList trimToSize() {
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

    public ObjectList<Long> boxed() {
        return boxed(0, size);
    }

    public ObjectList<Long> boxed(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Long[] b = new Long[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ObjectList.of(b);
    }

    @Override
    public List<Long> toList(final int fromIndex, final int toIndex, final IntFunction<List<Long>> supplier) {
        checkIndex(fromIndex, toIndex);

        final List<Long> list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public Set<Long> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<Long>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Set<Long> set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<Long> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<Long>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Multiset<Long> multiset = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    public LongStream stream() {
        return stream(0, size());
    }

    public LongStream stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return LongStream.of(elementData, fromIndex, toIndex);
    }

    //    public LongListBuilder __() {
    //        return Builder.of(this);
    //    }
    //
    //    public LongListBuilder __(Consumer<? super LongList> func) {
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

        if (obj instanceof LongList) {
            final LongList other = (LongList) obj;

            return this.size == other.size && N.equals(this.elementData, 0, other.elementData, 0, this.size);
        }

        return false;
    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_LONG_ARRAY) {
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
