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
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedIntConsumer;
import com.landawn.abacus.util.function.IntConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.IntPredicate;
import com.landawn.abacus.util.function.IntUnaryOperator;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;
import com.landawn.abacus.util.stream.IntStream;

/**
 *
 * @since 0.8
 *
 * @author Haiyang Li
 */
public final class IntList extends AbstractList<IntConsumer, IntPredicate, Integer, int[], IntList> {
    private static final long serialVersionUID = 8661773953226671696L;

    private int[] elementData = N.EMPTY_INT_ARRAY;
    private int size = 0;

    public IntList() {
        super();
    }

    public IntList(int initialCapacity) {
        elementData = initialCapacity == 0 ? N.EMPTY_INT_ARRAY : new int[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     *
     * @param a
     */
    public IntList(int[] a) {
        this(a, a.length);
    }

    public IntList(int[] a, int size) {
        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static IntList empty() {
        return new IntList(N.EMPTY_INT_ARRAY);
    }

    public static IntList of(int... a) {
        return a == null ? empty() : new IntList(a);
    }

    public static IntList of(int[] a, int size) {
        return a == null && size == 0 ? empty() : new IntList(a, size);
    }

    public static IntList from(char... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static IntList from(char[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final int[] elementData = new int[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static IntList from(byte... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static IntList from(byte[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final int[] elementData = new int[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static IntList from(short... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static IntList from(short[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final int[] elementData = new int[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static IntList from(long... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static IntList from(long[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final int[] elementData = new int[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (a[i] < Integer.MIN_VALUE || a[i] > Integer.MAX_VALUE) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (int) a[i];
        }

        return of(elementData);
    }

    public static IntList from(float... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static IntList from(float[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final int[] elementData = new int[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (N.compare(a[i], Integer.MIN_VALUE) < 0 || N.compare(a[i], Integer.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (int) a[i];
        }

        return of(elementData);
    }

    public static IntList from(double... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static IntList from(double[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final int[] elementData = new int[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (N.compare(a[i], Integer.MIN_VALUE) < 0 || N.compare(a[i], Integer.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (int) a[i];
        }

        return of(elementData);
    }

    //    public static IntList from(String... a) {
    //        return a == null ? empty() : from(a, 0, a.length);
    //    }
    //
    //    public static IntList from(String[] a, int startIndex, int endIndex) {
    //        if (a == null && (startIndex == 0 && endIndex == 0)) {
    //            return empty();
    //        }
    //
    //        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);
    //
    //        final int[] elementData = new int[endIndex - startIndex];
    //
    //        for (int i = startIndex; i < endIndex; i++) {
    //            elementData[i - startIndex] = N.asInt(a[i]);
    //        }
    //
    //        return of(elementData);
    //    }

    static IntList from(List<String> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, 0);
    }

    static IntList from(List<String> c, int defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final int[] a = new int[c.size()];
        int idx = 0;

        for (String e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                double val = N.asDouble(e);

                if (N.compare(val, Integer.MIN_VALUE) < 0 || N.compare(val, Integer.MAX_VALUE) > 0) {
                    throw new ArithmeticException("overflow");
                }

                a[idx++] = (int) val;
            }
        }

        return of(a);
    }

    public static IntList from(Collection<Integer> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, 0);
    }

    public static IntList from(Collection<Integer> c, int defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final int[] a = new int[c.size()];
        int idx = 0;

        for (Integer e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }

    public static IntList range(int startInclusive, final int endExclusive) {
        return of(Array.range(startInclusive, endExclusive));
    }

    public static IntList range(int startInclusive, final int endExclusive, final int by) {
        return of(Array.range(startInclusive, endExclusive, by));
    }

    public static IntList rangeClosed(int startInclusive, final int endInclusive) {
        return of(Array.rangeClosed(startInclusive, endInclusive));
    }

    public static IntList rangeClosed(int startInclusive, final int endInclusive, final int by) {
        return of(Array.range(startInclusive, endInclusive, by));
    }

    public static IntList repeat(int element, final int len) {
        return of(Array.repeat(element, len));
    }

    public static IntList random(final int len) {
        final int[] a = new int[len];

        for (int i = 0; i < len; i++) {
            a[i] = RAND.nextInt();
        }

        return of(a);
    }

    public static IntList random(final int startInclusive, final int endExclusive, final int len) {
        if (startInclusive >= endExclusive) {
            throw new IllegalArgumentException("'startInclusive' must be less than 'endExclusive'");
        }

        final int[] a = new int[len];
        final long mod = (long) endExclusive - (long) startInclusive;

        if (mod < Integer.MAX_VALUE) {
            final int n = (int) mod;

            for (int i = 0; i < len; i++) {
                a[i] = RAND.nextInt(n) + startInclusive;
            }
        } else {
            for (int i = 0; i < len; i++) {
                a[i] = (int) (Math.abs(RAND.nextLong() % mod) + startInclusive);
            }
        }

        return of(a);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
    public int[] array() {
        return elementData;
    }

    public int get(int index) {
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
    public int set(int index, int e) {
        rangeCheck(index);

        int oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(int e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, int e) {
        rangeCheckForAdd(index);

        ensureCapacityInternal(size + 1);

        int numMoved = size - index;

        if (numMoved > 0) {
            N.copy(elementData, index, elementData, index + 1, numMoved);
        }

        elementData[index] = e;

        size++;
    }

    public boolean addAll(IntList c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;

        return true;
    }

    public boolean addAll(int index, IntList c) {
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
    public boolean addAll(int[] a) {
        return addAll(size(), a);
    }

    @Override
    public boolean addAll(int index, int[] a) {
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
    public boolean remove(int e) {
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
    public boolean removeAllOccurrences(int e) {
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

    public boolean removeAll(IntList c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(int[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    @Override
    public boolean removeIf(IntPredicate p) {
        final IntList tmp = new IntList(size());

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

    public boolean retainAll(IntList c) {
        return batchRemove(c, true) > 0;
    }

    public boolean retainAll(int[] a) {
        return retainAll(IntList.of(a));
    }

    private int batchRemove(IntList c, boolean complement) {
        final int[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<Integer> set = c.toSet();

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
    public int delete(int index) {
        rangeCheck(index);

        int oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    @Override
    public void deleteAll(int... indices) {
        N.deleteAll(elementData, indices);
    }

    public int replaceAll(int oldVal, int newVal) {
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

    public void replaceAll(IntUnaryOperator operator) {
        for (int i = 0, len = size(); i < len; i++) {
            elementData[i] = operator.applyAsInt(elementData[i]);
        }
    }

    public boolean replaceIf(int newValue, IntPredicate predicate) {
        boolean result = false;

        for (int i = 0, len = size(); i < len; i++) {
            if (predicate.test(elementData[i])) {
                elementData[i] = newValue;

                result = true;
            }
        }

        return result;
    }

    public void fill(final int val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final int val) {
        checkIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(int e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(IntList c) {
        final int[] srcElementData = c.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Integer> set = c.toSet();

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
    public boolean containsAll(int[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return containsAll(of(a));
    }

    public boolean disjoint(final IntList c) {
        final IntList container = size() >= c.size() ? this : c;
        final int[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (iterElements.length > 3 && container.size() > 9) {
            final Set<Integer> set = container.toSet();

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
    public boolean disjoint(final int[] b) {
        if (N.isNullOrEmpty(b)) {
            return true;
        }

        return disjoint(of(b));
    }

    /**
     * Returns a new list with all the elements occurred in both <code>a</code> and <code>b</code> by occurrences.
     * 
     * <pre>
     * IntList a = IntList.of(0, 1, 2, 2, 3);
     * IntList b = IntList.of(2, 5, 1);
     * a.retainAll(b); // The elements remained in a will be: [1, 2, 2].
     * 
     * IntList a = IntList.of(0, 1, 2, 2, 3);
     * IntList b = IntList.of(2, 5, 1);
     * IntList c = a.intersection(b); // The elements c in a will be: [1, 2].
     * </pre>
     * 
     * @param b
     * @return
     */
    public IntList intersection(final IntList b) {
        final Multiset<Integer> bOccurrences = b.toMultiset();

        final IntList c = new IntList(N.min(9, size(), b.size()));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) > 0) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    public IntList intersection(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return empty();
        }

        return intersection(of(a));
    }

    /**
     * Returns a new list with all the elements in <code>b</code> removed by occurrences.
     * 
     * <pre>
     * IntList a = IntList.of(0, 1, 2, 2, 3);
     * IntList b = IntList.of(2, 5, 1);
     * a.removeAll(b); // The elements remained in a will be: [0, 3].
     * 
     * IntList a = IntList.of(0, 1, 2, 2, 3);
     * IntList b = IntList.of(2, 5, 1);
     * IntList c = a.difference(b); // The elements c in a will be: [0, 2, 3].
     * </pre>
     * 
     * @param b
     * @return
     */
    public IntList difference(final IntList b) {
        final Multiset<Integer> bOccurrences = b.toMultiset();

        final IntList c = new IntList(N.min(size(), N.max(9, size() - b.size())));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) < 1) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    public IntList difference(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return of(N.copyOfRange(elementData, 0, size()));
        }

        return difference(of(a));
    }

    /**
     * <pre>
     * IntList a = IntList.of(0, 1, 2, 2, 3);
     * IntList b = IntList.of(2, 5, 1);
     * IntList c = a.symmetricDifference(b); // The elements c in a will be: [0, 2, 3, 5].
     * </pre>
     * 
     * @param b
     * @return this.difference(b).addAll(b.difference(this))
     * @see IntList#difference(IntList)
     */
    public IntList symmetricDifference(final IntList b) {
        final Multiset<Integer> bOccurrences = b.toMultiset();

        final IntList c = new IntList(N.max(9, Math.abs(size() - b.size())));

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

    public IntList symmetricDifference(final int[] a) {
        if (N.isNullOrEmpty(a)) {
            return of(N.copyOfRange(elementData, 0, size()));
        }

        return symmetricDifference(of(a));
    }

    public int occurrencesOf(final int objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    public int indexOf(int e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, int e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(int e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, int e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public OptionalInt min() {
        return size() == 0 ? OptionalInt.empty() : OptionalInt.of(N.min(elementData, 0, size));
    }

    public OptionalInt min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalInt.empty() : OptionalInt.of(N.min(elementData, fromIndex, toIndex));
    }

    public OptionalInt median() {
        return size() == 0 ? OptionalInt.empty() : OptionalInt.of(N.median(elementData, 0, size));
    }

    public OptionalInt median(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalInt.empty() : OptionalInt.of(N.median(elementData, fromIndex, toIndex));
    }

    public OptionalInt max() {
        return size() == 0 ? OptionalInt.empty() : OptionalInt.of(N.max(elementData, 0, size));
    }

    public OptionalInt max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalInt.empty() : OptionalInt.of(N.max(elementData, fromIndex, toIndex));
    }

    public OptionalInt kthLargest(final int k) {
        return kthLargest(0, size(), k);
    }

    public OptionalInt kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? OptionalInt.empty() : OptionalInt.of(N.kthLargest(elementData, fromIndex, toIndex, k));
    }

    public Long sum() {
        return sum(0, size());
    }

    public Long sum(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return N.sum(elementData, fromIndex, toIndex);
    }

    public OptionalDouble average() {
        return average(0, size());
    }

    public OptionalDouble average(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(N.average(elementData, fromIndex, toIndex));
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, IntConsumer action) {
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

    public void forEach(IndexedIntConsumer action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, IndexedIntConsumer action) {
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

    public OptionalInt first() {
        return size() == 0 ? OptionalInt.empty() : OptionalInt.of(elementData[0]);
    }

    public OptionalInt last() {
        return size() == 0 ? OptionalInt.empty() : OptionalInt.of(elementData[size() - 1]);
    }

    //    /**
    //     * Return the first element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalInt findFirst() {
    //        return size() == 0 ? OptionalInt.empty() : OptionalInt.of(elementData[0]);
    //    }

    public OptionalInt findFirst(IntPredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(elementData[i]);
            }
        }

        return OptionalInt.empty();
    }

    //    /**
    //     * Return the last element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalInt findLast() {
    //        return size() == 0 ? OptionalInt.empty() : OptionalInt.of(elementData[size - 1]);
    //    }

    //    public Optional<IndexedInt> findFirst2(IntPredicate predicate) {
    //        for (int i = 0; i < size; i++) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedInt.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalInt findLast(IntPredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(elementData[i]);
            }
        }

        return OptionalInt.empty();
    }

    //    public Optional<IndexedInt> findLast2(IntPredicate predicate) {
    //        for (int i = size - 1; i >= 0; i--) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedInt.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalInt findFirstIndex(IntPredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    public OptionalInt findLastIndex(IntPredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, IntPredicate filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, IntPredicate filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, IntPredicate filter) {
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
    public int count(final int fromIndex, final int toIndex, IntPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public IntList filter(final int fromIndex, final int toIndex, IntPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public IntList filter(final int fromIndex, final int toIndex, IntPredicate filter, final int max) {
        checkIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter, max);
    }

    public <T> ObjectList<T> mapToObj(final IntFunction<? extends T> mapper) {
        return mapToObj(0, size, mapper);
    }

    public <T> ObjectList<T> mapToObj(final int fromIndex, final int toIndex, final IntFunction<? extends T> mapper) {
        checkIndex(fromIndex, toIndex);

        final ObjectList<T> result = new ObjectList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(mapper.apply(elementData[i]));
        }

        return result;
    }

    @Override
    public IntList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.distinct(elementData, fromIndex, toIndex));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
    }

    public IntList top(final int n) {
        return top(0, size(), n);
    }

    public IntList top(final int fromIndex, final int toIndex, final int n) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, n));
    }

    public IntList top(final int n, Comparator<? super Integer> cmp) {
        return top(0, size(), n, cmp);
    }

    public IntList top(final int fromIndex, final int toIndex, final int n, Comparator<? super Integer> cmp) {
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
    public int binarySearch(final int key) {
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
    public int binarySearch(final int fromIndex, final int toIndex, final int key) {
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
    public IntList copy() {
        return new IntList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public IntList copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new IntList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    @Override
    public IntList copy(final int from, final int to, final int step) {
        checkIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from);

        return new IntList(N.copyOfRange(elementData, from, to, step));
    }

    @Override
    public ObjectList<IntList> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final ObjectList<int[]> list = N.split(elementData, fromIndex, toIndex, size);
        @SuppressWarnings("rawtypes")
        final ObjectList<IntList> result = (ObjectList) list;

        for (int i = 0, len = list.size(); i < len; i++) {
            result.set(i, of(list.get(i)));
        }

        return result;
    }

    //    @Override
    //    public List<IntList> split(int fromIndex, int toIndex, IntPredicate predicate) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final List<IntList> result = new ArrayList<>();
    //        IntList piece = null;
    //
    //        for (int i = fromIndex; i < toIndex;) {
    //            if (piece == null) {
    //                piece = IntList.of(N.EMPTY_INT_ARRAY);
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
        checkIndex(fromIndex, toIndex);

        return N.join(elementData, fromIndex, toIndex, delimiter);
    }

    @Override
    public String join(int fromIndex, int toIndex, String delimiter) {
        checkIndex(fromIndex, toIndex);

        return N.join(elementData, fromIndex, toIndex, delimiter);
    }

    @Override
    public IntList trimToSize() {
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

    public ObjectList<Integer> boxed() {
        return boxed(0, size);
    }

    public ObjectList<Integer> boxed(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Integer[] b = new Integer[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ObjectList.of(b);
    }

    public LongList toLongList() {
        return LongList.from(elementData, 0, size);
    }

    public FloatList toFloatList() {
        return FloatList.from(elementData, 0, size);
    }

    public DoubleList toDoubleList() {
        return DoubleList.from(elementData, 0, size);
    }

    @Override
    public List<Integer> toList(final int fromIndex, final int toIndex, final IntFunction<List<Integer>> supplier) {
        checkIndex(fromIndex, toIndex);

        final List<Integer> list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public Set<Integer> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<Integer>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Set<Integer> set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<Integer> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<Integer>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Multiset<Integer> multiset = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    public <K> Map<K, List<Integer>> toMap(IntFunction<? extends K> classifier) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, List<Integer>>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, mapFactory);
    }

    public <K, M extends Map<K, List<Integer>>> M toMap(IntFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Integer, ?, List<Integer>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D> Map<K, D> toMap(IntFunction<? extends K> classifier, Collector<Integer, A, D> downstream) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, D>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D, M extends Map<K, D>> M toMap(final IntFunction<? extends K> classifier, final Collector<Integer, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Integer> downstreamAccumulator = downstream.accumulator();
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

    public <K, U> Map<K, U> toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = BinaryOperator.THROWING_MERGER;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U> Map<K, U> toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = 0; i < size; i++) {
            Seq.merge(result, keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]), mergeFunction);
        }

        return result;
    }

    public <K, U> Map<K, List<U>> toMap2(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper) {
        return toMap(keyMapper, (Collector<Integer, ?, List<U>>) (Collector<?, ?, ?>) mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> M toMap2(IntFunction<? extends K> keyMapper, IntFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        return toMap(keyMapper, (Collector<Integer, ?, List<U>>) (Collector) mapping(valueMapper, Collectors.toList()), mapFactory);
    }

    private <U, A, R> Collector<Integer, ?, R> mapping(final IntFunction<? extends U> mapper, final Collector<? super U, A, R> downstream) {
        return Collectors.mapping(new Function<Integer, U>() {
            @Override
            public U apply(Integer t) {
                return mapper.apply(t);
            }
        }, downstream);
    }

    //    public Seq<Integer> toSeq() {
    //        return toSeq(0, size());
    //    }
    //
    //    public Seq<Integer> toSeq(final int fromIndex, final int toIndex) {
    //        return Seq.of(toList(fromIndex, toIndex));
    //    }
    //
    //    public Seq<Integer> toSeq(final IntFunction<Collection<Integer>> supplier) {
    //        return toSeq(0, size(), supplier);
    //    }
    //
    //    public Seq<Integer> toSeq(final int fromIndex, final int toIndex, final IntFunction<Collection<Integer>> supplier) {
    //        final Collection<Integer> c = supplier.apply(toIndex - fromIndex);
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            c.add(elementData[i]);
    //        }
    //
    //        return Seq.of(c);
    //    }

    public IntIterator intIterator() {
        if (isEmpty()) {
            return IntIterator.EMPTY;
        }

        return new IntIterator() {
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < size;
            }

            @Override
            public int next() {
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

    public IntStream stream0() {
        return IntStream.of(elementData, 0, size());
    }

    public IntStream stream0(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return IntStream.of(elementData, fromIndex, toIndex);
    }

    //    public IntListBuilder __() {
    //        return Builder.of(this);
    //    }
    //
    //    public IntListBuilder __(Consumer<? super IntList> func) {
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

        if (obj instanceof IntList) {
            final IntList other = (IntList) obj;

            return this.size == other.size && N.equals(this.elementData, 0, other.elementData, 0, this.size);
        }

        return false;
    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_INT_ARRAY) {
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
