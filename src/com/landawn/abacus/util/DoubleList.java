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
import java.util.Random;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.DoubleBinaryOperator;
import com.landawn.abacus.util.function.DoubleConsumer;
import com.landawn.abacus.util.function.DoubleFunction;
import com.landawn.abacus.util.function.DoublePredicate;
import com.landawn.abacus.util.function.DoubleUnaryOperator;
import com.landawn.abacus.util.function.IndexedDoubleConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.DoubleStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class DoubleList extends AbstractList<DoubleConsumer, DoublePredicate, Double, double[], DoubleList> {
    private static final long serialVersionUID = 766157472430159621L;

    private double[] elementData = N.EMPTY_DOUBLE_ARRAY;
    private int size = 0;

    public DoubleList() {
        super();
    }

    public DoubleList(int initialCapacity) {
        elementData = initialCapacity == 0 ? N.EMPTY_DOUBLE_ARRAY : new double[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public DoubleList(double[] a) {
        this(a, a.length);
    }

    public DoubleList(double[] a, int size) {
        N.checkFromIndexSize(0, size, a.length);

        this.elementData = a;
        this.size = size;
    }

    public static DoubleList empty() {
        return new DoubleList(N.EMPTY_DOUBLE_ARRAY);
    }

    public static DoubleList of(double... a) {
        return a == null ? empty() : new DoubleList(a);
    }

    public static DoubleList of(double[] a, int size) {
        return a == null && size == 0 ? empty() : new DoubleList(a, size);
    }

    public static DoubleList from(int... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static DoubleList from(int[] a, int startIndex, int endIndex) {
        N.checkFromToIndex(startIndex, endIndex, a == null ? 0 : a.length);

        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        final double[] elementData = new double[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static DoubleList from(long... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static DoubleList from(long[] a, int startIndex, int endIndex) {
        N.checkFromToIndex(startIndex, endIndex, a == null ? 0 : a.length);

        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        final double[] elementData = new double[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static DoubleList from(float... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static DoubleList from(float[] a, int startIndex, int endIndex) {
        N.checkFromToIndex(startIndex, endIndex, a == null ? 0 : a.length);

        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        final double[] elementData = new double[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    static DoubleList from(List<String> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, 0d);
    }

    static DoubleList from(List<String> c, double defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final double[] a = new double[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : N.asDouble(e);
        }

        return of(a);
    }

    public static DoubleList from(Collection<Double> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, 0d);
    }

    public static DoubleList from(Collection<Double> c, double defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final double[] a = new double[c.size()];
        int idx = 0;

        for (Double e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }

    public static DoubleList repeat(double element, final int len) {
        return of(Array.repeat(element, len));
    }

    public static DoubleList random(final int len) {
        final double[] a = new double[len];

        for (int i = 0; i < len; i++) {
            a[i] = RAND.nextDouble();
        }

        return of(a);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
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

    public boolean addAll(DoubleList c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;

        return true;
    }

    public boolean addAll(int index, DoubleList c) {
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
    public boolean addAll(double[] a) {
        return addAll(size(), a);
    }

    @Override
    public boolean addAll(int index, double[] a) {
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
    public boolean remove(double e) {
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
     */
    public boolean removeAllOccurrences(double e) {
        int w = 0;

        for (int i = 0; i < size; i++) {
            if (!N.equals(elementData[i], e)) {
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

    public boolean removeAll(DoubleList c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(double[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    @Override
    public boolean removeIf(DoublePredicate p) {
        final DoubleList tmp = new DoubleList(size());

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

    public boolean retainAll(DoubleList c) {
        if (N.isNullOrEmpty(c)) {
            boolean result = size() > 0;
            clear();
            return result;
        }

        return batchRemove(c, true) > 0;
    }

    public boolean retainAll(double[] a) {
        if (N.isNullOrEmpty(a)) {
            boolean result = size() > 0;
            clear();
            return result;
        }

        return retainAll(DoubleList.of(a));
    }

    private int batchRemove(DoubleList c, boolean complement) {
        final double[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<Double> set = c.toSet();

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
    public double delete(int index) {
        rangeCheck(index);

        double oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    @Override
    public void deleteAll(int... indices) {
        N.deleteAll(elementData, indices);
    }

    public int replaceAll(double oldVal, double newVal) {
        if (size() == 0) {
            return 0;
        }

        int result = 0;

        for (int i = 0, len = size(); i < len; i++) {
            if (Double.compare(elementData[i], oldVal) == 0) {
                elementData[i] = newVal;

                result++;
            }
        }

        return result;
    }

    public void replaceAll(DoubleUnaryOperator operator) {
        for (int i = 0, len = size(); i < len; i++) {
            elementData[i] = operator.applyAsDouble(elementData[i]);
        }
    }

    public boolean replaceIf(double newValue, DoublePredicate predicate) {
        boolean result = false;

        for (int i = 0, len = size(); i < len; i++) {
            if (predicate.test(elementData[i])) {
                elementData[i] = newValue;

                result = true;
            }
        }

        return result;
    }

    public void fill(final double val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final double val) {
        checkFromToIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(double e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(DoubleList c) {
        if (N.isNullOrEmpty(c)) {
            return true;
        }

        final double[] srcElementData = c.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Double> set = c.toSet();

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
    public boolean containsAll(double[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return containsAll(of(a));
    }

    public boolean containsAny(DoubleList c) {
        if (this.isEmpty() || N.isNullOrEmpty(c)) {
            return false;
        }

        return !disjoint(c);
    }

    @Override
    public boolean containsAny(double[] a) {
        if (this.isEmpty() || N.isNullOrEmpty(a)) {
            return false;
        }

        return !disjoint(a);
    }

    public boolean disjoint(final DoubleList c) {
        if (N.isNullOrEmpty(c)) {
            return true;
        }

        final DoubleList container = size() >= c.size() ? this : c;
        final double[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (iterElements.length > 3 && container.size() > 9) {
            final Set<Double> set = container.toSet();

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
    public boolean disjoint(final double[] b) {
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
    public DoubleList intersection(final DoubleList b) {
        if (N.isNullOrEmpty(b)) {
            return empty();
        }

        final Multiset<Double> bOccurrences = b.toMultiset();

        final DoubleList c = new DoubleList(N.min(9, size(), b.size()));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) > 0) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    public DoubleList intersection(final double[] a) {
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
    public DoubleList difference(DoubleList b) {
        if (N.isNullOrEmpty(b)) {
            return of(N.copyOfRange(elementData, 0, size()));
        }

        final Multiset<Double> bOccurrences = b.toMultiset();

        final DoubleList c = new DoubleList(N.min(size(), N.max(9, size() - b.size())));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) < 1) {
                c.add(elementData[i]);
            }
        }

        return c;
    }

    public DoubleList difference(final double[] a) {
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
    public DoubleList symmetricDifference(DoubleList b) {
        if (N.isNullOrEmpty(b)) {
            return this.copy();
        } else if (this.isEmpty()) {
            return b.copy();
        }

        final Multiset<Double> bOccurrences = b.toMultiset();
        final DoubleList c = new DoubleList(N.max(9, Math.abs(size() - b.size())));

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

    public DoubleList symmetricDifference(final double[] a) {
        if (N.isNullOrEmpty(a)) {
            return of(N.copyOfRange(elementData, 0, size()));
        } else if (this.isEmpty()) {
            return of(N.copyOfRange(a, 0, a.length));
        }

        return symmetricDifference(of(a));
    }

    public int occurrencesOf(final double objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    public int indexOf(double e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, double e) {
        checkFromToIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(double e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, double e) {
        checkFromToIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public OptionalDouble min() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(N.min(elementData, 0, size));
    }

    public OptionalDouble min(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(N.min(elementData, fromIndex, toIndex));
    }

    public OptionalDouble median() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(N.median(elementData, 0, size));
    }

    public OptionalDouble median(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(N.median(elementData, fromIndex, toIndex));
    }

    public OptionalDouble max() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(N.max(elementData, 0, size));
    }

    public OptionalDouble max(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(N.max(elementData, fromIndex, toIndex));
    }

    public OptionalDouble kthLargest(final int k) {
        return kthLargest(0, size(), k);
    }

    public OptionalDouble kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkFromToIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? OptionalDouble.empty() : OptionalDouble.of(N.kthLargest(elementData, fromIndex, toIndex, k));
    }

    public Double sum() {
        return sum(0, size());
    }

    public Double sum(final int fromIndex, final int toIndex) {
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
    public void forEach(final int fromIndex, final int toIndex, DoubleConsumer action) {
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

    public void forEach(IndexedDoubleConsumer action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, IndexedDoubleConsumer action) {
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

    public OptionalDouble first() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(elementData[0]);
    }

    public OptionalDouble last() {
        return size() == 0 ? OptionalDouble.empty() : OptionalDouble.of(elementData[size() - 1]);
    }

    public OptionalDouble findFirst(DoublePredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalDouble.of(elementData[i]);
            }
        }

        return OptionalDouble.empty();
    }

    //    public Optional<IndexedDouble> findFirst2(DoublePredicate predicate) {
    //        for (int i = 0; i < size; i++) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedDouble.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalDouble findLast(DoublePredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalDouble.of(elementData[i]);
            }
        }

        return OptionalDouble.empty();
    }

    //    public Optional<IndexedDouble> findLast2(DoublePredicate predicate) {
    //        for (int i = size - 1; i >= 0; i--) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedDouble.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalInt findFirstIndex(DoublePredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    public OptionalInt findLastIndex(DoublePredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, DoublePredicate filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, DoublePredicate filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, DoublePredicate filter) {
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
    public int count(final int fromIndex, final int toIndex, DoublePredicate filter) {
        checkFromToIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public DoubleList filter(final int fromIndex, final int toIndex, DoublePredicate filter) {
        checkFromToIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public DoubleList filter(final int fromIndex, final int toIndex, DoublePredicate filter, final int max) {
        checkFromToIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter, max);
    }

    public <T> ExList<T> mapToObj(final DoubleFunction<? extends T> mapper) {
        return mapToObj(0, size, mapper);
    }

    public <T> ExList<T> mapToObj(final int fromIndex, final int toIndex, final DoubleFunction<? extends T> mapper) {
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
     *        return OptionalDouble.empty();
     *    }
     *
     *    double result = elementData[0];
     *
     *    for (int i = 1; i < size; i++) {
     *        result = accumulator.applyAsDouble(result, elementData[i]);
     *    }
     *
     *    return OptionalDouble.of(result);
     * </code>
     * </pre>
     * 
     * @param accumulator
     * @return
     */
    public OptionalDouble reduce(final DoubleBinaryOperator accumulator) {
        if (isEmpty()) {
            return OptionalDouble.empty();
        }

        double result = elementData[0];

        for (int i = 1; i < size; i++) {
            result = accumulator.applyAsDouble(result, elementData[i]);
        }

        return OptionalDouble.of(result);
    }

    /**
     * This is equivalent to:
     * <pre>
     * <code>
     *     if (isEmpty()) {
     *         return identity;
     *     }
     * 
     *     double result = identity;
     * 
     *     for (int i = 0; i < size; i++) {
     *         result = accumulator.applyAsDouble(result, elementData[i]);
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
    public double reduce(final double identity, final DoubleBinaryOperator accumulator) {
        if (isEmpty()) {
            return identity;
        }

        double result = identity;

        for (int i = 0; i < size; i++) {
            result = accumulator.applyAsDouble(result, elementData[i]);
        }

        return result;
    }

    @Override
    public DoubleList distinct(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.distinct(elementData, fromIndex, toIndex));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
    }

    public DoubleList top(final int n) {
        return top(0, size(), n);
    }

    public DoubleList top(final int fromIndex, final int toIndex, final int n) {
        checkFromToIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, n));
    }

    public DoubleList top(final int n, Comparator<? super Double> cmp) {
        return top(0, size(), n, cmp);
    }

    public DoubleList top(final int fromIndex, final int toIndex, final int n, Comparator<? super Double> cmp) {
        checkFromToIndex(fromIndex, toIndex);

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
    public int binarySearch(final double key) {
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
    public int binarySearch(final int fromIndex, final int toIndex, final double key) {
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
    public DoubleList copy() {
        return new DoubleList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public DoubleList copy(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return new DoubleList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    @Override
    public DoubleList copy(final int from, final int to, final int step) {
        checkFromToIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from);

        return new DoubleList(N.copyOfRange(elementData, from, to, step));
    }

    @Override
    public ExList<DoubleList> split(final int fromIndex, final int toIndex, final int size) {
        checkFromToIndex(fromIndex, toIndex);

        final ExList<double[]> list = N.split(elementData, fromIndex, toIndex, size);
        @SuppressWarnings("rawtypes")
        final ExList<DoubleList> result = (ExList) list;

        for (int i = 0, len = list.size(); i < len; i++) {
            result.set(i, of(list.get(i)));
        }

        return result;
    }

    //    @Override
    //    public List<DoubleList> split(int fromIndex, int toIndex, DoublePredicate predicate) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final List<DoubleList> result = new ArrayList<>();
    //        DoubleList piece = null;
    //
    //        for (int i = fromIndex; i < toIndex;) {
    //            if (piece == null) {
    //                piece = DoubleList.of(N.EMPTY_DOUBLE_ARRAY);
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

    public ExList<Double> boxed() {
        return boxed(0, size);
    }

    public ExList<Double> boxed(int fromIndex, int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        final Double[] b = new Double[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ExList.of(b);
    }

    @Override
    public <R extends List<Double>> R toList(final int fromIndex, final int toIndex, final IntFunction<R> supplier) {
        checkFromToIndex(fromIndex, toIndex);

        final R list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public <R extends Set<Double>> R toSet(final int fromIndex, final int toIndex, final IntFunction<R> supplier) {
        checkFromToIndex(fromIndex, toIndex);

        final R set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<Double> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<Double>> supplier) {
        checkFromToIndex(fromIndex, toIndex);

        final Multiset<Double> multiset = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    public <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyExtractor, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = BinaryOperator.THROWING_MERGER;

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U> Map<K, U> toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyExtractor, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(DoubleFunction<? extends K> keyExtractor, DoubleFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = 0; i < size; i++) {
            Seq.merge(result, keyExtractor.apply(elementData[i]), valueMapper.apply(elementData[i]), mergeFunction);
        }

        return result;
    }

    @SuppressWarnings("hiding")
    public <K, A, D> Map<K, D> toMap(DoubleFunction<? extends K> classifier, Collector<Double, A, D> downstream) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, D>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D, M extends Map<K, D>> M toMap(final DoubleFunction<? extends K> classifier, final Collector<Double, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Double> downstreamAccumulator = downstream.accumulator();
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

    public DoubleIterator iterator() {
        if (isEmpty()) {
            return DoubleIterator.EMPTY;
        }

        return DoubleIterator.of(elementData, 0, size);
    }

    public DoubleStream stream0() {
        return DoubleStream.of(elementData, 0, size());
    }

    public DoubleStream stream0(final int fromIndex, final int toIndex) {
        checkFromToIndex(fromIndex, toIndex);

        return DoubleStream.of(elementData, fromIndex, toIndex);
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this) {
            return true;
        }

        if (obj instanceof DoubleList) {
            final DoubleList other = (DoubleList) obj;

            return this.size == other.size && N.equals(this.elementData, 0, other.elementData, 0, this.size);
        }

        return false;
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
