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
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.BooleanConsumer;
import com.landawn.abacus.util.function.BooleanFunction;
import com.landawn.abacus.util.function.BooleanPredicate;
import com.landawn.abacus.util.function.BooleanUnaryOperator;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedBooleanConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class BooleanList extends AbstractList<BooleanConsumer, BooleanPredicate, Boolean, boolean[], BooleanList> {
    private boolean[] elementData = N.EMPTY_BOOLEAN_ARRAY;
    private int size = 0;

    public BooleanList() {
        super();
    }

    public BooleanList(int initialCapacity) {
        elementData = initialCapacity == 0 ? N.EMPTY_BOOLEAN_ARRAY : new boolean[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public BooleanList(boolean[] a) {
        this(a, a.length);
    }

    public BooleanList(boolean[] a, int size) {
        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static BooleanList empty() {
        return new BooleanList(N.EMPTY_BOOLEAN_ARRAY);
    }

    public static BooleanList of(boolean... a) {
        return a == null ? empty() : new BooleanList(a);
    }

    public static BooleanList of(boolean[] a, int size) {
        return a == null && size == 0 ? empty() : new BooleanList(a, size);
    }

    static BooleanList from(List<String> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, false);
    }

    static BooleanList from(List<String> c, boolean defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final boolean[] a = new boolean[c.size()];
        int idx = 0;

        for (String e : c) {
            a[idx++] = e == null ? defaultValueForNull : Boolean.valueOf(e);
        }

        return of(a);
    }

    public static BooleanList from(Collection<Boolean> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, false);
    }

    public static BooleanList from(Collection<Boolean> c, boolean defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final boolean[] a = new boolean[c.size()];
        int idx = 0;

        for (Boolean e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }

    public static BooleanList repeat(boolean element, final int len) {
        return of(Array.repeat(element, len));
    }

    public static BooleanList random(final int len) {
        final boolean[] a = new boolean[len];

        for (int i = 0; i < len; i++) {
            a[i] = RAND.nextBoolean();
        }

        return of(a);
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
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

    public void addAll(BooleanList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

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

    @Override
    public void addAll(boolean[] a) {
        addAll(size(), a);
    }

    @Override
    public void addAll(int index, boolean[] a) {
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
    public boolean remove(boolean e) {
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
    public boolean removeAllOccurrences(boolean e) {
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
    }

    private void fastRemove(int index) {
        int numMoved = size - index - 1;

        if (numMoved > 0) {
            N.copy(elementData, index + 1, elementData, index, numMoved);
        }

        elementData[--size] = false; // clear to let GC do its work
    }

    public boolean removeAll(BooleanList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(boolean[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    @Override
    public boolean removeIf(BooleanPredicate p) {
        final BooleanList tmp = new BooleanList(size());

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

    public boolean retainAll(BooleanList c) {
        return batchRemove(c, true) > 0;
    }

    public boolean retainAll(boolean[] a) {
        return retainAll(BooleanList.of(a));
    }

    private int batchRemove(BooleanList c, boolean complement) {
        final boolean[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<Boolean> set = c.toSet();

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
            N.fill(elementData, w, size, false);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element
     */
    public boolean delete(int index) {
        rangeCheck(index);

        boolean oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    @Override
    public void deleteAll(int... indices) {
        N.deleteAll(elementData, indices);
    }

    public int replaceAll(boolean oldVal, boolean newVal) {
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

    public void replaceAll(BooleanUnaryOperator operator) {
        for (int i = 0, len = size(); i < len; i++) {
            elementData[i] = operator.applyAsBoolean(elementData[i]);
        }
    }

    public boolean replaceIf(boolean newValue, BooleanPredicate predicate) {
        boolean result = false;

        for (int i = 0, len = size(); i < len; i++) {
            if (predicate.test(elementData[i])) {
                elementData[i] = newValue;

                result = true;
            }
        }

        return result;
    }

    public void fill(final boolean val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final boolean val) {
        checkIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(boolean e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(BooleanList c) {
        final boolean[] srcElementData = c.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Boolean> set = c.toSet();

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
    public boolean containsAll(boolean[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return containsAll(of(a));
    }

    public boolean disjoint(final BooleanList c) {
        final BooleanList container = size() >= c.size() ? this : c;
        final boolean[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (iterElements.length > 3 && container.size() > 9) {
            final Set<Boolean> set = container.toSet();

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
    public boolean disjoint(final boolean[] b) {
        if (N.isNullOrEmpty(b)) {
            return true;
        }

        return disjoint(of(b));
    }

    public int occurrencesOf(final boolean objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public BooleanList intersection(BooleanList b) {
        final Multiset<Boolean> bOccurrences = b.toMultiset();

        final BooleanList c = new BooleanList(N.min(9, size(), b.size()));

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
     * @return
     * @see IntList#difference(IntList)
     */
    public BooleanList difference(BooleanList b) {
        final Multiset<Boolean> bOccurrences = b.toMultiset();

        final BooleanList c = new BooleanList(N.min(size(), N.max(9, size() - b.size())));

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
     * @return this.difference(b).addAll(b.difference(this))
     * @see IntList#symmetricDifference(IntList)
     */
    public BooleanList symmetricDifference(BooleanList b) {
        //        final BooleanList result = this.difference(b);
        //
        //        result.addAll(b.difference(this));
        //
        //        return result;

        final Multiset<Boolean> bOccurrences = b.toMultiset();

        final BooleanList c = new BooleanList(N.max(9, Math.abs(size() - b.size())));

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

    public int indexOf(boolean e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, boolean e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(boolean e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, boolean e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, BooleanConsumer action) {
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

    public void forEach(IndexedBooleanConsumer action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, IndexedBooleanConsumer action) {
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

    public OptionalBoolean first() {
        return size() == 0 ? OptionalBoolean.empty() : OptionalBoolean.of(elementData[0]);
    }

    public OptionalBoolean last() {
        return size() == 0 ? OptionalBoolean.empty() : OptionalBoolean.of(elementData[size() - 1]);
    }

    public OptionalBoolean findFirst(BooleanPredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalBoolean.of(elementData[i]);
            }
        }

        return OptionalBoolean.empty();
    }

    //    public Optional<IndexedBoolean> findFirst2(BooleanPredicate predicate) {
    //        for (int i = 0; i < size; i++) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedBoolean.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalBoolean findLast(BooleanPredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalBoolean.of(elementData[i]);
            }
        }

        return OptionalBoolean.empty();
    }

    //    public Optional<IndexedBoolean> findLast2(BooleanPredicate predicate) {
    //        for (int i = size - 1; i >= 0; i--) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(IndexedBoolean.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalInt findFirstIndex(BooleanPredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    public OptionalInt findLastIndex(BooleanPredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, final BooleanPredicate filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, final BooleanPredicate filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, final BooleanPredicate filter) {
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
        if (size < 2) {
            return false;
        } else if (size == 2) {
            return elementData[0] == elementData[1];
        } else {
            return true;
        }
    }

    @Override
    public int count(final int fromIndex, final int toIndex, final BooleanPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public BooleanList filter(final int fromIndex, final int toIndex, final BooleanPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public BooleanList filter(final int fromIndex, final int toIndex, final BooleanPredicate filter, int max) {
        checkIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter, max);
    }

    public <T> ObjectList<T> mapToObj(final BooleanFunction<? extends T> mapper) {
        return mapToObj(0, size, mapper);
    }

    public <T> ObjectList<T> mapToObj(final int fromIndex, final int toIndex, final BooleanFunction<? extends T> mapper) {
        checkIndex(fromIndex, toIndex);

        final ObjectList<T> result = new ObjectList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(mapper.apply(elementData[i]));
        }

        return result;
    }

    @Override
    public BooleanList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.distinct(elementData, fromIndex, toIndex));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
    }

    @Override
    public void sort() {
        if (size <= 1) {
            return;
        }

        final int[] count = new int[2];

        for (int i = 0; i < size; i++) {
            count[elementData[i] == false ? 0 : 1]++;
        }

        if (count[0] > 0) {
            N.fill(elementData, 0, count[0], false);
        }

        if (count[1] > 0) {
            N.fill(elementData, count[0], count[0] + count[1], true);
        }
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
    public BooleanList copy() {
        return new BooleanList(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public BooleanList copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new BooleanList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    @Override
    public BooleanList copy(final int from, final int to, final int step) {
        checkIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from);

        return new BooleanList(N.copyOfRange(elementData, from, to, step));
    }

    @Override
    public List<BooleanList> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<boolean[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<BooleanList> result = new ArrayList<>(list.size());

        for (boolean[] a : list) {
            result.add(of(a));
        }

        return result;
    }

    //    @Override
    //    public List<BooleanList> split(int fromIndex, int toIndex, BooleanPredicate predicate) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final List<BooleanList> result = new ArrayList<>();
    //        BooleanList piece = null;
    //
    //        for (int i = fromIndex; i < toIndex;) {
    //            if (piece == null) {
    //                piece = BooleanList.of(N.EMPTY_BOOLEAN_ARRAY);
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

    public ObjectList<Boolean> boxed() {
        return boxed(0, size);
    }

    public ObjectList<Boolean> boxed(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Boolean[] b = new Boolean[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ObjectList.of(b);
    }

    @Override
    public List<Boolean> toList(final int fromIndex, final int toIndex, final IntFunction<List<Boolean>> supplier) {
        checkIndex(fromIndex, toIndex);

        final List<Boolean> list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public Set<Boolean> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<Boolean>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Set<Boolean> set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<Boolean> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<Boolean>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Multiset<Boolean> multiset = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    public <K> Map<K, List<Boolean>> toMap(BooleanFunction<? extends K> classifier) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, List<Boolean>>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, mapFactory);
    }

    public <K, M extends Map<K, List<Boolean>>> M toMap(BooleanFunction<? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<Boolean, ?, List<Boolean>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D> Map<K, D> toMap(BooleanFunction<? extends K> classifier, Collector<Boolean, A, D> downstream) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, D>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D, M extends Map<K, D>> M toMap(final BooleanFunction<? extends K> classifier, final Collector<Boolean, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Boolean> downstreamAccumulator = downstream.accumulator();
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

    public <K, U> Map<K, U> toMap(BooleanFunction<? extends K> keyMapper, BooleanFunction<? extends U> valueMapper) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(BooleanFunction<? extends K> keyMapper, BooleanFunction<? extends U> valueMapper, Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = BinaryOperator.THROWING_MERGER;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U> Map<K, U> toMap(BooleanFunction<? extends K> keyMapper, BooleanFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(BooleanFunction<? extends K> keyMapper, BooleanFunction<? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = 0; i < size; i++) {
            Seq.merge(result, keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]), mergeFunction);
        }

        return result;
    }

    public <K, U> Map<K, List<U>> toMap2(BooleanFunction<? extends K> keyMapper, BooleanFunction<? extends U> valueMapper) {
        return toMap(keyMapper, (Collector<Boolean, ?, List<U>>) (Collector<?, ?, ?>) mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> M toMap2(BooleanFunction<? extends K> keyMapper, BooleanFunction<? extends U> valueMapper,
            Supplier<M> mapFactory) {
        return toMap(keyMapper, (Collector<Boolean, ?, List<U>>) (Collector) mapping(valueMapper, Collectors.toList()), mapFactory);
    }

    private <U, A, R> Collector<Boolean, ?, R> mapping(final BooleanFunction<? extends U> mapper, final Collector<? super U, A, R> downstream) {
        return Collectors.mapping(new Function<Boolean, U>() {
            @Override
            public U apply(Boolean t) {
                return mapper.apply(t);
            }
        }, downstream);
    }

    //    public Seq<Boolean> toSeq() {
    //        return toSeq(0, size());
    //    }
    //
    //    public Seq<Boolean> toSeq(final int fromIndex, final int toIndex) {
    //        return Seq.of(toList(fromIndex, toIndex));
    //    }
    //
    //    public Seq<Boolean> toSeq(final IntFunction<Collection<Boolean>> supplier) {
    //        return toSeq(0, size(), supplier);
    //    }
    //
    //    public Seq<Boolean> toSeq(final int fromIndex, final int toIndex, final IntFunction<Collection<Boolean>> supplier) {
    //        final Collection<Boolean> c = supplier.apply(toIndex - fromIndex);
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            c.add(elementData[i]);
    //        }
    //
    //        return Seq.of(c);
    //    }

    //    public BooleanListBuilder __() {
    //        return Builder.of(this);
    //    }
    //
    //    public BooleanListBuilder __(Consumer<? super BooleanList> func) {
    //        return Builder.of(this).__(func);
    //    }

    public Stream<Boolean> stream() {
        return Stream.from(elementData, 0, size());
    }

    public Stream<Boolean> stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return Stream.from(elementData, fromIndex, toIndex);
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

        if (obj instanceof BooleanList) {
            final BooleanList other = (BooleanList) obj;

            return this.size == other.size && N.equals(this.elementData, 0, other.elementData, 0, this.size);
        }

        return false;
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
