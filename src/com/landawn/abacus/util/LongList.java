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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.annotation.Beta;
import com.landawn.abacus.util.function.LongBinaryOperator;
import com.landawn.abacus.util.function.LongConsumer;
import com.landawn.abacus.util.function.LongFunction;
import com.landawn.abacus.util.function.LongPredicate;
import com.landawn.abacus.util.stream.LongStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class LongList extends PrimitiveNumberList<LongConsumer, LongPredicate, Long, long[], LongList> {
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

    public static LongList of(long[] a) {
        return new LongList(a);
    }

    public static LongList of(long[] a, int size) {
        return new LongList(a, size);
    }

    public static LongList of(int[] a) {
        return of(a, 0, a.length);
    }

    public static LongList of(int[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final long[] elementData = new long[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            elementData[i - fromIndex] = a[i];
        }

        return of(elementData);
    }

    public static LongList of(float[] a) {
        return of(a, 0, a.length);
    }

    public static LongList of(float[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final long[] elementData = new long[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            if (N.compare(a[i], Long.MIN_VALUE) < 0 || N.compare(a[i], Long.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - fromIndex] = (long) a[i];
        }

        return of(elementData);
    }

    public static LongList of(double[] a) {
        return of(a, 0, a.length);
    }

    public static LongList of(double[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final long[] elementData = new long[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            if (N.compare(a[i], Long.MIN_VALUE) < 0 || N.compare(a[i], Long.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - fromIndex] = (long) a[i];
        }

        return of(elementData);
    }

    public static LongList of(String[] a) {
        return of(a, 0, a.length);
    }

    public static LongList of(String[] a, int fromIndex, int toIndex) {
        if (fromIndex < 0 || toIndex < 0 || toIndex < fromIndex) {
            throw new IllegalArgumentException("Invalid fromIndex or toIndex: " + fromIndex + ", " + toIndex);
        }

        final long[] elementData = new long[toIndex - fromIndex];

        for (int i = fromIndex; i < toIndex; i++) {
            double val = N.asDouble(a[i]);

            if (N.compare(val, Long.MIN_VALUE) < 0 || N.compare(val, Long.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - fromIndex] = (long) val;
        }

        return of(elementData);
    }

    public static LongList of(List<String> c) {
        return of(c, 0);
    }

    public static LongList of(List<String> c, long defaultValueForNull) {
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

    public static LongList of(Collection<? extends Number> c) {
        return of(c, 0);
    }

    public static LongList of(Collection<? extends Number> c, long defaultValueForNull) {
        final long[] a = new long[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                double val = e.doubleValue();

                if (N.compare(val, Long.MIN_VALUE) < 0 || N.compare(val, Long.MAX_VALUE) > 0) {
                    throw new ArithmeticException("overflow");
                }

                a[idx++] = (long) val;
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
    public long[] array() {
        return elementData;
    }

    /**
     * Return the first element of the array list.
     * @return
     */
    @Beta
    public OptionalLong findFirst() {
        return size() == 0 ? OptionalLong.empty() : OptionalLong.of(elementData[0]);
    }

    /**
     * Return the last element of the array list.
     * @return
     */
    @Beta
    public OptionalLong findLast() {
        return size() == 0 ? OptionalLong.empty() : OptionalLong.of(elementData[size - 1]);
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
     * @param removeAllOccurrences
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean remove(long e, boolean removeAllOccurrences) {
        if (removeAllOccurrences) {
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
    public boolean removeAll(LongList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean retainAll(LongList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(LongList c, boolean complement) {
        final long[] elementData = this.elementData;

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
     * @return the deleted element
     */
    public long delete(int index) {
        rangeCheck(index);

        long oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    public boolean contains(long e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(LongList c) {
        final long[] srcElementData = c.array();

        for (int i = 0, srcSize = c.size(); i < srcSize; i++) {

            if (!contains(srcElementData[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public LongList subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new LongList(N.copyOfRange(elementData, fromIndex, toIndex));
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

    public long min() {
        return N.min(elementData, 0, size);
    }

    public long min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return N.min(elementData, fromIndex, toIndex);
    }

    public long max() {
        return N.max(elementData, 0, size);
    }

    public long max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return N.max(elementData, fromIndex, toIndex);
    }

    @Override
    public Number sum(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return N.sum(elementData, fromIndex, toIndex);
    }

    @Override
    public Number avg(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return N.avg(elementData, fromIndex, toIndex);
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, LongConsumer action) {
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                action.accept(elementData[i]);
            }
        }
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
    public int count(final int fromIndex, final int toIndex, LongPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public LongList filter(final int fromIndex, final int toIndex, LongPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    public <R> List<R> map(final LongFunction<? extends R> func) {
        return map(0, size(), func);
    }

    public <R> List<R> map(final int fromIndex, final int toIndex, final LongFunction<? extends R> func) {
        return map(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final LongFunction<? extends R> func) {
        return map(collClass, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final LongFunction<? extends R> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.add(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap(final LongFunction<? extends Collection<? extends R>> func) {
        return flatMap(0, size(), func);
    }

    public <R> List<R> flatMap(final int fromIndex, final int toIndex, final LongFunction<? extends Collection<? extends R>> func) {
        return flatMap(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass, final LongFunction<? extends Collection<? extends R>> func) {
        return flatMap(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final LongFunction<? extends Collection<? extends R>> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap2(final LongFunction<R[]> func) {
        return flatMap2(0, size(), func);
    }

    public <R> List<R> flatMap2(final int fromIndex, final int toIndex, final LongFunction<R[]> func) {
        return flatMap2(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final LongFunction<R[]> func) {
        return flatMap2(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final LongFunction<R[]> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(Arrays.asList(func.apply(elementData[i])));
        }

        return res;
    }

    public <K> Map<K, List<Long>> groupBy(final LongFunction<? extends K> func) {
        return groupBy(0, size(), func);
    }

    public <K> Map<K, List<Long>> groupBy(final int fromIndex, final int toIndex, final LongFunction<? extends K> func) {
        return groupBy(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Long>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final LongFunction<? extends K> func) {
        return groupBy(HashMap.class, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Long>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final LongFunction<? extends K> func) {
        return groupBy(HashMap.class, List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Long>, R extends Map<? super K, V>> R groupBy(final Class<R> outputClass, final Class<? extends Collection> collClass,
            final LongFunction<? extends K> func) {

        return groupBy(outputClass, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Long>, R extends Map<? super K, V>> R groupBy(final Class<R> outputClass, final Class<? extends Collection> collClass,
            final int fromIndex, final int toIndex, final LongFunction<? extends K> func) {
        checkIndex(fromIndex, toIndex);

        final R outputResult = N.newInstance(outputClass);

        K key = null;
        V values = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = func.apply(elementData[i]);
            values = outputResult.get(key);

            if (values == null) {
                values = (V) N.newInstance(collClass);
                outputResult.put(key, values);
            }

            values.add(elementData[i]);
        }

        return outputResult;
    }

    public long reduce(final LongBinaryOperator accumulator) {
        return reduce(0, size(), accumulator);
    }

    public long reduce(final int fromIndex, final int toIndex, final LongBinaryOperator accumulator) {
        return reduce(fromIndex, toIndex, 0, accumulator);
    }

    public long reduce(final long identity, final LongBinaryOperator accumulator) {
        return reduce(0, size(), identity, accumulator);
    }

    public long reduce(final int fromIndex, final int toIndex, final long identity, final LongBinaryOperator accumulator) {
        checkIndex(fromIndex, toIndex);

        long result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.applyAsLong(result, elementData[i]);
        }

        return result;
    }

    @Override
    public LongList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (size > 1) {
            return of(N.removeDuplicates(elementData, fromIndex, toIndex, false));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
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
    public LongList top(final int top) {
        return top(0, size(), top);
    }

    @Override
    public LongList top(final int fromIndex, final int toIndex, final int top) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, top));
    }

    @Override
    public LongList top(final int top, Comparator<Long> cmp) {
        return top(0, size(), top, cmp);
    }

    @Override
    public LongList top(final int fromIndex, final int toIndex, final int top, Comparator<Long> cmp) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, top, cmp));
    }

    @Override
    public void sort() {
        if (size > 1) {
            N.sort(elementData, 0, size);
        }
    }

    @Override
    public LongList copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new LongList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    @Override
    public LongList trimToSize() {
        if (elementData.length != size) {
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
    public void toList(List<Long> list, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public void toSet(Set<Long> set, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public void toMultiset(Multiset<Long> multiset, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }
    }

    public LongStream stream() {
        return stream(0, size());
    }

    public LongStream stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return Stream.of(elementData, fromIndex, toIndex);
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size());
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof LongList && N.equals(elementData, 0, size(), ((LongList) obj).elementData));

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
