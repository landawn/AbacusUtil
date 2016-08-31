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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.landawn.abacus.util.function.ByteBinaryOperator;
import com.landawn.abacus.util.function.ByteConsumer;
import com.landawn.abacus.util.function.ByteFunction;
import com.landawn.abacus.util.function.BytePredicate;
import com.landawn.abacus.util.stream.ByteStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ByteList extends PrimitiveNumberList<ByteConsumer, BytePredicate, Byte, byte[], ByteList> {
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
    public ByteList(byte... a) {
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

    public static ByteList empty() {
        return new ByteList(N.EMPTY_BYTE_ARRAY);
    }

    public static ByteList of(byte... a) {
        return new ByteList(a);
    }

    public static ByteList of(byte[] a, int size) {
        return new ByteList(a, size);
    }

    public static ByteList from(int... a) {
        return from(a, 0, a.length);
    }

    public static ByteList from(int[] a, final int startIndex, final int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final byte[] elementData = new byte[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (a[i] < Byte.MIN_VALUE || a[i] > Byte.MAX_VALUE) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (byte) a[i];
        }

        return of(elementData);
    }

    public static ByteList from(String... a) {
        return from(a, 0, a.length);
    }

    public static ByteList from(String[] a, final int startIndex, final int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final byte[] elementData = new byte[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            double val = N.asDouble(a[i]);

            if (N.compare(val, Byte.MIN_VALUE) < 0 || N.compare(val, Byte.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (byte) val;
        }

        return of(elementData);
    }

    public static ByteList from(List<String> c) {
        return from(c, (byte) 0);
    }

    public static ByteList from(List<String> c, byte defaultValueForNull) {
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

    public static ByteList from(Collection<? extends Number> c) {
        return from(c, (byte) 0);
    }

    public static ByteList from(Collection<? extends Number> c, byte defaultValueForNull) {
        final byte[] a = new byte[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                double val = e.doubleValue();

                if (N.compare(val, Byte.MIN_VALUE) < 0 || N.compare(val, Byte.MAX_VALUE) > 0) {
                    throw new ArithmeticException("overflow");
                }

                a[idx++] = (byte) val;
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

    //    /**
    //     * Return the first element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalByte findFirst() {
    //        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(elementData[0]);
    //    }

    public OptionalByte findFirst(BytePredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalByte.of(elementData[i]);
            }
        }

        return OptionalByte.empty();
    }

    //    /**
    //     * Return the last element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalByte findLast() {
    //        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(elementData[size - 1]);
    //    }

    public OptionalByte findLast(BytePredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalByte.of(elementData[i]);
            }
        }

        return OptionalByte.empty();
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
    public ByteList subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new ByteList(N.copyOfRange(elementData, fromIndex, toIndex));
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
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(N.kthLargest(elementData, 0, size, k));
    }

    public OptionalByte kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(N.kthLargest(elementData, fromIndex, toIndex, k));
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
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                action.accept(elementData[i]);
            }
        }
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

    public <R> List<R> map(final ByteFunction<? extends R> func) {
        return map(0, size(), func);
    }

    public <R> List<R> map(final int fromIndex, final int toIndex, final ByteFunction<? extends R> func) {
        return map(List.class, fromIndex, toIndex, func);
    }

    public <R, V extends Collection<R>> V map(final Class<? extends V> collClass, final ByteFunction<? extends R> func) {
        return map(collClass, 0, size(), func);
    }

    public <R, V extends Collection<R>> V map(final Class<? extends V> collClass, final int fromIndex, final int toIndex,
            final ByteFunction<? extends R> func) {
        checkIndex(fromIndex, toIndex);

        final V res = N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.add(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap(final ByteFunction<? extends Collection<? extends R>> func) {
        return flatMap(0, size(), func);
    }

    public <R> List<R> flatMap(final int fromIndex, final int toIndex, final ByteFunction<? extends Collection<? extends R>> func) {
        return flatMap(List.class, fromIndex, toIndex, func);
    }

    public <R, V extends Collection<R>> V flatMap(final Class<? extends V> collClass, final ByteFunction<? extends Collection<? extends R>> func) {
        return flatMap(collClass, 0, size(), func);
    }

    public <R, V extends Collection<R>> V flatMap(final Class<? extends V> collClass, final int fromIndex, final int toIndex,
            final ByteFunction<? extends Collection<? extends R>> func) {
        checkIndex(fromIndex, toIndex);

        final V res = N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap2(final ByteFunction<R[]> func) {
        return flatMap2(0, size(), func);
    }

    public <R> List<R> flatMap2(final int fromIndex, final int toIndex, final ByteFunction<R[]> func) {
        return flatMap2(List.class, fromIndex, toIndex, func);
    }

    public <R, V extends Collection<R>> V flatMap2(final Class<? extends V> collClass, final ByteFunction<R[]> func) {
        return flatMap2(collClass, 0, size(), func);
    }

    public <R, V extends Collection<R>> V flatMap2(final Class<? extends V> collClass, final int fromIndex, final int toIndex, final ByteFunction<R[]> func) {
        checkIndex(fromIndex, toIndex);

        final V res = N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(Arrays.asList(func.apply(elementData[i])));
        }

        return res;
    }

    public <K> Map<K, List<Byte>> groupBy(final ByteFunction<? extends K> func) {
        return groupBy(0, size(), func);
    }

    public <K> Map<K, List<Byte>> groupBy(final int fromIndex, final int toIndex, final ByteFunction<? extends K> func) {
        return groupBy(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Byte>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final ByteFunction<? extends K> func) {
        return groupBy(HashMap.class, collClass, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Byte>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final ByteFunction<? extends K> func) {
        return groupBy(HashMap.class, collClass, fromIndex, toIndex, func);
    }

    public <K, V extends Collection<Byte>, M extends Map<? super K, V>> M groupBy(final Class<M> outputClass, final Class<? extends V> collClass,
            final ByteFunction<? extends K> func) {

        return groupBy(outputClass, collClass, 0, size(), func);
    }

    public <K, V extends Collection<Byte>, M extends Map<? super K, V>> M groupBy(final Class<M> outputClass, final Class<? extends V> collClass,
            final int fromIndex, final int toIndex, final ByteFunction<? extends K> func) {
        checkIndex(fromIndex, toIndex);

        final M outputResult = N.newInstance(outputClass);

        K key = null;
        V values = null;

        for (int i = fromIndex; i < toIndex; i++) {
            key = func.apply(elementData[i]);
            values = outputResult.get(key);

            if (values == null) {
                values = N.newInstance(collClass);
                outputResult.put(key, values);
            }

            values.add(elementData[i]);
        }

        return outputResult;
    }

    public OptionalByte reduce(final ByteBinaryOperator accumulator) {
        return size() == 0 ? OptionalByte.empty() : OptionalByte.of(reduce((byte) 0, accumulator));
    }

    public OptionalByte reduce(final int fromIndex, final int toIndex, final ByteBinaryOperator accumulator) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalByte.empty() : OptionalByte.of(reduce(fromIndex, toIndex, (byte) 0, accumulator));
    }

    public byte reduce(final byte identity, final ByteBinaryOperator accumulator) {
        return reduce(0, size(), identity, accumulator);
    }

    public byte reduce(final int fromIndex, final int toIndex, final byte identity, final ByteBinaryOperator accumulator) {
        checkIndex(fromIndex, toIndex);

        byte result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.applyAsByte(result, elementData[i]);
        }

        return result;
    }

    @Override
    public ByteList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.removeDuplicates(elementData, fromIndex, toIndex, false));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
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
    public void sort() {
        if (size > 1) {
            N.sort(elementData, 0, size);
        }
    }

    @Override
    public void reverse() {
        if (size > 1) {
            N.reverse(elementData, 0, size);
        }
    }

    @Override
    public void rotate(int distance) {
        if (size > 1) {
            N.rotate(elementData, distance);
        }
    }

    @Override
    public ByteList copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new ByteList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    @Override
    public ByteList trimToSize() {
        if (elementData.length != size) {
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
    public void toList(List<Byte> list, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public void toSet(Set<Byte> set, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public void toMultiset(Multiset<Byte> multiset, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }
    }

    public <K, U> Map<K, U> toMap(final ByteFunction<? extends K> keyMapper, final ByteFunction<? extends U> valueMapper) {
        return toMap(HashMap.class, keyMapper, valueMapper);
    }

    public <K, U, M extends Map<K, U>> M toMap(final Class<? extends M> outputClass, final ByteFunction<? extends K> keyMapper,
            final ByteFunction<? extends U> valueMapper) {
        return toMap(outputClass, 0, size(), keyMapper, valueMapper);
    }

    public <K, U> Map<K, U> toMap(final int fromIndex, final int toIndex, final ByteFunction<? extends K> keyMapper,
            final ByteFunction<? extends U> valueMapper) {
        return toMap(HashMap.class, fromIndex, toIndex, keyMapper, valueMapper);
    }

    public <K, U, M extends Map<K, U>> M toMap(final Class<? extends M> outputClass, final int fromIndex, final int toIndex,
            final ByteFunction<? extends K> keyMapper, final ByteFunction<? extends U> valueMapper) {
        checkIndex(fromIndex, toIndex);

        final Map<K, U> map = N.newInstance(outputClass);

        for (int i = fromIndex; i < toIndex; i++) {
            map.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
        }

        return (M) map;
    }

    public <K, U> Multimap<K, U, List<U>> toMultimap(final ByteFunction<? extends K> keyMapper, final ByteFunction<? extends U> valueMapper) {
        return toMultimap(HashMap.class, List.class, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final Class<? extends Map> outputClass, final Class<? extends Collection> collClass,
            final ByteFunction<? extends K> keyMapper, final ByteFunction<? extends U> valueMapper) {
        return toMultimap(outputClass, collClass, 0, size(), keyMapper, valueMapper);
    }

    public <K, U> Multimap<K, U, List<U>> toMultimap(final int fromIndex, final int toIndex, final ByteFunction<? extends K> keyMapper,
            final ByteFunction<? extends U> valueMapper) {
        return toMultimap(HashMap.class, List.class, fromIndex, toIndex, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final Class<? extends Map> outputClass, final Class<? extends Collection> collClass,
            final int fromIndex, final int toIndex, final ByteFunction<? extends K> keyMapper, final ByteFunction<? extends U> valueMapper) {
        checkIndex(fromIndex, toIndex);

        final Multimap<K, U, V> multimap = new Multimap(outputClass, collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            multimap.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
        }

        return multimap;
    }

    public ByteStream stream() {
        return stream(0, size());
    }

    public ByteStream stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return Stream.from(elementData, fromIndex, toIndex);
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
