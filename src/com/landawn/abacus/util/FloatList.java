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

import com.landawn.abacus.util.function.FloatBinaryOperator;
import com.landawn.abacus.util.function.FloatConsumer;
import com.landawn.abacus.util.function.FloatFunction;
import com.landawn.abacus.util.function.FloatPredicate;
import com.landawn.abacus.util.stream.FloatStream;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class FloatList extends PrimitiveNumberList<FloatConsumer, FloatPredicate, Float, float[], FloatList> {
    private float[] elementData = N.EMPTY_FLOAT_ARRAY;
    private int size = 0;

    public FloatList() {
        super();
    }

    public FloatList(int initialCapacity) {
        this();

        elementData = new float[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public FloatList(float... a) {
        this();

        elementData = a;
        size = a.length;
    }

    public FloatList(float[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static FloatList empty() {
        return new FloatList(N.EMPTY_FLOAT_ARRAY);
    }

    public static FloatList of(float... a) {
        return new FloatList(a);
    }

    public static FloatList of(float[] a, int size) {
        return new FloatList(a, size);
    }

    public static FloatList from(int... a) {
        return from(a, 0, a.length);
    }

    public static FloatList from(int[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final float[] elementData = new float[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static FloatList from(long... a) {
        return from(a, 0, a.length);
    }

    public static FloatList from(long[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final float[] elementData = new float[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            elementData[i - startIndex] = a[i];
        }

        return of(elementData);
    }

    public static FloatList from(double... a) {
        return from(a, 0, a.length);
    }

    public static FloatList from(double[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final float[] elementData = new float[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (N.compare(a[i], Float.MIN_VALUE) < 0 || N.compare(a[i], Float.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (float) a[i];
        }

        return of(elementData);
    }

    public static FloatList from(String... a) {
        return from(a, 0, a.length);
    }

    public static FloatList from(String[] a, int startIndex, int endIndex) {
        N.checkIndex(startIndex, endIndex, a.length);

        final float[] elementData = new float[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            double val = N.asDouble(a[i]);

            if (N.compare(val, Float.MIN_VALUE) < 0 || N.compare(val, Float.MAX_VALUE) > 0) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (float) val;
        }

        return of(elementData);
    }

    public static FloatList from(List<String> c) {
        return from(c, 0f);
    }

    public static FloatList from(List<String> c, float defaultValueForNull) {
        final float[] a = new float[c.size()];
        int idx = 0;

        for (String e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                double val = N.asDouble(e);

                if (N.compare(val, Float.MIN_VALUE) < 0 || N.compare(val, Float.MAX_VALUE) > 0) {
                    throw new ArithmeticException("overflow");
                }

                a[idx++] = (float) val;
            }
        }

        return of(a);
    }

    public static FloatList from(Collection<? extends Number> c) {
        return from(c, 0f);
    }

    public static FloatList from(Collection<? extends Number> c, float defaultValueForNull) {
        final float[] a = new float[c.size()];
        int idx = 0;

        for (Number e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else {
                double val = e.doubleValue();

                if (N.compare(val, Float.MIN_VALUE) < 0 || N.compare(val, Float.MAX_VALUE) > 0) {
                    throw new ArithmeticException("overflow");
                }

                a[idx++] = (float) val;
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
    public float[] array() {
        return elementData;
    }

    //    /**
    //     * Return the first element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalFloat findFirst() {
    //        return size() == 0 ? OptionalFloat.empty() : OptionalFloat.of(elementData[0]);
    //    }

    public OptionalFloat findFirst(FloatPredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalFloat.of(elementData[i]);
            }
        }

        return OptionalFloat.empty();
    }

    //    /**
    //     * Return the last element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalFloat findLast() {
    //        return size() == 0 ? OptionalFloat.empty() : OptionalFloat.of(elementData[size - 1]);
    //    }

    public OptionalFloat findLast(FloatPredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalFloat.of(elementData[i]);
            }
        }

        return OptionalFloat.empty();
    }

    public float get(int index) {
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
    public float set(int index, float e) {
        rangeCheck(index);

        float oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(float e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, float e) {
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
    public void addAll(FloatList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, FloatList c) {
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
    public boolean remove(float e) {
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
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean removeAllOccurrences(float e) {
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

    @Override
    public boolean removeAll(FloatList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean retainAll(FloatList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(FloatList c, boolean complement) {
        final float[] elementData = this.elementData;

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
    public float delete(int index) {
        rangeCheck(index);

        float oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    public boolean contains(float e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(FloatList c) {
        final float[] srcElementData = c.array();

        for (int i = 0, srcSize = c.size(); i < srcSize; i++) {

            if (!contains(srcElementData[i])) {
                return false;
            }
        }

        return true;
    }

    @Override
    public FloatList subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new FloatList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(float e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, float e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(float e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, float e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public OptionalFloat min() {
        return size() == 0 ? OptionalFloat.empty() : OptionalFloat.of(N.min(elementData, 0, size));
    }

    public OptionalFloat min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalFloat.empty() : OptionalFloat.of(N.min(elementData, fromIndex, toIndex));
    }

    public OptionalFloat median() {
        return size() == 0 ? OptionalFloat.empty() : OptionalFloat.of(N.median(elementData, 0, size));
    }

    public OptionalFloat median(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalFloat.empty() : OptionalFloat.of(N.median(elementData, fromIndex, toIndex));
    }

    public OptionalFloat max() {
        return size() == 0 ? OptionalFloat.empty() : OptionalFloat.of(N.max(elementData, 0, size));
    }

    public OptionalFloat max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalFloat.empty() : OptionalFloat.of(N.max(elementData, fromIndex, toIndex));
    }

    public OptionalFloat kthLargest(final int k) {
        return size() == 0 ? OptionalFloat.empty() : OptionalFloat.of(N.kthLargest(elementData, 0, size, k));
    }

    public OptionalFloat kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalFloat.empty() : OptionalFloat.of(N.kthLargest(elementData, fromIndex, toIndex, k));
    }

    public Double sum() {
        return sum(0, size());
    }

    public Double sum(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return N.sum(elementData, fromIndex, toIndex);
    }

    @Override
    public OptionalDouble average(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(N.average(elementData, fromIndex, toIndex));
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, FloatConsumer action) {
        checkIndex(fromIndex, toIndex);

        if (size > 0) {
            for (int i = fromIndex; i < toIndex; i++) {
                action.accept(elementData[i]);
            }
        }
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, FloatPredicate filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, FloatPredicate filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, FloatPredicate filter) {
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
    public int count(final int fromIndex, final int toIndex, FloatPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public FloatList filter(final int fromIndex, final int toIndex, FloatPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    @Override
    public FloatList filter(final int fromIndex, final int toIndex, FloatPredicate filter, final int max) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter, max));
    }

    public <R> List<R> map(final FloatFunction<? extends R> func) {
        return map(0, size(), func);
    }

    public <R> List<R> map(final int fromIndex, final int toIndex, final FloatFunction<? extends R> func) {
        return map(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final FloatFunction<? extends R> func) {
        return map(collClass, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V map(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final FloatFunction<? extends R> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.add(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap(final FloatFunction<? extends Collection<? extends R>> func) {
        return flatMap(0, size(), func);
    }

    public <R> List<R> flatMap(final int fromIndex, final int toIndex, final FloatFunction<? extends Collection<? extends R>> func) {
        return flatMap(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass, final FloatFunction<? extends Collection<? extends R>> func) {
        return flatMap(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final FloatFunction<? extends Collection<? extends R>> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(func.apply(elementData[i]));
        }

        return res;
    }

    public <R> List<R> flatMap2(final FloatFunction<R[]> func) {
        return flatMap2(0, size(), func);
    }

    public <R> List<R> flatMap2(final int fromIndex, final int toIndex, final FloatFunction<R[]> func) {
        return flatMap2(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final FloatFunction<R[]> func) {
        return flatMap2(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap2(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final FloatFunction<R[]> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(Arrays.asList(func.apply(elementData[i])));
        }

        return res;
    }

    public <R> List<R> flatMap3(final FloatFunction<? extends Collection<? extends R>> func) {
        return flatMap3(0, size(), func);
    }

    public <R> List<R> flatMap3(final int fromIndex, final int toIndex, final FloatFunction<? extends Collection<? extends R>> func) {
        return flatMap3(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap3(final Class<? extends Collection> collClass, final FloatFunction<? extends Collection<? extends R>> func) {
        return flatMap3(List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <R, V extends Collection<R>> V flatMap3(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final FloatFunction<? extends Collection<? extends R>> func) {
        checkIndex(fromIndex, toIndex);

        final V res = (V) N.newInstance(collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            res.addAll(func.apply(elementData[i]));
        }

        return res;
    }

    public <K> Map<K, List<Float>> groupBy(final FloatFunction<? extends K> func) {
        return groupBy(0, size(), func);
    }

    public <K> Map<K, List<Float>> groupBy(final int fromIndex, final int toIndex, final FloatFunction<? extends K> func) {
        return groupBy(List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Float>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final FloatFunction<? extends K> func) {
        return groupBy(HashMap.class, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Float>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
            final FloatFunction<? extends K> func) {
        return groupBy(HashMap.class, List.class, fromIndex, toIndex, func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Float>, M extends Map<? super K, V>> M groupBy(final Class<M> outputClass, final Class<? extends Collection> collClass,
            final FloatFunction<? extends K> func) {

        return groupBy(outputClass, List.class, 0, size(), func);
    }

    @SuppressWarnings("rawtypes")
    public <K, V extends Collection<Float>, M extends Map<? super K, V>> M groupBy(final Class<M> outputClass, final Class<? extends Collection> collClass,
            final int fromIndex, final int toIndex, final FloatFunction<? extends K> func) {
        checkIndex(fromIndex, toIndex);

        final M outputResult = N.newInstance(outputClass);

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

    public OptionalFloat reduce(final FloatBinaryOperator accumulator) {
        return size() == 0 ? OptionalFloat.empty() : OptionalFloat.of(reduce(0, accumulator));
    }

    public OptionalFloat reduce(final int fromIndex, final int toIndex, final FloatBinaryOperator accumulator) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalFloat.empty() : OptionalFloat.of(reduce(fromIndex, toIndex, 0, accumulator));
    }

    public float reduce(final float identity, final FloatBinaryOperator accumulator) {
        return reduce(0, size(), identity, accumulator);
    }

    public float reduce(final int fromIndex, final int toIndex, final float identity, final FloatBinaryOperator accumulator) {
        checkIndex(fromIndex, toIndex);

        float result = identity;

        for (int i = fromIndex; i < toIndex; i++) {
            result = accumulator.applyAsFloat(result, elementData[i]);
        }

        return result;
    }

    @Override
    public FloatList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.removeDuplicates(elementData, fromIndex, toIndex, false));
        } else {
            return of(N.copyOfRange(elementData, fromIndex, toIndex));
        }
    }

    @Override
    public List<FloatList> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<float[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<FloatList> result = new ArrayList<>(list.size());

        for (float[] a : list) {
            result.add(FloatList.of(a));
        }

        return result;
    }

    @Override
    public FloatList top(final int top) {
        return top(0, size(), top);
    }

    @Override
    public FloatList top(final int fromIndex, final int toIndex, final int top) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, top));
    }

    @Override
    public FloatList top(final int top, Comparator<Float> cmp) {
        return top(0, size(), top, cmp);
    }

    @Override
    public FloatList top(final int fromIndex, final int toIndex, final int top, Comparator<Float> cmp) {
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
    public FloatList copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new FloatList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    @Override
    public FloatList trimToSize() {
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

    public ObjectList<Float> boxed() {
        return boxed(0, size);
    }

    public ObjectList<Float> boxed(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Float[] b = new Float[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ObjectList.of(b);
    }

    @Override
    public void toList(List<Float> list, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }
    }

    @Override
    public void toSet(Set<Float> set, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }
    }

    @Override
    public void toMultiset(Multiset<Float> multiset, final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }
    }

    public <K, U> Map<K, U> toMap(final FloatFunction<? extends K> keyMapper, final FloatFunction<? extends U> valueMapper) {
        return toMap(HashMap.class, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, U>> M toMap(final Class<? extends Map> outputClass, final FloatFunction<? extends K> keyMapper,
            final FloatFunction<? extends U> valueMapper) {
        return toMap(outputClass, 0, size(), keyMapper, valueMapper);
    }

    public <K, U> Map<K, U> toMap(final int fromIndex, final int toIndex, final FloatFunction<? extends K> keyMapper,
            final FloatFunction<? extends U> valueMapper) {
        return toMap(HashMap.class, fromIndex, toIndex, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, U>> M toMap(final Class<? extends Map> outputClass, final int fromIndex, final int toIndex,
            final FloatFunction<? extends K> keyMapper, final FloatFunction<? extends U> valueMapper) {
        checkIndex(fromIndex, toIndex);

        final Map<K, U> map = N.newInstance(outputClass);

        for (int i = fromIndex; i < toIndex; i++) {
            map.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
        }

        return (M) map;
    }

    public <K, U> Multimap<K, U, List<U>> toMultimap(final FloatFunction<? extends K> keyMapper, final FloatFunction<? extends U> valueMapper) {
        return toMultimap(HashMap.class, List.class, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final Class<? extends Map> outputClass, final Class<? extends Collection> collClass,
            final FloatFunction<? extends K> keyMapper, final FloatFunction<? extends U> valueMapper) {
        return toMultimap(outputClass, collClass, 0, size(), keyMapper, valueMapper);
    }

    public <K, U> Multimap<K, U, List<U>> toMultimap(final int fromIndex, final int toIndex, final FloatFunction<? extends K> keyMapper,
            final FloatFunction<? extends U> valueMapper) {
        return toMultimap(HashMap.class, List.class, fromIndex, toIndex, keyMapper, valueMapper);
    }

    @SuppressWarnings("rawtypes")
    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final Class<? extends Map> outputClass, final Class<? extends Collection> collClass,
            final int fromIndex, final int toIndex, final FloatFunction<? extends K> keyMapper, final FloatFunction<? extends U> valueMapper) {
        checkIndex(fromIndex, toIndex);

        final Multimap<K, U, V> multimap = new Multimap(outputClass, collClass);

        for (int i = fromIndex; i < toIndex; i++) {
            multimap.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
        }

        return multimap;
    }

    public FloatStream stream() {
        return stream(0, size());
    }

    public FloatStream stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return Stream.from(elementData, fromIndex, toIndex);
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size());
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof FloatList && N.equals(elementData, 0, size(), ((FloatList) obj).elementData));

    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_FLOAT_ARRAY) {
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
