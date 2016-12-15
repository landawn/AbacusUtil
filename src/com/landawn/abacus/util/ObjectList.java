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

import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedBiFunction;
import com.landawn.abacus.util.function.IndexedConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public class ObjectList<T> extends AbstractList<Consumer<? super T>, Predicate<? super T>, T, T[], ObjectList<T>> {
    private T[] elementData = (T[]) N.EMPTY_OBJECT_ARRAY;
    private int size = 0;

    public ObjectList() {
        super();
    }

    public ObjectList(int initialCapacity) {
        elementData = initialCapacity == 0 ? (T[]) N.EMPTY_OBJECT_ARRAY : (T[]) new Object[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     * @throws IllegalArgumentException the specified <code>a</code> is null.
     */
    public ObjectList(T[] a) {
        this(a, a.length);
    }

    /**
     * 
     * @param a
     * @param size
     * @throws IllegalArgumentException the specified <code>a</code> is null.
     */
    public ObjectList(T[] a, int size) {
        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    /**
     * 
     * @param a
     * @return
     * @throws IllegalArgumentException the specified <code>a</code> is null.
     */
    public static <T> ObjectList<T> empty() {
        return new ObjectList<T>((T[]) N.EMPTY_OBJECT_ARRAY);
    }

    /**
     * 
     * @param a
     * @return
     * @throws IllegalArgumentException the specified <code>a</code> is null.
     */
    public static <T> ObjectList<T> of(T... a) {
        return new ObjectList<T>(a);
    }

    /**
     * 
     * @param a
     * @param size
     * @return
     * @throws IllegalArgumentException the specified <code>a</code> is null.
     */
    public static <T> ObjectList<T> of(T[] a, int size) {
        return new ObjectList<T>(a, size);
    }

    public static <T> ObjectList<T> from(Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return of((T[]) c.toArray());
    }

    public static <T> ObjectList<T> from(Collection<? extends T> c, T defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final T[] a = (T[]) c.toArray();

        for (int i = 0, len = a.length; i < len; i++) {
            if (a[i] == null) {
                a[i] = defaultValueForNull;
            }
        }

        return of(a);
    }

    public static ObjectList<Boolean> from(boolean... a) {
        return of(a == null ? N.EMPTY_BOOLEAN_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Boolean> from(boolean[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_BOOLEAN_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static ObjectList<Character> from(char... a) {
        return of(a == null ? N.EMPTY_CHARACTER_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Character> from(char[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_CHARACTER_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static ObjectList<Byte> from(byte... a) {
        return of(a == null ? N.EMPTY_BYTE_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Byte> from(byte[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_BYTE_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static ObjectList<Short> from(short... a) {
        return of(a == null ? N.EMPTY_SHORT_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Short> from(short[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_SHORT_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static ObjectList<Integer> from(int... a) {
        return of(a == null ? N.EMPTY_INTEGER_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Integer> from(int[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_INTEGER_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static ObjectList<Long> from(long... a) {
        return of(a == null ? N.EMPTY_LONG_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Long> from(long[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_LONG_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static ObjectList<Float> from(float... a) {
        return of(a == null ? N.EMPTY_FLOAT_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Float> from(float[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_FLOAT_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static ObjectList<Double> from(double... a) {
        return of(a == null ? N.EMPTY_DOUBLE_OBJECT_ARRAY : Array.box(a));
    }

    public static ObjectList<Double> from(double[] a, int fromIndex, int toIndex) {
        return of(a == null && (fromIndex == 0 && toIndex == 0) ? N.EMPTY_DOUBLE_OBJECT_ARRAY : Array.box(a, fromIndex, toIndex));
    }

    public static <T> ObjectList<T> repeat(T element, int len) {
        return new ObjectList<T>(Array.repeat(element, len));
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
    public Object[] array() {
        return elementData;
    }

    public T get(int index) {
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
    public T set(int index, T e) {
        rangeCheck(index);

        T oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    /**
     * 
     * @param e
     */
    public void add(T e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    /**
     * 
     * @param index
     * @param e
     */
    public void add(int index, T e) {
        rangeCheckForAdd(index);

        ensureCapacityInternal(size + 1);

        int numMoved = size - index;

        if (numMoved > 0) {
            N.copy(elementData, index, elementData, index + 1, numMoved);
        }

        elementData[index] = e;

        size++;
    }

    public void addAll(ObjectList<? extends T> c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    public void addAll(int index, ObjectList<? extends T> c) {
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

    @Override
    public void addAll(T[] a) {
        addAll(size(), a);
    }

    @Override
    public void addAll(int index, T[] a) {
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

    /**
     * 
     * @param e
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean remove(Object e) {
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
     * @return <tt>true</tt> if this list contained the specified element
     */
    public boolean removeAllOccurrences(Object e) {
        int w = 0;

        for (int i = 0; i < size; i++) {
            if (!N.equals(elementData[i], e)) {
                elementData[w++] = elementData[i];
            }
        }

        int numRemoved = size - w;

        if (numRemoved > 0) {
            N.fill(elementData, w, size, null);

            size = w;
        }

        return numRemoved > 0;
    }

    private void fastRemove(int index) {
        int numMoved = size - index - 1;

        if (numMoved > 0) {
            N.copy(elementData, index + 1, elementData, index, numMoved);
        }

        elementData[--size] = null; // clear to let GC do its work
    }

    /**
     * 
     * @param c
     * @return
     * @see Collection#removeAll(Collection)
     */
    public boolean removeAll(ObjectList<?> c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    /**
     * 
     * @param c
     * @return
     * @see Collection#retainAll(Collection)
     */
    public boolean retainAll(ObjectList<?> c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(ObjectList<?> c, boolean complement) {
        final T[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<?> set = c.toSet();

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
            N.fill(elementData, w, size, null);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element
     */
    public T delete(int index) {
        rangeCheck(index);

        T oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    @Override
    public void deleteAll(int... indices) {
        N.deleteAll(elementData, indices);
    }

    public int replaceAll(Object oldVal, T newVal) {
        if (size() == 0) {
            return 0;
        }

        int result = 0;

        if (oldVal == null) {
            for (int i = 0, len = size(); i < len; i++) {
                if (elementData[i] == null) {
                    elementData[i] = newVal;

                    result++;
                }
            }
        } else {
            for (int i = 0, len = size(); i < len; i++) {
                if (N.equals(elementData[i], oldVal)) {
                    elementData[i] = newVal;

                    result++;
                }
            }
        }

        return result;
    }

    public void fill(final T val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final T val) {
        checkIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(Object e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(ObjectList<?> c) {
        final Object[] srcElementData = c.array();

        if (c.size() > 3 && size() > 9) {
            final Set<?> set = c.toSet();

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
    public boolean containsAll(Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return containsAll(of(a));
    }

    public boolean disjoint(final ObjectList<?> c) {
        final ObjectList<?> container = size() >= c.size() ? this : c;
        final Object[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (iterElements.length > 3 && container.size() > 9) {
            final Set<?> set = container.toSet();

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
    public boolean disjoint(final Object[] b) {
        if (N.isNullOrEmpty(b)) {
            return true;
        }

        return disjoint(of(b));
    }

    public int occurrencesOf(final Object objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#except(IntList)
     */
    public ObjectList<T> except(ObjectList<?> b) {
        final Multiset<?> bOccurrences = b.toMultiset();

        final ObjectList<T> c = new ObjectList<T>((T[]) N.newArray(getComponentType(), N.min(size(), N.max(9, size() - b.size()))));

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
    public ObjectList<T> intersect(ObjectList<?> b) {
        final Multiset<?> bOccurrences = b.toMultiset();

        final ObjectList<T> c = new ObjectList<T>((T[]) N.newArray(getComponentType(), N.min(9, size(), b.size())));

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
    public ObjectList<T> xor(ObjectList<T> b) {
        final Multiset<T> bOccurrences = b.toMultiset();

        final ObjectList<T> c = new ObjectList<T>((T[]) N.newArray(getComponentType(), N.max(9, Math.abs(size() - b.size()))));

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

    public int indexOf(Object e) {
        return indexOf(0, e);
    }

    public int indexOf(final int fromIndex, Object e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(Object e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, Object e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (N.equals(elementData[i], e)) {
                return i;
            }
        }

        return -1;
    }

    public OptionalNullable<T> min() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of((T) N.min((Comparable[]) elementData, 0, size));
    }

    public OptionalNullable<T> min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? (OptionalNullable<T>) OptionalNullable.empty()
                : OptionalNullable.of((T) N.min((Comparable[]) elementData, fromIndex, toIndex));
    }

    public OptionalNullable<T> min(Comparator<? super T> cmp) {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.min(elementData, 0, size, cmp));
    }

    public OptionalNullable<T> min(final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.min(elementData, fromIndex, toIndex, cmp));
    }

    public OptionalNullable<T> median() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of((T) N.median((Comparable[]) elementData, 0, size));
    }

    public OptionalNullable<T> median(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? (OptionalNullable<T>) OptionalNullable.empty()
                : OptionalNullable.of((T) N.median((Comparable[]) elementData, fromIndex, toIndex));
    }

    public OptionalNullable<T> median(Comparator<? super T> cmp) {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.median(elementData, 0, size, cmp));
    }

    public OptionalNullable<T> median(final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.median(elementData, fromIndex, toIndex, cmp));
    }

    public OptionalNullable<T> max() {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of((T) N.max((Comparable[]) elementData, 0, size));
    }

    public OptionalNullable<T> max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? (OptionalNullable<T>) OptionalNullable.empty()
                : OptionalNullable.of((T) N.max((Comparable[]) elementData, fromIndex, toIndex));
    }

    public OptionalNullable<T> max(Comparator<? super T> cmp) {
        return size() == 0 ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.max(elementData, 0, size, cmp));
    }

    public OptionalNullable<T> max(final int fromIndex, final int toIndex, final Comparator<? super T> cmp) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? (OptionalNullable<T>) OptionalNullable.empty() : OptionalNullable.of(N.max(elementData, fromIndex, toIndex, cmp));
    }

    public OptionalNullable<T> kthLargest(final int k) {
        return kthLargest(0, size(), k);
    }

    public OptionalNullable<T> kthLargest(final int k, Comparator<? super T> cmp) {
        return kthLargest(0, size(), k, cmp);
    }

    public OptionalNullable<T> kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? (OptionalNullable<T>) OptionalNullable.empty()
                : OptionalNullable.of((T) N.kthLargest((Comparable[]) elementData, fromIndex, toIndex, k));
    }

    public OptionalNullable<T> kthLargest(final int fromIndex, final int toIndex, final int k, final Comparator<? super T> cmp) {
        checkIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? (OptionalNullable<T>) OptionalNullable.empty()
                : OptionalNullable.of(N.kthLargest(elementData, fromIndex, toIndex, k, cmp));
    }

    public Long sumInt() {
        return sumInt(0, size());
    }

    public Long sumInt(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (fromIndex == toIndex) {
            return 0L;
        }

        long result = 0L;

        for (int i = fromIndex; i < toIndex; i++) {
            if (elementData[i] != null) {
                result += ((Number) elementData[i]).intValue();
            }
        }

        return result;
    }

    public Long sumInt(ToIntFunction<? super T> mapper) {
        return sumInt(0, size(), mapper);
    }

    public Long sumInt(int fromIndex, int toIndex, ToIntFunction<? super T> mapper) {
        checkIndex(fromIndex, toIndex);

        if (fromIndex == toIndex) {
            return 0L;
        }

        final ToIntFunction<Object> tmp = (ToIntFunction<Object>) mapper;
        long result = 0L;

        for (int i = fromIndex; i < toIndex; i++) {
            result += tmp.applyAsInt(elementData[i]);
        }

        return result;
    }

    public Long sumLong() {
        return sumLong(0, size());
    }

    public Long sumLong(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (fromIndex == toIndex) {
            return 0L;
        }

        long result = 0L;

        for (int i = fromIndex; i < toIndex; i++) {
            if (elementData[i] != null) {
                result += ((Number) elementData[i]).longValue();
            }
        }

        return result;
    }

    public Long sumLong(ToLongFunction<? super T> mapper) {
        return sumLong(0, size(), mapper);
    }

    public Long sumLong(int fromIndex, int toIndex, ToLongFunction<? super T> mapper) {
        checkIndex(fromIndex, toIndex);

        if (fromIndex == toIndex) {
            return 0L;
        }

        final ToLongFunction<Object> tmp = (ToLongFunction<Object>) mapper;
        long result = 0L;

        for (int i = fromIndex; i < toIndex; i++) {
            result += tmp.applyAsLong(elementData[i]);
        }

        return result;
    }

    public Double sumDouble() {
        return sumDouble(0, size());
    }

    public Double sumDouble(int fromIndex, int toIndex) {
        return sumDouble(fromIndex, toIndex, (ToDoubleFunction<? super T>) new ToDoubleFunction<Number>() {
            @Override
            public double applyAsDouble(Number value) {
                return value == null ? 0d : value.doubleValue();
            }
        });
    }

    public Double sumDouble(ToDoubleFunction<? super T> mapper) {
        return sumDouble(0, size(), mapper);
    }

    public Double sumDouble(int fromIndex, int toIndex, ToDoubleFunction<? super T> mapper) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? 0d : Stream.of(elementData, fromIndex, toIndex).sumDouble(mapper);
    }

    public OptionalDouble averageInt() {
        return averageInt(0, size());
    }

    public OptionalDouble averageInt(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(sumInt(fromIndex, toIndex).doubleValue() / (toIndex - fromIndex));
    }

    public OptionalDouble averageInt(ToIntFunction<? super T> mapper) {
        return averageInt(0, size(), mapper);
    }

    public OptionalDouble averageInt(int fromIndex, int toIndex, ToIntFunction<? super T> mapper) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(sumInt(fromIndex, toIndex, mapper).doubleValue() / (toIndex - fromIndex));
    }

    public OptionalDouble averageLong() {
        return averageLong(0, size());
    }

    public OptionalDouble averageLong(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(sumLong(fromIndex, toIndex).doubleValue() / (toIndex - fromIndex));
    }

    public OptionalDouble averageLong(ToLongFunction<? super T> mapper) {
        return averageLong(0, size(), mapper);
    }

    public OptionalDouble averageLong(int fromIndex, int toIndex, ToLongFunction<? super T> mapper) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : OptionalDouble.of(sumLong(fromIndex, toIndex, mapper).doubleValue() / (toIndex - fromIndex));
    }

    public OptionalDouble averageDouble() {
        return averageDouble(0, size());
    }

    public OptionalDouble averageDouble(int fromIndex, int toIndex) {
        return averageDouble(fromIndex, toIndex, (ToDoubleFunction<? super T>) new ToDoubleFunction<Number>() {
            @Override
            public double applyAsDouble(Number value) {
                return value == null ? 0d : value.doubleValue();
            }
        });
    }

    public OptionalDouble averageDouble(ToDoubleFunction<? super T> mapper) {
        return averageDouble(0, size(), mapper);
    }

    public OptionalDouble averageDouble(int fromIndex, int toIndex, ToDoubleFunction<? super T> mapper) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalDouble.empty() : Stream.of(elementData, fromIndex, toIndex).averageDouble(mapper);
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, Consumer<? super T> action) {
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

    public void forEach(IndexedConsumer<T, ObjectList<T>> action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, final IndexedConsumer<? super T, ObjectList<T>> action) {
        if (fromIndex <= toIndex) {
            checkIndex(fromIndex, toIndex);
        } else {
            checkIndex(toIndex, fromIndex);
        }

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(i, elementData[i], this);
                }
            } else {
                for (int i = fromIndex - 1; i >= toIndex; i--) {
                    action.accept(i, elementData[i], this);
                }
            }
        }
    }

    public <R> R forEach(final R identity, BiFunction<R, ? super T, R> accumulator, final Predicate<? super R> predicate) {
        return forEach(0, size(), identity, accumulator, predicate);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>predicate</code> returns false.
     * 
     * @param fromIndex
     * @param toIndex
     * @param identity
     * @param accumulator
     * @param predicate break if the <code>predicate</code> returns false.
     * @return
     */
    public <R> R forEach(final int fromIndex, final int toIndex, final R identity, BiFunction<R, ? super T, R> accumulator,
            final Predicate<? super R> predicate) {
        if (fromIndex <= toIndex) {
            checkIndex(fromIndex, toIndex);
        } else {
            checkIndex(toIndex, fromIndex);
        }

        R result = identity;

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    result = accumulator.apply(result, elementData[i]);

                    if (predicate.test(result) == false) {
                        break;
                    }
                }
            } else {
                for (int i = fromIndex - 1; i >= toIndex; i--) {
                    result = accumulator.apply(result, elementData[i]);

                    if (predicate.test(result) == false) {
                        break;
                    }
                }
            }
        }

        return result;
    }

    public <R> R forEach(final R identity, IndexedBiFunction<? super T, ObjectList<T>, R, R> accumulator, final Predicate<? super R> predicate) {
        return forEach(0, size(), identity, accumulator, predicate);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>predicate</code> returns false.
     * 
     * @param fromIndex
     * @param toIndex
     * @param identity
     * @param accumulator
     * @param predicate break if the <code>predicate</code> returns false.
     * @return
     */
    public <R> R forEach(final int fromIndex, final int toIndex, final R identity, IndexedBiFunction<? super T, ObjectList<T>, R, R> accumulator,
            final Predicate<? super R> predicate) {
        if (fromIndex <= toIndex) {
            checkIndex(fromIndex, toIndex);
        } else {
            checkIndex(toIndex, fromIndex);
        }

        R result = identity;

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    result = accumulator.apply(i, elementData[i], this, result);

                    if (predicate.test(result) == false) {
                        break;
                    }
                }
            } else {
                for (int i = fromIndex - 1; i >= toIndex; i--) {
                    result = accumulator.apply(i, elementData[i], this, result);

                    if (predicate.test(result) == false) {
                        break;
                    }
                }
            }
        }

        return result;
    }

    public OptionalNullable<T> first() {
        if (size() == 0) {
            return OptionalNullable.empty();
        }

        return OptionalNullable.of(elementData[0]);
    }

    public OptionalNullable<T> last() {
        if (size() == 0) {
            return OptionalNullable.empty();
        }

        return OptionalNullable.of(elementData[size() - 1]);
    }

    public OptionalNullable<T> findFirst(Predicate<? super T> predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalNullable.of(elementData[i]);
            }
        }

        return OptionalNullable.empty();
    }

    public OptionalNullable<T> findLast(Predicate<? super T> predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalNullable.of(elementData[i]);
            }
        }

        return OptionalNullable.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, Predicate<? super T> filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, Predicate<? super T> filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, Predicate<? super T> filter) {
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
    public int count(final int fromIndex, final int toIndex, Predicate<? super T> filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public ObjectList<T> filter(final int fromIndex, final int toIndex, Predicate<? super T> filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    @Override
    public ObjectList<T> filter(final int fromIndex, final int toIndex, Predicate<? super T> filter, final int max) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter, max));
    }

    public <U> ObjectList<T> filter(final U check, final BiPredicate<? super T, ? super U> predicate) {
        return filter(0, size, check, predicate);
    }

    public <U> ObjectList<T> filter(final int fromIndex, final int toIndex, final U check, final BiPredicate<? super T, ? super U> predicate) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, check, predicate));
    }

    public <R> ObjectList<R> map(final Function<? super T, ? extends R> func) {
        return map(0, size(), func);
    }

    public <R> ObjectList<R> map(final int fromIndex, final int toIndex, final Function<? super T, ? extends R> func) {
        checkIndex(fromIndex, toIndex);

        final R[] res = (R[]) new Object[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.apply(elementData[i]);
        }

        return ObjectList.of(res);
    }

    public BooleanList mapToBoolean(final ToBooleanFunction<? super T> func) {
        return mapToBoolean(0, size(), func);
    }

    public BooleanList mapToBoolean(final int fromIndex, final int toIndex, final ToBooleanFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final boolean[] res = new boolean[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsBoolean(elementData[i]);
        }

        return BooleanList.of(res);
    }

    public CharList mapToChar(final ToCharFunction<? super T> func) {
        return mapToChar(0, size(), func);
    }

    public CharList mapToChar(final int fromIndex, final int toIndex, final ToCharFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final char[] res = new char[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsChar(elementData[i]);
        }

        return CharList.of(res);
    }

    public ByteList mapToByte(final ToByteFunction<? super T> func) {
        return mapToByte(0, size(), func);
    }

    public ByteList mapToByte(final int fromIndex, final int toIndex, final ToByteFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final byte[] res = new byte[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsByte(elementData[i]);
        }

        return ByteList.of(res);
    }

    public ShortList mapToShort(final ToShortFunction<? super T> func) {
        return mapToShort(0, size(), func);
    }

    public ShortList mapToShort(final int fromIndex, final int toIndex, final ToShortFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final short[] res = new short[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsShort(elementData[i]);
        }

        return ShortList.of(res);
    }

    public IntList mapToInt(final ToIntFunction<? super T> func) {
        return mapToInt(0, size(), func);
    }

    public IntList mapToInt(final int fromIndex, final int toIndex, final ToIntFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final int[] res = new int[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsInt(elementData[i]);
        }

        return IntList.of(res);
    }

    public LongList mapToLong(final ToLongFunction<? super T> func) {
        return mapToLong(0, size(), func);
    }

    public LongList mapToLong(final int fromIndex, final int toIndex, final ToLongFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final long[] res = new long[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsLong(elementData[i]);
        }

        return LongList.of(res);
    }

    public FloatList mapToFloat(final ToFloatFunction<? super T> func) {
        return mapToFloat(0, size(), func);
    }

    public FloatList mapToFloat(final int fromIndex, final int toIndex, final ToFloatFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final float[] res = new float[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsFloat(elementData[i]);
        }

        return FloatList.of(res);
    }

    public DoubleList mapToDouble(final ToDoubleFunction<? super T> func) {
        return mapToDouble(0, size(), func);
    }

    public DoubleList mapToDouble(final int fromIndex, final int toIndex, final ToDoubleFunction<? super T> func) {
        checkIndex(fromIndex, toIndex);

        final double[] res = new double[size()];

        for (int i = fromIndex; i < toIndex; i++) {
            res[i - fromIndex] = func.applyAsDouble(elementData[i]);
        }

        return DoubleList.of(res);
    }

    @Override
    public ObjectList<T> distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return of(N.distinct(elementData, fromIndex, toIndex));
    }

    /**
     * 
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    public ObjectList<T> distinct(final Function<? super T, ?> keyMapper) {
        return distinct(0, size(), keyMapper);
    }

    /**
     * Distinct by the value mapped from <code>keyMapper</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param keyMapper don't change value of the input parameter.
     * @return
     */
    public ObjectList<T> distinct(final int fromIndex, final int toIndex, final Function<? super T, ?> keyMapper) {
        checkIndex(fromIndex, toIndex);

        return of(N.distinct(elementData, fromIndex, toIndex, keyMapper));
    }

    public ObjectList<T> top(final int n) {
        return top(0, size(), n);
    }

    /**
     * The element array must be extended from Comparable[].
     * 
     * @param fromIndex
     * @param toIndex
     * @param n
     * @return
     */
    public ObjectList<T> top(final int fromIndex, final int toIndex, final int n) {
        checkIndex(fromIndex, toIndex);

        return of((T[]) N.top((Comparable[]) elementData, fromIndex, toIndex, n));
    }

    public ObjectList<T> top(final int n, final Comparator<? super T> cmp) {
        return top(0, size(), n, cmp);
    }

    public ObjectList<T> top(final int fromIndex, final int toIndex, final int n, final Comparator<? super T> cmp) {
        checkIndex(fromIndex, toIndex);

        return of(N.top(elementData, fromIndex, toIndex, n, cmp));
    }

    @Override
    public void sort() {
        if (size > 1) {
            N.sort(elementData, 0, size);
        }
    }

    public void sort(final Comparator<? super T> cmp) {
        if (size > 1) {
            N.sort(elementData, 0, size, cmp);
        }
    }

    public void parallelSort() {
        if (size > 1) {
            N.parallelSort(elementData, 0, size);
        }
    }

    public void parallelSort(final Comparator<? super T> cmp) {
        if (size > 1) {
            N.parallelSort(elementData, 0, size, cmp);
        }
    }

    /**
     * This List should be sorted first.
     * 
     * @param key
     * @return
     */
    public int binarySearch(final Object key) {
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
    public int binarySearch(final int fromIndex, final int toIndex, final Object key) {
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
    public ObjectList<T> copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new ObjectList<T>(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    @Override
    public ObjectList<T> copy() {
        return new ObjectList<>(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public List<ObjectList<T>> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<T[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<ObjectList<T>> result = new ArrayList<>(list.size());

        for (T[] a : list) {
            result.add(of(a));
        }

        return result;
    }

    //    @Override
    //    public List<ObjectList<T>> split(int fromIndex, int toIndex, Predicate<? super T> predicate) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final List<ObjectList<T>> result = new ArrayList<>();
    //        ObjectList<T> piece = null;
    //
    //        for (int i = fromIndex; i < toIndex;) {
    //            if (piece == null) {
    //                piece = ObjectList.of((T[]) N.newArray(getComponentType(), 0));
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
    public ObjectList<T> trimToSize() {
        if (elementData.length > size) {
            elementData = N.copyOfRange(elementData, 0, size);
        }

        return this;
    }

    @Override
    public void clear() {
        if (size > 0) {
            N.fill(elementData, 0, size, null);
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

    //    @SuppressWarnings("rawtypes")
    //    public <R extends com.landawn.abacus.util.AbstractList> R unboxed() {
    //        return unboxed(0, size);
    //    }
    //
    //    @SuppressWarnings("rawtypes")
    //    public <R extends com.landawn.abacus.util.AbstractList> R unboxed(int fromIndex, int toIndex) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final Class<?> componentType = getComponentType();
    //
    //        if (componentType.equals(Integer.class)) {
    //            return (R) IntList.of(Array.unbox((Integer[]) elementData, fromIndex, toIndex, 0));
    //        } else if (componentType.equals(Long.class)) {
    //            return (R) LongList.of(Array.unbox((Long[]) elementData, fromIndex, toIndex, 0));
    //        } else if (componentType.equals(Float.class)) {
    //            return (R) FloatList.of(Array.unbox((Float[]) elementData, fromIndex, toIndex, 0));
    //        } else if (componentType.equals(Double.class)) {
    //            return (R) DoubleList.of(Array.unbox((Double[]) elementData, fromIndex, toIndex, 0));
    //        } else if (componentType.equals(Boolean.class)) {
    //            return (R) BooleanList.of(Array.unbox((Boolean[]) elementData, fromIndex, toIndex, false));
    //        } else if (componentType.equals(Character.class)) {
    //            return (R) CharList.of(Array.unbox((Character[]) elementData, fromIndex, toIndex, (char) 0));
    //        } else if (componentType.equals(Byte.class)) {
    //            return (R) ByteList.of(Array.unbox((Byte[]) elementData, fromIndex, toIndex, (byte) 0));
    //        } else if (componentType.equals(Short.class)) {
    //            return (R) ShortList.of(Array.unbox((Short[]) elementData, fromIndex, toIndex, (short) 0));
    //        } else {
    //            throw new IllegalArgumentException(N.getClassName(componentType) + " is not a wrapper of primitive type");
    //        }
    //    }

    @Override
    public List<T> toList(final int fromIndex, final int toIndex, final IntFunction<List<T>> supplier) {
        checkIndex(fromIndex, toIndex);

        final List<T> list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public Set<T> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<T>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Set<T> set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<T> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<T>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Multiset<T> multiset = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    public Stream<T> stream() {
        return stream(0, size());
    }

    public Stream<T> stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return Stream.of(elementData, fromIndex, toIndex);
    }

    //    public ObjectListBuilder<T> __() {
    //        return Builder.of(this);
    //    }
    //
    //    public ObjectListBuilder<T> __(Consumer<? super ObjectList<T>> func) {
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

        if (obj instanceof ObjectList) {
            final ObjectList<T> other = (ObjectList<T>) obj;

            return this.size == other.size && N.equals(this.elementData, 0, other.elementData, 0, this.size);
        }

        return false;
    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private Class<?> getComponentType() {
        return elementData.getClass().getComponentType();
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (N.isNullOrEmpty(elementData)) {
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
