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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.RandomAccess;
import java.util.Set;

import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BiPredicate;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.Function;
import com.landawn.abacus.util.function.IndexedBiFunction;
import com.landawn.abacus.util.function.IndexedConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.function.Predicate;
import com.landawn.abacus.util.function.Supplier;
import com.landawn.abacus.util.function.ToBooleanFunction;
import com.landawn.abacus.util.function.ToByteFunction;
import com.landawn.abacus.util.function.ToCharFunction;
import com.landawn.abacus.util.function.ToDoubleFunction;
import com.landawn.abacus.util.function.ToFloatFunction;
import com.landawn.abacus.util.function.ToIntFunction;
import com.landawn.abacus.util.function.ToLongFunction;
import com.landawn.abacus.util.function.ToShortFunction;
import com.landawn.abacus.util.function.UnaryOperator;
import com.landawn.abacus.util.stream.Collector;
import com.landawn.abacus.util.stream.Collectors;
import com.landawn.abacus.util.stream.Stream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class ObjectList<T> extends AbstractList<Consumer<? super T>, Predicate<? super T>, T, T[], ObjectList<T>> implements List<T> {
    private static final long serialVersionUID = 5075714034035989332L;

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
        return new ObjectList<>((T[]) N.EMPTY_OBJECT_ARRAY);
    }

    /**
     * 
     * @param a
     * @return
     * @throws IllegalArgumentException the specified <code>a</code> is null.
     */
    public static <T> ObjectList<T> of(T... a) {
        return new ObjectList<>(a);
    }

    /**
     * 
     * @param a
     * @param size
     * @return
     * @throws IllegalArgumentException the specified <code>a</code> is null.
     */
    public static <T> ObjectList<T> of(T[] a, int size) {
        return new ObjectList<>(a, size);
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
        if (element == null) {
            return new ObjectList<>((T[]) new Object[len]);
        }

        return new ObjectList<>(Array.repeat(element, len));
    }

    /**
     * Returns the original element array without copying.
     * 
     * @return
     */
    public Object[] array() {
        return elementData;
    }

    @Override
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
    @Override
    public T set(int index, T e) {
        rangeCheck(index);

        T oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    /**
     * 
     * @param e
     * @return always return <code>true</code>
     */
    @Override
    public boolean add(T e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;

        return true;
    }

    /**
     * 
     * @param index
     * @param e
     */
    @Override
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

    @Override
    public boolean addAll(Collection<? extends T> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        if (c instanceof ObjectList) {
            N.copy(((ObjectList<T>) c).array(), 0, elementData, size, numNew);
        } else {
            int idx = size();

            for (T e : c) {
                elementData[idx++] = e;
            }
        }

        size += numNew;

        return true;
    }

    @Override
    public boolean addAll(int index, Collection<? extends T> c) {
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

        if (c instanceof ObjectList) {
            N.copy(((ObjectList<T>) c).array(), 0, elementData, index, numNew);
        } else {
            int idx = index;

            for (T e : c) {
                elementData[idx++] = e;
            }
        }

        size += numNew;

        return true;
    }

    private void rangeCheckForAdd(int index) {
        if (index > size || index < 0) {
            throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
        }
    }

    @Override
    public boolean addAll(T[] a) {
        return addAll(size(), a);
    }

    @Override
    public boolean addAll(int index, T[] a) {
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

    /**
     * 
     * @param e
     * @return <tt>true</tt> if this list contained the specified element
     */
    @Override
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
    @Override
    public boolean removeAll(Collection<?> c) {
        if (N.isNullOrEmpty(c)) {
            return false;
        }

        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    @Override
    public boolean removeIf(Predicate<? super T> p) {
        final ObjectList<T> tmp = new ObjectList<>(size());

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

    /**
     * 
     * @param c
     * @return
     * @see Collection#retainAll(Collection)
     */
    @Override
    public boolean retainAll(Collection<?> c) {
        return batchRemove(c, true) > 0;
    }

    public boolean retainAll(Object[] a) {
        return retainAll(a == null ? empty() : ObjectList.of(a));
    }

    private int batchRemove(Collection<?> c, boolean complement) {
        final T[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);

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

    @Override
    public T remove(int index) {
        return delete(index);
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

    public void replaceAll(UnaryOperator<T> operator) {
        for (int i = 0, len = size(); i < len; i++) {
            elementData[i] = operator.apply(elementData[i]);
        }
    }

    public boolean replaceIf(T newValue, Predicate<? super T> predicate) {
        boolean result = false;

        for (int i = 0, len = size(); i < len; i++) {
            if (predicate.test(elementData[i])) {
                elementData[i] = newValue;

                result = true;
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

    @Override
    public boolean contains(Object e) {
        return indexOf(e) >= 0;
    }

    @Override
    public boolean containsAll(Collection<?> c) {
        if (c.size() > 3 && size() > 9) {
            final Set<?> set = c instanceof Set ? (Set<?>) c : new HashSet<>(c);

            for (Object e : c) {
                if (set.contains(e) == false) {
                    return false;
                }
            }
        } else {
            for (Object e : c) {
                if (contains(e) == false) {
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

    public boolean disjoint(final Collection<?> c) {
        final Collection<?> container = c instanceof Set || c.size() >= size() ? c : this;
        final Collection<?> iterElements = container == c ? this : c;

        if (iterElements.size() > 3 && container.size() > 9) {
            final Set<?> set = container instanceof Set ? (Set<?>) container : new HashSet<>(container);

            for (Object e : iterElements) {
                if (set.contains(e)) {
                    return false;
                }
            }
        } else {
            for (Object e : iterElements) {
                if (container.contains(e)) {
                    return false;
                }
            }
        }

        return true;
    }

    @Override
    public boolean disjoint(final Object[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return disjoint(of(a));
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#intersection(IntList)
     */
    public ObjectList<T> intersection(final Collection<?> b) {
        final Multiset<?> bOccurrences = Multiset.from(b);

        final ObjectList<T> result = new ObjectList<>((T[]) N.newArray(getComponentType(), N.min(9, size(), b.size())));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) > 0) {
                result.add(elementData[i]);
            }
        }

        return result;
    }

    public ObjectList<T> intersection(final Object[] a) {
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
    public ObjectList<T> difference(final Collection<?> b) {
        final Multiset<?> bOccurrences = Multiset.from(b);

        final ObjectList<T> result = new ObjectList<>((T[]) N.newArray(getComponentType(), N.min(size(), N.max(9, size() - b.size()))));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) < 1) {
                result.add(elementData[i]);
            }
        }

        return result;
    }

    public ObjectList<T> difference(final Object[] a) {
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
    public ObjectList<T> symmetricDifference(final Collection<T> b) {
        final Multiset<T> bOccurrences = Multiset.from(b);

        final ObjectList<T> result = new ObjectList<>((T[]) N.newArray(getComponentType(), N.max(9, Math.abs(size() - b.size()))));

        for (int i = 0, len = size(); i < len; i++) {
            if (bOccurrences.getAndRemove(elementData[i]) < 1) {
                result.add(elementData[i]);
            }
        }

        for (T e : b) {
            if (bOccurrences.getAndRemove(e) > 0) {
                result.add(e);
            }

            if (bOccurrences.isEmpty()) {
                break;
            }
        }

        return result;
    }

    public ObjectList<T> symmetricDifference(final T[] a) {
        if (N.isNullOrEmpty(a)) {
            return of(N.copyOfRange(elementData, 0, size()));
        }

        return symmetricDifference(of(a));
    }

    public int occurrencesOf(final Object objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    @Override
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

    @Override
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

        return fromIndex == toIndex ? 0d : N.sumDouble(elementData, fromIndex, toIndex, mapper);
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

        return fromIndex == toIndex ? OptionalDouble.empty() : N.averageDouble(elementData, fromIndex, toIndex, mapper);
    }

    @Override
    public void forEach(final int fromIndex, final int toIndex, Consumer<? super T> action) {
        N.checkIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex, size);

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

    public void forEach(IndexedConsumer<T, ObjectList<T>> action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, final IndexedConsumer<? super T, ObjectList<T>> action) {
        N.checkIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex, size);

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    action.accept(i, elementData[i], this);
                }
            } else {
                for (int i = N.min(size - 1, fromIndex); i > toIndex; i--) {
                    action.accept(i, elementData[i], this);
                }
            }
        }
    }

    public <R> R forEach(final R seed, BiFunction<R, ? super T, R> accumulator, final BiPredicate<? super T, ? super R> conditionToBreak) {
        return forEach(0, size(), seed, accumulator, conditionToBreak);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param seed
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public <R> R forEach(final int fromIndex, final int toIndex, final R seed, BiFunction<R, ? super T, R> accumulator,
            final BiPredicate<? super T, ? super R> conditionToBreak) {
        N.checkIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex, size);

        R result = seed;

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    result = accumulator.apply(result, elementData[i]);

                    if (conditionToBreak.test(elementData[i], result)) {
                        break;
                    }
                }
            } else {
                for (int i = N.min(size - 1, fromIndex); i > toIndex; i--) {
                    result = accumulator.apply(result, elementData[i]);

                    if (conditionToBreak.test(elementData[i], result)) {
                        break;
                    }
                }
            }
        }

        return result;
    }

    public <R> R forEach(final R seed, IndexedBiFunction<R, ? super T, ObjectList<T>, R> accumulator, BiPredicate<? super T, ? super R> conditionToBreak) {
        return forEach(0, size(), seed, accumulator, conditionToBreak);
    }

    /**
     * Execute <code>accumulator</code> on each element till <code>true</code> is returned by <code>conditionToBreak</code>
     * 
     * @param fromIndex
     * @param toIndex
     * @param seed
     * @param accumulator
     * @param conditionToBreak break if <code>true</code> is return.
     * @return
     */
    public <R> R forEach(final int fromIndex, final int toIndex, final R seed, IndexedBiFunction<R, ? super T, ObjectList<T>, R> accumulator,
            final BiPredicate<? super T, ? super R> conditionToBreak) {
        N.checkIndex(fromIndex < toIndex ? fromIndex : (toIndex == -1 ? 0 : toIndex), fromIndex < toIndex ? toIndex : fromIndex, size);

        R result = seed;

        if (size > 0) {
            if (fromIndex <= toIndex) {
                for (int i = fromIndex; i < toIndex; i++) {
                    result = accumulator.apply(result, i, elementData[i], this);

                    if (conditionToBreak.test(elementData[i], result)) {
                        break;
                    }
                }
            } else {
                for (int i = N.min(size - 1, fromIndex); i > toIndex; i--) {
                    result = accumulator.apply(result, i, elementData[i], this);

                    if (conditionToBreak.test(elementData[i], result)) {
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

    //    public Optional<Indexed<T>> findFirst2(Predicate<? super T> predicate) {
    //        for (int i = 0; i < size; i++) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(Indexed.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalNullable<T> findLast(Predicate<? super T> predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalNullable.of(elementData[i]);
            }
        }

        return OptionalNullable.empty();
    }

    //    public Optional<Indexed<T>> findLast2(Predicate<? super T> predicate) {
    //        for (int i = size - 1; i >= 0; i--) {
    //            if (predicate.test(elementData[i])) {
    //                return Optional.of(Indexed.of(i, elementData[i]));
    //            }
    //        }
    //
    //        return Optional.empty();
    //    }

    public OptionalInt findFirstIndex(Predicate<? super T> predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
    }

    public OptionalInt findLastIndex(Predicate<? super T> predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalInt.of(i);
            }
        }

        return OptionalInt.empty();
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

        return N.filter(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public ObjectList<T> filter(final int fromIndex, final int toIndex, Predicate<? super T> filter, final int max) {
        checkIndex(fromIndex, toIndex);

        return N.filter(elementData, fromIndex, toIndex, filter, max);
    }

    public <U> ObjectList<T> filter(final U seed, final BiPredicate<? super T, ? super U> predicate) {
        return filter(0, size, seed, predicate);
    }

    public <U> ObjectList<T> filter(final int fromIndex, final int toIndex, final U seed, final BiPredicate<? super T, ? super U> predicate) {
        checkIndex(fromIndex, toIndex);

        return filter(fromIndex, toIndex, new Predicate<T>() {
            @Override
            public boolean test(T value) {
                return predicate.test(value, seed);
            }
        });
    }

    public <R> ObjectList<R> map(final Function<? super T, ? extends R> func) {
        return map(0, size(), func);
    }

    public <R> ObjectList<R> map(final int fromIndex, final int toIndex, final Function<? super T, ? extends R> func) {
        checkIndex(fromIndex, toIndex);

        final ObjectList<R> result = new ObjectList<>(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            result.add(func.apply(elementData[i]));
        }

        return result;
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

    public <R> ObjectList<R> flatMap(final Function<? super T, ? extends Collection<R>> func) {
        return flatMap(0, size(), func);
    }

    public <R> ObjectList<R> flatMap(final int fromIndex, final int toIndex, final Function<? super T, ? extends Collection<R>> func) {
        checkIndex(fromIndex, toIndex);

        final ObjectList<R> result = new ObjectList<>(size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : size() * 2);

        for (int i = fromIndex; i < toIndex; i++) {
            result.addAll(func.apply(elementData[i]));
        }

        return result;
    }

    public <R> ObjectList<R> flatMap2(final Function<? super T, ? extends R[]> func) {
        return flatMap2(0, size(), func);
    }

    public <R> ObjectList<R> flatMap2(final int fromIndex, final int toIndex, final Function<? super T, ? extends R[]> func) {
        checkIndex(fromIndex, toIndex);

        final ObjectList<R> result = new ObjectList<>(size() > Integer.MAX_VALUE / 2 ? Integer.MAX_VALUE : size() * 2);

        for (int i = fromIndex; i < toIndex; i++) {
            result.addAll(func.apply(elementData[i]));
        }

        return result;
    }

    /**
     * This is equivalent to:
     * <pre>
     * <code>
     *    if (isEmpty()) {
     *        return OptionalNullable.empty();
     *    }
     *
     *    T result = elementData[0];
     *
     *    for (int i = 1; i < size; i++) {
     *        result = accumulator.apply(result, elementData[i]);
     *    }
     *
     *    return OptionalNullable.of(result);
     * </code>
     * </pre>
     * 
     * @param accumulator
     * @return
     */
    public OptionalNullable<T> reduce(final BinaryOperator<T> accumulator) {
        if (isEmpty()) {
            return OptionalNullable.empty();
        }

        T result = elementData[0];

        for (int i = 1; i < size; i++) {
            result = accumulator.apply(result, elementData[i]);
        }

        return OptionalNullable.of(result);
    }

    /**
     * This is equivalent to:
     * <pre>
     * <code>
     *     if (isEmpty()) {
     *         return identity;
     *     }
     * 
     *     U result = identity;
     * 
     *     for (int i = 0; i < size; i++) {
     *         result = accumulator.apply(result, elementData[i]);
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
    public <U> U reduce(final U identity, final BiFunction<U, ? super T, U> accumulator) {
        if (isEmpty()) {
            return identity;
        }

        U result = identity;

        for (int i = 0; i < size; i++) {
            result = accumulator.apply(result, elementData[i]);
        }

        return result;
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
        if (size() > 1) {
            N.shuffle(elementData);
        }
    }

    @Override
    public void swap(int i, int j) {
        rangeCheck(i);
        rangeCheck(j);

        set(i, set(j, elementData[i]));
    }

    @Override
    public ObjectList<T> copy() {
        return new ObjectList<>(N.copyOfRange(elementData, 0, size));
    }

    @Override
    public ObjectList<T> copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new ObjectList<>(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    /**
     * @param from
     * @param to
     * @param step
     * 
     * @see N#copyOfRange(int[], int, int, int)
     */
    @Override
    public ObjectList<T> copy(final int from, final int to, final int step) {
        checkIndex(from < to ? from : (to == -1 ? 0 : to), from < to ? to : from);

        return new ObjectList<>(N.copyOfRange(elementData, from, to, step));
    }

    @Override
    public ObjectList<ObjectList<T>> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final ObjectList<T[]> list = N.split(elementData, fromIndex, toIndex, size);
        @SuppressWarnings("rawtypes")
        final ObjectList<ObjectList<T>> result = (ObjectList) list;

        for (int i = 0, len = list.size(); i < len; i++) {
            result.set(i, of(list.get(i)));
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

    public <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, List<T>>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, mapFactory);
    }

    public <K, M extends Map<K, List<T>>> M toMap(Function<? super T, ? extends K> classifier, Supplier<M> mapFactory) {
        final Collector<? super T, ?, List<T>> downstream = Collectors.toList();

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D> Map<K, D> toMap(Function<? super T, ? extends K> classifier, Collector<? super T, A, D> downstream) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, D>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(classifier, downstream, mapFactory);
    }

    @SuppressWarnings("hiding")
    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, ? super T> downstreamAccumulator = downstream.accumulator();
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

    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        final BinaryOperator<U> mergeFunction = BinaryOperator.THROWING_MERGER;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
        @SuppressWarnings("rawtypes")
        final Supplier<Map<K, U>> mapFactory = (Supplier) Supplier.MAP;

        return toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    }

    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
        final M result = mapFactory.get();

        for (int i = 0; i < size; i++) {
            Seq.merge(result, keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]), mergeFunction);
        }

        return result;
    }

    public <K, U> Map<K, List<U>> toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
        return toMap(keyMapper, (Collector<T, ?, List<U>>) (Collector<?, ?, ?>) Collectors.mapping(valueMapper, Collectors.toList()));
    }

    @SuppressWarnings("rawtypes")
    public <K, U, M extends Map<K, List<U>>> M toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
            Supplier<M> mapFactory) {
        return toMap(keyMapper, (Collector<T, ?, List<U>>) (Collector) Collectors.mapping(valueMapper, Collectors.toList()), mapFactory);
    }

    //    /**
    //     * 
    //     * @param classifier
    //     * @return
    //     * @see Collectors#groupingBy(Function)
    //     */
    //    public <K> Map<K, List<T>> toMap(Function<? super T, ? extends K> classifier) {
    //        return stream().toMap(classifier);
    //    }
    //
    //    /**
    //     * 
    //     * @param classifier
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#groupingBy(Function, Supplier)
    //     */
    //    public <K, M extends Map<K, List<T>>> M toMap(final Function<? super T, ? extends K> classifier, final Supplier<M> mapFactory) {
    //        return stream().toMap(classifier, mapFactory);
    //    }
    //
    //    /**
    //     * 
    //     * @param classifier
    //     * @param downstream
    //     * @return
    //     * @see Collectors#groupingBy(Function, Collector)
    //     */
    //    @SuppressWarnings("hiding")
    //    public <K, A, D> Map<K, D> toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream) {
    //        return stream().toMap(classifier, downstream);
    //    }
    //
    //    /**
    //     * 
    //     * @param classifier
    //     * @param downstream
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#groupingBy(Function, Collector, Supplier)
    //     */
    //    @SuppressWarnings("hiding")
    //    public <K, A, D, M extends Map<K, D>> M toMap(final Function<? super T, ? extends K> classifier, final Collector<? super T, A, D> downstream,
    //            final Supplier<M> mapFactory) {
    //        return stream().toMap(classifier, downstream, mapFactory);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @return
    //     * @see Collectors#toMap(Function, Function)
    //     */
    //    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
    //        return stream().toMap(keyMapper, valueMapper);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#toMap(Function, Function, Supplier)
    //     */
    //    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
    //            Supplier<M> mapFactory) {
    //        return stream().toMap(keyMapper, valueMapper, mapFactory);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mergeFunction
    //     * @return
    //     * @see Collectors#toMap(Function, Function, BinaryOperator)
    //     */
    //    public <K, U> Map<K, U> toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper, BinaryOperator<U> mergeFunction) {
    //        return stream().toMap(keyMapper, valueMapper, mergeFunction);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mergeFunction
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#toMap(Function, Function, BinaryOperator, Supplier)
    //     */
    //    public <K, U, M extends Map<K, U>> M toMap(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
    //            BinaryOperator<U> mergeFunction, Supplier<M> mapFactory) {
    //        return stream().toMap(keyMapper, valueMapper, mergeFunction, mapFactory);
    //    }
    //
    //    public <K, U> Map<K, List<U>> toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper) {
    //        return stream().toMap2(keyMapper, valueMapper);
    //    }
    //
    //    /**
    //     * 
    //     * @param keyMapper
    //     * @param valueMapper
    //     * @param mapFactory
    //     * @return
    //     * @see Collectors#toMultimap(Function, Function, Supplier)
    //     */
    //    public <K, U, M extends Map<K, List<U>>> M toMap2(Function<? super T, ? extends K> keyMapper, Function<? super T, ? extends U> valueMapper,
    //            Supplier<M> mapFactory) {
    //        return stream().toMap2(keyMapper, valueMapper, mapFactory);
    //    }

    //    public Seq<T> toSeq() {
    //        return toSeq(0, size());
    //    }
    //
    //    public Seq<T> toSeq(final int fromIndex, final int toIndex) {
    //        return Seq.of(toList(fromIndex, toIndex));
    //    }
    //
    //    public Seq<T> toSeq(final IntFunction<Collection<T>> supplier) {
    //        return toSeq(0, size(), supplier);
    //    }
    //
    //    public Seq<T> toSeq(final int fromIndex, final int toIndex, final IntFunction<Collection<T>> supplier) {
    //        final Collection<T> c = supplier.apply(toIndex - fromIndex);
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            c.add(elementData[i]);
    //        }
    //
    //        return Seq.of(c);
    //    }

    @Override
    public List<T> subList(int fromIndex, int toIndex) {
        this.checkIndex(fromIndex, toIndex);

        return asList().subList(fromIndex, toIndex);
    }

    @Override
    public Iterator<T> iterator() {
        if (isEmpty()) {
            return com.landawn.abacus.util.Iterator.EMPTY;
        }

        return new com.landawn.abacus.util.Iterator<T>() {
            private int cursor = 0;

            @Override
            public boolean hasNext() {
                return cursor < size;
            }

            @Override
            public T next() {
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

    @Override
    public ListIterator<T> listIterator() {
        return asList().listIterator();
    }

    @Override
    public ListIterator<T> listIterator(int index) {
        rangeCheck(index);

        return asList().listIterator(index);
    }

    private List<T> asList() {
        return new ArrayList2<>(elementData, size);
    }

    public Stream<T> stream0() {
        return Stream.of(elementData, 0, size());
    }

    public Stream<T> stream0(final int fromIndex, final int toIndex) {
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
    public Object[] toArray() {
        return N.copyOfRange(elementData, 0, size);
    }

    @Override
    public <A> A[] toArray(A[] a) {
        if (a.length < size) {
            return N.copyOfRange(elementData, 0, size, (Class<? extends A[]>) a.getClass());
        }

        N.copy(elementData, 0, a, 0, size());

        return a;
    }

    public T[] toArray(Class<T[]> cls) {
        return N.copyOfRange(elementData, 0, size, cls);
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

    /**
     * @serial include
     */
    static final class ArrayList2<E> extends java.util.AbstractList<E> implements RandomAccess {
        private final E[] a;
        private final int size;

        ArrayList2(final E[] array, final int size) {
            this.a = array;
            this.size = size;
        }

        @Override
        public int size() {
            return size;
        }

        @Override
        public Object[] toArray() {
            return N.copyOfRange(a, 0, size);
        }

        @Override
        @SuppressWarnings("unchecked")
        public <T> T[] toArray(final T[] a) {
            if (a.length < size) {
                return N.copyOfRange(this.a, 0, size, (Class<? extends T[]>) a.getClass());
            }

            System.arraycopy(this.a, 0, a, 0, size);

            return a;
        }

        @Override
        public E get(int index) {
            rangeCheck(index);

            return a[index];
        }

        @Override
        public E set(int index, E element) {
            rangeCheck(index);

            E oldValue = a[index];
            a[index] = element;
            return oldValue;
        }

        @Override
        public int indexOf(Object o) {
            final E[] a = this.a;

            if (o == null) {
                for (int i = 0; i < size; i++) {
                    if (a[i] == null) {
                        return i;
                    }
                }
            } else {
                for (int i = 0; i < size; i++) {
                    if (o.equals(a[i])) {
                        return i;
                    }
                }
            }

            return -1;
        }

        @Override
        public boolean contains(Object o) {
            return indexOf(o) != -1;
        }

        public void sort(Comparator<? super E> c) {
            N.sort(a, 0, size, c);
        }

        private void rangeCheck(int index) {
            if (index >= size) {
                throw new IndexOutOfBoundsException("Index: " + index + ", Size: " + size);
            }
        }
    }
}
