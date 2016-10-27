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
import java.util.Set;

import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.IndexedCharConsumer;
import com.landawn.abacus.util.function.IntFunction;
import com.landawn.abacus.util.stream.CharStream;

/**
 * 
 * @since 0.8
 * 
 * @author Haiyang Li
 */
public final class CharList extends AbstractList<CharConsumer, CharPredicate, Character, char[], CharList> {
    private char[] elementData = N.EMPTY_CHAR_ARRAY;
    private int size = 0;

    public CharList() {
        super();
    }

    public CharList(int initialCapacity) {
        this();

        elementData = new char[initialCapacity];
    }

    /**
     * The specified array is used as the element array for this list without copying action.
     * 
     * @param a
     */
    public CharList(char[] a) {
        this();

        elementData = a;
        size = a.length;
    }

    public CharList(char[] a, int size) {
        this();

        if (a.length < size) {
            throw new IllegalArgumentException("The specified size is bigger than the length of the specified array");
        }

        this.elementData = a;
        this.size = size;
    }

    public static CharList empty() {
        return new CharList(N.EMPTY_CHAR_ARRAY);
    }

    public static CharList of(char... a) {
        return a == null ? empty() : new CharList(a);
    }

    public static CharList of(char[] a, int size) {
        return a == null && size == 0 ? empty() : new CharList(a, size);
    }

    public static CharList from(int... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static CharList from(int[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final char[] elementData = new char[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (a[i] < Character.MIN_VALUE || a[i] > Character.MAX_VALUE) {
                throw new ArithmeticException("overflow");
            }

            elementData[i - startIndex] = (char) a[i];
        }

        return of(elementData);
    }

    public static CharList from(String... a) {
        return a == null ? empty() : from(a, 0, a.length);
    }

    public static CharList from(String[] a, int startIndex, int endIndex) {
        if (a == null && (startIndex == 0 && endIndex == 0)) {
            return empty();
        }

        N.checkIndex(startIndex, endIndex, a == null ? 0 : a.length);

        final char[] elementData = new char[endIndex - startIndex];

        for (int i = startIndex; i < endIndex; i++) {
            if (a[i] == null) {
                elementData[i - startIndex] = 0;
            } else if (a[i].length() == 1) {
                elementData[i - startIndex] = a[i].charAt(0);
            } else {
                throw new IllegalArgumentException("Invalid char: " + a[i]);
            }
        }

        return of(elementData);
    }

    static CharList from(List<String> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, (char) 0);
    }

    static CharList from(List<String> c, char defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final char[] a = new char[c.size()];
        int idx = 0;

        for (String e : c) {
            if (e == null) {
                a[idx++] = defaultValueForNull;
            } else if (e.length() == 1) {
                a[idx++] = e.charAt(0);
            } else {
                throw new IllegalArgumentException("Invalid char: " + e);
            }
        }

        return of(a);
    }

    public static CharList from(Collection<Character> c) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        return from(c, (char) 0);
    }

    public static CharList from(Collection<Character> c, char defaultValueForNull) {
        if (N.isNullOrEmpty(c)) {
            return empty();
        }

        final char[] a = new char[c.size()];
        int idx = 0;

        for (Character e : c) {
            a[idx++] = e == null ? defaultValueForNull : e;
        }

        return of(a);
    }

    public static CharList range(char startInclusive, final char endExclusive) {
        return of(Array.range(startInclusive, endExclusive));
    }

    public static CharList rangeClosed(char startInclusive, final char endInclusive) {
        return of(Array.rangeClosed(startInclusive, endInclusive));
    }

    public static CharList repeat(char element, final int len) {
        return of(Array.repeat(element, len));
    }

    public static CharList random(final char startInclusive, final char endInclusive, final int len) {
        if (startInclusive == endInclusive) {
            return repeat(startInclusive, len);
        }

        final char[] a = new char[len];
        final int mod = endInclusive - startInclusive + 1;

        for (int i = 0; i < len; i++) {
            a[i] = (char) (Math.abs(RAND.nextInt() % mod) + startInclusive);
        }

        return of(a);
    }

    public static CharList random(final char startInclusive, final char endInclusive, CharPredicate predicate, final int len) {
        if (startInclusive == endInclusive) {
            return repeat(startInclusive, len);
        }

        final char[] a = new char[len];
        final int mod = endInclusive - startInclusive + 1;
        char ch = 0;

        for (int i = 0; i < len;) {
            ch = (char) (Math.abs(RAND.nextInt() % mod) + startInclusive);

            if (predicate.test(ch)) {
                a[i++] = ch;
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
    public char[] array() {
        return elementData;
    }

    //    /**
    //     * Return the first element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalChar findFirst() {
    //        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(elementData[0]);
    //    }

    //    /**
    //     * Return the last element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalChar findLast() {
    //        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(elementData[size - 1]);
    //    }

    public char get(int index) {
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
    public char set(int index, char e) {
        rangeCheck(index);

        char oldValue = elementData[index];

        elementData[index] = e;

        return oldValue;
    }

    public void add(char e) {
        ensureCapacityInternal(size + 1);

        elementData[size++] = e;
    }

    public void add(int index, char e) {
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
    public void addAll(CharList c) {
        int numNew = c.size();

        ensureCapacityInternal(size + numNew);

        N.copy(c.array(), 0, elementData, size, numNew);

        size += numNew;
    }

    @Override
    public void addAll(int index, CharList c) {
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
    public void addAll(char[] a) {
        addAll(size(), a);
    }

    @Override
    public void addAll(int index, char[] a) {
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
    public boolean remove(char e) {
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
    public boolean removeAllOccurrences(char e) {
        int w = 0;

        for (int i = 0; i < size; i++) {
            if (elementData[i] != e) {
                elementData[w++] = elementData[i];
            }
        }

        int numRemoved = size - w;

        if (numRemoved > 0) {
            N.fill(elementData, w, size, (char) 0);

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

    public boolean removeAll(CharList c) {
        return batchRemove(c, false) > 0;
    }

    @Override
    public boolean removeAll(char[] a) {
        if (N.isNullOrEmpty(a)) {
            return false;
        }

        return removeAll(of(a));
    }

    public boolean retainAll(CharList c) {
        return batchRemove(c, true) > 0;
    }

    private int batchRemove(CharList c, boolean complement) {
        final char[] elementData = this.elementData;

        int w = 0;

        if (c.size() > 3 && size() > 9) {
            final Set<Character> set = c.toSet();

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
            N.fill(elementData, w, size, (char) 0);

            size = w;
        }

        return numRemoved;
    }

    /**
     * 
     * @param index
     * @return the deleted element
     */
    public char delete(int index) {
        rangeCheck(index);

        char oldValue = elementData[index];

        fastRemove(index);

        return oldValue;
    }

    @Override
    public void deleteAll(int... indices) {
        N.deleteAll(elementData, indices);
    }

    public void fill(final char val) {
        fill(0, size(), val);
    }

    public void fill(final int fromIndex, final int toIndex, final char val) {
        checkIndex(fromIndex, toIndex);

        N.fill(elementData, fromIndex, toIndex, val);
    }

    public boolean contains(char e) {
        return indexOf(e) >= 0;
    }

    public boolean containsAll(CharList c) {
        final char[] srcElementData = c.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Character> set = c.toSet();

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
    public boolean containsAll(char[] a) {
        if (N.isNullOrEmpty(a)) {
            return true;
        }

        return containsAll(of(a));
    }

    public boolean joint(final CharList c) {
        final CharList container = size() >= c.size() ? this : c;
        final char[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Character> set = container.toSet();

            for (int i = 0, srcSize = size() >= c.size() ? c.size() : this.size(); i < srcSize; i++) {
                if (set.contains(iterElements[i])) {
                    return true;
                }
            }
        } else {
            for (int i = 0, srcSize = size() >= c.size() ? c.size() : this.size(); i < srcSize; i++) {
                if (container.contains(iterElements[i])) {
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    public boolean joint(final char[] b) {
        if (N.isNullOrEmpty(b)) {
            return false;
        }

        return joint(of(b));
    }

    public boolean disjoint(final CharList c) {
        final CharList container = size() >= c.size() ? this : c;
        final char[] iterElements = size() >= c.size() ? c.array() : this.array();

        if (c.size() > 3 && size() > 9) {
            final Set<Character> set = container.toSet();

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
    public boolean disjoint(final char[] b) {
        if (N.isNullOrEmpty(b)) {
            return true;
        }

        return disjoint(of(b));
    }

    public int occurrencesOf(final char objectToFind) {
        return N.occurrencesOf(elementData, objectToFind);
    }

    @Override
    public CharList subList(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new CharList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    public int indexOf(char e) {
        return indexOf(0, e);
    }

    /**
     * 
     * @param b
     * @return
     * @see IntList#except(IntList)
     */
    public CharList except(CharList b) {
        final Multiset<Character> bOccurrences = b.toMultiset();

        final CharList c = new CharList(N.min(size(), N.max(9, size() - b.size())));

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
    public CharList intersect(CharList b) {
        final Multiset<Character> bOccurrences = b.toMultiset();

        final CharList c = new CharList(N.min(9, size(), b.size()));

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
    public CharList xor(CharList b) {
        final CharList result = this.except(b);

        result.addAll(b.except(this));

        return result;
    }

    public int indexOf(final int fromIndex, char e) {
        checkIndex(fromIndex, size);

        for (int i = fromIndex; i < size; i++) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public int lastIndexOf(char e) {
        return lastIndexOf(size, e);
    }

    /**
     * 
     * @param fromIndex the start index to traverse backwards from. Inclusive.
     * @param e
     * @return
     */
    public int lastIndexOf(final int fromIndex, char e) {
        checkIndex(0, fromIndex);

        for (int i = fromIndex == size ? size - 1 : fromIndex; i >= 0; i--) {
            if (elementData[i] == e) {
                return i;
            }
        }

        return -1;
    }

    public OptionalChar min() {
        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(N.min(elementData, 0, size));
    }

    public OptionalChar min(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalChar.empty() : OptionalChar.of(N.min(elementData, fromIndex, toIndex));
    }

    public OptionalChar median() {
        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(N.median(elementData, 0, size));
    }

    public OptionalChar median(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalChar.empty() : OptionalChar.of(N.median(elementData, fromIndex, toIndex));
    }

    public OptionalChar max() {
        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(N.max(elementData, 0, size));
    }

    public OptionalChar max(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return fromIndex == toIndex ? OptionalChar.empty() : OptionalChar.of(N.max(elementData, fromIndex, toIndex));
    }

    public OptionalChar kthLargest(final int k) {
        return kthLargest(0, size(), k);
    }

    public OptionalChar kthLargest(final int fromIndex, final int toIndex, final int k) {
        checkIndex(fromIndex, toIndex);

        return toIndex - fromIndex < k ? OptionalChar.empty() : OptionalChar.of(N.kthLargest(elementData, fromIndex, toIndex, k));
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
    public void forEach(final int fromIndex, final int toIndex, CharConsumer action) {
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

    public void forEach(IndexedCharConsumer action) {
        forEach(0, size(), action);
    }

    public void forEach(final int fromIndex, final int toIndex, IndexedCharConsumer action) {
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

    //    /**
    //     * Return the first element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalChar findFirst() {
    //        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(elementData[0]);
    //    }

    public OptionalChar findFirst(CharPredicate predicate) {
        for (int i = 0; i < size; i++) {
            if (predicate.test(elementData[i])) {
                return OptionalChar.of(elementData[i]);
            }
        }

        return OptionalChar.empty();
    }

    //    /**
    //     * Return the last element of the array list.
    //     * @return
    //     */
    //    @Beta
    //    public OptionalChar findLast() {
    //        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(elementData[size - 1]);
    //    }

    public OptionalChar findLast(CharPredicate predicate) {
        for (int i = size - 1; i >= 0; i--) {
            if (predicate.test(elementData[i])) {
                return OptionalChar.of(elementData[i]);
            }
        }

        return OptionalChar.empty();
    }

    @Override
    public boolean allMatch(final int fromIndex, final int toIndex, CharPredicate filter) {
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
    public boolean anyMatch(final int fromIndex, final int toIndex, CharPredicate filter) {
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
    public boolean noneMatch(final int fromIndex, final int toIndex, CharPredicate filter) {
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
    public int count(final int fromIndex, final int toIndex, CharPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return N.count(elementData, fromIndex, toIndex, filter);
    }

    @Override
    public CharList filter(final int fromIndex, final int toIndex, CharPredicate filter) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter));
    }

    @Override
    public CharList filter(final int fromIndex, final int toIndex, CharPredicate filter, final int max) {
        checkIndex(fromIndex, toIndex);

        return of(N.filter(elementData, fromIndex, toIndex, filter, max));
    }

    // TODO 1, replace with Stream APIs. 2, "final Class<? extends V> collClass" should be replaced with IntFunction<List<R>> supplier

    //    public <R> List<R> map(final CharFunction<? extends R> func) {
    //        return map(0, size(), func);
    //    }
    //
    //    public <R> List<R> map(final int fromIndex, final int toIndex, final CharFunction<? extends R> func) {
    //        return map(List.class, fromIndex, toIndex, func);
    //    }
    //
    //    public <R, V extends Collection<R>> V map(final Class<? extends V> collClass, final CharFunction<? extends R> func) {
    //        return map(collClass, 0, size(), func);
    //    }
    //
    //    public <R, V extends Collection<R>> V map(final Class<? extends V> collClass, final int fromIndex, final int toIndex,
    //            final CharFunction<? extends R> func) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final V res = N.newInstance(collClass);
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            res.add(func.apply(elementData[i]));
    //        }
    //
    //        return res;
    //    }
    //
    //    public <R> List<R> flatMap(final CharFunction<? extends Collection<? extends R>> func) {
    //        return flatMap(0, size(), func);
    //    }
    //
    //    public <R> List<R> flatMap(final int fromIndex, final int toIndex, final CharFunction<? extends Collection<? extends R>> func) {
    //        return flatMap(List.class, fromIndex, toIndex, func);
    //    }
    //
    //    public <R, V extends Collection<R>> V flatMap(final Class<? extends V> collClass, final CharFunction<? extends Collection<? extends R>> func) {
    //        return flatMap(collClass, 0, size(), func);
    //    }
    //
    //    public <R, V extends Collection<R>> V flatMap(final Class<? extends V> collClass, final int fromIndex, final int toIndex,
    //            final CharFunction<? extends Collection<? extends R>> func) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final V res = N.newInstance(collClass);
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            res.addAll(func.apply(elementData[i]));
    //        }
    //
    //        return res;
    //    }
    //
    //    public <R> List<R> flatMap2(final CharFunction<R[]> func) {
    //        return flatMap2(0, size(), func);
    //    }
    //
    //    public <R> List<R> flatMap2(final int fromIndex, final int toIndex, final CharFunction<R[]> func) {
    //        return flatMap2(List.class, fromIndex, toIndex, func);
    //    }
    //
    //    public <R, V extends Collection<R>> V flatMap2(final Class<? extends V> collClass, final CharFunction<R[]> func) {
    //        return flatMap2(collClass, 0, size(), func);
    //    }
    //
    //    public <R, V extends Collection<R>> V flatMap2(final Class<? extends V> collClass, final int fromIndex, final int toIndex, final CharFunction<R[]> func) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final V res = N.newInstance(collClass);
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            res.addAll(Arrays.asList(func.apply(elementData[i])));
    //        }
    //
    //        return res;
    //    }
    //
    //    public <K> Map<K, List<Character>> groupBy(final CharFunction<? extends K> func) {
    //        return groupBy(0, size(), func);
    //    }
    //
    //    public <K> Map<K, List<Character>> groupBy(final int fromIndex, final int toIndex, final CharFunction<? extends K> func) {
    //        return groupBy(List.class, fromIndex, toIndex, func);
    //    }
    //
    //    @SuppressWarnings("rawtypes")
    //    public <K, V extends Collection<Character>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final CharFunction<? extends K> func) {
    //        return groupBy(HashMap.class, collClass, 0, size(), func);
    //    }
    //
    //    @SuppressWarnings("rawtypes")
    //    public <K, V extends Collection<Character>> Map<K, V> groupBy(final Class<? extends Collection> collClass, final int fromIndex, final int toIndex,
    //            final CharFunction<? extends K> func) {
    //        return groupBy(HashMap.class, collClass, fromIndex, toIndex, func);
    //    }
    //
    //    public <K, V extends Collection<Character>, M extends Map<? super K, V>> M groupBy(final Class<M> outputClass, final Class<? extends V> collClass,
    //            final CharFunction<? extends K> func) {
    //
    //        return groupBy(outputClass, collClass, 0, size(), func);
    //    }
    //
    //    public <K, V extends Collection<Character>, M extends Map<? super K, V>> M groupBy(final Class<M> outputClass, final Class<? extends V> collClass,
    //            final int fromIndex, final int toIndex, final CharFunction<? extends K> func) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final M outputResult = N.newInstance(outputClass);
    //
    //        K key = null;
    //        V values = null;
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            key = func.apply(elementData[i]);
    //            values = outputResult.get(key);
    //
    //            if (values == null) {
    //                values = N.newInstance(collClass);
    //                outputResult.put(key, values);
    //            }
    //
    //            values.add(elementData[i]);
    //        }
    //
    //        return outputResult;
    //    }
    //
    //    public OptionalChar reduce(final CharBinaryOperator accumulator) {
    //        return size() == 0 ? OptionalChar.empty() : OptionalChar.of(reduce((char) 0, accumulator));
    //    }
    //
    //    public OptionalChar reduce(final int fromIndex, final int toIndex, final CharBinaryOperator accumulator) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        return fromIndex == toIndex ? OptionalChar.empty() : OptionalChar.of(reduce(fromIndex, toIndex, (char) 0, accumulator));
    //    }
    //
    //    public char reduce(final char identity, final CharBinaryOperator accumulator) {
    //        return reduce(0, size(), identity, accumulator);
    //    }
    //
    //    public char reduce(final int fromIndex, final int toIndex, final char identity, final CharBinaryOperator accumulator) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        char result = identity;
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            result = accumulator.applyAsChar(result, elementData[i]);
    //        }
    //
    //        return result;
    //    }

    @Override
    public CharList distinct(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        if (toIndex - fromIndex > 1) {
            return of(N.removeDuplicates(elementData, fromIndex, toIndex, false));
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
    public int binarySearch(final char key) {
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
    public int binarySearch(final int fromIndex, final int toIndex, final char key) {
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
    public CharList copy(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return new CharList(N.copyOfRange(elementData, fromIndex, toIndex));
    }

    @Override
    public List<CharList> split(final int fromIndex, final int toIndex, final int size) {
        checkIndex(fromIndex, toIndex);

        final List<char[]> list = N.split(elementData, fromIndex, toIndex, size);
        final List<CharList> result = new ArrayList<>(list.size());

        for (char[] a : list) {
            result.add(of(a));
        }

        return result;
    }

    @Override
    public List<CharList> split(int fromIndex, int toIndex, CharPredicate predicate) {
        checkIndex(fromIndex, toIndex);

        final List<CharList> result = new ArrayList<>();
        CharList piece = null;

        for (int i = fromIndex; i < toIndex;) {
            if (piece == null) {
                piece = CharList.of(N.EMPTY_CHAR_ARRAY);
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
    public CharList trimToSize() {
        if (elementData.length > size) {
            elementData = N.copyOfRange(elementData, 0, size);
        }

        return this;
    }

    @Override
    public void clear() {
        if (size > 0) {
            N.fill(elementData, 0, size, (char) 0);
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

    public ObjectList<Character> boxed() {
        return boxed(0, size);
    }

    public ObjectList<Character> boxed(int fromIndex, int toIndex) {
        checkIndex(fromIndex, toIndex);

        final Character[] b = new Character[toIndex - fromIndex];

        for (int i = fromIndex, j = 0; i < toIndex; i++, j++) {
            b[j] = elementData[i];
        }

        return ObjectList.of(b);
    }

    @Override
    public List<Character> toList(final int fromIndex, final int toIndex, final IntFunction<List<Character>> supplier) {
        checkIndex(fromIndex, toIndex);

        final List<Character> list = supplier.apply(toIndex - fromIndex);

        for (int i = fromIndex; i < toIndex; i++) {
            list.add(elementData[i]);
        }

        return list;
    }

    @Override
    public Set<Character> toSet(final int fromIndex, final int toIndex, final IntFunction<Set<Character>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Set<Character> set = supplier.apply(N.min(16, toIndex - fromIndex));

        for (int i = fromIndex; i < toIndex; i++) {
            set.add(elementData[i]);
        }

        return set;
    }

    @Override
    public Multiset<Character> toMultiset(final int fromIndex, final int toIndex, final IntFunction<Multiset<Character>> supplier) {
        checkIndex(fromIndex, toIndex);

        final Multiset<Character> multiset = supplier.apply(N.min(16, toIndex - fromIndex));
        ;

        for (int i = fromIndex; i < toIndex; i++) {
            multiset.add(elementData[i]);
        }

        return multiset;
    }

    // Replaced with Stream.toMap(...)/toMultimap(...).

    //    public <K, U> Map<K, U> toMap(final CharFunction<? extends K> keyMapper, final CharFunction<? extends U> valueMapper) {
    //        final IntFunction<Map<K, U>> supplier = createMapSupplier();
    //
    //        return toMap(keyMapper, valueMapper, supplier);
    //    }
    //
    //    public <K, U, M extends Map<K, U>> M toMap(final CharFunction<? extends K> keyMapper, final CharFunction<? extends U> valueMapper,
    //            final IntFunction<M> supplier) {
    //        return toMap(0, size(), keyMapper, valueMapper, supplier);
    //    }
    //
    //    public <K, U> Map<K, U> toMap(final int fromIndex, final int toIndex, final CharFunction<? extends K> keyMapper,
    //            final CharFunction<? extends U> valueMapper) {
    //        final IntFunction<Map<K, U>> supplier = createMapSupplier();
    //
    //        return toMap(fromIndex, toIndex, keyMapper, valueMapper, supplier);
    //    }
    //
    //    public <K, U, M extends Map<K, U>> M toMap(final int fromIndex, final int toIndex, final CharFunction<? extends K> keyMapper,
    //            final CharFunction<? extends U> valueMapper, final IntFunction<M> supplier) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final Map<K, U> map = supplier.apply(N.min(16, toIndex - fromIndex));
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            map.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
    //        }
    //
    //        return (M) map;
    //    }
    //
    //    public <K, U> Multimap<K, U, List<U>> toMultimap(final CharFunction<? extends K> keyMapper, final CharFunction<? extends U> valueMapper) {
    //        final IntFunction<Multimap<K, U, List<U>>> supplier = createMultimapSupplier();
    //
    //        return toMultimap(keyMapper, valueMapper, supplier);
    //    }
    //
    //    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final CharFunction<? extends K> keyMapper, final CharFunction<? extends U> valueMapper,
    //            final IntFunction<Multimap<K, U, V>> supplier) {
    //        return toMultimap(0, size(), keyMapper, valueMapper, supplier);
    //    }
    //
    //    public <K, U> Multimap<K, U, List<U>> toMultimap(final int fromIndex, final int toIndex, final CharFunction<? extends K> keyMapper,
    //            final CharFunction<? extends U> valueMapper) {
    //        final IntFunction<Multimap<K, U, List<U>>> supplier = createMultimapSupplier();
    //
    //        return toMultimap(fromIndex, toIndex, keyMapper, valueMapper, supplier);
    //    }
    //
    //    public <K, U, V extends Collection<U>> Multimap<K, U, V> toMultimap(final int fromIndex, final int toIndex, final CharFunction<? extends K> keyMapper,
    //            final CharFunction<? extends U> valueMapper, final IntFunction<Multimap<K, U, V>> supplier) {
    //        checkIndex(fromIndex, toIndex);
    //
    //        final Multimap<K, U, V> multimap = supplier.apply(N.min(16, toIndex - fromIndex));
    //
    //        for (int i = fromIndex; i < toIndex; i++) {
    //            multimap.put(keyMapper.apply(elementData[i]), valueMapper.apply(elementData[i]));
    //        }
    //
    //        return multimap;
    //    }

    public CharStream stream() {
        return stream(0, size());
    }

    public CharStream stream(final int fromIndex, final int toIndex) {
        checkIndex(fromIndex, toIndex);

        return CharStream.of(elementData, fromIndex, toIndex);
    }

    @Override
    public int hashCode() {
        return N.hashCode(elementData, 0, size());
    }

    @Override
    public boolean equals(Object obj) {
        return obj == this || (obj instanceof CharList && N.equals(elementData, 0, size(), ((CharList) obj).elementData));

    }

    @Override
    public String toString() {
        return size == 0 ? "[]" : N.toString(elementData, 0, size);
    }

    private void ensureCapacityInternal(int minCapacity) {
        if (elementData == N.EMPTY_CHAR_ARRAY) {
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
