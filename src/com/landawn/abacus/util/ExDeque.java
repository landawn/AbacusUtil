/*
 * Copyright (c) 2017, Haiyang Li.
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

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * 
 * @since 0.9.55
 * 
 * @author Haiyang Li
 */
public interface ExDeque<T> extends Deque<T>, ExQueue<T> {

    static <T> ExDeque<T> newDeque() {
        return of(new LinkedList<T>());
    }

    static <T> ExDeque<T> newArrayDeque() {
        return of(new ArrayDeque<T>());
    }

    static <T> ExDeque<T> newArrayDeque(int capacity) {
        return of(new ArrayDeque<T>(capacity));
    }

    static <T> ExDeque<T> of(final T... a) {
        final Deque<T> Deque = N.asDeque(a);
        return ExDeque.of(Deque);
    }

    static <T> ExDeque<T> of(final Deque<? extends T> c) {
        return new ExDeque<T>() {
            private final Deque<T> deque = (Deque<T>) c;

            @Override
            public boolean offer(T e) {
                return deque.offer(e);
            }

            @Override
            public T remove() {
                return deque.remove();
            }

            @Override
            public T poll() {
                return deque.poll();
            }

            @Override
            public T element() {
                return deque.element();
            }

            @Override
            public T peek() {
                return deque.peek();
            }

            @Override
            public void addFirst(T e) {
                deque.addFirst(e);
            }

            @Override
            public void addLast(T e) {
                deque.addLast(e);
            }

            @Override
            public boolean offerFirst(T e) {
                return deque.offerFirst(e);
            }

            @Override
            public boolean offerLast(T e) {
                return deque.offerLast(e);
            }

            @Override
            public T removeFirst() {
                return deque.removeFirst();
            }

            @Override
            public T removeLast() {
                return deque.removeLast();
            }

            @Override
            public T pollFirst() {
                return deque.pollFirst();
            }

            @Override
            public T pollLast() {
                return deque.pollLast();
            }

            @Override
            public T getFirst() {
                return deque.getFirst();
            }

            @Override
            public T getLast() {
                return deque.getLast();
            }

            @Override
            public T peekFirst() {
                return deque.peekFirst();
            }

            @Override
            public T peekLast() {
                return deque.peekLast();
            }

            @Override
            public boolean removeFirstOccurrence(Object o) {
                return deque.removeFirstOccurrence(o);
            }

            @Override
            public boolean removeLastOccurrence(Object o) {
                return deque.removeLastOccurrence(o);
            }

            @Override
            public void push(T e) {
                deque.push(e);
            }

            @Override
            public T pop() {
                return deque.pop();
            }

            @Override
            public Iterator<T> descendingIterator() {
                return deque.descendingIterator();
            }

            @Override
            public int size() {
                return deque.size();
            }

            @Override
            public boolean isEmpty() {
                return deque.isEmpty();
            }

            @Override
            public boolean contains(Object o) {
                return deque.contains(o);
            }

            @Override
            public Iterator<T> iterator() {
                return deque.iterator();
            }

            @Override
            public Object[] toArray() {
                return deque.toArray();
            }

            @Override
            public <A> A[] toArray(A[] a) {
                return deque.toArray(a);
            }

            @Override
            public boolean add(T e) {
                return deque.add(e);
            }

            @Override
            public boolean remove(Object o) {
                return deque.remove(o);
            }

            @Override
            public boolean containsAll(Collection<?> c) {
                return deque.containsAll(c);
            }

            @Override
            public boolean addAll(Collection<? extends T> c) {
                return deque.addAll(c);
            }

            @Override
            public boolean retainAll(Collection<?> c) {
                return deque.retainAll(c);
            }

            @Override
            public boolean removeAll(Collection<?> c) {
                return deque.removeAll(c);
            }

            @Override
            public void clear() {
                deque.clear();
            }

            @Override
            public int hashCode() {
                return deque.hashCode();
            }

            @Override
            public boolean equals(Object obj) {
                return deque.equals(obj);
            }

            @Override
            public String toString() {
                return deque.toString();
            }
        };
    }

    static <T> ExDeque<T> from(final Collection<? extends T> c) {
        return of(new LinkedList<T>(c));
    }
}
