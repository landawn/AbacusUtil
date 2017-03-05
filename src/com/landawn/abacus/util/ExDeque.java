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

    static <T> ExDeque<T> of(final T... a) {
        final Deque<T> Deque = N.asDeque(a);
        return ExDeque.of(Deque);
    }

    static <T> ExDeque<T> of(final Deque<? extends T> c) {
        return new ExDeque<T>() {
            private final Deque<T> deque = (Deque<T>) c;

            @Override
            public boolean offer(T e) {
                return this.deque.offer(e);
            }

            @Override
            public T remove() {
                return this.deque.remove();
            }

            @Override
            public T poll() {
                return this.deque.poll();
            }

            @Override
            public T element() {
                return this.deque.element();
            }

            @Override
            public T peek() {
                return this.deque.peek();
            }

            @Override
            public void addFirst(T e) {
                this.deque.addFirst(e);
            }

            @Override
            public void addLast(T e) {
                this.deque.addLast(e);
            }

            @Override
            public boolean offerFirst(T e) {
                return this.deque.offerFirst(e);
            }

            @Override
            public boolean offerLast(T e) {
                return this.deque.offerLast(e);
            }

            @Override
            public T removeFirst() {
                return this.deque.removeFirst();
            }

            @Override
            public T removeLast() {
                return this.deque.removeLast();
            }

            @Override
            public T pollFirst() {
                return this.deque.pollFirst();
            }

            @Override
            public T pollLast() {
                return this.deque.pollLast();
            }

            @Override
            public T getFirst() {
                return this.deque.getFirst();
            }

            @Override
            public T getLast() {
                return this.deque.getLast();
            }

            @Override
            public T peekFirst() {
                return this.deque.peekFirst();
            }

            @Override
            public T peekLast() {
                return this.deque.peekLast();
            }

            @Override
            public boolean removeFirstOccurrence(Object o) {
                return this.deque.removeFirstOccurrence(o);
            }

            @Override
            public boolean removeLastOccurrence(Object o) {
                return this.deque.removeLastOccurrence(o);
            }

            @Override
            public void push(T e) {
                this.deque.push(e);
            }

            @Override
            public T pop() {
                return this.deque.pop();
            }

            @Override
            public Iterator<T> descendingIterator() {
                return this.deque.descendingIterator();
            }

            @Override
            public int size() {
                return this.deque.size();
            }

            @Override
            public boolean isEmpty() {
                return this.deque.isEmpty();
            }

            @Override
            public boolean contains(Object o) {
                return this.deque.contains(o);
            }

            @Override
            public Iterator<T> iterator() {
                return this.deque.iterator();
            }

            @Override
            public Object[] toArray() {
                return this.deque.toArray();
            }

            @Override
            public <A> A[] toArray(A[] a) {
                return this.deque.toArray(a);
            }

            @Override
            public boolean add(T e) {
                return this.deque.add(e);
            }

            @Override
            public boolean remove(Object o) {
                return this.deque.remove(o);
            }

            @Override
            public boolean containsAll(Collection<?> c) {
                return this.deque.containsAll(c);
            }

            @Override
            public boolean addAll(Collection<? extends T> c) {
                return this.deque.addAll(c);
            }

            @Override
            public boolean retainAll(Collection<?> c) {
                return this.deque.retainAll(c);
            }

            @Override
            public boolean removeAll(Collection<?> c) {
                return this.deque.removeAll(c);
            }

            @Override
            public void clear() {
                this.deque.clear();
            }

            @Override
            public int hashCode() {
                return this.deque.hashCode();
            }

            @Override
            public boolean equals(Object obj) {
                return this.deque.equals(obj);
            }

            @Override
            public String toString() {
                return this.deque.toString();
            }
        };
    }

    static <T> ExDeque<T> from(final Collection<? extends T> c) {
        return of(new LinkedList<T>(c));
    }
}
