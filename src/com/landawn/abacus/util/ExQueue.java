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
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Queue;

/**
 * 
 * @since 0.9.55
 * 
 * @author Haiyang Li
 */
public interface ExQueue<T> extends Queue<T>, ExCollection<T> {

    static <T> ExQueue<T> newQueue() {
        return of(new LinkedList<T>());
    }

    static <T> ExQueue<T> of(final T... a) {
        final Queue<T> Queue = N.asQueue(a);
        return ExQueue.of(Queue);
    }

    static <T> ExQueue<T> of(final Queue<? extends T> c) {
        return new ExQueue<T>() {
            private final Queue<T> queue = (Queue<T>) c;

            @Override
            public boolean offer(T e) {
                return queue.offer(e);
            }

            @Override
            public T remove() {
                return queue.remove();
            }

            @Override
            public T poll() {
                return queue.poll();
            }

            @Override
            public T element() {
                return queue.element();
            }

            @Override
            public T peek() {
                return queue.peek();
            }

            @Override
            public int size() {
                return queue.size();
            }

            @Override
            public boolean isEmpty() {
                return queue.isEmpty();
            }

            @Override
            public boolean contains(Object o) {
                return queue.contains(o);
            }

            @Override
            public Iterator<T> iterator() {
                return queue.iterator();
            }

            @Override
            public Object[] toArray() {
                return queue.toArray();
            }

            @Override
            public <A> A[] toArray(A[] a) {
                return queue.toArray(a);
            }

            @Override
            public boolean add(T e) {
                return queue.add(e);
            }

            @Override
            public boolean remove(Object o) {
                return queue.remove(o);
            }

            @Override
            public boolean containsAll(Collection<?> c) {
                return queue.containsAll(c);
            }

            @Override
            public boolean addAll(Collection<? extends T> c) {
                return queue.addAll(c);
            }

            @Override
            public boolean retainAll(Collection<?> c) {
                return queue.retainAll(c);
            }

            @Override
            public boolean removeAll(Collection<?> c) {
                return queue.removeAll(c);
            }

            @Override
            public void clear() {
                queue.clear();
            }

            @Override
            public int hashCode() {
                return queue.hashCode();
            }

            @Override
            public boolean equals(Object obj) {
                return queue.equals(obj);
            }

            @Override
            public String toString() {
                return queue.toString();
            }
        };
    }

    static <T> ExQueue<T> from(final Collection<? extends T> c) {
        return of(new LinkedList<T>(c));
    }
}
