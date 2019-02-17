/*
 * Copyright (C) 2016, 2017, 2018, 2019 HaiYang Li
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package com.landawn.abacus.util.stream;

import java.util.Collection;
import java.util.Deque;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.Executor;

import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.Fn;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.Optional;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.Try;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.CharBinaryOperator;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharToIntFunction;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * 
 */
class IteratorCharStream extends AbstractCharStream {
    CharIteratorEx elements;

    //    OptionalChar head;
    //    CharStream tail;

    //    CharStream head2;
    //    OptionalChar tail2;

    IteratorCharStream(final CharIterator values) {
        this(values, null);
    }

    IteratorCharStream(final CharIterator values, final Collection<Runnable> closeHandlers) {
        this(values, false, closeHandlers);
    }

    IteratorCharStream(final CharIterator values, final boolean sorted, final Collection<Runnable> closeHandlers) {
        super(sorted, closeHandlers);

        CharIteratorEx tmp = null;

        if (values instanceof CharIteratorEx) {
            tmp = (CharIteratorEx) values;
        } else {
            tmp = new CharIteratorEx() {
                @Override
                public boolean hasNext() {
                    return values.hasNext();
                }

                @Override
                public char nextChar() {
                    return values.nextChar();
                }
            };
        }

        this.elements = tmp;
    }

    @Override
    public CharStream filter(final CharPredicate predicate) {
        return newStream(new CharIteratorEx() {
            private boolean hasNext = false;
            private char next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    while (elements.hasNext()) {
                        next = elements.nextChar();

                        if (predicate.test(next)) {
                            hasNext = true;
                            break;
                        }
                    }
                }

                return hasNext;
            }

            @Override
            public char nextChar() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }
        }, sorted);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate) {
        return newStream(new CharIteratorEx() {
            private boolean hasMore = true;
            private boolean hasNext = false;
            private char next = 0;

            @Override
            public boolean hasNext() {
                if (hasNext == false && hasMore && elements.hasNext()) {
                    next = elements.nextChar();

                    if (predicate.test(next)) {
                        hasNext = true;
                    } else {
                        hasMore = false;
                    }
                }

                return hasNext;
            }

            @Override
            public char nextChar() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate) {
        return newStream(new CharIteratorEx() {
            private boolean hasNext = false;
            private char next = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        dropped = true;

                        while (elements.hasNext()) {
                            next = elements.nextChar();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }
                    } else if (elements.hasNext()) {
                        next = elements.nextChar();
                        hasNext = true;
                    }
                }

                return hasNext;
            }

            @Override
            public char nextChar() {
                if (hasNext == false && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                hasNext = false;

                return next;
            }

        }, sorted);
    }

    @Override
    public CharStream map(final CharUnaryOperator mapper) {
        return newStream(new CharIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char nextChar() {
                return mapper.applyAsChar(elements.nextChar());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false);
    }

    @Override
    public IntStream mapToInt(final CharToIntFunction mapper) {
        return newStream(new IntIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return mapper.applyAsInt(elements.nextChar());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false);
    }

    @Override
    public <U> Stream<U> mapToObj(final CharFunction<? extends U> mapper) {
        return newStream(new ObjIteratorEx<U>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public U next() {
                return mapper.apply(elements.nextChar());
            }

            //            @Override
            //            public long count() {
            //                return elements.count();
            //            }
            //
            //            @Override
            //            public void skip(long n) {
            //                elements.skip(n);
            //            }
        }, false, null);
    }

    @Override
    public CharStream flatMap(final CharFunction<? extends CharStream> mapper) {
        final CharIteratorEx iter = new CharIteratorEx() {
            private CharIterator cur = null;
            private CharStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextChar());

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Deque<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public char nextChar() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextChar();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Deque<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalArrayDeque<Runnable>(1)
                : new LocalArrayDeque<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorCharStream(iter, newCloseHandlers);
    }

    @Override
    public IntStream flatMapToInt(final CharFunction<? extends IntStream> mapper) {
        final IntIteratorEx iter = new IntIteratorEx() {
            private IntIterator cur = null;
            private IntStream s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextChar());

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Deque<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public int nextInt() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.nextInt();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Deque<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalArrayDeque<Runnable>(1)
                : new LocalArrayDeque<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorIntStream(iter, newCloseHandlers);
    }

    @Override
    public <T> Stream<T> flatMapToObj(final CharFunction<? extends Stream<T>> mapper) {
        final ObjIteratorEx<T> iter = new ObjIteratorEx<T>() {
            private Iterator<? extends T> cur = null;
            private Stream<? extends T> s = null;
            private Runnable closeHandle = null;

            @Override
            public boolean hasNext() {
                while ((cur == null || cur.hasNext() == false) && elements.hasNext()) {
                    if (closeHandle != null) {
                        final Runnable tmp = closeHandle;
                        closeHandle = null;
                        tmp.run();
                    }

                    s = mapper.apply(elements.nextChar());

                    if (N.notNullOrEmpty(s.closeHandlers)) {
                        final Deque<Runnable> tmp = s.closeHandlers;

                        closeHandle = new Runnable() {
                            @Override
                            public void run() {
                                Stream.close(tmp);
                            }
                        };
                    }

                    cur = s.iterator();
                }

                return cur != null && cur.hasNext();
            }

            @Override
            public T next() {
                if ((cur == null || cur.hasNext() == false) && hasNext() == false) {
                    throw new NoSuchElementException();
                }

                return cur.next();
            }

            @Override
            public void close() {
                if (closeHandle != null) {
                    final Runnable tmp = closeHandle;
                    closeHandle = null;
                    tmp.run();
                }
            }
        };

        final Deque<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalArrayDeque<Runnable>(1)
                : new LocalArrayDeque<Runnable>(closeHandlers);

        newCloseHandlers.add(new Runnable() {
            @Override
            public void run() {
                iter.close();
            }
        });

        return new IteratorStream<>(iter, newCloseHandlers);
    }

    @Override
    public Stream<CharList> splitToList(final int size) {
        N.checkArgPositive(size, "size");

        return newStream(new ObjIteratorEx<CharList>() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public CharList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final CharList result = new CharList(size);

                while (result.size() < size && elements.hasNext()) {
                    result.add(elements.nextChar());
                }

                return result;
            }

            @Override
            public long count() {
                final long len = elements.count();
                return len % size == 0 ? len / size : len / size + 1;
            }

            @Override
            public void skip(long n) {
                elements.skip(n >= Long.MAX_VALUE / size ? Long.MAX_VALUE : n * size);
            }
        }, false, null);
    }

    @Override
    public Stream<CharList> splitToList(final CharPredicate predicate) {
        return newStream(new ObjIteratorEx<CharList>() {
            private char next;
            private boolean hasNext = false;
            private boolean preCondition = false;

            @Override
            public boolean hasNext() {
                return hasNext == true || elements.hasNext();
            }

            @Override
            public CharList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                final CharList result = new CharList();

                if (hasNext == false) {
                    next = elements.nextChar();
                    hasNext = true;
                }

                while (hasNext) {
                    if (result.size() == 0) {
                        result.add(next);
                        preCondition = predicate.test(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextChar() : 0;
                    } else if (predicate.test(next) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextChar() : 0;
                    } else {
                        break;
                    }
                }

                return result;
            }

        }, false, null);
    }

    @Override
    public Stream<CharList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return newStream(new ObjIteratorEx<CharList>() {
            private CharList prev = null;

            @Override
            public boolean hasNext() {
                if (prev != null && increment > windowSize) {
                    int skipNum = increment - windowSize;

                    while (skipNum-- > 0 && elements.hasNext()) {
                        elements.nextChar();
                    }

                    prev = null;
                }

                return elements.hasNext();
            }

            @Override
            public CharList next() {
                if (hasNext() == false) {
                    throw new NoSuchElementException();
                }

                CharList result = null;
                int cnt = 0;

                if (prev != null && increment < windowSize) {
                    cnt = windowSize - increment;

                    if (cnt <= 8) {
                        result = new CharList(windowSize);

                        for (int i = windowSize - cnt; i < windowSize; i++) {
                            result.add(prev.get(i));
                        }
                    } else {
                        final char[] dest = new char[windowSize];
                        N.copy(prev.trimToSize().array(), windowSize - cnt, dest, 0, cnt);
                        result = CharList.of(dest, cnt);
                    }
                } else {
                    result = new CharList(windowSize);
                }

                while (cnt++ < windowSize && elements.hasNext()) {
                    result.add(elements.nextChar());
                }

                return prev = result;
            }

        }, false, null);
    }

    @Override
    public CharStream peek(final CharConsumer action) {
        return newStream(new CharIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public char nextChar() {
                final char next = elements.nextChar();

                action.accept(next);
                return next;
            }
        }, sorted);
    }

    @Override
    public CharStream limit(final long maxSize) {
        N.checkArgNotNegative(maxSize, "maxSize");

        return newStream(new CharIteratorEx() {
            private long cnt = 0;

            @Override
            public boolean hasNext() {
                return cnt < maxSize && elements.hasNext();
            }

            @Override
            public char nextChar() {
                if (cnt >= maxSize) {
                    throw new NoSuchElementException();
                }

                cnt++;
                return elements.nextChar();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted);
    }

    @Override
    public CharStream skip(final long n) {
        N.checkArgNotNegative(n, "n");

        if (n == 0) {
            return this;
        }

        return newStream(new CharIteratorEx() {
            private boolean skipped = false;

            @Override
            public boolean hasNext() {
                if (skipped == false) {
                    skipped = true;
                    elements.skip(n);
                }

                return elements.hasNext();
            }

            @Override
            public char nextChar() {
                if (skipped == false) {
                    skipped = true;
                    elements.skip(n);
                }

                return elements.nextChar();
            }

            @Override
            public long count() {
                if (skipped == false) {
                    skipped = true;
                    elements.skip(n);
                }

                return elements.count();
            }

            @Override
            public void skip(long n2) {
                if (skipped == false) {
                    skipped = true;
                    elements.skip(n);
                }

                elements.skip(n2);
            }

            @Override
            public char[] toArray() {
                if (skipped == false) {
                    skipped = true;
                    elements.skip(n);
                }

                return elements.toArray();
            }
        }, sorted);
    }

    @Override
    public <E extends Exception> void forEach(final Try.CharConsumer<E> action) throws E {
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                action.accept(elements.nextChar());
            }
        } finally {
            close();
        }
    }

    @Override
    public char[] toArray() {
        assertNotClosed();

        try {
            return elements.toArray();
        } finally {
            close();
        }
    }

    @Override
    public CharList toCharList() {
        assertNotClosed();

        try {
            return elements.toList();
        } finally {
            close();
        }
    }

    @Override
    public List<Character> toList() {
        return toCollection(Fn.Suppliers.<Character> ofList());
    }

    @Override
    public Set<Character> toSet() {
        return toCollection(Fn.Suppliers.<Character> ofSet());
    }

    @Override
    public <C extends Collection<Character>> C toCollection(Supplier<? extends C> supplier) {
        assertNotClosed();

        try {
            final C result = supplier.get();

            while (elements.hasNext()) {
                result.add(elements.nextChar());
            }

            return result;
        } finally {
            close();
        }
    }

    @Override
    public Multiset<Character> toMultiset() {
        return toMultiset(Fn.Suppliers.<Character> ofMultiset());
    }

    @Override
    public Multiset<Character> toMultiset(Supplier<? extends Multiset<Character>> supplier) {
        assertNotClosed();

        try {
            final Multiset<Character> result = supplier.get();

            while (elements.hasNext()) {
                result.add(elements.nextChar());
            }

            return result;
        } finally {
            close();
        }
    }

    @Override
    public LongMultiset<Character> toLongMultiset() {
        return toLongMultiset(Fn.Suppliers.<Character> ofLongMultiset());
    }

    @Override
    public LongMultiset<Character> toLongMultiset(Supplier<? extends LongMultiset<Character>> supplier) {
        assertNotClosed();

        try {
            final LongMultiset<Character> result = supplier.get();

            while (elements.hasNext()) {
                result.add(elements.nextChar());
            }

            return result;
        } finally {
            close();
        }
    }

    @Override
    public <K, V, M extends Map<K, V>> M toMap(CharFunction<? extends K> keyExtractor, CharFunction<? extends V> valueMapper, BinaryOperator<V> mergeFunction,
            Supplier<M> mapFactory) {
        assertNotClosed();

        try {
            final M result = mapFactory.get();
            char element = 0;

            while (elements.hasNext()) {
                element = elements.nextChar();
                Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
            }

            return result;
        } finally {
            close();
        }
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final CharFunction<? extends K> classifier, final Collector<Character, A, D> downstream,
            final Supplier<M> mapFactory) {
        assertNotClosed();

        try {
            final M result = mapFactory.get();
            final Supplier<A> downstreamSupplier = downstream.supplier();
            final BiConsumer<A, Character> downstreamAccumulator = downstream.accumulator();
            final Map<K, A> intermediate = (Map<K, A>) result;
            K key = null;
            A v = null;
            char element = 0;

            while (elements.hasNext()) {
                element = elements.nextChar();
                key = N.checkArgNotNull(classifier.apply(element), "element cannot be mapped to a null key");

                if ((v = intermediate.get(key)) == null) {
                    if ((v = downstreamSupplier.get()) != null) {
                        intermediate.put(key, v);
                    }
                }

                downstreamAccumulator.accept(v, element);
            }

            final BiFunction<? super K, ? super A, ? extends A> function = new BiFunction<K, A, A>() {
                @Override
                public A apply(K k, A v) {
                    return (A) downstream.finisher().apply(v);
                }
            };

            Collectors.replaceAll(intermediate, function);

            return result;
        } finally {
            close();
        }
    }

    @Override
    public char reduce(char identity, CharBinaryOperator op) {
        assertNotClosed();

        try {
            char result = identity;

            while (elements.hasNext()) {
                result = op.applyAsChar(result, elements.nextChar());
            }

            return result;
        } finally {
            close();
        }
    }

    @Override
    public OptionalChar reduce(CharBinaryOperator op) {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalChar.empty();
            }

            char result = elements.nextChar();

            while (elements.hasNext()) {
                result = op.applyAsChar(result, elements.nextChar());
            }

            return OptionalChar.of(result);
        } finally {
            close();
        }
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        assertNotClosed();

        try {
            final R result = supplier.get();

            while (elements.hasNext()) {
                accumulator.accept(result, elements.nextChar());
            }

            return result;
        } finally {
            close();
        }
    }

    //    @Override
    //    public OptionalChar head() {
    //        if (head == null) {
    //            head = elements.hasNext() ? OptionalChar.of(elements.nextChar()) : OptionalChar.empty();
    //            tail = newStream(elements, sorted);
    //        }
    //
    //        return head;
    //    }
    //
    //    @Override
    //    public CharStream tail() {
    //        if (tail == null) {
    //            head = elements.hasNext() ? OptionalChar.of(elements.nextChar()) : OptionalChar.empty();
    //            tail = newStream(elements, sorted);
    //        }
    //
    //        return tail;
    //    }

    //    @Override
    //    public CharStream headd() {
    //        if (head2 == null) {
    //            final char[] a = elements.toArray();
    //            head2 = newStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted);
    //            tail2 = a.length == 0 ? OptionalChar.empty() : OptionalChar.of(a[a.length - 1]);
    //        }
    //
    //        return head2;
    //    }
    //
    //    @Override
    //    public OptionalChar taill() {
    //        if (tail2 == null) {
    //            final char[] a = elements.toArray();
    //            head2 = newStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted);
    //            tail2 = a.length == 0 ? OptionalChar.empty() : OptionalChar.of(a[a.length - 1]);
    //        }
    //
    //        return tail2;
    //    }

    @Override
    public OptionalChar min() {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalChar.empty();
            } else if (sorted) {
                return OptionalChar.of(elements.nextChar());
            }

            char candidate = elements.nextChar();
            char next = 0;

            while (elements.hasNext()) {
                next = elements.nextChar();

                if (next < candidate) {
                    candidate = next;
                }
            }

            return OptionalChar.of(candidate);
        } finally {
            close();
        }
    }

    @Override
    public OptionalChar max() {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalChar.empty();
            } else if (sorted) {
                char next = 0;

                while (elements.hasNext()) {
                    next = elements.nextChar();
                }

                return OptionalChar.of(next);
            }

            char candidate = elements.nextChar();
            char next = 0;

            while (elements.hasNext()) {
                next = elements.nextChar();

                if (next > candidate) {
                    candidate = next;
                }
            }

            return OptionalChar.of(candidate);
        } finally {
            close();
        }
    }

    @Override
    public OptionalChar kthLargest(int k) {
        N.checkArgPositive(k, "k");
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalChar.empty();
            }

            final Optional<Character> optional = boxed().kthLargest(k, CHAR_COMPARATOR);

            return optional.isPresent() ? OptionalChar.of(optional.get()) : OptionalChar.empty();
        } finally {
            close();
        }
    }

    @Override
    public int sum() {
        assertNotClosed();

        try {
            long result = 0;

            while (elements.hasNext()) {
                result += elements.nextChar();
            }

            return N.toIntExact(result);
        } finally {
            close();
        }
    }

    @Override
    public OptionalDouble average() {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalDouble.empty();
            }

            long sum = 0;
            long count = 0;

            while (elements.hasNext()) {
                sum += elements.nextChar();
                count++;
            }

            return OptionalDouble.of(((double) sum) / count);
        } finally {
            close();
        }
    }

    @Override
    public long count() {
        assertNotClosed();

        try {
            return elements.count();
        } finally {
            close();
        }
    }

    @Override
    public CharSummaryStatistics summarize() {
        assertNotClosed();

        try {
            final CharSummaryStatistics result = new CharSummaryStatistics();

            while (elements.hasNext()) {
                result.accept(elements.nextChar());
            }

            return result;
        } finally {
            close();
        }
    }

    @Override
    public <E extends Exception> boolean anyMatch(final Try.CharPredicate<E> predicate) throws E {
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                if (predicate.test(elements.nextChar())) {
                    return true;
                }
            }
        } finally {
            close();
        }

        return false;
    }

    @Override
    public <E extends Exception> boolean allMatch(final Try.CharPredicate<E> predicate) throws E {
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                if (predicate.test(elements.nextChar()) == false) {
                    return false;
                }
            }
        } finally {
            close();
        }

        return true;
    }

    @Override
    public <E extends Exception> boolean noneMatch(final Try.CharPredicate<E> predicate) throws E {
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                if (predicate.test(elements.nextChar())) {
                    return false;
                }
            }
        } finally {
            close();
        }

        return true;
    }

    @Override
    public <E extends Exception> OptionalChar findFirst(final Try.CharPredicate<E> predicate) throws E {
        assertNotClosed();

        try {
            while (elements.hasNext()) {
                char e = elements.nextChar();

                if (predicate.test(e)) {
                    return OptionalChar.of(e);
                }
            }
        } finally {
            close();
        }

        return OptionalChar.empty();
    }

    @Override
    public <E extends Exception> OptionalChar findLast(final Try.CharPredicate<E> predicate) throws E {
        assertNotClosed();

        try {
            if (elements.hasNext() == false) {
                return OptionalChar.empty();
            }

            boolean hasResult = false;
            char e = 0;
            char result = 0;

            while (elements.hasNext()) {
                e = elements.nextChar();

                if (predicate.test(e)) {
                    result = e;
                    hasResult = true;
                }
            }

            return hasResult ? OptionalChar.of(result) : OptionalChar.empty();
        } finally {
            close();
        }
    }

    @Override
    public IntStream asIntStream() {
        return newStream(new IntIteratorEx() {
            @Override
            public boolean hasNext() {
                return elements.hasNext();
            }

            @Override
            public int nextInt() {
                return elements.nextChar();
            }

            @Override
            public long count() {
                return elements.count();
            }

            @Override
            public void skip(long n) {
                elements.skip(n);
            }
        }, sorted);
    }

    @Override
    public Stream<Character> boxed() {
        return new IteratorStream<>(iterator(), sorted, sorted ? CHAR_COMPARATOR : null, closeHandlers);
    }

    @Override
    CharIteratorEx iteratorEx() {
        return elements;
    }

    @Override
    public CharStream parallel(int maxThreadNum, Splitor splitor) {
        return new ParallelIteratorCharStream(elements, sorted, maxThreadNum, checkSplitor(splitor), asyncExecutor(), closeHandlers);
    }

    @Override
    public CharStream parallel(final int maxThreadNum, final Executor executor) {
        return new ParallelIteratorCharStream(elements, sorted, maxThreadNum, splitor(), createAsyncExecutor(executor), closeHandlers);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final Deque<Runnable> newCloseHandlers = new LocalArrayDeque<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        newCloseHandlers.add(wrapCloseHandlers(closeHandler));

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        return new IteratorCharStream(elements, sorted, newCloseHandlers);
    }
}
