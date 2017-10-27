/*
 * Copyright (C) 2016 HaiYang Li
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import com.landawn.abacus.util.CharIterator;
import com.landawn.abacus.util.CharList;
import com.landawn.abacus.util.CharSummaryStatistics;
import com.landawn.abacus.util.IntIterator;
import com.landawn.abacus.util.LongMultiset;
import com.landawn.abacus.util.Multiset;
import com.landawn.abacus.util.N;
import com.landawn.abacus.util.NullabLe;
import com.landawn.abacus.util.OptionalChar;
import com.landawn.abacus.util.OptionalDouble;
import com.landawn.abacus.util.function.BiConsumer;
import com.landawn.abacus.util.function.BiFunction;
import com.landawn.abacus.util.function.BinaryOperator;
import com.landawn.abacus.util.function.CharBinaryOperator;
import com.landawn.abacus.util.function.CharConsumer;
import com.landawn.abacus.util.function.CharFunction;
import com.landawn.abacus.util.function.CharPredicate;
import com.landawn.abacus.util.function.CharToIntFunction;
import com.landawn.abacus.util.function.CharUnaryOperator;
import com.landawn.abacus.util.function.Consumer;
import com.landawn.abacus.util.function.ObjCharConsumer;
import com.landawn.abacus.util.function.Supplier;

/**
 * This class is a sequential, stateful and immutable stream implementation.
 *
 * @since 0.8
 * 
 * @author Haiyang Li
 */
class IteratorCharStream extends AbstractCharStream {
    final CharIteratorEx elements;

    OptionalChar head;
    CharStream tail;

    CharStream head2;
    OptionalChar tail2;

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
        return new IteratorCharStream(new CharIteratorEx() {
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
        }, sorted, closeHandlers);
    }

    @Override
    public CharStream takeWhile(final CharPredicate predicate) {
        return new IteratorCharStream(new CharIteratorEx() {
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

        }, sorted, closeHandlers);
    }

    @Override
    public CharStream dropWhile(final CharPredicate predicate) {
        return new IteratorCharStream(new CharIteratorEx() {
            private boolean hasNext = false;
            private char next = 0;
            private boolean dropped = false;

            @Override
            public boolean hasNext() {
                if (hasNext == false) {
                    if (dropped == false) {
                        while (elements.hasNext()) {
                            next = elements.nextChar();

                            if (predicate.test(next) == false) {
                                hasNext = true;
                                break;
                            }
                        }

                        dropped = true;
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

        }, sorted, closeHandlers);
    }

    @Override
    public CharStream map(final CharUnaryOperator mapper) {
        return new IteratorCharStream(new CharIteratorEx() {
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
        }, closeHandlers);
    }

    @Override
    public IntStream mapToInt(final CharToIntFunction mapper) {
        return new IteratorIntStream(new IntIteratorEx() {
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
        }, closeHandlers);
    }

    @Override
    public <U> Stream<U> mapToObj(final CharFunction<? extends U> mapper) {
        return new IteratorStream<>(new ObjIteratorEx<U>() {
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
        }, closeHandlers);
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
                        final Set<Runnable> tmp = s.closeHandlers;

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

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1) : new LocalLinkedHashSet<Runnable>(closeHandlers);

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
                        final Set<Runnable> tmp = s.closeHandlers;

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

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1) : new LocalLinkedHashSet<Runnable>(closeHandlers);

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
                        final Set<Runnable> tmp = s.closeHandlers;

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

        final Set<Runnable> newCloseHandlers = N.isNullOrEmpty(closeHandlers) ? new LocalLinkedHashSet<Runnable>(1) : new LocalLinkedHashSet<Runnable>(closeHandlers);

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
        N.checkArgument(size > 0, "'size' must be bigger than 0");

        return new IteratorStream<>(new ObjIteratorEx<CharList>() {
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
        }, closeHandlers);
    }

    @Override
    public Stream<CharList> splitToList(final CharPredicate predicate) {
        return new IteratorStream<>(new ObjIteratorEx<CharList>() {
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

        }, closeHandlers);
    }

    @Override
    public <U> Stream<CharList> splitToList(final U seed, final BiFunction<? super Character, ? super U, Boolean> predicate,
            final Consumer<? super U> seedUpdate) {
        return new IteratorStream<>(new ObjIteratorEx<CharList>() {
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
                        preCondition = predicate.apply(next, seed);
                        next = (hasNext = elements.hasNext()) ? elements.nextChar() : 0;
                    } else if (predicate.apply(next, seed) == preCondition) {
                        result.add(next);
                        next = (hasNext = elements.hasNext()) ? elements.nextChar() : 0;
                    } else {
                        if (seedUpdate != null) {
                            seedUpdate.accept(seed);
                        }

                        break;
                    }
                }

                return result;
            }

        }, closeHandlers);
    }

    @Override
    public Stream<CharList> slidingToList(final int windowSize, final int increment) {
        N.checkArgument(windowSize > 0 && increment > 0, "'windowSize'=%s and 'increment'=%s must not be less than 1", windowSize, increment);

        return new IteratorStream<>(new ObjIteratorEx<CharList>() {
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

        }, closeHandlers);
    }

    @Override
    public CharStream sorted() {
        if (sorted) {
            return this;
        }

        return new IteratorCharStream(new CharIteratorEx() {
            char[] a = null;
            int toIndex = 0;
            int cursor = 0;

            @Override
            public boolean hasNext() {
                if (a == null) {
                    sort();
                }

                return cursor < toIndex;
            }

            @Override
            public char nextChar() {
                if (a == null) {
                    sort();
                }

                if (cursor >= toIndex) {
                    throw new NoSuchElementException();
                }

                return a[cursor++];
            }

            @Override
            public long count() {
                if (a == null) {
                    sort();
                }

                return toIndex - cursor;
            }

            @Override
            public void skip(long n) {
                if (a == null) {
                    sort();
                }

                cursor = n < toIndex - cursor ? cursor + (int) n : toIndex;
            }

            @Override
            public char[] toArray() {
                if (a == null) {
                    sort();
                }

                if (cursor == 0) {
                    return a;
                } else {
                    return N.copyOfRange(a, cursor, toIndex);
                }
            }

            private void sort() {
                a = elements.toArray();
                toIndex = a.length;

                N.sort(a);
            }
        }, true, closeHandlers);
    }

    @Override
    public CharStream peek(final CharConsumer action) {
        return new IteratorCharStream(new CharIteratorEx() {
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
        }, sorted, closeHandlers);
    }

    @Override
    public CharStream limit(final long maxSize) {
        if (maxSize < 0) {
            throw new IllegalArgumentException("'maxSize' can't be negative: " + maxSize);
        }

        return new IteratorCharStream(new CharIteratorEx() {
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
        }, sorted, closeHandlers);
    }

    @Override
    public CharStream skip(final long n) {
        if (n < 0) {
            throw new IllegalArgumentException("The skipped number can't be negative: " + n);
        } else if (n == 0) {
            return this;
        }

        return new IteratorCharStream(new CharIteratorEx() {
            private boolean skipped = false;

            @Override
            public boolean hasNext() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.hasNext();
            }

            @Override
            public char nextChar() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.nextChar();
            }

            @Override
            public long count() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.count();
            }

            @Override
            public void skip(long n2) {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                elements.skip(n2);
            }

            @Override
            public char[] toArray() {
                if (skipped == false) {
                    elements.skip(n);
                    skipped = true;
                }

                return elements.toArray();
            }
        }, sorted, closeHandlers);
    }

    @Override
    public void forEach(CharConsumer action) {
        while (elements.hasNext()) {
            action.accept(elements.nextChar());
        }
    }

    @Override
    public char[] toArray() {
        return elements.toArray();
    }

    @Override
    public CharList toCharList() {
        return CharList.of(toArray());
    }

    @Override
    public List<Character> toList() {
        final List<Character> result = new ArrayList<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public <R extends List<Character>> R toList(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public Set<Character> toSet() {
        final Set<Character> result = new HashSet<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public <R extends Set<Character>> R toSet(Supplier<R> supplier) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset() {
        final Multiset<Character> result = new Multiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public Multiset<Character> toMultiset(Supplier<? extends Multiset<Character>> supplier) {
        final Multiset<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset() {
        final LongMultiset<Character> result = new LongMultiset<>();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public LongMultiset<Character> toLongMultiset(Supplier<? extends LongMultiset<Character>> supplier) {
        final LongMultiset<Character> result = supplier.get();

        while (elements.hasNext()) {
            result.add(elements.nextChar());
        }

        return result;
    }

    @Override
    public <K, U, M extends Map<K, U>> M toMap(CharFunction<? extends K> keyExtractor, CharFunction<? extends U> valueMapper, BinaryOperator<U> mergeFunction,
            Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        char element = 0;

        while (elements.hasNext()) {
            element = elements.nextChar();
            Collectors.merge(result, keyExtractor.apply(element), valueMapper.apply(element), mergeFunction);
        }

        return result;
    }

    @Override
    public <K, A, D, M extends Map<K, D>> M toMap(final CharFunction<? extends K> classifier, final Collector<Character, A, D> downstream,
            final Supplier<M> mapFactory) {
        final M result = mapFactory.get();
        final Supplier<A> downstreamSupplier = downstream.supplier();
        final BiConsumer<A, Character> downstreamAccumulator = downstream.accumulator();
        final Map<K, A> intermediate = (Map<K, A>) result;
        K key = null;
        A v = null;
        char element = 0;

        while (elements.hasNext()) {
            element = elements.nextChar();
            key = N.requireNonNull(classifier.apply(element), "element cannot be mapped to a null key");

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
    }

    @Override
    public char reduce(char identity, CharBinaryOperator op) {
        char result = identity;

        while (elements.hasNext()) {
            result = op.applyAsChar(result, elements.nextChar());
        }

        return result;
    }

    @Override
    public OptionalChar reduce(CharBinaryOperator op) {
        if (elements.hasNext() == false) {
            return OptionalChar.empty();
        }

        char result = elements.nextChar();

        while (elements.hasNext()) {
            result = op.applyAsChar(result, elements.nextChar());
        }

        return OptionalChar.of(result);
    }

    @Override
    public <R> R collect(Supplier<R> supplier, ObjCharConsumer<R> accumulator, BiConsumer<R, R> combiner) {
        final R result = supplier.get();

        while (elements.hasNext()) {
            accumulator.accept(result, elements.nextChar());
        }

        return result;
    }

    @Override
    public OptionalChar head() {
        if (head == null) {
            head = elements.hasNext() ? OptionalChar.of(elements.nextChar()) : OptionalChar.empty();
            tail = new IteratorCharStream(elements, sorted, closeHandlers);
        }

        return head;
    }

    @Override
    public CharStream tail() {
        if (tail == null) {
            head = elements.hasNext() ? OptionalChar.of(elements.nextChar()) : OptionalChar.empty();
            tail = new IteratorCharStream(elements, sorted, closeHandlers);
        }

        return tail;
    }

    @Override
    public CharStream head2() {
        if (head2 == null) {
            final char[] a = elements.toArray();
            head2 = new ArrayCharStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalChar.empty() : OptionalChar.of(a[a.length - 1]);
        }

        return head2;
    }

    @Override
    public OptionalChar tail2() {
        if (tail2 == null) {
            final char[] a = elements.toArray();
            head2 = new ArrayCharStream(a, 0, a.length == 0 ? 0 : a.length - 1, sorted, closeHandlers);
            tail2 = a.length == 0 ? OptionalChar.empty() : OptionalChar.of(a[a.length - 1]);
        }

        return tail2;
    }

    @Override
    public OptionalChar min() {
        if (elements.hasNext() == false) {
            return OptionalChar.empty();
        } else if (sorted) {
            return OptionalChar.of(elements.nextChar());
        }

        char candidate = elements.nextChar();
        char next = 0;

        while (elements.hasNext()) {
            next = elements.nextChar();

            if (N.compare(next, candidate) < 0) {
                candidate = next;
            }
        }

        return OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar max() {
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

            if (N.compare(next, candidate) > 0) {
                candidate = next;
            }
        }

        return OptionalChar.of(candidate);
    }

    @Override
    public OptionalChar kthLargest(int k) {
        N.checkArgument(k > 0, "'k' must be bigger than 0");

        if (elements.hasNext() == false) {
            return OptionalChar.empty();
        }

        final NullabLe<Character> optional = boxed().kthLargest(k, CHAR_COMPARATOR);

        return optional.isPresent() ? OptionalChar.of(optional.get()) : OptionalChar.empty();
    }

    @Override
    public long sum() {
        long result = 0;

        while (elements.hasNext()) {
            result += elements.nextChar();
        }

        return result;
    }

    @Override
    public OptionalDouble average() {
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
    }

    @Override
    public long count() {
        return elements.count();
    }

    @Override
    public CharSummaryStatistics summarize() {
        final CharSummaryStatistics result = new CharSummaryStatistics();

        while (elements.hasNext()) {
            result.accept(elements.nextChar());
        }

        return result;
    }

    @Override
    public boolean anyMatch(CharPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextChar())) {
                return true;
            }
        }

        return false;
    }

    @Override
    public boolean allMatch(CharPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextChar()) == false) {
                return false;
            }
        }

        return true;
    }

    @Override
    public boolean noneMatch(CharPredicate predicate) {
        while (elements.hasNext()) {
            if (predicate.test(elements.nextChar())) {
                return false;
            }
        }

        return true;
    }

    @Override
    public OptionalChar findFirst(CharPredicate predicate) {
        while (elements.hasNext()) {
            char e = elements.nextChar();

            if (predicate.test(e)) {
                return OptionalChar.of(e);
            }
        }

        return OptionalChar.empty();
    }

    @Override
    public OptionalChar findLast(CharPredicate predicate) {
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
    }

    @Override
    public IntStream asIntStream() {
        return new IteratorIntStream(new IntIteratorEx() {
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
        }, sorted, closeHandlers);
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
    public CharStream parallel(int maxThreadNum, com.landawn.abacus.util.stream.BaseStream.Splitor splitor) {
        if (maxThreadNum < 1 || maxThreadNum > MAX_THREAD_NUM_PER_OPERATION) {
            throw new IllegalArgumentException("'maxThreadNum' must not less than 1 or exceeded: " + MAX_THREAD_NUM_PER_OPERATION);
        }

        return new ParallelIteratorCharStream(elements, sorted, maxThreadNum, splitor, closeHandlers);
    }

    @Override
    public CharStream onClose(Runnable closeHandler) {
        final Set<Runnable> newCloseHandlers = new AbstractStream.LocalLinkedHashSet<>(N.isNullOrEmpty(this.closeHandlers) ? 1 : this.closeHandlers.size() + 1);

        if (N.notNullOrEmpty(this.closeHandlers)) {
            newCloseHandlers.addAll(this.closeHandlers);
        }

        newCloseHandlers.add(closeHandler);

        return new IteratorCharStream(elements, sorted, newCloseHandlers);
    }
}
